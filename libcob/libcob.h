/*
 * Copyright (C) 2001-2002 Keisuke Nishida
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#ifndef _LIBCOB_H_
#define _LIBCOB_H_

#include <stdio.h>
#include <stdlib.h>
#include <db1/db.h>
#include <gmp.h>


/*
 * Field structure
 */

#define COB_VOID		 0
#define COB_ALPHABETIC		'A'
#define COB_NUMERIC		'9'
#define COB_NUMERIC_EDITED	'0'
#define COB_ALPHANUMERIC	'X'
#define COB_ALPHANUMERIC_EDITED	'E'
#define COB_NATIONAL		'N'
#define COB_NATIONAL_EDITED	'M'
#define COB_BOOLEAN		'1'

#define COB_BINARY		'B'
#define COB_PACKED		'C'

struct cob_field {
  struct cob_field_desc {
    unsigned long size;
    char type;
    char digits;
    char decimals;
    char have_sign     : 1;
    char sign_separate : 1;
    char sign_leading  : 1;
    char blank_zero    : 1;
    char justified     : 1;
    char *pic;
  } *desc;
  unsigned char *data;
};

#define COB_FIELD_TYPE(f)	((f).desc->type)
#define COB_FIELD_SIZE(f)	((f).desc->size)
#define COB_FIELD_DECIMALS(f)	((f).desc->decimals)
#define COB_FIELD_DATA(f)	((f).data)
#define COB_FIELD_BASE(f) \
  ((f).data + (((f).desc->sign_separate && (f).desc->sign_leading) ? 1 : 0))
#define COB_FIELD_LENGTH(f) \
  ((f).desc->size - ((f).desc->sign_separate ? 1 : 0))

#define COB_FIELD_SIGNED_P(f)	((f).desc->have_sign)
#define COB_FIELD_NUMERIC_P(x)				\
  ({							\
    int _t = COB_FIELD_TYPE (x);			\
    (_t == '9' || _t == 'B' || _t == 'C');		\
  })


/*
 * Supporting macros
 */

/* frame stack */

struct cob_frame {
  int perform_through;
  void *return_address;
};

#define cob_perform(label,from,until)			\
  frame_index++;					\
  frame_stack[frame_index].perform_through = until;	\
  frame_stack[frame_index].return_address = &&label;	\
  goto from;						\
  label:						\
  frame_index--

#define cob_exit(label)					\
 if (frame_stack[frame_index].perform_through == label)	\
   goto *frame_stack[frame_index].return_address

/* reference modification */

#define cob_ref(var,off,len) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (len); var; })

#define cob_ref_rest(var,off,siz) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (siz) - cob_ref_off; var; })

/* miscellaneous macros */

#define COB_INDEX(i,max) ((i) - 1)

#define cob_cmp(x,y) ((x) - (y))

#define cob_exit_program() goto l_exit;


/* common.c */

/* command line arguments */

extern int cob_argc;
extern char **cob_argv;

/* source location */

extern char *cob_source_file;
extern int cob_source_line;

/* environment variables */

extern unsigned char cob_decimal_point;
extern unsigned char cob_currency_symbol;
#define cob_numeric_separator ((cob_decimal_point == '.') ? ',' : '.')

/* operation status */

extern int cob_status;

#define COB_STATUS_SUCCESS	0
#define COB_STATUS_OVERFLOW	1

/* constants */

extern struct cob_field cob_zero;
extern struct cob_field cob_space;
extern struct cob_field cob_high;
extern struct cob_field cob_low;
extern struct cob_field cob_quote;
extern struct cob_field cob_return_code;
extern int cob_return_code_value;
extern char cob_switch[];

extern long cob_exp10[10];
extern long long cob_exp10LL[19];

extern void cob_init (int argc, char **argv);
extern void cob_stop_run (void);
extern int cob_index (int i, int max);

extern int cob_str_cmp (struct cob_field f1, struct cob_field f2);
extern int cob_cmp_str (struct cob_field f1, unsigned char *data2, int len2);
extern int cob_cmp_all (unsigned char *data, unsigned char c, int len);

extern void cob_check_numeric (struct cob_field f);
extern int cob_is_numeric (struct cob_field f);
extern int cob_is_alpha (struct cob_field f);
extern int cob_is_upper (struct cob_field f);
extern int cob_is_lower (struct cob_field f);

extern int cob_get_sign (struct cob_field f);
extern void cob_put_sign (struct cob_field f, int sign);
extern char *cob_field_to_string (struct cob_field f, char *s);
extern void cob_runtime_error (char *fmt, ...);


/* move.c */

extern void cob_move (struct cob_field src, struct cob_field dst);
extern void cob_move_alphanum_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_alphanum (struct cob_field f1, struct cob_field f2);
extern void cob_move_alphanum_to_alphanum (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_packed (struct cob_field f1, struct cob_field f2);
extern void cob_move_packed_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_binary (struct cob_field f1, struct cob_field f2);
extern void cob_move_binary_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_edited (struct cob_field f1, struct cob_field f2);
extern void cob_move_alphanum_to_edited (struct cob_field f1, struct cob_field f2);
extern void cob_mem_move (struct cob_field dst, unsigned char *src, int len);
extern int cob_to_int (struct cob_field f);
extern void cob_set_int (struct cob_field f, int n);


/* math.c */

struct cob_decimal {
  mpz_t number;
  int decimals;
};

typedef struct cob_decimal *cob_decimal;

extern cob_decimal cob_d1, cob_d2, cob_d3, cob_d4, cob_dt;

extern void cob_init_math (void);
extern void cob_decimal_init (cob_decimal d);
extern void cob_decimal_print (cob_decimal d);
extern void cob_decimal_add (cob_decimal d1, cob_decimal d2);
extern void cob_decimal_sub (cob_decimal d1, cob_decimal d2);
extern void cob_decimal_mul (cob_decimal d1, cob_decimal d2);
extern void cob_decimal_div (cob_decimal d1, cob_decimal d2);
extern void cob_decimal_pow (cob_decimal d1, cob_decimal d2);
extern int cob_decimal_cmp (cob_decimal d1, cob_decimal d2);
extern void cob_decimal_set (cob_decimal dst, cob_decimal src);
extern void cob_decimal_set_int (cob_decimal d, int n, int decimals);
extern void cob_decimal_set_int64 (cob_decimal d, long long n, int decimals);
extern void cob_decimal_set_display (cob_decimal d, struct cob_field f);
extern void cob_decimal_set_field (cob_decimal d, struct cob_field f);
extern void cob_decimal_get (cob_decimal d, struct cob_field f);
extern void cob_decimal_get_rounded (cob_decimal d, struct cob_field f);

extern void cob_add_int (struct cob_field f, int n, int decimals, int round);
extern void cob_add_int64 (struct cob_field f, long long n, int decimals, int round);
extern void cob_add (struct cob_field f1, struct cob_field f2, int round);
extern void cob_sub_int (struct cob_field f, int n, int decimals, int round);
extern void cob_sub_int64 (struct cob_field f, long long n, int decimals, int round);
extern void cob_sub (struct cob_field f1, struct cob_field f2, int round);
extern void cob_div (struct cob_field dividend, struct cob_field divisor, struct cob_field quotient, int round);
extern void cob_div_reminder (struct cob_field remainder);


/* basicio.c */

#define COB_STDIN	0
#define COB_STDOUT	1
#define COB_STDERR	2

extern FILE *cob_stream[];

extern void cob_init_basicio (void);
extern void cob_display (struct cob_field f, int fd);
extern void cob_newline (int fd);
extern void cob_debug_print (struct cob_field f);
extern void cob_accept (struct cob_field f);
extern void cob_accept_date (struct cob_field f);
extern void cob_accept_day (struct cob_field f);
extern void cob_accept_day_of_week (struct cob_field f);
extern void cob_accept_time (struct cob_field f);
extern void cob_accept_command_line (struct cob_field f);
extern void cob_accept_environment (struct cob_field f, struct cob_field env);


/* fileio.c */

#define COB_ORG_INDEXED		1
#define COB_ORG_SEQUENTIAL	2
#define COB_ORG_RELATIVE	3
#define COB_ORG_LINE_SEQUENTIAL	4

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

#define FMOD_INPUT 		1
#define FMOD_IO 		2
#define FMOD_OUTPUT 		3
#define FMOD_EXTEND 		4

struct cob_file_desc {
  struct cob_field_desc *filename_desc;
  unsigned char *filename_data;
  signed long reclen;		/* length of record */
  char *record;
  unsigned char organization;
  unsigned char access_mode;
  char *status;
  int open_mode;
  DB *dbp;			/* pointer for libdb operations */
  char *start_record;		/* record for start verb control (Andrew Cameron) */
  int optional:1;
  int file_missing:1;
	/******* from now on, only present for indexed files *********/
  short unsigned rec_index;	/* offset of index field in record */
  struct cob_field_desc *ixd_desc;	/* offset (DGROUP) index field descriptor */
  struct altkey_desc *key_in_use;
  struct altkey_desc {
    short int offset;		/* offset of alternate key field in record */
    struct cob_field_desc *descriptor;	/* descriptor for this field */
    short int duplicates;		/* = 1 if duplicates allowed */
    DB *alt_dbp;			/* handle for the alternate key file */
  } *altkeys;
};


/* string.c */

/* INSPECT options */
#define COB_INSPECT_END			0
#define COB_INSPECT_CHARACTERS		1
#define COB_INSPECT_ALL			2
#define COB_INSPECT_LEADING		3
#define COB_INSPECT_FIRST	      	4
#define COB_INSPECT_CONVERTING     	5
#define COB_INSPECT_BEFORE      	6
#define COB_INSPECT_AFTER		7

/* STRING options */
#define COB_STRING_END			0
#define COB_STRING_CONCATENATE		1
#define COB_STRING_DELIMITED_NAME	2
#define COB_STRING_DELIMITED_SIZE	3
#define COB_STRING_WITH_POINTER		4

/* UNSTRING options */
#define COB_UNSTRING_END		0
#define COB_UNSTRING_INTO		1
#define COB_UNSTRING_DELIMITER		2
#define COB_UNSTRING_COUNT		3
#define COB_UNSTRING_DELIMITED_BY	4
#define COB_UNSTRING_DELIMITED_ALL	5
#define COB_UNSTRING_WITH_POINTER	6
#define COB_UNSTRING_TALLYING		7

extern void cob_inspect_tallying (struct cob_field var, ...);
extern void cob_inspect_replacing (struct cob_field var, ...);
extern void cob_inspect_converting (struct cob_field var, ...);
extern void cob_string (struct cob_field dst, ...);
extern void cob_unstring (struct cob_field src, ...);


/* call.c */

extern void cob_init_call (void);
extern void cob_set_library_path (const char *path);
extern void *cob_resolve (const char *name);
extern const char *cob_resolve_error (void);

extern void *cob_call_resolve (struct cob_field f);
extern void cob_call_error (void);
extern void cob_cancel (struct cob_field f);

#endif /* _LIBCOB_H_ */
