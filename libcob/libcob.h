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

/* INSPECT options */
#define INSPECT_END		0
#define INSPECT_CHARACTERS 	1
#define INSPECT_ALL        	2
#define INSPECT_LEADING    	3
#define INSPECT_FIRST      	4
#define INSPECT_CONVERTING     	5
#define INSPECT_BEFORE      	6
#define INSPECT_AFTER      	7

/* STRING options */
#define STRING_END		0
#define STRING_CONCATENATE	1
#define STRING_DELIMITED_NAME	2
#define STRING_DELIMITED_SIZE	3
#define STRING_WITH_POINTER	4

/* UNSTRING options */
#define UNSTRING_END		0
#define UNSTRING_INTO		1
#define UNSTRING_DELIMITER	2
#define UNSTRING_COUNT		3
#define UNSTRING_DELIMITED_BY	4
#define UNSTRING_DELIMITED_ALL	5
#define UNSTRING_WITH_POINTER	6
#define UNSTRING_TALLYING	7


/*
 * Field structure
 */

struct cob_field {
  struct cob_field_desc
  {
    unsigned long size;
    char type;
    char digits;
    char decimals;
    char have_sign     : 1;
    char separate_sign : 1;
    char leading_sign  : 1;
    char blank_zero    : 1;
    char just_r        : 1;
    char *pic;
  } *desc;
  unsigned char *data;
};

#define FIELD_TYPE(f)		((f).desc->type)
#define FIELD_SIZE(f)		((f).desc->size)
#define FIELD_DECIMALS(f)	((f).desc->decimals)
#define FIELD_DATA(f)		((f).data)
#define FIELD_BASE(f) \
  ((f).data + (((f).desc->separate_sign && (f).desc->leading_sign) ? 1 : 0))
#define FIELD_LENGTH(f) \
  ((f).desc->size - ((f).desc->separate_sign ? 1 : 0))

#define FIELD_SIGNED_P(f)	((f).desc->have_sign)
#define FIELD_NUMERIC_P(x)				\
  ({							\
    int _t = FIELD_TYPE (x);				\
    (_t == '9' || _t == 'B' || _t == 'C');		\
  })


/*
 * File structure
 */

struct cob_file_desc
{
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
  struct altkey_desc
  {
    short int offset;		/* offset of alternate key field in record */
    struct cob_field_desc *descriptor;	/* descriptor for this field */
    short int duplicates;		/* = 1 if duplicates allowed */
    DB *alt_dbp;			/* handle for the alternate key file */
  } *altkeys;
};


/* reference modification */

#define cob_ref(var,off,len) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (len); var; })

#define cob_ref_rest(var,off,siz) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (siz) - cob_ref_off; var; })

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

/* command line arguments */

extern int cob_argc;
extern char **cob_argv;

/* source location */

extern char *cob_source_file;
extern int cob_source_line;

/* operation status */

extern int cob_status;

#define COB_STATUS_SUCCESS	0
#define COB_STATUS_OVERFLOW	1

/* environment variables */

extern unsigned char cob_decimal_point;
extern unsigned char cob_currency_symbol;
#define cob_numeric_separator ((cob_decimal_point == '.') ? ',' : '.')

extern int cob_reloading_flag;

extern long cob_exp10[10];
extern long long cob_exp10LL[19];

/* figurative constants */

extern struct cob_field cob_zero;
extern struct cob_field cob_space;
extern struct cob_field cob_high;
extern struct cob_field cob_low;
extern struct cob_field cob_quote;

/* functional macros */

#define cob_cmp(x,y) ((x) - (y))

#define cob_exit_program() return 0

/* functions */

extern void cob_init (int argc, char **argv);
extern void cob_stop_run (void);
extern void cob_runtime_error (char *fmt, ...);

extern int cob_index (int i, int max);
extern void cob_check_numeric (struct cob_field f);

extern int get_sign (struct cob_field f);
extern void put_sign (struct cob_field f, int sign);
extern char *cob_field_to_string (struct cob_field f, char *s);

extern void cob_display (struct cob_field f);
extern void cob_newline (void);
extern void cob_move (struct cob_field f1, struct cob_field f2);
extern void cob_mem_move (struct cob_field dst, unsigned char *src, int len);
extern int cob_to_int (struct cob_field f);

extern void cob_push_int (int n, int decimals);
extern void cob_push_decimal (struct cob_field f);
extern void cob_num_add (void);
extern void cob_add_int (struct cob_field f, int n);
extern void cob_set (struct cob_field f);
extern void cob_set_int (struct cob_field f, int n);
extern int cob_num_cmp (void);
extern int cob_str_cmp (struct cob_field f1, struct cob_field f2);

extern void cob_set_library_path (const char *path);
extern void *cob_resolve (const char *name);
extern const char *cob_resolve_error (void);
extern void *cob_call_resolve (struct cob_field f);
extern void cob_call_error (void);
extern void cob_cancel (struct cob_field f);

#endif /* _LIBCOB_H_ */
