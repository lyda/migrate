/*
 * Copyright (C) 2002-2003 Keisuke Nishida
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

#ifndef COB_COMMON_H
#define COB_COMMON_H

#include <stdio.h>


/*
 * Field
 */

/* field types */

#define COB_TYPE_UNKNOWN		0x00
#define COB_TYPE_GROUP			0x01
#define COB_TYPE_BOOLEAN		0x02
#define COB_TYPE_POINTER		0x03

#define COB_TYPE_NUMERIC		0x10
#define COB_TYPE_NUMERIC_DISPLAY	0x10
#define COB_TYPE_NUMERIC_BINARY		0x11
#define COB_TYPE_NUMERIC_PACKED		0x12
#define COB_TYPE_NUMERIC_FLOAT		0x13
#define COB_TYPE_NUMERIC_EDITED		0x24

#define COB_TYPE_ALPHANUMERIC		0x21
#define COB_TYPE_ALPHANUMERIC_ALL	0x22
#define COB_TYPE_ALPHANUMERIC_EDITED	0x23

#define COB_TYPE_NATIONAL		0x40
#define COB_TYPE_NATIONAL_EDITED	0x41

/* field flags */

#define COB_FLAG_HAVE_SIGN		0x01
#define COB_FLAG_SIGN_SEPARATE		0x02
#define COB_FLAG_SIGN_LEADING		0x04
#define COB_FLAG_BLANK_ZERO		0x08
#define COB_FLAG_JUSTIFIED		0x10
#define COB_FLAG_BINARY_SWAP		0x20

#define COB_FIELD_HAVE_SIGN(f)	    ((f)->attr->flags & COB_FLAG_HAVE_SIGN)
#define COB_FIELD_SIGN_SEPARATE(f)  ((f)->attr->flags & COB_FLAG_SIGN_SEPARATE)
#define COB_FIELD_SIGN_LEADING(f)   ((f)->attr->flags & COB_FLAG_SIGN_LEADING)
#define COB_FIELD_BLANK_ZERO(f)	    ((f)->attr->flags & COB_FLAG_BLANK_ZERO)
#define COB_FIELD_JUSTIFIED(f)	    ((f)->attr->flags & COB_FLAG_JUSTIFIED)
#define COB_FIELD_BINARY_SWAP(f)    ((f)->attr->flags & COB_FLAG_BINARY_SWAP)

/* field attributes */

typedef struct {
  char type;
  char digits;
  char scale;
  char flags;
  const char *pic;
} cob_field_attr;

/* field structure */

typedef struct {
  size_t size;
  unsigned char *data;
  cob_field_attr *attr;
} cob_field;

#define COB_FIELD_TYPE(f)	((f)->attr->type)
#define COB_FIELD_DIGITS(f)	((f)->attr->digits)
#define COB_FIELD_SCALE(f)	((f)->attr->scale)
#define COB_FIELD_DATA(f)						  \
  ((f)->data +								  \
   ((COB_FIELD_SIGN_SEPARATE (f) && COB_FIELD_SIGN_LEADING (f)) ? 1 : 0))
#define COB_FIELD_SIZE(f)						\
  ((f)->size - (COB_FIELD_SIGN_SEPARATE (f) ? 1 : 0))

#define COB_FIELD_IS_NUMERIC(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NUMERIC)


/*
 * Module
 */

enum cob_display_sign {
  COB_DISPLAY_SIGN_ASCII,
  COB_DISPLAY_SIGN_EBCDIC,
  COB_DISPLAY_SIGN_ASCII10,
  COB_DISPLAY_SIGN_ASCII20,
};

typedef struct __cob_module {
  unsigned char decimal_point;
  unsigned char currency_symbol;
  unsigned char numeric_separator;
  const unsigned char *collating_sequence;
  enum cob_display_sign display_sign;
  unsigned long flag_filename_mapping : 1;
  unsigned long flag_binary_truncate : 1;
  unsigned long flag_pretty_display : 1;
  struct __cob_module *next;
} cob_module;

extern cob_module *cob_current_module;


/*
 * Exception
 */

/* Exception identifier */
enum cob_exception_id {
  COB_EC_ZERO,
#undef COB_EXCEPTION
#define COB_EXCEPTION(CODE,TAG,NAME,CRITICAL) TAG,
#include <libcob/exception.def>
  COB_EC_MAX
};

/* Exception data table */
extern struct cob_exception {
  int code;
  const char *name;
  int critical;
} cob_exception_table[];

/* Set cob_exception_code from an exception id */
#define COB_SET_EXCEPTION(id) \
  cob_exception_code = cob_exception_table[id].code

extern int cob_exception_code;


/*
 * Global variables
 */

extern int cob_initialized;

extern int cob_argc;
extern char **cob_argv;

extern const char *cob_source_file;
extern unsigned int cob_source_line;
extern const char *cob_source_statement;

extern int cob_linage_counter;

extern cob_field cob_zero;		/* ZERO */
extern cob_field cob_space;		/* SPACE */
extern cob_field cob_high;		/* HIGH-VALUE */
extern cob_field cob_low;		/* LOW-VALUE */
extern cob_field cob_quote;		/* QUOTE */

extern const long cob_exp10[];
extern const long long cob_exp10LL[];
extern const unsigned char cob_a2e[];	/* ASCII -> EBCDIC */
extern const unsigned char cob_e2a[];	/* EBCDIC -> ASCII */


/* convert a digit (e.g., '0') into an integer (e.g., 0) */
#define cob_d2i(x)	((x) - '0')

/* convert an integer (e.g., 0) into a digit (e.g., '0') */
#define cob_i2d(x)	((x) + '0')


/*
 * Function declaration
 */

/* General functions */

extern void cob_init (int argc, char **argv);
extern void cob_module_enter (cob_module *module);
extern void cob_module_leave (cob_module *module);
extern void cob_stop_run (int status);

/* Utilities */

#define cob_get_sign(f) (COB_FIELD_HAVE_SIGN (f) ? cob_real_get_sign (f) : 0)
#define cob_put_sign(f,s) if (COB_FIELD_HAVE_SIGN (f)) cob_real_put_sign (f, s)

extern int cob_real_get_sign (cob_field *f);
extern void cob_real_put_sign (cob_field *f, int sign);
extern char *cob_field_to_string (cob_field *f, char *s);

/* Switch */

extern int cob_get_switch (int n);
extern void cob_set_switch (int n, int flag);

/* Comparison */

extern int cob_cmp (cob_field *f1, cob_field *f2);
extern int cob_cmp_int (cob_field *f1, int n);

/* Class check */

extern int cob_is_numeric (cob_field *f);
extern int cob_is_alpha (cob_field *f);
extern int cob_is_upper (cob_field *f);
extern int cob_is_lower (cob_field *f);

/* Run-time error checking */

extern void cob_runtime_error (const char *fmt, ...);
extern void cob_check_numeric (cob_field *f, const char *name);
extern void cob_check_odo (int i, int min, int max, const char *name);
extern void cob_check_subscript (int i, int min, int max, const char *name);
extern void cob_check_ref_mod (int offset, int length, int size, const char *name);

#endif /* COB_COMMON_H_ */
