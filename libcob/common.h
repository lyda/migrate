/*
 * Copyright (C) 2002 Keisuke Nishida
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


/* COBOL 2002 exceptions */

#define COB_EC_ARGUMENT			0x0100
#define COB_EC_ARGUMENT_FUNCTION	0x0101
#define COB_EC_ARGUMENT_IMP		0x0102
#define COB_EC_BOUND			0x0200
#define COB_EC_BOUND_IMP		0x0201
#define COB_EC_BOUND_ODO		0x0202
#define COB_EC_BOUND_PTR		0x0203
#define COB_EC_BOUND_REF_MOD		0x0204
#define COB_EC_BOUND_SUBSCRIPT		0x0205
#define COB_EC_DATA			0x0300
#define COB_EC_DATA_CONVERSION		0x0301
#define COB_EC_DATA_IMP			0x0302
#define COB_EC_DATA_INCOMPATIBLE	0x0303
#define COB_EC_DATA_PTR_NULL		0x0304
#define COB_EC_FLOW			0x0400
#define COB_EC_FLOW_GLOBAL_EXIT		0x0401
#define COB_EC_FLOW_GLOBAL_GOBACK	0x0402
#define COB_EC_FLOW_IMP			0x0403
#define COB_EC_FLOW_RELEASE		0x0404
#define COB_EC_FLOW_REPORT		0x0405
#define COB_EC_FLOW_RETURN		0x0406
#define COB_EC_FLOW_USE			0x0407
#define COB_EC_I_O			0x0500
#define COB_EC_I_O_AT_END		0x0501
#define COB_EC_I_O_EOP			0x0502
#define COB_EC_I_O_EOP_OVERFLOW		0x0503
#define COB_EC_I_O_FILE_SHARING		0x0504
#define COB_EC_I_O_IMP			0x0505
#define COB_EC_I_O_INVALID_KEY		0x0506
#define COB_EC_I_O_LINAGE		0x0507
#define COB_EC_I_O_LOGIC_ERROR		0x0508
#define COB_EC_I_O_PERMANENT_ERROR	0x0509
#define COB_EC_I_O_RECORD_OPERATION	0x050a
#define COB_EC_IMP			0x0600
#define COB_EC_LOCATE			0x0700
#define COB_EC_LOCATE_IMP		0x0701
#define COB_EC_LOCATE_INCOMPATIBLE	0x0702
#define COB_EC_LOCATE_INVALID		0x0703
#define COB_EC_LOCATE_INVALID_PTR	0x0704
#define COB_EC_LOCATE_MISSING		0x0705
#define COB_EC_LOCATE_SIZE		0x0706
#define COB_EC_OO			0x0800
#define COB_EC_OO_CONFORMANCE		0x0801
#define COB_EC_OO_EXCEPTION		0x0802
#define COB_EC_OO_IMP			0x0803
#define COB_EC_OO_METHOD		0x0804
#define COB_EC_OO_NULL			0x0805
#define COB_EC_OO_RESOURCE		0x0806
#define COB_EC_OO_UNIVERSAL		0x0807
#define COB_EC_ORDER			0x0900
#define COB_EC_ORDER_IMP		0x0901
#define COB_EC_ORDER_NOT_SUPPORTED	0x0902
#define COB_EC_OVERFLOW			0x0a00
#define COB_EC_OVERFLOW_IMP		0x0a01
#define COB_EC_OVERFLOW_STRING		0x0a02
#define COB_EC_OVERFLOW_UNSTRING	0x0a03
#define COB_EC_PROGRAM			0x0b00
#define COB_EC_PROGRAM_ARG_MISMATCH	0x0b01
#define COB_EC_PROGRAM_ARG_OMITTED	0x0b02
#define COB_EC_PROGRAM_CANCEL_ACTIVE	0x0b03
#define COB_EC_PROGRAM_IMP		0x0b04
#define COB_EC_PROGRAM_NOT_FOUND	0x0b05
#define COB_EC_PROGRAM_PTR_NULL		0x0b06
#define COB_EC_PROGRAM_RECURSIVE_CALL	0x0b07
#define COB_EC_PROGRAM_RESOURCES	0x0b08
#define COB_EC_RAISING			0x0c00
#define COB_EC_RAISING_IMP		0x0c01
#define COB_EC_RAISING_NOT_SPECIFIED	0x0c02
#define COB_EC_RANGE			0x0d00
#define COB_EC_RANGE_IMP		0x0d01
#define COB_EC_RANGE_INDEX		0x0d02
#define COB_EC_RANGE_INSPECT_SIZE	0x0d03
#define COB_EC_RANGE_INVALID		0x0d04
#define COB_EC_RANGE_PERFORM_VARYING	0x0d05
#define COB_EC_RANGE_PTR		0x0d06
#define COB_EC_RANGE_SEARCH_INDEX	0x0d07
#define COB_EC_RANGE_SEARCH_NO_MATCH	0x0d08
#define COB_EC_REPORT			0x0e00
#define COB_EC_REPORT_ACTIVE		0x0e01
#define COB_EC_REPORT_COLUMN_OVERLAP	0x0e02
#define COB_EC_REPORT_FILE_MODE		0x0e03
#define COB_EC_REPORT_IMP		0x0e04
#define COB_EC_REPORT_INACTIVE		0x0e05
#define COB_EC_REPORT_LINE_OVERLAP	0x0e06
#define COB_EC_REPORT_NOT_TERMINATED	0x0e08
#define COB_EC_REPORT_PAGE_LIMIT	0x0e09
#define COB_EC_REPORT_PAGE_WIDTH	0x0e0a
#define COB_EC_REPORT_SUM_SIZE		0x0e0b
#define COB_EC_REPORT_VARYING		0x0e0c
#define COB_EC_SCREEN			0x0f00
#define COB_EC_SCREEN_FIELD_OVERLAP	0x0f01
#define COB_EC_SCREEN_IMP		0x0f02
#define COB_EC_SCREEN_ITEM_TRUNCATED	0x0f03
#define COB_EC_SCREEN_LINE_NUMBER	0x0f04
#define COB_EC_SCREEN_STARTING_COLUMN	0x0f05
#define COB_EC_SIZE			0x1000
#define COB_EC_SIZE_ADDRESS		0x1001
#define COB_EC_SIZE_EXPONENTIATION	0x1002
#define COB_EC_SIZE_IMP			0x1003
#define COB_EC_SIZE_OVERFLOW		0x1004
#define COB_EC_SIZE_TRUNCATION		0x1005
#define COB_EC_SIZE_UNDERFLOW		0x1006
#define COB_EC_SIZE_ZERO_DIVIDE		0x1007
#define COB_EC_SORT_MERGE		0x1100
#define COB_EC_SORT_MERGE_ACTIVE	0x1101
#define COB_EC_SORT_MERGE_FILE_OPEN	0x1102
#define COB_EC_SORT_MERGE_IMP		0x1103
#define COB_EC_SORT_MERGE_RELEASE	0x1104
#define COB_EC_SORT_MERGE_RETURN	0x1105
#define COB_EC_SORT_MERGE_SEQUENCE	0x1106
#define COB_EC_STORAGE			0x1200
#define COB_EC_STORAGE_IMP		0x1201
#define COB_EC_STORAGE_NOT_ALLOC	0x1202
#define COB_EC_STORAGE_NOT_AVAIL	0x1203
#define COB_EC_USER			0x1300
#define COB_EC_VALIDATE			0x1400
#define COB_EC_VALIDATE_CONTENT		0x1401
#define COB_EC_VALIDATE_FORMAT		0x1402
#define COB_EC_VALIDATE_IMP		0x1403
#define COB_EC_VALIDATE_RELATION	0x1404
#define COB_EC_VALIDATE_VARYING		0x1405


/*
 * Field
 */

/* field types */

#define COB_TYPE_UNKNOWN		0x00
#define COB_TYPE_GROUP			0x01
#define COB_TYPE_BOOLEAN		0x02

#define COB_TYPE_NUMERIC		0x10
#define COB_TYPE_NUMERIC_DISPLAY	0x10
#define COB_TYPE_NUMERIC_BINARY		0x11
#define COB_TYPE_NUMERIC_PACKED		0x12
#define COB_TYPE_NUMERIC_EDITED		0x24

#define COB_TYPE_ALPHABETIC		0x20
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
#define COB_FLAG_JUSTFIED		0x10

#define COB_FIELD_HAVE_SIGN(f)	    ((f)->attr->flags & COB_FLAG_HAVE_SIGN)
#define COB_FIELD_SIGN_SEPARATE(f)  ((f)->attr->flags & COB_FLAG_SIGN_SEPARATE)
#define COB_FIELD_SIGN_LEADING(f)   ((f)->attr->flags & COB_FLAG_SIGN_LEADING)
#define COB_FIELD_BLANK_ZERO(f)	    ((f)->attr->flags & COB_FLAG_BLANK_ZERO)
#define COB_FIELD_JUSTIFIED(f)	    ((f)->attr->flags & COB_FLAG_JUSTFIED)

/* field attributes */

typedef struct {
  char type;
  char digits;
  char decimals;
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
#define COB_FIELD_DECIMALS(f)	((f)->attr->decimals)
#define COB_FIELD_DATA(f)						  \
  ((f)->data +								  \
   ((COB_FIELD_SIGN_SEPARATE (f) && COB_FIELD_SIGN_LEADING (f)) ? 1 : 0))
#define COB_FIELD_SIZE(f)						\
  ((f)->size - (COB_FIELD_SIGN_SEPARATE (f) ? 1 : 0))

#define COB_FIELD_IS_NUMERIC(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NUMERIC)



extern int cob_initialized;

extern int cob_error_code;
extern int cob_return_code;
extern int cob_cmp_result;

/* command line arguments */

extern int cob_argc;
extern char **cob_argv;

/* source location */

extern char *cob_source_file;
extern int cob_source_line;

/* environment variables */

typedef struct __cob_environment {
  unsigned char decimal_point;
  unsigned char currency_symbol;
  unsigned char numeric_separator;
  struct __cob_environment *next;
} cob_environment;

extern cob_environment *cob_env;

/* constants */

extern cob_field_attr cob_group_attr;	/* group item */
extern cob_field_attr cob_alnum_attr;	/* PIC X(n) */
extern cob_field_attr cob_just_attr;	/* PIC X(n) JUSTIFIED RIGHT */
extern cob_field_attr cob_uint_attr[];	/* PIC 9(n) */
extern cob_field_attr cob_sint_attr[];	/* PIC S9(n) */
extern cob_field_attr cob_ubin_attr[];	/* PIC 9(n) BINARY */
extern cob_field_attr cob_sbin_attr[];	/* PIC S9(n) BINARY */
extern cob_field_attr cob_all_attr;	/* ALL <literal> */

extern cob_field cob_zero;		/* ZERO */
extern cob_field cob_space;		/* SPACE */
extern cob_field cob_high;		/* HIGH-VALUE */
extern cob_field cob_low;		/* LOW-VALUE */
extern cob_field cob_quote;		/* QUOTE */
extern int cob_switch[];

extern long cob_exp10[];
extern long long cob_exp10LL[];

extern void cob_init (int argc, char **argv);
extern void cob_module_init (void);
extern void cob_stop_run (void);
extern int cob_index (int i, int max, const char *name);
extern int cob_index_depending (int i, int min, int max, int dep, const char *name, const char *depname);

extern int cob_cmp (cob_field *f1, cob_field *f2);
extern int cob_cmp_int (cob_field *f1, int n);

extern void cob_check_numeric (cob_field *f, const char *name);
extern int cob_is_numeric (cob_field *f);
extern int cob_is_alpha (cob_field *f);
extern int cob_is_upper (cob_field *f);
extern int cob_is_lower (cob_field *f);

extern int cob_get_sign (cob_field *f);
extern void cob_put_sign (cob_field *f, int sign);
extern char *cob_field_to_string (cob_field *f, char *s);
extern void cob_runtime_error (char *fmt, ...);
extern const char *cob_config_lookup (const char *key);
extern int cob_config_compare (const char *key, const char *val);

#endif /* COB_COMMON_H_ */
