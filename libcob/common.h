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

#define COB_VOID		 0
#define COB_GROUP		'G'
#define COB_ALPHABETIC		'A'
#define COB_NUMERIC		'9'
#define COB_NUMERIC_EDITED	'0'
#define COB_ALPHANUMERIC	'X'
#define COB_ALPHANUMERIC_EDITED	'E'
#define COB_NATIONAL		'N'
#define COB_NATIONAL_EDITED	'M'
#define COB_BOOLEAN		'1'

#define COB_BINARY		'2'
#define COB_PACKED		'3'
#define COB_DISPLAY		'9'

#define COB_ASCENDING		1
#define COB_DESCENDING		2

struct cob_field {
  size_t size;
  unsigned char *data;
  struct cob_field_desc {
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
};

#define COB_FIELD_TYPE(f)	((f).desc ? (f).desc->type : COB_GROUP)
#define COB_FIELD_DECIMALS(f)	((f).desc->decimals)
#define COB_FIELD_BASE(f) \
  ((f).data + (((f).desc && (f).desc->sign_separate && (f).desc->sign_leading) ? 1 : 0))
#define COB_FIELD_LENGTH(f) \
  ((f).size - ((f).desc && (f).desc->sign_separate ? 1 : 0))

#define COB_FIELD_IS_VALID(f)	((f).size > 0)



extern int cob_initialized;

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
extern int cob_return_code;

#define COB_STATUS_SUCCESS	0
#define COB_STATUS_OVERFLOW	1

/* constants */

extern struct cob_field_desc cob_alnum_desc;

extern struct cob_field cob_zero;
extern struct cob_field cob_space;
extern struct cob_field cob_high;
extern struct cob_field cob_low;
extern struct cob_field cob_quote;
extern char cob_switch[];

extern long cob_exp10[10];
extern long long cob_exp10LL[19];

extern void cob_init (int argc, char **argv);
extern void cob_module_init (void);
extern void cob_stop_run (void);
extern int cob_index (int i, int max, const char *name);
extern int cob_index_depending (int i, int min, int max, int dep, const char *name, const char *depname);

extern int cob_cmp_field (struct cob_field f1, struct cob_field f2);
extern int cob_cmp_str (struct cob_field f1, unsigned char *data2, size_t size2);
extern int cob_cmp_all (struct cob_field f1, unsigned char *str);

extern void cob_check_numeric (struct cob_field f, const char *name);
extern int cob_is_numeric (struct cob_field f);
extern int cob_is_alpha (struct cob_field f);
extern int cob_is_upper (struct cob_field f);
extern int cob_is_lower (struct cob_field f);

extern int cob_get_sign (struct cob_field f);
extern void cob_put_sign (struct cob_field f, int sign);
extern char *cob_field_to_string (struct cob_field f, char *s);
extern void cob_runtime_error (char *fmt, ...);
extern const char *cob_config_lookup (const char *key);
extern int cob_config_compare (const char *key, const char *val);

#endif /* COB_COMMON_H_ */
