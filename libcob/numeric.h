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

#ifndef COB_NUMERIC_H
#define COB_NUMERIC_H

#include <gmp.h>
#include <libcob/common.h>

#define COB_STORE_ROUND			0x01
#define COB_STORE_KEEP_ON_OVERFLOW	0x02
#define COB_STORE_TRUNC_ON_OVERFLOW	0x02

/*
 * Internal representation of decimal numbers.
 *
 *   n = value / 10^scale
 */
typedef struct {
  mpz_t value;
  char scale;
} cob_decimal;

extern void cob_decimal_init (cob_decimal *d);
extern void cob_decimal_clear (cob_decimal *d);
extern void cob_decimal_print (cob_decimal *d);
extern void cob_decimal_set (cob_decimal *dst, cob_decimal *src);
extern void cob_decimal_set_int (cob_decimal *d, int n);
extern int cob_decimal_get_int (cob_decimal *d);
extern void cob_decimal_set_double (cob_decimal *d, double v);
extern double cob_decimal_get_double (cob_decimal *d);
extern void cob_decimal_set_field (cob_decimal *d, cob_field *f);
extern int cob_decimal_get_field (cob_decimal *d, cob_field *f, int opt);
extern void cob_decimal_add (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_sub (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_mul (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_div (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_pow (cob_decimal *d1, cob_decimal *d2);
extern int cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2);

extern int cob_add (cob_field *f1, cob_field *f2, int opt);
extern int cob_sub (cob_field *f1, cob_field *f2, int opt);
extern int cob_add_int (cob_field *f, int n);
extern int cob_sub_int (cob_field *f, int n);
extern int cob_div_quotient (cob_field *dividend, cob_field *divisor, cob_field *quotient, int opt);
extern int cob_div_remainder (cob_field *remainder, int opt);

extern int cob_numeric_cmp (cob_field *f1, cob_field *f2);

extern void cob_init_numeric (void);

#endif /* COB_NUMERIC_H_ */
