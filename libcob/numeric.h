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

#ifndef COB_NUMERIC_H
#define COB_NUMERIC_H

#include <gmp.h>
#include <libcob/common.h>

/*
 * Internal representation of decimal numbers.
 *
 *   n = data * 10^expt
 */
typedef struct {
  mpz_t data;		/* multi-byte binary integer */
  char expt;		/* exponent */
} cob_decimal;

extern void cob_init_numeric (void);

extern void cob_decimal_init (cob_decimal *d);
extern void cob_decimal_clear (cob_decimal *d);
extern void cob_decimal_print (cob_decimal *d);
extern void cob_decimal_set (cob_decimal *dst, cob_decimal *src);
extern void cob_decimal_set_int (cob_decimal *d, int n);
extern int cob_decimal_get_int (cob_decimal *d);
extern void cob_decimal_set_double (cob_decimal *d, double v);
extern double cob_decimal_get_double (cob_decimal *d);
extern void cob_decimal_set_display (cob_decimal *d, cob_field *f);
extern void cob_decimal_get_display (cob_decimal *d, cob_field *f);
extern void cob_decimal_set_binary (cob_decimal *d, cob_field *f);
extern void cob_decimal_get_binary (cob_decimal *d, cob_field *f);
extern void cob_decimal_set_packed (cob_decimal *d, cob_field *f);
extern void cob_decimal_set_field (cob_decimal *d, cob_field *f);
extern void cob_decimal_get_field (cob_decimal *d, cob_field *f);
extern void cob_decimal_get_field_r (cob_decimal *d, cob_field *f);
extern void cob_decimal_add (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_sub (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_mul (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_div (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_pow (cob_decimal *d1, cob_decimal *d2);
extern int cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2);

extern void cob_add (cob_field *f1, cob_field *f2);
extern void cob_sub (cob_field *f1, cob_field *f2);
extern void cob_add_r (cob_field *f1, cob_field *f2);
extern void cob_sub_r (cob_field *f1, cob_field *f2);
extern void cob_add_int (cob_field *f, int n);
extern void cob_sub_int (cob_field *f, int n);
extern void cob_div_quotient (cob_field *dividend, cob_field *divisor, cob_field *quotient, int round);
extern void cob_div_remainder (cob_field *remainder);

extern int cob_numeric_cmp (cob_field *f1, cob_field *f2);

#endif /* COB_NUMERIC_H_ */
