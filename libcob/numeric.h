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

struct cob_decimal {
  char exp;
  mpz_t number;
};

extern struct cob_decimal *cob_d1, *cob_d2, *cob_d3, *cob_d4, *cob_dt;

extern void cob_init_numeric (void);
extern void cob_decimal_init (struct cob_decimal *d);
extern void cob_decimal_clear (struct cob_decimal *d);
extern void cob_decimal_print (struct cob_decimal *d);
extern void cob_decimal_set (struct cob_decimal *dst, struct cob_decimal *src);
extern void cob_decimal_set_int (struct cob_decimal *d, int n, int decimals);
extern void cob_decimal_set_int64 (struct cob_decimal *d, long long n, int decimals);
extern void cob_decimal_set_double (struct cob_decimal *d, double v);
extern void cob_decimal_set_display (struct cob_decimal *d, struct cob_field f);
extern void cob_decimal_set_field (struct cob_decimal *d, struct cob_field f);
extern void cob_decimal_get (struct cob_decimal *d, struct cob_field f);
extern void cob_decimal_get_rounded (struct cob_decimal *d, struct cob_field f);
extern double cob_decimal_get_double (struct cob_decimal *d);
extern void cob_decimal_add (struct cob_decimal *d1, struct cob_decimal *d2);
extern void cob_decimal_sub (struct cob_decimal *d1, struct cob_decimal *d2);
extern void cob_decimal_mul (struct cob_decimal *d1, struct cob_decimal *d2);
extern void cob_decimal_div (struct cob_decimal *d1, struct cob_decimal *d2);
extern void cob_decimal_pow (struct cob_decimal *d1, struct cob_decimal *d2);
extern int cob_decimal_cmp (struct cob_decimal *d1, struct cob_decimal *d2);

extern void cob_add (struct cob_field f1, struct cob_field f2, int round);
extern void cob_add_int (struct cob_field f, int n, int decimals, int round);
extern void cob_add_int64 (struct cob_field f, long long n, int decimals, int round);
extern void cob_sub (struct cob_field f1, struct cob_field f2, int round);
extern void cob_sub_int (struct cob_field f, int n, int decimals, int round);
extern void cob_sub_int64 (struct cob_field f, long long n, int decimals, int round);
extern void cob_div_quotient (struct cob_field dividend, struct cob_field divisor, struct cob_field quotient, int round);
extern void cob_div_remainder (struct cob_field remainder);

#endif /* COB_NUMERIC_H_ */
