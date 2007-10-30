/*
 * Copyright (C) 2002-2007 Keisuke Nishida
 * Copyright (C) 2007 Roger While
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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_NUMERIC_H
#define COB_NUMERIC_H

#include <gmp.h>
#include <libcob/common.h>

#define COB_STORE_ROUND			0x01
#define COB_STORE_KEEP_ON_OVERFLOW	0x02
#define COB_STORE_TRUNC_ON_OVERFLOW	0x04

/*
 * Internal representation of decimal numbers.
 *
 *   n = value / 10^scale
 */
typedef struct {
	mpz_t	value;
	int	scale;
} cob_decimal;

extern void cob_decimal_init (cob_decimal *d);
extern void cob_decimal_set_field (cob_decimal *d, cob_field *f);
extern int cob_decimal_get_field (cob_decimal *d, cob_field *f, const int opt);
extern void cob_decimal_add (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_sub (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_mul (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_div (cob_decimal *d1, cob_decimal *d2);
extern void cob_decimal_pow (cob_decimal *d1, cob_decimal *d2);
extern int cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2);

extern int cob_add (cob_field *f1, cob_field *f2, const int opt);
extern int cob_sub (cob_field *f1, cob_field *f2, const int opt);
extern int cob_add_int (cob_field *f, const int n);
extern int cob_sub_int (cob_field *f, const int n);
extern int cob_div_quotient (cob_field *dividend, cob_field *divisor, cob_field *quotient, const int opt);
extern int cob_div_remainder (cob_field *fld_remainder, const int opt);

extern int cob_cmp_int (cob_field *f1, const int n);
extern int cob_cmp_uint (cob_field *f1, const unsigned int n);
extern int cob_cmp_packed (cob_field *f, int n);
extern int cob_cmp_numdisp (const unsigned char *data, const size_t size, const int n);
extern int cob_cmp_sign_numdisp (const unsigned char *data, const size_t size, const int n);
extern int cob_cmp_long_numdisp (const unsigned char *data, const size_t size, const int n);
extern int cob_cmp_long_sign_numdisp (const unsigned char *data, const size_t size, const int n);
extern void cob_set_packed_zero (cob_field *f);
extern void cob_set_packed_int (cob_field *f, const int val);

#endif /* COB_NUMERIC_H */
