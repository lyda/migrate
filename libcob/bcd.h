/*
 * Copyright (C) 2001 Keisuke Nishida
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

#ifndef _BCD_H_
#define _BCD_H_

enum bcd_sign { BCD_POSITIVE, BCD_NEGATIVE, BCD_NAN };

struct bcd_num
{
  enum bcd_sign sign;		/* sign */
  int ndigits;			/* the number of digits */
  int weight;			/* weight of the first digit */
  char *base;			/* decimal digits base address */
  char *digits;			/* decimal digits start address */
};

#define BCD_SIGN(num)		((num)->sign)
#define BCD_NDIGITS(num)	((num)->ndigits)
#define BCD_WEIGHT(num)		((num)->weight)
#define BCD_PRECISION(num)	((num)->ndigits - (num)->weight - 1)
#define BCD_BASE(num)		((num)->base)
#define BCD_DIGITS(num)		((num)->digits)
#define BCD_REF(num,i)		((num)->digits[i])
#define BCD_SET(num,i,n)	((num)->digits[i] = (n))

typedef struct bcd_num *bcd;

extern bcd bcd_new (int ndigits, int weight);
extern void bcd_free (bcd num);
extern bcd long_to_bcd (long val);
extern long bcd_to_long (bcd num);
extern bcd long_long_to_bcd (long long val);
extern long long bcd_to_long_long (bcd num);
extern bcd double_to_bcd (double val);
extern double bcd_to_double (bcd num);
extern bcd string_to_bcd (const char *str);
extern char *bcd_to_string (bcd num, char *buf);
extern int bcd_cmp (bcd n1, bcd n2);
extern bcd bcd_add (bcd n1, bcd n2);
extern bcd bcd_sub (bcd n1, bcd n2);
extern bcd bcd_mul (bcd n1, bcd n2);
extern bcd bcd_div (bcd n1, bcd n2);

#endif /* _BCD_H_ */
