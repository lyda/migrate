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

#ifndef _DECIMAL_H_
#define _DECIMAL_H_

enum decimal_sign { DECIMAL_POSITIVE, DECIMAL_NEGATIVE, DECIMAL_NAN };

struct decimal_num
{
  enum decimal_sign sign;		/* sign */
  int ndigits;			/* the number of digits */
  int weight;			/* weight of the first digit */
  char *base;			/* decimal digits base address */
  char *digits;			/* decimal digits start address */
};

#define DECIMAL_SIGN(num)		((num)->sign)
#define DECIMAL_NDIGITS(num)	((num)->ndigits)
#define DECIMAL_WEIGHT(num)		((num)->weight)
#define DECIMAL_PRECISION(num)	((num)->ndigits - (num)->weight - 1)
#define DECIMAL_BASE(num)		((num)->base)
#define DECIMAL_DIGITS(num)		((num)->digits)
#define DECIMAL_REF(num,i)		((num)->digits[i])
#define DECIMAL_SET(num,i,n)	((num)->digits[i] = (n))

typedef struct decimal_num *decimal;

extern decimal decimal_new (int ndigits, int weight);
extern void decimal_free (decimal num);
extern decimal long_to_decimal (long val);
extern long decimal_to_long (decimal num);
extern decimal long_long_to_decimal (long long val);
extern long long decimal_to_long_long (decimal num);
extern decimal double_to_decimal (double val);
extern double decimal_to_double (decimal num);
extern decimal string_to_decimal (const char *str);
extern char *decimal_to_string (decimal num, char *buf);
extern int decimal_cmp (decimal n1, decimal n2);
extern decimal decimal_add (decimal n1, decimal n2);
extern decimal decimal_sub (decimal n1, decimal n2);
extern decimal decimal_mul (decimal n1, decimal n2);
extern decimal decimal_div (decimal n1, decimal n2);

#endif /* _DECIMAL_H_ */
