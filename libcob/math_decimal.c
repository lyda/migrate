/* Binary Coded Decimal (BCD) Math Module
 *
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

#include "_libcob.h"
#include "decimal.h"

void
cob_fld_to_decimal (struct fld_desc *f, unsigned char *s, union numeric_type *p)
{
  decimal num;

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: num = long_to_decimal (*((char *) s)); break;
	case 2: num = long_to_decimal (*((short *) s)); break;
	case 4: num = long_to_decimal (*((long *) s)); break;
	case 8: num = long_long_to_decimal (*((long long *) s)); break;
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_fld_to_decimal: not implemented yet");
      break;

    default:
      {
	int i;
	int sign = extract_sign (f, s);

	num = decimal_new (f->len, f->len - f->decimals - 1);
	DECIMAL_SIGN (num) = sign ? DECIMAL_NEGATIVE : DECIMAL_POSITIVE;

	for (i = 0; i < f->len; i++)
	  DECIMAL_SET (num, i, (s[i] == ' ') ? 0 : s[i] - '0');

	put_sign (f, s, sign);
	break;
      }
    }

  p->n_decimal = num;
}

int
cob_decimal_to_fld (struct fld_desc *f, char *s, int round, union numeric_type val)
{
  decimal num = val.n_decimal;

  /* FIXME: Do rounding here */


  /* Check for overflow */
  if (f->len - f->decimals < DECIMAL_WEIGHT (num) + 1)
    {
      decimal_free (num);
      return 1;
    }

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: *((char *) s) = decimal_to_long (num); break;
	case 2: *((short *) s) = decimal_to_long (num); break;
	case 4: *((long *) s) = decimal_to_long (num); break;
	case 8: *((long long *) s) = decimal_to_long_long (num); break;
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_decimal_to_fld: not implemented yet");
      decimal_free (num);
      return 1;

    default:
      {
	int i, j, len;

	j = (DECIMAL_WEIGHT (num) + 1) - (f->len - f->decimals);
	len = DECIMAL_NDIGITS (num);
	for (i = 0; i < f->len; i++, j++)
	  s[i] = (0 <= j && j < len) ? (DECIMAL_REF (num, j) + '0') : '0';

	put_sign (f, s, (DECIMAL_SIGN (num) == DECIMAL_POSITIVE) ? 0 : 1);
	break;
      }
    }
  decimal_free (num);
  return 0;
}

void
add_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal num = decimal_add ((*v1).n_decimal, v2.n_decimal);
  decimal_free ((*v1).n_decimal);
  decimal_free (v2.n_decimal);
  (*v1).n_decimal = num;
}

void
subtract_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal num = decimal_sub ((*v1).n_decimal, v2.n_decimal);
  decimal_free ((*v1).n_decimal);
  decimal_free (v2.n_decimal);
  (*v1).n_decimal = num;
}

void
multiply_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal num = decimal_mul ((*v1).n_decimal, v2.n_decimal);
  decimal_free ((*v1).n_decimal);
  decimal_free (v2.n_decimal);
  (*v1).n_decimal = num;
}

void
divide_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal num = decimal_div ((*v1).n_decimal, v2.n_decimal);
  decimal_free ((*v1).n_decimal);
  decimal_free (v2.n_decimal);
  (*v1).n_decimal = num;
}

void
pow_decimal (union numeric_type *v1, union numeric_type v2)
{
  puts ("pow_decimal: not implemented yet");
  abort ();
}

int
compare_decimal (union numeric_type v1, union numeric_type v2)
{
  int val = decimal_cmp (v1.n_decimal, v2.n_decimal);
  decimal_free (v1.n_decimal);
  decimal_free (v2.n_decimal);
  return val;
}
