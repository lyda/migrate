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
#include "bcd.h"

void
cob_fld_to_bcd (struct fld_desc *f, unsigned char *s, union numeric_type *p)
{
  bcd num;

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: num = long_to_bcd (*((char *) s)); break;
	case 2: num = long_to_bcd (*((short *) s)); break;
	case 4: num = long_to_bcd (*((long *) s)); break;
	case 8: num = long_long_to_bcd (*((long long *) s)); break;
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_fld_to_bcd: not implemented yet");
      break;

    default:
      {
	int i;
	int sign = extract_sign (f, s);

	num = bcd_new (f->len, f->len - f->decimals - 1);
	BCD_SIGN (num) = sign ? BCD_NEGATIVE : BCD_POSITIVE;

	for (i = 0; i < f->len; i++)
	  BCD_SET (num, i, (s[i] == ' ') ? 0 : s[i] - '0');

	put_sign (f, s, sign);
	break;
      }
    }

  p->n_bcd = num;
}

int
cob_bcd_to_fld (struct fld_desc *f, char *s, int round, union numeric_type val)
{
  bcd num = val.n_bcd;

  /* FIXME: Do rounding here */


  /* Check for overflow */
  if (f->len - f->decimals < BCD_WEIGHT (num) + 1)
    {
      bcd_free (num);
      return 1;
    }

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: *((char *) s) = bcd_to_long (num); break;
	case 2: *((short *) s) = bcd_to_long (num); break;
	case 4: *((long *) s) = bcd_to_long (num); break;
	case 8: *((long long *) s) = bcd_to_long_long (num); break;
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_bcd_to_fld: not implemented yet");
      bcd_free (num);
      return 1;

    default:
      {
	int i, j, len;

	j = (BCD_WEIGHT (num) + 1) - (f->len - f->decimals);
	len = BCD_NDIGITS (num);
	for (i = 0; i < f->len; i++, j++)
	  s[i] = (0 <= j && j < len) ? (BCD_REF (num, j) + '0') : '0';

	put_sign (f, s, (BCD_SIGN (num) == BCD_POSITIVE) ? 0 : 1);
	break;
      }
    }
  bcd_free (num);
  return 0;
}

void
add_bcd (union numeric_type *v1, union numeric_type v2)
{
  bcd num = bcd_add ((*v1).n_bcd, v2.n_bcd);
  bcd_free ((*v1).n_bcd);
  bcd_free (v2.n_bcd);
  (*v1).n_bcd = num;
}

void
subtract_bcd (union numeric_type *v1, union numeric_type v2)
{
  bcd num = bcd_sub ((*v1).n_bcd, v2.n_bcd);
  bcd_free ((*v1).n_bcd);
  bcd_free (v2.n_bcd);
  (*v1).n_bcd = num;
}

void
multiply_bcd (union numeric_type *v1, union numeric_type v2)
{
  bcd num = bcd_mul ((*v1).n_bcd, v2.n_bcd);
  bcd_free ((*v1).n_bcd);
  bcd_free (v2.n_bcd);
  (*v1).n_bcd = num;
}

void
divide_bcd (union numeric_type *v1, union numeric_type v2)
{
  bcd num = bcd_div ((*v1).n_bcd, v2.n_bcd);
  bcd_free ((*v1).n_bcd);
  bcd_free (v2.n_bcd);
  (*v1).n_bcd = num;
}

void
pow_bcd (union numeric_type *v1, union numeric_type v2)
{
  puts ("pow_bcd: not implemented yet");
  abort ();
}

int
compare_bcd (union numeric_type v1, union numeric_type v2)
{
  int val = bcd_cmp (v1.n_bcd, v2.n_bcd);
  bcd_free (v1.n_bcd);
  bcd_free (v2.n_bcd);
  return val;
}
