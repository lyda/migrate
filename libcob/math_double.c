/* Double Floating Point Math Module
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

#define ABS(x) ((x) > 0 ? (x) : -(x))

static double exp10[] =
  { 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10,
  1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20,
  1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29, 1e30,
  1e31, 1e32, 1e33, 1e34, 1e35, 1e36, 1e37, 1e38, 1e39, 1e40,
  1e41, 1e42, 1e43, 1e44, 1e45, 1e46, 1e47, 1e48, 1e49, 1e50
};

void
cob_fld_to_double (struct fld_desc *f, unsigned char *s, union numeric_type *p)
{
  int i;
  int sign = 0;
  double val = 0;

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: val = (double) *((char *) s); break;
	case 2: val = (double) *((short *) s); break;
	case 4: val = (double) *((long *) s); break;
	case 8: val = (double) *((long long *) s); break;
	}
      break;

    case 'C':
      {
	int high = 1;

	i = 0;
	val = 0;
	while (1)
	  {
	    int d = (high) ? s[i] >> 4 : s[i++] & 0x0f;
	    if (d < 10)
	      {
		high ^= 1;
		val = val * 10. + (double) d;
	      }
	    else
	      {
		sign = (d == 0x0d) ? 1 : 0;
		break;
	      }
	  }
      }
      break;

    case 'U':
      switch (f->len)
	{
	case 4: val = (double) *((float *) s); break;
	case 8: val = (double) *((double *) s); break;
	}
      break;

    default:
      sign = extract_sign (f, s);
      for (i = 0; i < f->len; i++)
	{
	  int d = (s[i] == ' ') ? 0 : s[i] - '0';
	  val = val * 10. + (double) d;
	}
      put_sign (f, s, sign);
      break;
    }

  /* scale the number with it's (fld_desc *)->decimals */
  /* No decimal scaling required for floating types */
  if (f->type != 'U')
    {
      int scale = f->decimals;
      if (scale > 0 && scale <= 50)
	val /= exp10[scale];
      else if (scale > 50)	/* because our table don't have everything */
	val /= pow (10., scale);

      if (sign)
	val = -val;
    }

  p->n_double = val;
}

int
cob_double_to_fld (struct fld_desc *f, char *s, int round,
		   union numeric_type val)
{
  int len;
  double num = val.n_double;

  len = picCompLength (f);
  if (len < f->decimals)
    return 1;

  if (ABS (num) >= exp10[len - f->decimals])
    return 1;

  switch (f->type)
    {
    case 'B':
      num *= exp10[f->decimals];

      if (round)
	num += (num > 0.) ? .5 : -.5;

      switch (f->len)
	{
	case 1: *((char *) s) = (char) num; break;
	case 2: *((short *) s) = (short) num; break;
	case 4: *((long *) s) = (long) num; break;
	case 8: *((long long *) s) = (long long) num; break;
	}

    case 'U':
      switch (f->len)
	{
	case 4: *((float *) s) = (float) num; break;
	case 8: *((double *) s) = num; break;
	}

    default:
      {
	int i, mini, k, digit;
	int sign = 0;
	unsigned long fint;

	if (num < 0.)
	  {
	    sign = 1;
	    num = -num;
	  }

	if (num > exp10[f->len - f->decimals])
	  {
	    fprintf (stderr, "Invalid contents of numeric field\n");
	    fprintf (stderr, "field len: %ld, type: %c, decimals: %d\n",
		     f->len, f->type, f->decimals);
	    abort ();
	  }

	num *= exp10[f->decimals];
	if (round)
	  num += (num > 0) ? .5 : -.5;

	if (f->type == 'C')
	  {
	    /* convert scaled number in num to our result */
	    for (i = f->len - 1; i >= 0; i--)
	      {
		k = i >> 1;
		digit = ((long long) num) % 10;
		num /= 10.;
		if (i % 2)
		  {
		    s[k] &= 0xf0;
		    s[k] |= digit;
		  }
		else
		  {
		    s[k] &= 0x0f;
		    s[k] |= digit << 4;
		  }
	      }
	    k = f->len >> 1;
	    if (f->len % 2)
	      {
		s[k] &= 0xf0;
		s[k] |= sign ? 0x0d : 0x0c;
	      }
	    else
	      {
		s[k] &= 0x0f;
		s[k] |= sign ? 0xd0 : 0xc0;
	      }
	  }
	else
	  {
	    // digits 10 and up
	    if (num > exp10[9])
	      {			// num > 1,000,000,000
		fint = (unsigned long) (num / exp10[9]);
		// subtrai parte alta do numero
		num -= fint * exp10[9];
		// converte parte alta para string
		for (i = f->len - 10; i >= 0; i--)
		  {
		    s[i] = (char) (fint % 10) + '0';
		    fint /= 10;
		  }
	      }
	    // digits 9 and below
	    fint = (unsigned long) num;
	    mini = (f->len > 9) ? f->len - 9 : 0;
	    for (i = f->len - 1; i >= mini; i--)
	      {
		s[i] = (char) (fint % 10) + '0';
		fint /= 10;
	      }
	    put_sign (f, s, sign);
	  }
      }
    }

  return 0;
}

void
add_double (union numeric_type *v1, union numeric_type v2)
{
  (*v1).n_double += v2.n_double;
}

void
subtract_double (union numeric_type *v1, union numeric_type v2)
{
  (*v1).n_double -= v2.n_double;
}

void
multiply_double (union numeric_type *v1, union numeric_type v2)
{
  (*v1).n_double *= v2.n_double;
}

void
divide_double (union numeric_type *v1, union numeric_type v2)
{
  (*v1).n_double /= v2.n_double;
}

void
pow_double (union numeric_type *v1, union numeric_type v2)
{
  (*v1).n_double = pow ((*v1).n_double, v2.n_double);
}

int
compare_double (union numeric_type v1, union numeric_type v2)
{
  if (v1.n_double > v2.n_double) return 1;
  if (v1.n_double < v2.n_double) return -1;
  return 0;
}
