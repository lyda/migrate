/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "move.h"
#include "numeric.h"

#define DECIMAL_NAN	-128
#define DECIMAL_CHECK(d1,d2) \
  if (d1->expt == DECIMAL_NAN || d2->expt == DECIMAL_NAN) \
    { \
      d1->expt = DECIMAL_NAN; \
      return; \
    }

static cob_decimal cob_d1;
static cob_decimal cob_d2;
static cob_decimal cob_d3;
static cob_decimal cob_d4;

void
cob_init_numeric (void)
{
  cob_decimal_init (&cob_d1);
  cob_decimal_init (&cob_d2);
  cob_decimal_init (&cob_d3);
  cob_decimal_init (&cob_d4);
}


/*
 * Decimal number
 */

void
cob_decimal_init (cob_decimal *d)
{
  mpz_init (d->data);
  d->expt = 0;
}

void
cob_decimal_clear (cob_decimal *d)
{
  mpz_clear (d->data);
}

#ifdef COB_DEBUG
void
cob_decimal_print (cob_decimal *d)
{
  mpz_out_str (stdout, 10, d->data);
  if (d->expt)
    fprintf (stdout, " * 10^%d", d->expt);
  fputs ("\n", stdout);
}
#endif

/* d->data *= 10^n, d->expt -= n */
static void
shift_decimal (cob_decimal *d, int n)
{
  if (n > 0)
    {
      if (n < 10)
	/* 0 < n < 10 */
	mpz_mul_ui (d->data, d->data, cob_exp10[n]);
      else
	{
	  /* n >= 10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, n);
	  mpz_mul (d->data, d->data, m);
	  mpz_clear (m);
	}
    }
  else if (n < 0)
    {
      if (n > -10)
	/* -10 < n < 0 */
	mpz_tdiv_q_ui (d->data, d->data, cob_exp10[-n]);
      else
	{
	  /* n <= -10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, -n);
	  mpz_tdiv_q (d->data, d->data, m);
	  mpz_clear (m);
	}
    }
  d->expt -= n;
}

static void
arrange_decimal (cob_decimal *d1, cob_decimal *d2)
{
  if (d1->expt > d2->expt)
    shift_decimal (d1, d1->expt - d2->expt);
  else if (d1->expt < d2->expt)
    shift_decimal (d2, d2->expt - d1->expt);
}


/*
 * Decimal set/get
 */

void
cob_decimal_set (cob_decimal *dst, cob_decimal *src)
{
  mpz_set (dst->data, src->data);
  dst->expt = src->expt;
}

void
cob_decimal_set_int (cob_decimal *d, int n, int decimals)
{
  mpz_set_si (d->data, n);
  d->expt = - decimals;
}

void
cob_decimal_set_int64 (cob_decimal *d, long long n, int decimals)
{
  mpz_set_si (d->data, n >> 32);
  mpz_mul_2exp (d->data, d->data, 32);
  mpz_add_ui (d->data, d->data, n & 0xffffffff);
  d->expt = - decimals;
}

void
cob_decimal_set_double (cob_decimal *d, double v)
{
  mpz_set_d (d->data, v * cob_exp10[9]);
  d->expt = -9;
}

void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
  int sign = cob_get_sign (f);
  int len = COB_FIELD_SIZE (f);
  unsigned char *base = COB_FIELD_DATA (f);
  unsigned char buff[len + 1];
  memcpy (buff, base, len);
  buff[len] = 0;
  mpz_set_str (d->data, buff, 10);
  if (sign < 0)
    mpz_neg (d->data, d->data);
  d->expt = f->attr ? - f->attr->decimals : 0;
  cob_put_sign (f, sign);
}

void
cob_decimal_set_field (cob_decimal *d, cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
      {
	int n = f->attr->decimals;
	switch (f->size)
	  {
	  case 1: cob_decimal_set_int (d, *(char *) f->data, n); break;
	  case 2: cob_decimal_set_int (d, *(short *) f->data, n); break;
	  case 4: cob_decimal_set_int (d, *(long *) f->data, n); break;
	  case 8: cob_decimal_set_int64 (d, *(long long *) f->data, n); break;
	  }
	break;
      }
    default:
      cob_decimal_set_display (d, f);
      break;
    }
}

void
cob_decimal_get (cob_decimal *d, cob_field *f)
{
  if (d->expt == DECIMAL_NAN)
    goto overflow;

  cob_exception_code = 0;

  /* work copy */
  if (d != &cob_d1)
    {
      cob_decimal_set (&cob_d1, d);
      d = &cob_d1;
    }

  /* append or truncate decimal digits */
  shift_decimal (d, f->attr->decimals + d->expt);

  /* store number */
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
      {
	int digits = f->attr->digits;
	if (f->size <= 4)
	  {
	    int val;
	    if (!mpz_fits_sint_p (d->data))
	      goto overflow;
	    val = mpz_get_si (d->data);
	    if (val <= -cob_exp10[digits] || val >= cob_exp10[digits])
	      goto overflow;
	    if (!COB_FIELD_HAVE_SIGN (f) && val < 0)
	      val = -val;
	    switch (f->size)
	      {
	      case 1: *(char *) f->data = val; break;
	      case 2: *(short *) f->data = val; break;
	      case 4: *(long *) f->data = val; break;
	      }
	  }
	else
	  {
	    long long val;
	    mpz_t r;
	    mpz_init (r);
	    mpz_fdiv_r_2exp (r, d->data, 32);
	    mpz_fdiv_q_2exp (d->data, d->data, 32);
	    if (!mpz_fits_sint_p (d->data))
	      {
		mpz_clear (r);
		goto overflow;
	      }
	    val = mpz_get_si (d->data);
	    val = (val << 32) + mpz_get_ui (r);
	    mpz_clear (r);
	    if (val <= -cob_exp10LL[digits] || val >= cob_exp10LL[digits])
	      goto overflow;
	    if (!COB_FIELD_HAVE_SIGN (f) && val < 0)
	      val = -val;
	    *(long long *) f->data = val;
	  }
	return;
      }

    default:
      {
	char *p, buff[32];
	int size;
	int sign = (mpz_sgn (d->data) < 0) ? -1 : 1;

	/* Build string */
	mpz_abs (d->data, d->data);
	size = mpz_sizeinbase (d->data, 10);
	p = (size < 32) ? buff : alloca (size + 1);
	mpz_get_str (p, 10, d->data);
	size = strlen (p);

	if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY)
	  {
	    size_t len = COB_FIELD_SIZE (f);
	    unsigned char *base = COB_FIELD_DATA (f);
	    if (len < size)
	      goto overflow;
	    if (len == size)
	      memcpy (base, p, size);
	    else
	      {
		int pre = len - size;
		memset (base, '0', pre);
		memcpy (base + pre, p, size);
	      }
	    cob_put_sign (f, sign);
	  }
	else
	  {
	    cob_field_attr attr = {
	      COB_TYPE_NUMERIC_DISPLAY,
	      f->attr->digits,
	      f->attr->decimals,
	      COB_FLAG_HAVE_SIGN
	    };
	    cob_field temp = {size, buff, &attr};
	    if (f->attr->digits < size)
	      goto overflow;
	    cob_put_sign (&temp, sign);
	    cob_move (&temp, f);
	  }
	return;
      }
    }

 overflow:
  cob_exception_code = COB_EC_SIZE_OVERFLOW;
}

void
cob_decimal_get_r (cob_decimal *d, cob_field *f)
{
  if (f->attr->decimals < - d->expt)
    {
      int sign = mpz_sgn (d->data);
      if (sign != 0)
	{
	  /* work copy */
	  cob_decimal_set (&cob_d1, d);
	  d = &cob_d1;

	  /* rounding */
	  shift_decimal (d, f->attr->decimals + d->expt + 1);
	  if (sign > 0)
	    mpz_add_ui (d->data, d->data, 5);
	  else
	    mpz_sub_ui (d->data, d->data, 5);
	}
    }
  cob_decimal_get (d, f);
}

double
cob_decimal_get_double (cob_decimal *d)
{
  int n = d->expt;
  double v = mpz_get_d (d->data);
  for (; n > 0; n--) v *= 10;
  for (; n < 0; n++) v /= 10;
  return v;
}


/*
 * Decimal arithmetic
 */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  arrange_decimal (d1, d2);
  mpz_add (d1->data, d1->data, d2->data);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  arrange_decimal (d1, d2);
  mpz_sub (d1->data, d1->data, d2->data);
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  d1->expt += d2->expt;
  mpz_mul (d1->data, d1->data, d2->data);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  /* check for division by zero */
  if (mpz_sgn (d2->data) == 0)
    {
      d1->expt = DECIMAL_NAN;
      return;
    }

  d1->expt -= d2->expt;
  shift_decimal (d1, 19 + ((d1->expt > 0) ? d1->expt : 0));
  mpz_tdiv_q (d1->data, d1->data, d2->data);
}

void
cob_decimal_pow (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  if (d2->expt == 0 && mpz_fits_ulong_p (d2->data))
    {
      unsigned int n = mpz_get_ui (d2->data);
      mpz_pow_ui (d1->data, d1->data, n);
      d1->expt *= n;
    }
  else
    {
      cob_decimal_set_double (d1, pow (cob_decimal_get_double (d1),
				       cob_decimal_get_double (d2)));
    }
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
  arrange_decimal (d1, d2);
  return mpz_cmp (d1->data, d2->data);
}


/*
 * Convenience functions
 */

void
cob_add (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_add (&cob_d1, &cob_d2);
  cob_decimal_get (&cob_d1, f1);
}

void
cob_sub (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  cob_decimal_get (&cob_d1, f1);
}

void
cob_add_r (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_add (&cob_d1, &cob_d2);
  cob_decimal_get_r (&cob_d1, f1);
}

void
cob_sub_r (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  cob_decimal_get_r (&cob_d1, f1);
}

void
cob_add_int (cob_field *f, int n)
{
  cob_decimal_set_field (&cob_d1, f);
  cob_decimal_set_int (&cob_d2, n, 0);
  cob_decimal_add (&cob_d1, &cob_d2);
  cob_decimal_get (&cob_d1, f);
}

void
cob_sub_int (cob_field *f, int n)
{
  cob_decimal_set_field (&cob_d1, f);
  cob_decimal_set_int (&cob_d2, n, 0);
  cob_decimal_sub (&cob_d1, &cob_d2);
  cob_decimal_get (&cob_d1, f);
}

void
cob_div_quotient (cob_field *dividend, cob_field *divisor,
		  cob_field *quotient, int round)
{
  cob_decimal_set_field (&cob_d1, dividend);
  cob_decimal_set_field (&cob_d2, divisor);
  cob_decimal_set (&cob_d3, &cob_d1);

  /* compute quotient */
  cob_decimal_div (&cob_d1, &cob_d2);
  if (cob_d1.expt == DECIMAL_NAN)
    {
      cob_d3.expt = DECIMAL_NAN;
      return;
    }

  /* set quotient */
  cob_decimal_set (&cob_d4, &cob_d1);
  if (round)
    cob_decimal_get_r (&cob_d1, quotient);
  else
    cob_decimal_get (&cob_d1, quotient);

  /* truncate digits from the quotient */
  shift_decimal (&cob_d4, quotient->attr->decimals + cob_d4.expt);

  /* compute remainder */
  cob_decimal_mul (&cob_d4, &cob_d2);
  cob_decimal_sub (&cob_d3, &cob_d4);
}

void
cob_div_remainder (cob_field *remainder)
{
  cob_decimal_get (&cob_d3, remainder);
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  return cob_decimal_cmp (&cob_d1, &cob_d2);
}
