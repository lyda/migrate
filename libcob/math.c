/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#include "libcob.h"

static struct cob_decimal cob_d1_data;
static struct cob_decimal cob_d2_data;
static struct cob_decimal cob_d3_data;
static struct cob_decimal cob_d4_data;
static struct cob_decimal cob_dt_data;

cob_decimal cob_d1 = &cob_d1_data;
cob_decimal cob_d2 = &cob_d2_data;
cob_decimal cob_d3 = &cob_d3_data;
cob_decimal cob_d4 = &cob_d4_data;
cob_decimal cob_dt = &cob_dt_data;

void
cob_init_math (void)
{
  cob_decimal_init (cob_d1);
  cob_decimal_init (cob_d2);
  cob_decimal_init (cob_d3);
  cob_decimal_init (cob_d4);
  cob_decimal_init (cob_dt);
}

/*
 * Decimal number
 */

void
cob_decimal_init (cob_decimal d)
{
  mpz_init (d->number);
  d->decimals = 0;
}

void
cob_decimal_print (cob_decimal d)
{
  mpz_out_str (stdout, 10, d->number);
  if (d->decimals)
    fprintf (stdout, " * 10^%d", -d->decimals);
  fputs ("\n", stdout);
}


/*
 * Decimal arithmetic
 */

/* d->number *= 10^n, d->decimals += n */
static void
shift_decimal (cob_decimal d, int n)
{
  if (n > 0)
    {
      if (n < 10)
	/* 0 < n < 10 */
	mpz_mul_ui (d->number, d->number, cob_exp10[n]);
      else
	{
	  /* n >= 10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, n);
	  mpz_mul (d->number, d->number, m);
	  mpz_clear (m);
	}
    }
  else if (n < 0)
    {
      if (n > -10)
	/* -10 < n < 0 */
	mpz_tdiv_q_ui (d->number, d->number, cob_exp10[-n]);
      else
	{
	  /* n <= -10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, -n);
	  mpz_tdiv_q (d->number, d->number, m);
	  mpz_clear (m);
	}
    }
  d->decimals += n;
}

static void
arrange_decimal (cob_decimal d1, cob_decimal d2)
{
  if (d1->decimals < d2->decimals)
    shift_decimal (d1, d2->decimals - d1->decimals);
  else if (d1->decimals > d2->decimals)
    shift_decimal (d2, d1->decimals - d2->decimals);
}

void
cob_decimal_add (cob_decimal d1, cob_decimal d2)
{
  arrange_decimal (d1, d2);
  mpz_add (d1->number, d1->number, d2->number);
}

void
cob_decimal_sub (cob_decimal d1, cob_decimal d2)
{
  arrange_decimal (d1, d2);
  mpz_sub (d1->number, d1->number, d2->number);
}

void
cob_decimal_mul (cob_decimal d1, cob_decimal d2)
{
  d1->decimals += d2->decimals;
  mpz_mul (d1->number, d1->number, d2->number);
}

void
cob_decimal_div (cob_decimal d1, cob_decimal d2)
{
  /* check for division by zero */
  if (mpz_sgn (d2->number) == 0)
    {
      cob_status = COB_STATUS_OVERFLOW;
      return;
    }

  d1->decimals -= d2->decimals;
  shift_decimal (d1, 19 + ((d1->decimals < 0) ? -d1->decimals : 0));
  mpz_tdiv_q (d1->number, d1->number, d2->number);
}

void
cob_decimal_pow (cob_decimal d1, cob_decimal d2)
{
  if (d2->decimals == 0 && mpz_fits_ulong_p (d2->number))
    {
      int n = mpz_get_ui (d2->number);
      mpz_pow_ui (d1->number, d1->number, n);
      d1->decimals *= n;
    }
  else
    {
      cob_runtime_error ("%s: not implemented yet", __FUNCTION__);
    }
}

int
cob_decimal_cmp (cob_decimal d1, cob_decimal d2)
{
  arrange_decimal (d1, d2);
  return mpz_cmp (d1->number, d2->number);
}


/*
 * Decimal set/get
 */

void
cob_decimal_set (cob_decimal dst, cob_decimal src)
{
  mpz_set (dst->number, src->number);
  dst->decimals = src->decimals;
}

void
cob_decimal_set_int (cob_decimal d, int n, int decimals)
{
  mpz_set_si (d->number, n);
  d->decimals = decimals;
}

void
cob_decimal_set_int64 (cob_decimal d, long long n, int decimals)
{
  mpz_set_si (d->number, n >> 32);
  mpz_mul_2exp (d->number, d->number, 32);
  mpz_add_ui (d->number, d->number, n & 0xffffffff);
  d->decimals = decimals;
}

void
cob_decimal_set_display (cob_decimal d, struct cob_field f)
{
  int sign = cob_get_sign (f);
  int len = FIELD_LENGTH (f);
  unsigned char *base = FIELD_BASE (f);
  unsigned char buff[len + 1];
  memcpy (buff, base, len);
  buff[len] = 0;
  mpz_set_str (d->number, buff, 10);
  if (sign == 1) /* negative */
    mpz_neg (d->number, d->number);
  d->decimals = f.desc->decimals;
  cob_put_sign (f, sign);
}

void
cob_decimal_set_field (cob_decimal d, struct cob_field f)
{
  switch (f.desc->type)
    {
    case COB_BINARY:
      {
	int n = f.desc->decimals;
	switch (f.desc->size)
	  {
	  case 1: cob_decimal_set_int (d, *(char *) f.data, n); break;
	  case 2: cob_decimal_set_int (d, *(short *) f.data, n); break;
	  case 4: cob_decimal_set_int (d, *(long *) f.data, n); break;
	  case 8: cob_decimal_set_int64 (d, *(long long *) f.data, n); break;
	  }
	break;
      }
    case COB_PACKED:
      cob_runtime_error ("COB_PACKED: not implemented");
      break;
    default:
      cob_decimal_set_display (d, f);
      break;
    }
}

void
cob_decimal_get (cob_decimal d, struct cob_field f)
{
  if (cob_status == COB_STATUS_OVERFLOW)
    return;

  /* Append or truncate decimal digits */
  shift_decimal (d, f.desc->decimals - d->decimals);

  /* Store number */
  switch (FIELD_TYPE (f))
    {
    case 'B':
      {
	int digits = f.desc->digits;
	if (f.desc->size <= 4)
	  {
	    int val;
	    if (!mpz_fits_sint_p (d->number))
	      goto overflow;
	    val = mpz_get_si (d->number);
	    if (val <= -cob_exp10[digits] || val >= cob_exp10[digits])
	      goto overflow;
	    if (!f.desc->have_sign && val < 0)
	      val = -val;
	    switch (f.desc->size)
	      {
	      case 1: *(char *) f.data = val; break;
	      case 2: *(short *) f.data = val; break;
	      case 4: *(long *) f.data = val; break;
	      }
	  }
	else
	  {
	    long long val;
	    mpz_t r;
	    mpz_init (r);
	    mpz_fdiv_r_2exp (r, d->number, 32);
	    mpz_fdiv_q_2exp (d->number, d->number, 32);
	    if (!mpz_fits_sint_p (d->number))
	      {
		mpz_clear (r);
		goto overflow;
	      }
	    val = mpz_get_si (d->number);
	    val = (val << 32) + mpz_get_ui (r);
	    mpz_clear (r);
	    if (val <= -cob_exp10LL[digits] || val >= cob_exp10LL[digits])
	      goto overflow;
	    if (!f.desc->have_sign && val < 0)
	      val = -val;
	    *(long long *) f.data = val;
	  }
	return;
      }

    case 'C':
      puts ("cob_set: not implemented yet");
      return;

    default:
      {
	char *p, buff[32];
	int size;
	int sign = (mpz_sgn (d->number) >= 0) ? 0 : 1;

	/* Build string */
	mpz_abs (d->number, d->number);
	size = mpz_sizeinbase (d->number, 10);
	p = (size < 32) ? buff : alloca (size + 1);
	mpz_get_str (p, 10, d->number);
	size = strlen (p);

	if (FIELD_TYPE (f) == '9')
	  {
	    int len = FIELD_LENGTH (f);
	    unsigned char *base = FIELD_BASE (f);
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
	    struct cob_field_desc desc =
	      {size, '9', f.desc->digits, f.desc->decimals, 1};
	    struct cob_field temp = {&desc, buff};
	    if (f.desc->digits < size)
	      goto overflow;
	    cob_put_sign (temp, sign);
	    cob_move (temp, f);
	  }
	return;
      }
    }

 overflow:
  cob_status = COB_STATUS_OVERFLOW;
  return;
}

void
cob_decimal_get_rounded (cob_decimal d, struct cob_field f)
{
  if (f.desc->decimals < d->decimals)
    {
      int sign = mpz_sgn (d->number);
      if (sign != 0)
	{
	  shift_decimal (d, f.desc->decimals - d->decimals + 1);
	  if (sign > 0)
	    mpz_add_ui (d->number, d->number, 5);
	  else
	    mpz_sub_ui (d->number, d->number, 5);
	}
    }
  cob_decimal_get (d, f);
}


/*
 * Convenience functions
 */

static void
decimal_get (cob_decimal d, struct cob_field f, int round)
{
  if (round)
    cob_decimal_get_rounded (d, f);
  else
    cob_decimal_get (d, f);
}

void
cob_add_int (struct cob_field f, int n, int decimals, int round)
{
  cob_decimal_set_field (cob_d1, f);
  cob_decimal_set_int (cob_d2, n, decimals);
  cob_decimal_add (cob_d1, cob_d2);
  decimal_get (cob_d1, f, round);
}

void
cob_add_int64 (struct cob_field f, long long n, int decimals, int round)
{
  cob_decimal_set_field (cob_d1, f);
  cob_decimal_set_int64 (cob_d2, n, decimals);
  cob_decimal_add (cob_d1, cob_d2);
  decimal_get (cob_d1, f, round);
}

void
cob_add (struct cob_field f1, struct cob_field f2, int round)
{
  cob_decimal_set_field (cob_d1, f1);
  cob_decimal_set_field (cob_d2, f2);
  cob_decimal_add (cob_d1, cob_d2);
  decimal_get (cob_d1, f1, round);
}

void
cob_sub_int (struct cob_field f, int n, int decimals, int round)
{
  cob_decimal_set_field (cob_d1, f);
  cob_decimal_set_int (cob_d2, n, decimals);
  cob_decimal_sub (cob_d1, cob_d2);
  decimal_get (cob_d1, f, round);
}

void
cob_sub_int64 (struct cob_field f, long long n, int decimals, int round)
{
  cob_decimal_set_field (cob_d1, f);
  cob_decimal_set_int64 (cob_d2, n, decimals);
  cob_decimal_sub (cob_d1, cob_d2);
  decimal_get (cob_d1, f, round);
}

void
cob_sub (struct cob_field f1, struct cob_field f2, int round)
{
  cob_decimal_set_field (cob_d1, f1);
  cob_decimal_set_field (cob_d2, f2);
  cob_decimal_sub (cob_d1, cob_d2);
  decimal_get (cob_d1, f1, round);
}

void
cob_div (struct cob_field dividend, struct cob_field divisor,
	 struct cob_field quotient, int round)
{
  cob_decimal_set_field (cob_d1, dividend);
  cob_decimal_set_field (cob_d2, divisor);
  cob_decimal_set (cob_d3, cob_d1);

  /* compute quotient */
  cob_decimal_div (cob_d1, cob_d2);
  cob_decimal_set (cob_d4, cob_d1);
  decimal_get (cob_d1, quotient, round);

  /* truncate digits from the quotient */
  shift_decimal (cob_d4, quotient.desc->decimals - cob_d4->decimals);

  /* compute remainder */
  cob_decimal_mul (cob_d4, cob_d2);
  cob_decimal_sub (cob_d3, cob_d4);
}

void
cob_div_reminder (struct cob_field remainder)
{
  decimal_get (cob_d3, remainder, 0);
}

void
cob_set_int (struct cob_field f, int n)
{
  cob_decimal_set_int (cob_d1, n, 0);
  cob_decimal_get (cob_d1, f);
}
