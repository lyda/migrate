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
align_decimal (cob_decimal *d1, cob_decimal *d2)
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

/* int */

void
cob_decimal_set_int (cob_decimal *d, int n)
{
  mpz_set_si (d->data, n);
  d->expt = 0;
}

int
cob_decimal_get_int (cob_decimal *d)
{
  cob_decimal_set (&cob_d1, d);
  shift_decimal (&cob_d1, cob_d1.expt);
  return mpz_get_si (cob_d1.data);
}

/* double */

void
cob_decimal_set_double (cob_decimal *d, double v)
{
  mpz_set_d (d->data, v * 10000000000000000.0);
  d->expt = -16;
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

/* DISPLAY */

void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
  int sign = cob_get_sign (f);
  size_t size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);

  /* skip leading zeros */
  while (size > 1 && cob_d2i (data[0]) == 0)
    {
      size--;
      data++;
    }

  /* set value */
  if (size < 10)
    {
      unsigned char *endp = data + size;
      int n = cob_d2i (*data++);
      while (data < endp)
	n = n * 10 + cob_d2i (*data++);
      mpz_set_si (d->data, n);
    }
  else
    {
      unsigned char buff[size + 1];
      memcpy (buff, data, size);
      buff[size] = 0;
      mpz_set_str (d->data, buff, 10);
    }

  /* set sign and expt */
  if (sign < 0)
    mpz_neg (d->data, d->data);
  d->expt = f->attr->expt;
  cob_put_sign (f, sign);
}

void
cob_decimal_get_display (cob_decimal *d, cob_field *f)
{
  int diff;
  size_t size;
  char *p, buff[32];
  int sign = mpz_sgn (d->data);
  unsigned char *data = COB_FIELD_DATA (f);

  /* build string */
  mpz_abs (d->data, d->data);
  size = mpz_sizeinbase (d->data, 10);
  p = (size < 32) ? buff : alloca (size + 1);
  mpz_get_str (p, 10, d->data);
  size = strlen (p);

  /* check overflow */
  diff = COB_FIELD_SIZE (f) - size;
  if (diff < 0)
    {
      COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
      return;
    }

  /* store number */
  memset (data, '0', diff);
  memcpy (data + diff, p, size);
  cob_put_sign (f, sign);
}

/* BINARY */

void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
  switch (f->size)
    {
    case 1: cob_decimal_set_int (d, *(char *) f->data); break;
    case 2: cob_decimal_set_int (d, *(short *) f->data); break;
    case 4: cob_decimal_set_int (d, *(long *) f->data); break;
    case 8:
      {
	long long val = *(long long *) f->data;
	mpz_set_si (d->data, val >> 32);
	mpz_mul_2exp (d->data, d->data, 32);
	mpz_add_ui (d->data, d->data, val & 0xffffffff);
	break;
      }
    }
  d->expt = f->attr->expt;
}

void
cob_decimal_get_binary (cob_decimal *d, cob_field *f)
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

 overflow:
  COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
}

/* PACKED-DECIMAL */

void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
  int sign = cob_get_sign (f);
  int digits = f->attr->digits;
  unsigned char *p = f->data;

  mpz_set_si (d->data, 0);
  while (digits > 1)
    {
      mpz_mul_ui (d->data, d->data, 100);
      mpz_add_ui (d->data, d->data, (*p >> 4) * 10 + (*p & 0x0f));
      digits -= 2;
      p++;
    }
  if (digits > 0)
    {
      mpz_mul_ui (d->data, d->data, 10);
      mpz_add_ui (d->data, d->data, (*p >> 4));
    }

  if (sign < 0)
    mpz_neg (d->data, d->data);
  d->expt = f->attr->expt;
}

/* General field */

void
cob_decimal_set_field (cob_decimal *d, cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
      cob_decimal_set_binary (d, f);
      break;
    case COB_TYPE_NUMERIC_PACKED:
      cob_decimal_set_packed (d, f);
      break;
    default:
      cob_decimal_set_display (d, f);
      break;
    }
}

void
cob_decimal_get_field (cob_decimal *d, cob_field *f)
{
  if (d->expt == DECIMAL_NAN)
    {
      COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
      return;
    }

  COB_SET_EXCEPTION (COB_EC_ZERO);

  /* work copy */
  if (d != &cob_d1)
    {
      cob_decimal_set (&cob_d1, d);
      d = &cob_d1;
    }

  /* append or truncate decimal digits */
  shift_decimal (d, d->expt - f->attr->expt);

  /* store number */
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      cob_decimal_get_display (d, f);
      break;
    case COB_TYPE_NUMERIC_BINARY:
      cob_decimal_get_binary (d, f);
      break;
    default:
      {
	cob_field_attr attr = {
	  COB_TYPE_NUMERIC_DISPLAY,
	  f->attr->digits,
	  f->attr->expt,
	  COB_FLAG_HAVE_SIGN
	};
	unsigned char data[f->attr->digits];
	cob_field temp = {f->attr->digits, data, &attr};
	cob_decimal_get_display (d, &temp);
	if (cob_exception_code == 0)
	  cob_move (&temp, f);
	break;
      }
    }
}

void
cob_decimal_get_field_r (cob_decimal *d, cob_field *f)
{
  if (f->attr->expt > d->expt)
    {
      int sign = mpz_sgn (d->data);
      if (sign != 0)
	{
	  /* work copy */
	  cob_decimal_set (&cob_d1, d);
	  d = &cob_d1;

	  /* rounding */
	  shift_decimal (d, d->expt - f->attr->expt + 1);
	  if (sign > 0)
	    mpz_add_ui (d->data, d->data, 5);
	  else
	    mpz_sub_ui (d->data, d->data, 5);
	}
    }
  cob_decimal_get_field (d, f);
}


/*
 * Decimal arithmetic
 */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  align_decimal (d1, d2);
  mpz_add (d1->data, d1->data, d2->data);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  align_decimal (d1, d2);
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
  align_decimal (d1, d2);
  return mpz_cmp (d1->data, d2->data);
}


/*
 * Optimized arithmetic for DISPLAY
 */

/* Array of three digits: {"\0\0\0", "\0\0\1", ..., "\9\9\9"} */
static unsigned char digit_table[1000][3];

static void
init_digit_table(void)
{
  int n = 0;
  int i, j, k;

  for (i = 0; i < 10; i++)
    for (j = 0; j < 10; j++)
      for (k = 0; k < 10; k++)
	{
	  digit_table[n][0] = i;
	  digit_table[n][1] = j;
	  digit_table[n][2] = k;
	  n++;
	}
}

static int
display_add_int (unsigned char *data, size_t size, unsigned int n)
{
  int carry = 0;
  unsigned char *dp;
  unsigned char *sp = data + size;

  while (n > 0)
    {
      /* get the least significant 3 digits from n */
      int i = n;
      n = n / 1000;
      dp = digit_table[i % 1000];

      /* add it to the string */
      for (i = 2; i >= 0; i--)
	{
	  /* check for overflow */
	  if (--sp < data)
	    {
	      for (; i >= 0; i--)
		carry += dp[i];
	      return carry;
	    }

	  /* perform addition */
	  if ((*sp += dp[i] + carry) > '9')
	    carry = 1, *sp -= 10;
	  else
	    carry = 0;
	}
    }
  if (carry == 0)
    return 0;

  /* carry up */
  while (--sp >= data)
    {
      if ((*sp += 1) <= '9')
	return 0;
      *sp = '0';
    }
  return 1;
}

static int
display_sub_int (unsigned char *data, size_t size, unsigned int n)
{
  int carry = 0;
  unsigned char *dp;
  unsigned char *sp = data + size;

  while (n > 0)
    {
      /* get the least significant 3 digits from n */
      int i = n;
      n = n / 1000;
      dp = digit_table[i % 1000];

      /* subtract it from the string */
      for (i = 2; i >= 0; i--)
	{
	  /* check for overflow */
	  if (--sp < data)
	    {
	      for (; i >= 0; i--)
		carry += dp[i];
	      return carry;
	    }

	  /* perform subtraction */
	  if ((*sp -= dp[i] + carry) < '0')
	    carry = 1, *sp += 10;
	  else
	    carry = 0;
	}
    }
  if (carry == 0)
    return 0;

  /* carry up */
  while (--sp >= data)
    {
      if ((*sp -= 1) >= '0')
	return 0;
      *sp = '9';
    }
  return 1;
}

static void
cob_add_int_to_display (cob_field *f, int n)
{
  int sign = cob_get_sign (f);
  unsigned char *data = COB_FIELD_DATA (f);
  size_t size = COB_FIELD_SIZE (f);
  int expt = COB_FIELD_EXPT (f);

  COB_SET_EXCEPTION (COB_EC_ZERO);

  /* -x + n = -(x - n) */
  if (sign < 0)
    n = -n;

  if (expt > 0)
    {
      /* PIC 9(n)P(m) */
      if (expt < 10)
	n /= cob_exp10[expt];
      else
	n = 0;
    }
  else
    {
      /* PIC 9(n)V9(m) */
      size += expt;
      if (size < 0)
	goto overflow;
    }

  if (n > 0)
    {
      /* add n to the field */
      if (display_add_int (data, size, n))
	{
	  /* if there wes an overflow, recover the last value */
	  display_sub_int (data, size, n);
	  goto overflow;
	}
    }
  else if (n < 0)
    {
      /* subtract n from the field */
      if (display_sub_int (data, size, -n))
	{
	  /* if there wes an overflow, inverse the sign */
	  int i;
	  for (i = 0; i < size; i++)
	    data[i] = cob_i2d (9 - cob_d2i (data[i]));
	  display_add_int (data, size, 1);
	  sign = - sign;
	}
    }

  cob_put_sign (f, sign);
  return;

 overflow:
  COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
  cob_put_sign (f, sign);
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
  cob_decimal_get_field (&cob_d1, f1);
}

void
cob_sub (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  cob_decimal_get_field (&cob_d1, f1);
}

void
cob_add_r (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_add (&cob_d1, &cob_d2);
  cob_decimal_get_field_r (&cob_d1, f1);
}

void
cob_sub_r (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  cob_decimal_get_field_r (&cob_d1, f1);
}

void
cob_add_int (cob_field *f, int n)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
    case COB_TYPE_NUMERIC_PACKED:
      /* not optimized */
      cob_decimal_set_field (&cob_d1, f);
      cob_decimal_set_int (&cob_d2, n);
      cob_decimal_add (&cob_d1, &cob_d2);
      cob_decimal_get_field (&cob_d1, f);
      break;
    default:
      cob_add_int_to_display (f, n);
      break;
    }
}

void
cob_sub_int (cob_field *f, int n)
{
  cob_add_int (f, -n);
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
    cob_decimal_get_field_r (&cob_d1, quotient);
  else
    cob_decimal_get_field (&cob_d1, quotient);

  /* truncate digits from the quotient */
  shift_decimal (&cob_d4, cob_d4.expt - quotient->attr->expt);

  /* compute remainder */
  cob_decimal_mul (&cob_d4, &cob_d2);
  cob_decimal_sub (&cob_d3, &cob_d4);
}

void
cob_div_remainder (cob_field *remainder)
{
  cob_decimal_get_field (&cob_d3, remainder);
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  return cob_decimal_cmp (&cob_d1, &cob_d2);
}


void
cob_init_numeric (void)
{
  cob_decimal_init (&cob_d1);
  cob_decimal_init (&cob_d2);
  cob_decimal_init (&cob_d3);
  cob_decimal_init (&cob_d4);

  init_digit_table ();
}
