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
  if (d1->scale == DECIMAL_NAN || d2->scale == DECIMAL_NAN) \
    { \
      d1->scale = DECIMAL_NAN; \
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
  mpz_init (d->value);
  d->scale = 0;
}

void
cob_decimal_clear (cob_decimal *d)
{
  mpz_clear (d->value);
}

void
cob_decimal_print (cob_decimal *d)
{
  mpz_out_str (stdout, 10, d->value);
  if (d->scale)
    fprintf (stdout, " * 10^%d", -d->scale);
  fputs ("\n", stdout);
}

/* d->value *= 10^n, d->scale += n */
static void
shift_decimal (cob_decimal *d, int n)
{
  if (n > 0)
    {
      if (n < 10)
	/* 0 < n < 10 */
	mpz_mul_ui (d->value, d->value, cob_exp10[n]);
      else
	{
	  /* n >= 10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, n);
	  mpz_mul (d->value, d->value, m);
	  mpz_clear (m);
	}
    }
  else if (n < 0)
    {
      if (n > -10)
	/* -10 < n < 0 */
	mpz_tdiv_q_ui (d->value, d->value, cob_exp10[-n]);
      else
	{
	  /* n <= -10 */
	  mpz_t m;
	  mpz_init (m);
	  mpz_ui_pow_ui (m, 10, -n);
	  mpz_tdiv_q (d->value, d->value, m);
	  mpz_clear (m);
	}
    }
  d->scale += n;
}

static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
  if (d1->scale < d2->scale)
    shift_decimal (d1, d2->scale - d1->scale);
  else if (d1->scale > d2->scale)
    shift_decimal (d2, d1->scale - d2->scale);
}


/*
 * Decimal set/get
 */

void
cob_decimal_set (cob_decimal *dst, cob_decimal *src)
{
  mpz_set (dst->value, src->value);
  dst->scale = src->scale;
}

/* int */

void
cob_decimal_set_int (cob_decimal *d, int n)
{
  mpz_set_si (d->value, n);
  d->scale = 0;
}

int
cob_decimal_get_int (cob_decimal *d)
{
  cob_decimal_set (&cob_d1, d);
  shift_decimal (&cob_d1, -cob_d1.scale);
  return mpz_get_si (cob_d1.value);
}

/* double */

void
cob_decimal_set_double (cob_decimal *d, double v)
{
  mpz_set_d (d->value, v * 1.0e18);
  d->scale = 18;
}

double
cob_decimal_get_double (cob_decimal *d)
{
  int n = d->scale;
  double v = mpz_get_d (d->value);
  for (; n > 0; n--) v *= 10;
  for (; n < 0; n++) v /= 10;
  return v;
}

/* DISPLAY */

static void
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
      mpz_set_si (d->value, n);
    }
  else
    {
      unsigned char buff[size + 1];
      memcpy (buff, data, size);
      buff[size] = 0;
      mpz_set_str (d->value, buff, 10);
    }

  /* set sign and scale */
  if (sign < 0)
    mpz_neg (d->value, d->value);
  d->scale = f->attr->scale;
  cob_put_sign (f, sign);
}

static int
cob_decimal_get_display (cob_decimal *d, cob_field *f)
{
  int diff;
  size_t size;
  char *p, buff[32];
  int sign = mpz_sgn (d->value);
  unsigned char *data = COB_FIELD_DATA (f);

  /* build string */
  mpz_abs (d->value, d->value);
  size = mpz_sizeinbase (d->value, 10);
  p = (size < 32) ? buff : alloca (size + 1);
  mpz_get_str (p, 10, d->value);
  size = strlen (p);

  /* check overflow */
  diff = COB_FIELD_SIZE (f) - size;
  if (diff < 0)
    {
      COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
      return cob_exception_code;
    }

  /* store number */
  memset (data, '0', diff);
  memcpy (data + diff, p, size);
  cob_put_sign (f, sign);
  return 0;
}

/* BINARY */

static void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
  if (f->size <= 4)
    cob_decimal_set_int (d, cob_binary_get_int (f));
  else
    {
      long long val = cob_binary_get_int64 (f);
      mpz_set_si (d->value, val >> 32);
      mpz_mul_2exp (d->value, d->value, 32);
      mpz_add_ui (d->value, d->value, val & 0xffffffff);
    }
  d->scale = f->attr->scale;
}

static int
cob_decimal_get_binary (cob_decimal *d, cob_field *f)
{
  int digits = f->attr->digits;
  if (f->size <= 4)
    {
      int val;
      if (!mpz_fits_sint_p (d->value))
	goto overflow;
      val = mpz_get_si (d->value);
      if (val <= -cob_exp10[digits] || val >= cob_exp10[digits])
	goto overflow;
      if (!COB_FIELD_HAVE_SIGN (f) && val < 0)
	val = -val;
      cob_binary_set_int (f, val);
    }
  else
    {
      long long val;
      mpz_t r;
      mpz_init (r);
      mpz_fdiv_r_2exp (r, d->value, 32);
      mpz_fdiv_q_2exp (d->value, d->value, 32);
      if (!mpz_fits_sint_p (d->value))
	{
	  mpz_clear (r);
	  goto overflow;
	}
      val = mpz_get_si (d->value);
      val = (val << 32) + mpz_get_ui (r);
      mpz_clear (r);
      if (val <= -cob_exp10LL[digits] || val >= cob_exp10LL[digits])
	goto overflow;
      if (!COB_FIELD_HAVE_SIGN (f) && val < 0)
	val = -val;
      cob_binary_set_int64 (f, val);
    }
  return 0;

 overflow:
  COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
  return cob_exception_code;
}

/* PACKED-DECIMAL */

static void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
  int sign = cob_get_sign (f);
  int digits = f->attr->digits;
  unsigned char *p = f->data;

  mpz_set_ui (d->value, 0);
  if (digits % 2 == 0)
    {
      mpz_add_ui (d->value, d->value, (*p & 0x0f));
      digits--;
      p++;
    }
  while (digits > 1)
    {
      mpz_mul_ui (d->value, d->value, 100);
      mpz_add_ui (d->value, d->value, (*p >> 4) * 10 + (*p & 0x0f));
      digits -= 2;
      p++;
    }
  mpz_mul_ui (d->value, d->value, 10);
  mpz_add_ui (d->value, d->value, (*p >> 4));

  if (sign < 0)
    mpz_neg (d->value, d->value);
  d->scale = f->attr->scale;
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

int
cob_decimal_get_field (cob_decimal *d, cob_field *f)
{
  if (d->scale == DECIMAL_NAN)
    {
      COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
      return cob_exception_code;
    }

  /* work copy */
  if (d != &cob_d1)
    {
      cob_decimal_set (&cob_d1, d);
      d = &cob_d1;
    }

  /* append or truncate decimal digits */
  shift_decimal (d, f->attr->scale - d->scale);

  /* store number */
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      return cob_decimal_get_display (d, f);
    case COB_TYPE_NUMERIC_BINARY:
      return cob_decimal_get_binary (d, f);
    default:
      {
	cob_field_attr attr = {
	  COB_TYPE_NUMERIC_DISPLAY,
	  f->attr->digits,
	  f->attr->scale,
	  COB_FLAG_HAVE_SIGN
	};
	unsigned char data[f->attr->digits];
	cob_field temp = {f->attr->digits, data, &attr};
	if (cob_decimal_get_display (d, &temp) == 0)
	  cob_move (&temp, f);
	return cob_exception_code;
      }
    }
}

int
cob_decimal_get_field_round (cob_decimal *d, cob_field *f)
{
  if (f->attr->scale < d->scale)
    {
      int sign = mpz_sgn (d->value);
      if (sign != 0)
	{
	  /* work copy */
	  cob_decimal_set (&cob_d1, d);
	  d = &cob_d1;

	  /* rounding */
	  shift_decimal (d, f->attr->scale - d->scale + 1);
	  if (sign > 0)
	    mpz_add_ui (d->value, d->value, 5);
	  else
	    mpz_sub_ui (d->value, d->value, 5);
	}
    }
  return cob_decimal_get_field (d, f);
}


/*
 * Decimal arithmetic
 */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  align_decimal (d1, d2);
  mpz_add (d1->value, d1->value, d2->value);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  align_decimal (d1, d2);
  mpz_sub (d1->value, d1->value, d2->value);
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  d1->scale += d2->scale;
  mpz_mul (d1->value, d1->value, d2->value);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  /* check for division by zero */
  if (mpz_sgn (d2->value) == 0)
    {
      d1->scale = DECIMAL_NAN;
      COB_SET_EXCEPTION (COB_EC_SIZE_ZERO_DIVIDE);
      return;
    }

  d1->scale -= d2->scale;
  shift_decimal (d1, 19 + ((d1->scale < 0) ? -d1->scale : 0));
  mpz_tdiv_q (d1->value, d1->value, d2->value);
}

void
cob_decimal_pow (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  if (d2->scale == 0 && mpz_fits_ulong_p (d2->value))
    {
      unsigned int n = mpz_get_ui (d2->value);
      mpz_pow_ui (d1->value, d1->value, n);
      d1->scale *= n;
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
  return mpz_cmp (d1->value, d2->value);
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

static int
cob_add_int_to_display (cob_field *f, int n)
{
  int sign = cob_get_sign (f);
  unsigned char *data = COB_FIELD_DATA (f);
  size_t size = COB_FIELD_SIZE (f);
  int scale = COB_FIELD_SCALE (f);

  /* -x + n = -(x - n) */
  if (sign < 0)
    n = -n;

  if (scale < 0)
    {
      /* PIC 9(n)P(m) */
      if (-scale < 10)
	n /= cob_exp10[-scale];
      else
	n = 0;
    }
  else
    {
      /* PIC 9(n)V9(m) */
      size -= scale;
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
  return 0;

 overflow:
  cob_put_sign (f, sign);
  COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
  return cob_exception_code;
}


/*
 * Convenience functions
 */

int
cob_add (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_add (&cob_d1, &cob_d2);
  return cob_decimal_get_field (&cob_d1, f1);
}

int
cob_sub (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  return cob_decimal_get_field (&cob_d1, f1);
}

int
cob_add_round (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_add (&cob_d1, &cob_d2);
  return cob_decimal_get_field_round (&cob_d1, f1);
}

int
cob_sub_round (cob_field *f1, cob_field *f2)
{
  cob_decimal_set_field (&cob_d1, f1);
  cob_decimal_set_field (&cob_d2, f2);
  cob_decimal_sub (&cob_d1, &cob_d2);
  return cob_decimal_get_field_round (&cob_d1, f1);
}

int
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
      return cob_decimal_get_field (&cob_d1, f);
    default:
      return cob_add_int_to_display (f, n);
    }
}

int
cob_sub_int (cob_field *f, int n)
{
  return cob_add_int (f, -n);
}

int
cob_div_quotient (cob_field *dividend, cob_field *divisor,
		  cob_field *quotient, int round)
{
  int ret;

  cob_decimal_set_field (&cob_d1, dividend);
  cob_decimal_set_field (&cob_d2, divisor);
  cob_decimal_set (&cob_d3, &cob_d1);

  /* compute quotient */
  cob_decimal_div (&cob_d1, &cob_d2);
  if (cob_d1.scale == DECIMAL_NAN)
    {
      cob_d3.scale = DECIMAL_NAN;
      return cob_exception_code;
    }

  /* set quotient */
  cob_decimal_set (&cob_d4, &cob_d1);
  if (round)
    ret = cob_decimal_get_field_round (&cob_d1, quotient);
  else
    ret = cob_decimal_get_field (&cob_d1, quotient);

  /* truncate digits from the quotient */
  shift_decimal (&cob_d4, quotient->attr->scale - cob_d4.scale);

  /* compute remainder */
  cob_decimal_mul (&cob_d4, &cob_d2);
  cob_decimal_sub (&cob_d3, &cob_d4);

  return ret;
}

int
cob_div_remainder (cob_field *remainder)
{
  return cob_decimal_get_field (&cob_d3, remainder);
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
