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
}

void
cob_decimal_clear (cob_decimal *d)
{
}

#ifdef COB_DEBUG
void
cob_decimal_print (cob_decimal *d)
{
  fprintf (stdout, "%lld", d->data);
  if (d->expt)
    fprintf (stdout, " * 10^%d", d->expt);
  fputs ("\n", stdout);
}
#endif

static int
log10_decimal (cob_decimal *d)
{
  long long i64 = (d->data < 0) ? -d->data : d->data;

  if (i64 < cob_exp10LL[9])
    {
      long i32 = (long) i64;
      if (i32 < cob_exp10[4])
	{
	  if (i32 < cob_exp10[2])
	    return (i32 < cob_exp10[1]) ? 1 : 2;
	  else
	    return (i32 < cob_exp10[3]) ? 3 : 4;
	}
      else
	{
	  if (i32 < cob_exp10[6])
	    return (i32 < cob_exp10[5]) ? 5 : 6;
	  else if (i32 < cob_exp10[8])
	    return (i32 < cob_exp10[7]) ? 7 : 8;
	  else
	    return 9;
	}
    }
  else
    {
      if (i64 < cob_exp10LL[13])
	{
	  if (i64 < cob_exp10LL[11])
	    return (i64 < cob_exp10LL[10]) ? 10 : 11;
	  else
	    return (i64 < cob_exp10LL[12]) ? 12 : 13;
	}
      else
	{
	  if (i64 < cob_exp10LL[15])
	    return (i64 < cob_exp10LL[14]) ? 14 : 15;
	  else if (i64 < cob_exp10LL[17])
	    return (i64 < cob_exp10LL[16]) ? 16 : 17;
	  else
	    return 18;
	}
    }
}

/* d->data *= 10^n, d->expt -= n */
static void
shift_decimal (cob_decimal *d, int n)
{
  if (n > 0)
    d->data *= cob_exp10LL[n];
  else if (n < 0)
    d->data /= cob_exp10LL[-n];
  d->expt -= n;
}

static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
  int diff = d1->expt - d2->expt;
  if (diff > 0)
    {
      int log10 = log10_decimal (d1);
      if (log10 + diff > 18)
	{
	  shift_decimal (d1, 18 - log10);
	  shift_decimal (d2, 18 - log10 - diff);
	}
      else
	{
	  shift_decimal (d1, diff);
	}
    }
  else if (diff < 0)
    {
      int log10 = log10_decimal (d2);
      if (log10 - diff > 18)
	{
	  shift_decimal (d2, 18 - log10);
	  shift_decimal (d1, 18 - log10 + diff);
	}
      else
	{
	  shift_decimal (d2, - diff);
	}
    }
}


/*
 * Decimal set/get
 */

void
cob_decimal_set (cob_decimal *dst, cob_decimal *src)
{
  dst->data = src->data;
  dst->expt = src->expt;
}

/* int */

void
cob_decimal_set_int (cob_decimal *d, int n)
{
  d->data = n;
  d->expt = 0;
}

int
cob_decimal_get_int (cob_decimal *d)
{
  cob_decimal_set (&cob_d1, d);
  shift_decimal (&cob_d1, cob_d1.expt);
  return cob_d1.data;
}

/* double */

void
cob_decimal_set_double (cob_decimal *d, double v)
{
  d->data = v * 1e9;
  d->expt = -9;
}

double
cob_decimal_get_double (cob_decimal *d)
{
  int n = d->expt;
  double v = d->data;
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
      d->data = n;
    }
  else
    {
      unsigned char *endp = data + size;
      long long n = cob_d2i (*data++);
      while (data < endp)
	n = n * 10 + cob_d2i (*data++);
      d->data = n;
    }

  /* set sign and expt */
  if (sign < 0)
    d->data = -d->data;
  d->expt = f->attr->expt;
  cob_put_sign (f, sign);
}

void
cob_decimal_get_display (cob_decimal *d, cob_field *f)
{
  int diff;
  size_t size;
  char buff[32];
  int sign = 0;
  unsigned char *data = COB_FIELD_DATA (f);

  /* build string */
  if (d->data < 0)
    {
      sign = -1;
      d->data = -d->data;
    }
  sprintf (buff, "%lld", d->data);
  size = strlen (buff);

  /* check overflow */
  diff = COB_FIELD_SIZE (f) - size;
  if (diff < 0)
    {
      COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
      return;
    }

  /* store number */
  memset (data, '0', diff);
  memcpy (data + diff, buff, size);
  cob_put_sign (f, sign);
}

/* BINARY */

void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
  switch (f->size)
    {
    case 1: d->data = *(char *) f->data; break;
    case 2: d->data = *(short *) f->data; break;
    case 4: d->data = *(long *) f->data; break;
    case 8: d->data = *(long long *) f->data; break;
    }
  d->expt = f->attr->expt;
}

void
cob_decimal_get_binary (cob_decimal *d, cob_field *f)
{
  int digits = f->attr->digits;
  long long val = d->data;
  if (val <= -cob_exp10LL[digits] || val >= cob_exp10LL[digits])
    goto overflow;
  if (!COB_FIELD_HAVE_SIGN (f) && val < 0)
    val = -val;
  switch (f->size)
    {
    case 1: *(char *) f->data = val; break;
    case 2: *(short *) f->data = val; break;
    case 4: *(long *) f->data = val; break;
    case 8: *(long long *) f->data = val; break;
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

  d->data = 0;
  while (digits > 1)
    {
      d->data = d->data * 100 + (*p >> 4) * 10 + (*p & 0x0f);
      digits -= 2;
      p++;
    }
  if (digits > 0)
    {
      d->data = d->data * 10 + (*p >> 4);
    }

  if (sign < 0)
    d->data = -d->data;
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
      if (d->data != 0)
	{
	  /* work copy */
	  cob_decimal_set (&cob_d1, d);
	  d = &cob_d1;

	  /* rounding */
	  shift_decimal (d, d->expt - f->attr->expt + 1);
	  if (d->data > 0)
	    d->data += 5;
	  else
	    d->data -= 5;
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
  d1->data += d2->data;
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);
  align_decimal (d1, d2);
  d1->data -= d2->data;
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
  int s1, s2;
  unsigned long h1, h2, l1, l2;

  DECIMAL_CHECK (d1, d2);
  d1->expt += d2->expt;

  /* d = s * (h * 2^32 + l) */
 
  /* get sign */
  s1 = s2 = 1;
  if (d1->data < 0)
    {
      s1 = -1;
      d1->data = - d1->data;
    }
  if (d2->data < 0)
    {
      s2 = -1;
      d2->data = - d2->data;
    }
 
  h1 = d1->data >> 32;
  h2 = d2->data >> 32;
  l1 = d1->data & 0xffffffff;
  l2 = d2->data & 0xffffffff;
 
  /* d1 * d2 = s1 * (h1 * 2^32 + l1) * s2 * (h2 * 2^32 + l2)
     = s1 * s2 *
     {h1 * h2 * 2^64 + (h1 * l2 + h2 * l1) * 2^32 + l1 * l2} */
 
  if (h1 && h2)
    goto overflow;
  d1->data = (long long) h1 * l2 + (long long) h2 * l1;
  if ((d1->data >> 32) > 0)
    goto overflow;
  d1->data = s1 * s2 * ((long long) (d1->data << 32) + (long long) l1 * l2);
  return;
 
 overflow:
  d1->expt = DECIMAL_NAN;
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  /* check for division by zero */
  if (d2->data == 0)
    {
      d1->expt = DECIMAL_NAN;
      return;
    }

  d1->expt -= d2->expt;
  shift_decimal (d1, 18 - log10_decimal (d1));
  d1->data = d1->data / d2->data;
}

void
cob_decimal_pow (cob_decimal *d1, cob_decimal *d2)
{
  DECIMAL_CHECK (d1, d2);

  cob_decimal_set_double (d1, pow (cob_decimal_get_double (d1),
 				   cob_decimal_get_double (d2)));
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
  long long ret;
  align_decimal (d1, d2);
  ret = d1->data - d2->data;
  return (ret < 0) ? -1 : (ret > 0) ? 1 : 0;
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
