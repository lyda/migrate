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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "bcd.h"

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

static bcd const_nan = &(struct bcd_num) { BCD_NAN, 0, 0, NULL };
static bcd const_zero = &(struct bcd_num) { BCD_POSITIVE, 0, 0, NULL };


/*
 * BCD type
 */

bcd bcd_new (int ndigits, int weight)
{
  bcd num = malloc (sizeof (struct bcd_num));
  num->sign = BCD_NAN;
  num->ndigits = ndigits;
  num->weight = weight;
  num->base = malloc (ndigits);
  num->digits = num->base;
  memset (num->base, 0, ndigits);
  return num;
}

void
bcd_free (bcd num)
{
  if (num == const_nan || num == const_zero)
    return;

  free (num->base);
  free (num);
}

/* Remove unnecessary zeros.
 * 
 *  ex. 00123.4500  ->  123.45
 */
static void
simplify (bcd num)
{
  while (BCD_NDIGITS (num) > 0 && BCD_REF (num, 0) == 0)
    {
      BCD_DIGITS (num)++;
      BCD_NDIGITS (num)--;
      BCD_WEIGHT (num)--;
    }
  while (BCD_PRECISION (num) > 0 && BCD_REF (num, BCD_NDIGITS (num) - 1) == 0)
    {
      BCD_NDIGITS (num)--;
    }
}


/*
 * BCD <-> long
 */

bcd
long_to_bcd (long val)
{
  int i;
  bcd num;

  if (val == 0)
    return const_zero;

  if (val >= 1000000000 || val <= -1000000000)
    return const_nan;

  num = bcd_new (9, 8);
  BCD_SIGN (num) = BCD_POSITIVE;

  if (val < 0)
    {
      BCD_SIGN (num) = BCD_NEGATIVE;
      val = -val;
    }

  for (i = 8; i >= 0; i--)
    {
      BCD_SET (num, i, val % 10);
      val /= 10;
    }

  simplify (num);
  return num;
}

/* Ignore the figures below the decimal point.
   No check for overflow. */
long
bcd_to_long (bcd num)
{
  int i;
  long val = 0;

  for (i = 0; i <= BCD_WEIGHT (num); i++)
    val = val * 10 + BCD_REF (num, i);

  if (BCD_SIGN (num) == BCD_NEGATIVE)
    val = -val;

  return val;
}


/*
 * BCD <-> long long
 */

bcd
long_long_to_bcd (long long val)
{
  int i;
  bcd num;

  if (val == 0)
    return const_zero;

  if (val >= 1000000000000000000 || val <= -1000000000000000000)
    return const_nan;

  num = bcd_new (18, 17);
  BCD_SIGN (num) = BCD_POSITIVE;

  if (val < 0)
    {
      BCD_SIGN (num) = BCD_NEGATIVE;
      val = -val;
    }

  for (i = 17; i >= 0; i--)
    {
      BCD_SET (num, i, val % 10);
      val /= 10;
    }

  simplify (num);
  return num;
}

/* Ignore the figures below the decimal point.
   No check for overflow. */
long long
bcd_to_long_long (bcd num)
{
  int i;
  long long val = 0;

  for (i = 0; i <= BCD_WEIGHT (num); i++)
    val = val * 10 + BCD_REF (num, i);

  if (BCD_SIGN (num) == BCD_NEGATIVE)
    val = -val;

  return val;
}


/*
 * BCD <-> double
 */

bcd
double_to_bcd (double val)
{
  puts ("not implemented yet");
  abort ();
}

double
bcd_to_double (bcd num)
{
  int i;
  double scale = pow (10.0, BCD_WEIGHT (num));
  double result = 0.0;

  for (i = 0; i < BCD_NDIGITS (num); i++)
    {
      result += BCD_REF (num, i) * scale;
      scale /= 10.0;
    }

  return result;
}


/*
 * BCD <-> string
 */

bcd string_to_bcd (const char *str)
{
  enum bcd_sign sign = BCD_POSITIVE;
  int ndigits = 0;
  int ndecimal = 0;
  int have_decimal = 0;
  char *digits = malloc (strlen (str));
  char *p = digits;
  char c;
  bcd num;

  /* Check the sign of the number */
  if (*str == '+')
    {
      sign = BCD_POSITIVE;
      str++;
    }
  else if (*str == '-')
    {
      sign = BCD_NEGATIVE;
      str++;
    }

  /* Parse digits */
  for (c = *str; c; c = *++str)
    {
      /* Decimal point */
      if (c == '.')
	{
	  if (have_decimal)
	    {
	      /* multiple decimal points */
	      num = const_nan;
	      goto end;
	    }
	  else
	    have_decimal = 1;
	}
      /* Digits */
      else if ('0' <= c && c <= '9')
	{
	  *p++ = c - '0';
	  ndigits++;
	  if (have_decimal)
	    ndecimal++;
	}
      /* Invalid character */
      else
	{
	  num = const_nan;
	  goto end;
	}
    }

  num = bcd_new (ndigits, ndigits - ndecimal - 1);
  BCD_SIGN (num) = sign;
  memcpy (BCD_DIGITS (num), digits, ndigits);
end:
  free (digits);
  simplify (num);
  return num;
}

char *
bcd_to_string (bcd num, char *buf)
{
  int i, j;
  char *p = buf;

  if (BCD_SIGN (num) == BCD_NAN)
    {
      strcpy (buf, "NaN");
      return buf;
    }

  if (BCD_NDIGITS (num) == 0)
    {
      strcpy (buf, "0");
      return buf;
    }

  if (BCD_SIGN (num) == BCD_NEGATIVE)
    *p++ = '-';

  for (i = 0; i <= BCD_WEIGHT (num); i++)
    *p++ = BCD_REF (num, i) + '0';

  if (i != BCD_NDIGITS (num))
    {
      *p++ = '.';
      for (j = BCD_WEIGHT (num); j < -1; j++)
	*p++ = '0';
      for (; i < BCD_NDIGITS (num); i++)
	*p++ = BCD_REF (num, i) + '0';
    }

  *p = '\0';
  return buf;
}


/*
 * Low level functions
 */

/* Return 0 if |n1| = |n2|
 *        1 if |n1| > |n2|
 *       -1 if |n1| < |n2|
 */
static int
cmp_abs (bcd n1, bcd n2)
{
  int i1, i2;
  int w1 = BCD_WEIGHT (n1);
  int w2 = BCD_WEIGHT (n2);

  for (i1 = 0; w1 > w2 && i1 < BCD_NDIGITS (n1); i1++, w1--)
    if (BCD_REF (n1, i1) != 0)
      return 1;

  for (i2 = 0; w2 > w1 && i2 < BCD_NDIGITS (n2); i2++, w2--)
    if (BCD_REF (n2, i2) != 0)
      return -1;

  if (w1 == w2)
    for (; i1 < BCD_NDIGITS (n1) && i2 < BCD_NDIGITS (n2); i1++, i2++)
      {
	int diff = BCD_REF (n1, i1) - BCD_REF (n2, i2);
	if (diff > 0)
	  return 1;
	else if (diff < 0)
	  return -1;
      }

  for (; i1 < BCD_NDIGITS (n1); i1++)
    if (BCD_REF (n1, i1) != 0)
      return 1;

  for (; i2 < BCD_NDIGITS (n2); i2++)
    if (BCD_REF (n2, i2) != 0)
      return -1;

  return 0;
}

/* |rv| := |n1| + |n2|
 */
static void
add_abs (bcd n1, bcd n2, bcd rv)
{
  int i1, i2, ir;
  int carry = 0;

  i1 = BCD_WEIGHT (n1) + BCD_PRECISION (rv);
  i2 = BCD_WEIGHT (n2) + BCD_PRECISION (rv);
  for (ir = BCD_NDIGITS (rv) - 1; ir >= 0; ir--, i1--, i2--)
    {
      if (0 <= i1 && i1 < BCD_NDIGITS (n1))
	carry += BCD_REF (n1, i1);
      if (0 <= i2 && i2 < BCD_NDIGITS (n2))
	carry += BCD_REF (n2, i2);

      BCD_SET (rv, ir, carry % 10);
      carry /= 10;
    }

  simplify (rv);
}

/* |rv| := |n1| - |n2|
 */
static void
sub_abs (bcd n1, bcd n2, bcd rv)
{
  int i1, i2, ir;
  int borrow = 0;

  i1 = BCD_WEIGHT (n1) + BCD_PRECISION (rv);
  i2 = BCD_WEIGHT (n2) + BCD_PRECISION (rv);
  for (ir = BCD_NDIGITS (rv) - 1; ir >= 0; ir--, i1--, i2--)
    {
      if (0 <= i1 && i1 < BCD_NDIGITS (n1))
	borrow += BCD_REF (n1, i1);
      if (0 <= i2 && i2 < BCD_NDIGITS (n2))
	borrow -= BCD_REF (n2, i2);

      if (borrow < 0)
	{
	  BCD_SET (rv, ir, borrow + 10);
	  borrow = -1;
	}
      else
	{
	  BCD_SET (rv, ir, borrow);
	  borrow = 0;
	}
    }

  simplify (rv);
}


/*
 * Operations
 */

int bcd_cmp (bcd n1, bcd n2)
{
  /* Check for NaN */
  if (BCD_SIGN (n1) == BCD_NAN || BCD_SIGN (n2) == BCD_NAN)
    abort ();

  if (BCD_SIGN (n1) == BCD_SIGN (n2))
    {
      if (BCD_SIGN (n1) == BCD_POSITIVE)
	return cmp_abs (n1, n2);
      else
	return - cmp_abs (n1, n2);
    }
  else
    {
      if (BCD_SIGN (n1) == BCD_POSITIVE)
	return 1;
      else
	return -1;
    }
}

bcd bcd_add (bcd n1, bcd n2)
{
  int ndigits, weight;
  bcd rv;

  /* Check for NaN */
  if (BCD_SIGN (n1) == BCD_NAN || BCD_SIGN (n2) == BCD_NAN)
    return const_nan;

  /* Create return value */
  weight = MAX (BCD_WEIGHT (n1), BCD_WEIGHT (n2)) + 1;
  ndigits = weight + MAX (BCD_PRECISION (n1), BCD_PRECISION (n2)) + 1;
  rv = bcd_new (ndigits, weight);

  /* If the same sign... */
  if (BCD_SIGN (n1) == BCD_SIGN (n2))
    {
      add_abs (n1, n2, rv);
      BCD_SIGN (rv) = BCD_SIGN (n1);
      return rv;
    }

  /* If dirrent signs... */
  switch (cmp_abs (n1, n2))
    {
    case 0:
      return const_zero;

    case 1:
      sub_abs (n1, n2, rv);
      BCD_SIGN (rv) = BCD_SIGN (n1);
      return rv;

    case -1:
      sub_abs (n2, n1, rv);
      BCD_SIGN (rv) = BCD_SIGN (n2);
      return rv;
    }

  return const_nan;
}

bcd bcd_sub (bcd n1, bcd n2)
{
  int ndigits, weight;
  bcd rv;

  /* Check for NaN */
  if (BCD_SIGN (n1) == BCD_NAN || BCD_SIGN (n2) == BCD_NAN)
    return const_nan;

  /* Create return value */
  weight = MAX (BCD_WEIGHT (n1), BCD_WEIGHT (n2)) + 1;
  ndigits = weight + MAX (BCD_PRECISION (n1), BCD_PRECISION (n2)) + 1;
  rv = bcd_new (ndigits, weight);

  /* If different signs... */
  if (BCD_SIGN (n1) != BCD_SIGN (n2))
    {
      add_abs (n1, n2, rv);
      BCD_SIGN (rv) = BCD_SIGN (n1);
      return rv;
    }

  /* If the same sign... */
  switch (cmp_abs (n1, n2))
    {
    case 0:
      return const_zero;

    case 1:
      sub_abs (n1, n2, rv);
      BCD_SIGN (rv) = BCD_SIGN (n1);
      return rv;

    case -1:
      sub_abs (n2, n1, rv);
      BCD_SIGN (rv) = ((BCD_SIGN (n1) == BCD_POSITIVE)
		       ? BCD_NEGATIVE : BCD_POSITIVE);
      return rv;
    }

  return const_nan;
}

bcd bcd_mul (bcd n1, bcd n2)
{
  int i1, i2, ir;
  int ndigits, weight;
  bcd rv;

  /* Check for NaN */
  if (BCD_SIGN (n1) == BCD_NAN || BCD_SIGN (n2) == BCD_NAN)
    return const_nan;

  /* Create return value */
  ndigits = BCD_NDIGITS (n1) + BCD_NDIGITS (n2);
  weight = BCD_WEIGHT (n1) + BCD_WEIGHT (n2) + 1;
  rv = bcd_new (ndigits, weight);

  /* Sign */
  BCD_SIGN (rv) =
    (BCD_SIGN (n1) == BCD_SIGN (n2)) ? BCD_POSITIVE : BCD_NEGATIVE;

  /* Multiply */
  ir = ndigits;
  for (i1 = BCD_NDIGITS (n1) - 1; i1 >= 0; i1--)
    {
      int sum = 0;
      int i = --ir;
      for (i2 = BCD_NDIGITS (n2) - 1; i2 >= 0; i2--)
	{
	  sum += BCD_REF (rv, i) + BCD_REF (n1, i1) * BCD_REF (n2, i2);
	  BCD_SET (rv, i--, sum % 10);
	  sum /= 10;
	}
      BCD_SET (rv, i, sum);
    }

  simplify (rv);
  return rv;
}

bcd bcd_div (bcd n1, bcd n2)
{
  int ndigits, weight;
  bcd rv, dividend, divisor[10];
  int weight_tmp;
  int i, ri;
  long guess;
  long first_div, first_have;
  int first_nextdigit;

  /* Divided by 0 */
  if (BCD_NDIGITS (n2) == 0)
    return const_nan;

  /* Divide 0 */
  if (BCD_NDIGITS (n1) == 0)
    return const_zero;

  /* Init return value */
  weight = BCD_WEIGHT (n1) - BCD_WEIGHT (n2) + 1;
  ndigits = MAX (6 + weight, 1);
  rv = bcd_new (ndigits + 2, weight);
  BCD_SIGN (rv) = (n1->sign == n2->sign) ? BCD_POSITIVE : BCD_NEGATIVE;
  BCD_SET (rv, 0, 0);

  /* Init dividend */
  dividend = bcd_new (BCD_NDIGITS (n1), 0);
  BCD_SIGN (dividend) = BCD_POSITIVE;
  memcpy (BCD_DIGITS (dividend), BCD_DIGITS (n1), BCD_NDIGITS (n1));

  /* Init divisors */
  for (i = 2; i < 10; i++)
    divisor[i] = NULL;
  divisor[1] = bcd_new (BCD_NDIGITS (n2) + 1, 1);
  BCD_SIGN (divisor[1]) = BCD_POSITIVE;
  BCD_SET (divisor[1], 0, 0);
  memcpy (BCD_DIGITS (divisor[1]) + 1, BCD_DIGITS (n2), BCD_NDIGITS (n2));

  first_have = 0;
  first_nextdigit = 0;
  first_div = BCD_REF (divisor[1], 1) * 10;
  if (BCD_NDIGITS (divisor[1]) > 2)
    first_div += BCD_REF (divisor[1], 2);

  weight_tmp = 1;

  for (ri = 0; ri <= ndigits; ri++)
    {
      int stat = 0;

      first_have = first_have * 10;
      if (first_nextdigit >= 0 && first_nextdigit < BCD_NDIGITS (dividend))
	first_have += BCD_REF (dividend, first_nextdigit);
      first_nextdigit++;

      guess = (first_have * 10) / first_div + 1;
      if (guess > 9)
	guess = 9;
      for (; guess > 0; guess--)
	{
	  if (divisor[guess] == NULL)
	    {
	      int i;
	      long sum = 0;
	      divisor[guess] = bcd_new (BCD_NDIGITS (divisor[1]), 1);
	      for (i = BCD_NDIGITS (divisor[1]) - 1; i >= 0; i--)
		{
		  sum += BCD_REF (divisor[1], i) * guess;
		  BCD_SET (divisor[guess], i, sum % 10);
		  sum /= 10;
		}
	    }

	  BCD_WEIGHT (divisor[guess]) = weight_tmp;

	  stat = cmp_abs (dividend, divisor[guess]);
	  if (stat >= 0)
	    break;
	}

      BCD_SET (rv, ri + 1, guess);
      if (stat == 0)
	{
	  ri++;
	  break;
	}

      weight_tmp--;

      if (guess == 0)
	continue;

      sub_abs (dividend, divisor[guess], dividend);

      first_nextdigit = BCD_WEIGHT (dividend) - weight_tmp;
      first_have = 0;
      if (first_nextdigit >= 0 && first_nextdigit < BCD_NDIGITS (dividend))
	first_have = BCD_REF (dividend, first_nextdigit);
      first_nextdigit++;
    }

  BCD_NDIGITS (rv) = ri + 1;

  return rv;
}


/*
 * Debug print
 */

void
bcd_print (bcd num)
{
  char buff[BUFSIZ];
  puts (bcd_to_string (num, buff));
}


#if 0
int
main ()
{
  char buff[256];
  bcd n1, n2, rv;

  n1 = long_to_bcd (2);
  n2 = string_to_bcd ("20.55");
  rv = bcd_div (n1, n2);
  puts (bcd_to_string (rv, buff));
  printf ("%ld\n", bcd_to_long (rv));

  return 0;
}
#endif
