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

#include "decimal.h"

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

static decimal const_nan = &(struct decimal_num) { DECIMAL_NAN, 0, 0, NULL };
static decimal const_zero = &(struct decimal_num) { DECIMAL_POSITIVE, 0, 0, NULL };


/*
 * DECIMAL type
 */

decimal decimal_new (int ndigits, int weight)
{
  decimal num = malloc (sizeof (struct decimal_num));
  num->sign = DECIMAL_NAN;
  num->ndigits = ndigits;
  num->weight = weight;
  num->base = malloc (ndigits);
  num->digits = num->base;
  memset (num->base, 0, ndigits);
  return num;
}

void
decimal_free (decimal num)
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
simplify (decimal num)
{
  while (DECIMAL_NDIGITS (num) > 0 && DECIMAL_REF (num, 0) == 0)
    {
      DECIMAL_DIGITS (num)++;
      DECIMAL_NDIGITS (num)--;
      DECIMAL_WEIGHT (num)--;
    }
  while (DECIMAL_PRECISION (num) > 0 && DECIMAL_REF (num, DECIMAL_NDIGITS (num) - 1) == 0)
    {
      DECIMAL_NDIGITS (num)--;
    }
}


/*
 * DECIMAL <-> long
 */

decimal
long_to_decimal (long val)
{
  int i;
  decimal num;

  if (val == 0)
    return const_zero;

  if (val >= 1000000000 || val <= -1000000000)
    return const_nan;

  num = decimal_new (9, 8);
  DECIMAL_SIGN (num) = DECIMAL_POSITIVE;

  if (val < 0)
    {
      DECIMAL_SIGN (num) = DECIMAL_NEGATIVE;
      val = -val;
    }

  for (i = 8; i >= 0; i--)
    {
      DECIMAL_SET (num, i, val % 10);
      val /= 10;
    }

  simplify (num);
  return num;
}

/* Ignore the figures below the decimal point.
   No check for overflow. */
long
decimal_to_long (decimal num)
{
  int i;
  long val = 0;

  for (i = 0; i <= DECIMAL_WEIGHT (num); i++)
    val = val * 10 + DECIMAL_REF (num, i);

  if (DECIMAL_SIGN (num) == DECIMAL_NEGATIVE)
    val = -val;

  return val;
}


/*
 * DECIMAL <-> long long
 */

decimal
long_long_to_decimal (long long val)
{
  int i;
  decimal num;

  if (val == 0)
    return const_zero;

  if (val >= 1000000000000000000 || val <= -1000000000000000000)
    return const_nan;

  num = decimal_new (18, 17);
  DECIMAL_SIGN (num) = DECIMAL_POSITIVE;

  if (val < 0)
    {
      DECIMAL_SIGN (num) = DECIMAL_NEGATIVE;
      val = -val;
    }

  for (i = 17; i >= 0; i--)
    {
      DECIMAL_SET (num, i, val % 10);
      val /= 10;
    }

  simplify (num);
  return num;
}

/* Ignore the figures below the decimal point.
   No check for overflow. */
long long
decimal_to_long_long (decimal num)
{
  int i;
  long long val = 0;

  for (i = 0; i <= DECIMAL_WEIGHT (num); i++)
    val = val * 10 + DECIMAL_REF (num, i);

  if (DECIMAL_SIGN (num) == DECIMAL_NEGATIVE)
    val = -val;

  return val;
}


/*
 * DECIMAL <-> double
 */

decimal
double_to_decimal (double val)
{
  puts ("not implemented yet");
  abort ();
}

double
decimal_to_double (decimal num)
{
  int i;
  double scale = pow (10.0, DECIMAL_WEIGHT (num));
  double result = 0.0;

  for (i = 0; i < DECIMAL_NDIGITS (num); i++)
    {
      result += DECIMAL_REF (num, i) * scale;
      scale /= 10.0;
    }

  return result;
}


/*
 * DECIMAL <-> string
 */

decimal string_to_decimal (const char *str)
{
  enum decimal_sign sign = DECIMAL_POSITIVE;
  int ndigits = 0;
  int ndecimal = 0;
  int have_decimal = 0;
  char *digits = malloc (strlen (str));
  char *p = digits;
  char c;
  decimal num;

  /* Check the sign of the number */
  if (*str == '+')
    {
      sign = DECIMAL_POSITIVE;
      str++;
    }
  else if (*str == '-')
    {
      sign = DECIMAL_NEGATIVE;
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

  num = decimal_new (ndigits, ndigits - ndecimal - 1);
  DECIMAL_SIGN (num) = sign;
  memcpy (DECIMAL_DIGITS (num), digits, ndigits);
end:
  free (digits);
  simplify (num);
  return num;
}

char *
decimal_to_string (decimal num, char *buf)
{
  int i, j;
  char *p = buf;

  if (DECIMAL_SIGN (num) == DECIMAL_NAN)
    {
      strcpy (buf, "NaN");
      return buf;
    }

  if (DECIMAL_NDIGITS (num) == 0)
    {
      strcpy (buf, "0");
      return buf;
    }

  if (DECIMAL_SIGN (num) == DECIMAL_NEGATIVE)
    *p++ = '-';

  for (i = 0; i <= DECIMAL_WEIGHT (num); i++)
    *p++ = DECIMAL_REF (num, i) + '0';

  if (i != DECIMAL_NDIGITS (num))
    {
      *p++ = '.';
      for (j = DECIMAL_WEIGHT (num); j < -1; j++)
	*p++ = '0';
      for (; i < DECIMAL_NDIGITS (num); i++)
	*p++ = DECIMAL_REF (num, i) + '0';
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
cmp_abs (decimal n1, decimal n2)
{
  int i1, i2;
  int w1 = DECIMAL_WEIGHT (n1);
  int w2 = DECIMAL_WEIGHT (n2);

  for (i1 = 0; w1 > w2 && i1 < DECIMAL_NDIGITS (n1); i1++, w1--)
    if (DECIMAL_REF (n1, i1) != 0)
      return 1;

  for (i2 = 0; w2 > w1 && i2 < DECIMAL_NDIGITS (n2); i2++, w2--)
    if (DECIMAL_REF (n2, i2) != 0)
      return -1;

  if (w1 == w2)
    for (; i1 < DECIMAL_NDIGITS (n1) && i2 < DECIMAL_NDIGITS (n2); i1++, i2++)
      {
	int diff = DECIMAL_REF (n1, i1) - DECIMAL_REF (n2, i2);
	if (diff > 0)
	  return 1;
	else if (diff < 0)
	  return -1;
      }

  for (; i1 < DECIMAL_NDIGITS (n1); i1++)
    if (DECIMAL_REF (n1, i1) != 0)
      return 1;

  for (; i2 < DECIMAL_NDIGITS (n2); i2++)
    if (DECIMAL_REF (n2, i2) != 0)
      return -1;

  return 0;
}

/* |rv| := |n1| + |n2|
 */
static void
add_abs (decimal n1, decimal n2, decimal rv)
{
  int i1, i2, ir;
  int carry = 0;

  i1 = DECIMAL_WEIGHT (n1) + DECIMAL_PRECISION (rv);
  i2 = DECIMAL_WEIGHT (n2) + DECIMAL_PRECISION (rv);
  for (ir = DECIMAL_NDIGITS (rv) - 1; ir >= 0; ir--, i1--, i2--)
    {
      if (0 <= i1 && i1 < DECIMAL_NDIGITS (n1))
	carry += DECIMAL_REF (n1, i1);
      if (0 <= i2 && i2 < DECIMAL_NDIGITS (n2))
	carry += DECIMAL_REF (n2, i2);

      DECIMAL_SET (rv, ir, carry % 10);
      carry /= 10;
    }

  simplify (rv);
}

/* |rv| := |n1| - |n2|
 */
static void
sub_abs (decimal n1, decimal n2, decimal rv)
{
  int i1, i2, ir;
  int borrow = 0;

  i1 = DECIMAL_WEIGHT (n1) + DECIMAL_PRECISION (rv);
  i2 = DECIMAL_WEIGHT (n2) + DECIMAL_PRECISION (rv);
  for (ir = DECIMAL_NDIGITS (rv) - 1; ir >= 0; ir--, i1--, i2--)
    {
      if (0 <= i1 && i1 < DECIMAL_NDIGITS (n1))
	borrow += DECIMAL_REF (n1, i1);
      if (0 <= i2 && i2 < DECIMAL_NDIGITS (n2))
	borrow -= DECIMAL_REF (n2, i2);

      if (borrow < 0)
	{
	  DECIMAL_SET (rv, ir, borrow + 10);
	  borrow = -1;
	}
      else
	{
	  DECIMAL_SET (rv, ir, borrow);
	  borrow = 0;
	}
    }

  simplify (rv);
}


/*
 * Operations
 */

int decimal_cmp (decimal n1, decimal n2)
{
  /* Check for NaN */
  if (DECIMAL_SIGN (n1) == DECIMAL_NAN || DECIMAL_SIGN (n2) == DECIMAL_NAN)
    abort ();

  if (DECIMAL_SIGN (n1) == DECIMAL_SIGN (n2))
    {
      if (DECIMAL_SIGN (n1) == DECIMAL_POSITIVE)
	return cmp_abs (n1, n2);
      else
	return - cmp_abs (n1, n2);
    }
  else
    {
      if (DECIMAL_SIGN (n1) == DECIMAL_POSITIVE)
	return 1;
      else
	return -1;
    }
}

decimal decimal_add (decimal n1, decimal n2)
{
  int ndigits, weight;
  decimal rv;

  /* Check for NaN */
  if (DECIMAL_SIGN (n1) == DECIMAL_NAN || DECIMAL_SIGN (n2) == DECIMAL_NAN)
    return const_nan;

  /* Create return value */
  weight = MAX (DECIMAL_WEIGHT (n1), DECIMAL_WEIGHT (n2)) + 1;
  ndigits = weight + MAX (DECIMAL_PRECISION (n1), DECIMAL_PRECISION (n2)) + 1;
  rv = decimal_new (ndigits, weight);

  /* If the same sign... */
  if (DECIMAL_SIGN (n1) == DECIMAL_SIGN (n2))
    {
      add_abs (n1, n2, rv);
      DECIMAL_SIGN (rv) = DECIMAL_SIGN (n1);
      return rv;
    }

  /* If dirrent signs... */
  switch (cmp_abs (n1, n2))
    {
    case 0:
      return const_zero;

    case 1:
      sub_abs (n1, n2, rv);
      DECIMAL_SIGN (rv) = DECIMAL_SIGN (n1);
      return rv;

    case -1:
      sub_abs (n2, n1, rv);
      DECIMAL_SIGN (rv) = DECIMAL_SIGN (n2);
      return rv;
    }

  return const_nan;
}

decimal decimal_sub (decimal n1, decimal n2)
{
  int ndigits, weight;
  decimal rv;

  /* Check for NaN */
  if (DECIMAL_SIGN (n1) == DECIMAL_NAN || DECIMAL_SIGN (n2) == DECIMAL_NAN)
    return const_nan;

  /* Create return value */
  weight = MAX (DECIMAL_WEIGHT (n1), DECIMAL_WEIGHT (n2)) + 1;
  ndigits = weight + MAX (DECIMAL_PRECISION (n1), DECIMAL_PRECISION (n2)) + 1;
  rv = decimal_new (ndigits, weight);

  /* If different signs... */
  if (DECIMAL_SIGN (n1) != DECIMAL_SIGN (n2))
    {
      add_abs (n1, n2, rv);
      DECIMAL_SIGN (rv) = DECIMAL_SIGN (n1);
      return rv;
    }

  /* If the same sign... */
  switch (cmp_abs (n1, n2))
    {
    case 0:
      return const_zero;

    case 1:
      sub_abs (n1, n2, rv);
      DECIMAL_SIGN (rv) = DECIMAL_SIGN (n1);
      return rv;

    case -1:
      sub_abs (n2, n1, rv);
      DECIMAL_SIGN (rv) = ((DECIMAL_SIGN (n1) == DECIMAL_POSITIVE)
		       ? DECIMAL_NEGATIVE : DECIMAL_POSITIVE);
      return rv;
    }

  return const_nan;
}

decimal decimal_mul (decimal n1, decimal n2)
{
  int i1, i2, ir;
  int ndigits, weight;
  decimal rv;

  /* Check for NaN */
  if (DECIMAL_SIGN (n1) == DECIMAL_NAN || DECIMAL_SIGN (n2) == DECIMAL_NAN)
    return const_nan;

  /* Create return value */
  ndigits = DECIMAL_NDIGITS (n1) + DECIMAL_NDIGITS (n2);
  weight = DECIMAL_WEIGHT (n1) + DECIMAL_WEIGHT (n2) + 1;
  rv = decimal_new (ndigits, weight);

  /* Sign */
  DECIMAL_SIGN (rv) =
    (DECIMAL_SIGN (n1) == DECIMAL_SIGN (n2)) ? DECIMAL_POSITIVE : DECIMAL_NEGATIVE;

  /* Multiply */
  ir = ndigits;
  for (i1 = DECIMAL_NDIGITS (n1) - 1; i1 >= 0; i1--)
    {
      int sum = 0;
      int i = --ir;
      for (i2 = DECIMAL_NDIGITS (n2) - 1; i2 >= 0; i2--)
	{
	  sum += DECIMAL_REF (rv, i) + DECIMAL_REF (n1, i1) * DECIMAL_REF (n2, i2);
	  DECIMAL_SET (rv, i--, sum % 10);
	  sum /= 10;
	}
      DECIMAL_SET (rv, i, sum);
    }

  simplify (rv);
  return rv;
}

decimal decimal_div (decimal n1, decimal n2)
{
  int ndigits, weight;
  decimal rv, dividend, divisor[10];
  int weight_tmp;
  int i, ri;
  long guess;
  long first_div, first_have;
  int first_nextdigit;

  /* Divided by 0 */
  if (DECIMAL_NDIGITS (n2) == 0)
    return const_nan;

  /* Divide 0 */
  if (DECIMAL_NDIGITS (n1) == 0)
    return const_zero;

  /* Init return value */
  weight = DECIMAL_WEIGHT (n1) - DECIMAL_WEIGHT (n2) + 1;
  ndigits = MAX (6 + weight, 1);
  rv = decimal_new (ndigits + 2, weight);
  DECIMAL_SIGN (rv) = (n1->sign == n2->sign) ? DECIMAL_POSITIVE : DECIMAL_NEGATIVE;
  DECIMAL_SET (rv, 0, 0);

  /* Init dividend */
  dividend = decimal_new (DECIMAL_NDIGITS (n1), 0);
  DECIMAL_SIGN (dividend) = DECIMAL_POSITIVE;
  memcpy (DECIMAL_DIGITS (dividend), DECIMAL_DIGITS (n1), DECIMAL_NDIGITS (n1));

  /* Init divisors */
  for (i = 2; i < 10; i++)
    divisor[i] = NULL;
  divisor[1] = decimal_new (DECIMAL_NDIGITS (n2) + 1, 1);
  DECIMAL_SIGN (divisor[1]) = DECIMAL_POSITIVE;
  DECIMAL_SET (divisor[1], 0, 0);
  memcpy (DECIMAL_DIGITS (divisor[1]) + 1, DECIMAL_DIGITS (n2), DECIMAL_NDIGITS (n2));

  first_have = 0;
  first_nextdigit = 0;
  first_div = DECIMAL_REF (divisor[1], 1) * 10;
  if (DECIMAL_NDIGITS (divisor[1]) > 2)
    first_div += DECIMAL_REF (divisor[1], 2);

  weight_tmp = 1;

  for (ri = 0; ri <= ndigits; ri++)
    {
      int stat = 0;

      first_have = first_have * 10;
      if (first_nextdigit >= 0 && first_nextdigit < DECIMAL_NDIGITS (dividend))
	first_have += DECIMAL_REF (dividend, first_nextdigit);
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
	      divisor[guess] = decimal_new (DECIMAL_NDIGITS (divisor[1]), 1);
	      for (i = DECIMAL_NDIGITS (divisor[1]) - 1; i >= 0; i--)
		{
		  sum += DECIMAL_REF (divisor[1], i) * guess;
		  DECIMAL_SET (divisor[guess], i, sum % 10);
		  sum /= 10;
		}
	    }

	  DECIMAL_WEIGHT (divisor[guess]) = weight_tmp;

	  stat = cmp_abs (dividend, divisor[guess]);
	  if (stat >= 0)
	    break;
	}

      DECIMAL_SET (rv, ri + 1, guess);
      if (stat == 0)
	{
	  ri++;
	  break;
	}

      weight_tmp--;

      if (guess == 0)
	continue;

      sub_abs (dividend, divisor[guess], dividend);

      first_nextdigit = DECIMAL_WEIGHT (dividend) - weight_tmp;
      first_have = 0;
      if (first_nextdigit >= 0 && first_nextdigit < DECIMAL_NDIGITS (dividend))
	first_have = DECIMAL_REF (dividend, first_nextdigit);
      first_nextdigit++;
    }

  DECIMAL_NDIGITS (rv) = ri + 1;

  return rv;
}


/*
 * Debug print
 */

void
decimal_print (decimal num)
{
  char buff[BUFSIZ];
  puts (decimal_to_string (num, buff));
}


#if 0
int
main ()
{
  char buff[256];
  decimal n1, n2, rv;

  n1 = long_to_decimal (2);
  n2 = string_to_decimal ("20.55");
  rv = decimal_div (n1, n2);
  puts (decimal_to_string (rv, buff));
  printf ("%ld\n", decimal_to_long (rv));

  return 0;
}
#endif
