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

#include "_libcob.h"
#include "decimal.h"


/*
 * Global variables
 */

int cob_size_error_flag;


/*
 * Decimal number
 */

decimal
make_decimal (void)
{
  decimal d = malloc (sizeof (struct decimal_number));
  mpz_init (d->number);
  d->decimals = 0;
  return d;
}

void
free_decimal (decimal d)
{
  mpz_clear (d->number);
  free (d);
}

void
print_decimal (decimal d)
{
  printf ("decimal(%p) = ", d);
  mpz_out_str (stdout, 10, d->number);
  if (d->decimals)
    printf (" * 10^-%d", d->decimals);
  puts ("");
}

/* d->number *= 10^n, d->decimals += n */
static void
shift_decimal (decimal d, int n)
{
  static unsigned long exp[10] =
    {1, 10, 100, 1000, 10000, 10000, 1000000, 10000000, 100000000, 1000000000};

  if (n == 0)
    return;

  if (n > 0)
    {
      if (n < 10)
	/* 0 < n < 10 */
	mpz_mul_ui (d->number, d->number, exp[n]);
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
  else
    {
      if (n > -10)
	/* -10 < n < 0 */
	mpz_tdiv_q_ui (d->number, d->number, exp[-n]);
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
arrange_decimal (decimal d1, decimal d2)
{
  if (d1->decimals < d2->decimals)
    shift_decimal (d1, d2->decimals - d1->decimals);
  else if (d2->decimals < d1->decimals)
    shift_decimal (d2, d1->decimals - d2->decimals);
}


/*
 * Data conversion
 */

void
cob_fld_to_decimal (struct fld_desc *f, unsigned char *s, union numeric_type *p)
{
  decimal d = make_decimal ();

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: mpz_set_si (d->number, *((signed char *) s)); break;
	case 2: mpz_set_si (d->number, *((signed short *) s)); break;
	case 4: mpz_set_si (d->number, *((signed long *) s)); break;
	case 8: mpz_set_si (d->number, *((signed long long *) s)); break;
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_fld_to_decimal: not implemented yet");
      break;

    default:
      {
	char *p, buff[32];
	int sign = extract_sign (f, s);

	p = (f->len < 32) ? buff : alloca (f->len + 1);
	memcpy (p, s, f->len);
	p[f->len] = 0;
	mpz_set_str (d->number, p, 10);

	if (sign == 1) /* negative */
	  mpz_neg (d->number, d->number);

	put_sign (f, s, sign);
	break;
      }
    }

  d->decimals = f->decimals;
  p->n_decimal = d;
}

void
cob_decimal_to_fld (struct fld_desc *f, char *s, int round, union numeric_type val)
{
  decimal d = val.n_decimal;

  /* Initialize global flags */
  cob_size_error_flag = 0;

  /* Append or truncate decimal digits */
  if (round && f->decimals < d->decimals)
    {
      /* with rounding */
      shift_decimal (d, f->decimals - d->decimals + 1);
      mpz_add_ui (d->number, d->number, 5);
      shift_decimal (d, -1);
    }
  else
    {
      /* without rounding */
      shift_decimal (d, f->decimals - d->decimals);
    }

  switch (f->type)
    {
    case 'B':
      {
	int val;
	if (!mpz_fits_sint_p (d->number))
	  goto size_error;
	val = mpz_get_si (d->number);
	switch (f->len)
	  {
	  case 1:
	    if (val < -99 || val > 99)
	      goto size_error;
	    *((signed char *) s) = val;
	    break;
	  case 2:
	    if (val < -9999 || val > 9999)
	      goto size_error;
	    *((signed short *) s) = val;
	    break;
	  case 4:
	    if (val < -99999999 || val > 99999999)
	      goto size_error;
	    *((signed long *) s) = val;
	    break;
	  case 8:
	    /* FIXME: val should be long long, and
	     * FIXME: this should allow 18 digits */
	    if (val < -99999999 || val > 99999999)
	      goto size_error;
	    *((signed long long *) s) = val;
	    break;
	  }
      }
      break;

    case 'C':
    case 'U':
      puts ("cob_decimal_to_fld: not implemented yet");
      return;

    default:
      {
	char *p, buff[32];
	int size;
	int sign = (mpz_sgn (d->number) >= 0) ? 0 : 1;
	mpz_abs (d->number, d->number);

	/* Build string */
	size = mpz_sizeinbase (d->number, 10);
	p = (size < 32) ? buff : alloca (size + 1);
	mpz_get_str (p, 10, d->number);
	size = strlen (p);

	/* Check for overflow */
	if (f->len < size)
	  {
	  size_error:
	    puts ("warning: size error in numeric operation");
	    cob_size_error_flag = 1;
	    goto end;
	  }

	/* Copy string */
	memset (s, '0', f->len - size);
	memcpy (s + f->len - size, p, size);

	put_sign (f, s, sign);
	break;
      }
    }

 end:
  free_decimal (d);
  return;
}


/*
 * Operations
 */

void
add_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal d1 = (*v1).n_decimal;
  decimal d2 = v2.n_decimal;
  arrange_decimal (d1, d2);
  mpz_add (d1->number, d1->number, d2->number);
  free_decimal (d2);
}

void
subtract_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal d1 = (*v1).n_decimal;
  decimal d2 = v2.n_decimal;
  arrange_decimal (d1, d2);
  mpz_sub (d1->number, d1->number, d2->number);
  free_decimal (d2);
}

void
multiply_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal d1 = (*v1).n_decimal;
  decimal d2 = v2.n_decimal;
  mpz_mul (d1->number, d1->number, d2->number);
  d1->decimals += d2->decimals;
  free_decimal (d2);
}

void
divide_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal d1 = (*v1).n_decimal;
  decimal d2 = v2.n_decimal;
  shift_decimal (d1, 15 - d1->decimals);
  mpz_tdiv_q (d1->number, d1->number, d2->number);
  d1->decimals -= d2->decimals;
  free_decimal (d2);
}

void
pow_decimal (union numeric_type *v1, union numeric_type v2)
{
  decimal d1 = (*v1).n_decimal;
  decimal d2 = v2.n_decimal;

  if (d2->decimals == 0 && mpz_fits_ulong_p (d2->number))
    {
      int n = mpz_get_ui (d2->number);
      mpz_pow_ui (d1->number, d1->number, n);
      d1->decimals *= n;
      free_decimal (d2);
    }
  else
    {
      puts ("pow_decimal: not implemented yet");
      abort ();
    }
}

int
compare_decimal (union numeric_type v1, union numeric_type v2)
{
  int val;
  arrange_decimal (v1.n_decimal, v2.n_decimal);
  val = mpz_cmp (v1.n_decimal->number, v2.n_decimal->number);
  return val;
}
