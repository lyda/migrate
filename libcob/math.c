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
#include <gmp.h>

#include "libcob.h"

struct cob_decimal {
  mpz_t number;
  int decimals;
};

typedef struct cob_decimal *decimal;

struct cob_object {
  int type;
  union {
    struct cob_decimal decimal;
  } data;
};

typedef struct cob_object *cob_object;

#define COB_TYPE_BOOLEAN	0
#define COB_TYPE_DECIMAL	1
#define COB_TYPE_FIELD		2

#define COB_TYPE(x)		((x)->type)

#define COB_DECIMAL(x)		(&((x)->data.decimal))

void
cob_print_decimal (decimal d)
{
  fputs ("decimal(", stdout);
  mpz_out_str (stdout, 10, d->number);
  if (d->decimals)
    fprintf (stdout, " * 10^-%d", d->decimals);
  fputs (")\n", stdout);
}


/*
 * Runtime stack
 */

#define STACK_SIZE	16

static int stack_index = -1;
static struct cob_object cob_stack[STACK_SIZE];

#define POP()	(&cob_stack[stack_index--])
#define TOP()	(&cob_stack[stack_index])
#define REF(n)	(&cob_stack[stack_index - (n)])
#define DROP(n)	(stack_index -= (n))

void
cob_print_stack (void)
{
  int i;
  printf ("%d:\n", stack_index);
  for (i = 0; i <= stack_index; i++)
    cob_print_decimal (COB_DECIMAL (&cob_stack[i]));
}

static cob_object
grab_object (int type)
{
  cob_object o;

  /* Check stack overflow */
  if (++stack_index == STACK_SIZE)
    {
      fputs ("libcob: stack overflow\n", stderr);
      abort ();
    }

  /* Prepare data space */
  o = TOP ();
  o->type = type;
  return o;
}

void
cob_push_copy (int n)
{
  cob_object src = REF (n);
  cob_object dst = grab_object (src->type);
  switch (src->type)
    {
    case COB_TYPE_DECIMAL:
      mpz_set (COB_DECIMAL (dst)->number, COB_DECIMAL (src)->number);
      COB_DECIMAL (dst)->decimals = COB_DECIMAL (src)->decimals;
      break;
    }
}


/*
 * Decimal number
 */

/* d->number *= 10^n, d->decimals += n */
static void
shift_decimal (decimal d, int n)
{
  if (n == 0)
    return;

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
  else
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
arrange_decimal (decimal d1, decimal d2)
{
  if (d1->decimals < d2->decimals)
    shift_decimal (d1, d2->decimals - d1->decimals);
  else if (d2->decimals < d1->decimals)
    shift_decimal (d2, d1->decimals - d2->decimals);
}

void
cob_num_add (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  arrange_decimal (d1, d2);
  mpz_add (d1->number, d1->number, d2->number);
}

void
cob_num_sub (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  arrange_decimal (d1, d2);
  mpz_sub (d1->number, d1->number, d2->number);
}

void
cob_num_mul (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  d1->decimals += d2->decimals;
  mpz_mul (d1->number, d1->number, d2->number);
}

void
cob_num_div (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());

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
cob_num_pow (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());

  if (d2->decimals == 0 && mpz_fits_ulong_p (d2->number))
    {
      int n = mpz_get_ui (d2->number);
      mpz_pow_ui (d1->number, d1->number, n);
      d1->decimals *= n;
    }
  else
    {
      puts ("pow_decimal: not implemented yet");
      abort ();
    }
}

int
cob_num_cmp (void)
{
  int val;
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (POP ());
  arrange_decimal (d1, d2);
  val = mpz_cmp (d1->number, d2->number); /* return value may not be -1,0,1 */
  return (val < 0) ? -1 : (val > 0) ? 1 : 0;
}


/*
 * Stack object
 */

static decimal
grab_decimal ()
{
  decimal d = COB_DECIMAL (grab_object (COB_TYPE_DECIMAL));
  mpz_init (d->number);
  return d;
}

void
cob_push_int (int n, int decimals)
{
  decimal d = grab_decimal ();
  mpz_set_si (d->number, n);
  d->decimals = decimals;
}

void
cob_push_str (char *s, int decimals)
{
  decimal d = grab_decimal ();
  mpz_set_str (d->number, s, 10);
  d->decimals = decimals;
}

void
cob_dis_check (struct cob_field f)
{
  int i;
  int sign = get_sign (f);
  int len = FIELD_LENGTH (f);
  unsigned char *p = FIELD_BASE (f);
  for (i = 0; i < len; i++)
    if (!isdigit (p[i]))
      {
	fprintf (stderr, "\007ERROR: non-numeric value: `%s'\n", p);
	break;
      }
  put_sign (f, sign);
}

void
cob_push_decimal (struct cob_field f)
{
  decimal d = grab_decimal ();

  switch (FIELD_TYPE (f))
    {
    case 'B':
      switch (f.desc->len)
	{
	case 1: mpz_set_si (d->number, *(char *) f.data); break;
	case 2: mpz_set_si (d->number, *(short *) f.data); break;
	case 4: mpz_set_si (d->number, *(long *) f.data); break;
	case 8:
	  {
	    long long val = *(long long *) f.data;
	    mpz_set_si (d->number, val >> 32);
	    mpz_mul_2exp (d->number, d->number, 32);
	    mpz_add_ui (d->number, d->number, val & 0xffffffff);
	    break;
	  }
	}
      break;

    case 'C':
      puts ("cob_push: not implemented yet");
      break;

    default:
      {
	char *p, buff[32];
	int sign = get_sign (f);
	int len = FIELD_LENGTH (f);
	unsigned char *base = FIELD_BASE (f);

	p = (len < 32) ? buff : alloca (len + 1);
	memcpy (p, base, len);
	p[len] = 0;
	mpz_set_str (d->number, p, 10);
	if (sign == 1) /* negative */
	  mpz_neg (d->number, d->number);

	put_sign (f, sign);
	break;
      }
    }
  d->decimals = f.desc->decimals;
}

void
cob_round (struct cob_field f)
{
  decimal d = COB_DECIMAL (TOP ());
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
}

void
cob_set (struct cob_field f)
{
  decimal d = COB_DECIMAL (POP ());

  /* Just return if something has happened */
  if (cob_status == COB_STATUS_OVERFLOW)
    return;

  /* Append or truncate decimal digits */
  shift_decimal (d, f.desc->decimals - d->decimals);

  /* Store number */
  switch (FIELD_TYPE (f))
    {
    case 'B':
      {
	int len = picCompLength (f.desc->pic);
	if (!len)
	  len = 4;
	if (f.desc->len <= 4)
	  {
	    int val;
	    if (!mpz_fits_sint_p (d->number))
	      goto overflow;
	    val = mpz_get_si (d->number);
	    if (val <= -cob_exp10[len] || val >= cob_exp10[len])
	      goto overflow;
	    switch (f.desc->len)
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
	    if (val <= -cob_exp10[len] || val >= cob_exp10[len])
	      goto overflow;
	    *(long long *) f.data = val;
	  }
	return;
      }

    case 'C':
      puts ("cob_decimal_to_fld: not implemented yet");
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
	    put_sign (f, sign);
	  }
	else
	  {
	    struct cob_field_desc desc = {size, '9', f.desc->decimals, 0, 1};
	    struct cob_field temp = {&desc, buff};
	    put_sign (temp, sign);
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
cob_set_int (struct cob_field f, int n)
{
  int saved_status = cob_status;
  cob_status = COB_STATUS_SUCCESS;
  cob_push_int (n, 0);
  cob_set (f);
  cob_status = saved_status;
}

void
cob_add_int (struct cob_field f, int n)
{
  int saved_status = cob_status;
  if (n == 0) return;
  cob_status = COB_STATUS_SUCCESS;
  cob_push_decimal (f);
  cob_push_int (n, 0);
  cob_num_add ();
  cob_set (f);
  cob_status = saved_status;
}

void
cob_add (struct cob_field f1, struct cob_field f2, int round)
{
  cob_push_decimal (f1);
  cob_push_decimal (f2);
  cob_num_add ();
  if (round)
    cob_round (f2);
  cob_set (f2);
}

void
cob_sub (struct cob_field f1, struct cob_field f2, int round)
{
  cob_push_decimal (f1);
  cob_push_decimal (f2);
  cob_num_sub ();
  if (round)
    cob_round (f2);
  cob_set (f2);
}

void
cob_divide (struct cob_field q, struct cob_field r, int round)
{
  decimal d;

  /* duplicate divisor and dividend */
  cob_push_copy (1);
  cob_push_copy (1);

  /* compute quotient */
  cob_num_div ();
  cob_push_copy (0);		/* save the quotient */
  if (round)
    cob_round (q);
  cob_set (q);

  /* truncate digits from quotient */
  d = COB_DECIMAL (TOP ());
  shift_decimal (d, q.desc->decimals - d->decimals);

  /* compute remainder */
  cob_num_mul ();
  cob_num_sub ();
  cob_set (r);
}
