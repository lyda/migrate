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

#include <gmp.h>


/*
 * Global variables
 */

int cob_size_error_flag;


/*
 * Objects
 */

struct cob_boolean {
  enum { cob_false = 0, cob_true = 1 } value;
};

struct cob_decimal {
  mpz_t number;
  int decimals;
};

typedef struct cob_decimal *decimal;

struct cob_field {
  struct fld_desc *f;
  char *s;
};

struct cob_object {
  int type;
  union {
    struct cob_boolean boolean;
    struct cob_decimal decimal;
    struct cob_field field;
  } data;
};

typedef struct cob_object *cob_object;

#define COB_TYPE_BOOLEAN	0
#define COB_TYPE_DECIMAL	1
#define COB_TYPE_FIELD		2

#define COB_TYPE(x)		((x)->type)

#define COB_BOOLEAN_P(x)	(COB_TYPE (x) == COB_BOOLEAN)
#define COB_DECIMAL_P(x)	(COB_TYPE (x) == COB_DECIMAL)
#define COB_FIELD_P(x)		(COB_TYPE (x) == COB_FIELD)

#define COB_BOOLEAN(x)		(&((x)->data.boolean))
#define COB_DECIMAL(x)		(&((x)->data.decimal))
#define COB_FIELD(x)		(&((x)->data.field))

void
cob_debug_print (cob_object o)
{
  switch (COB_TYPE (o))
    {
    case COB_TYPE_BOOLEAN:
      {
	if (COB_BOOLEAN (o)->value)
	  fputs ("true\n", stdout);
	else
	  fputs ("false\n", stdout);
      }
      break;

    case COB_TYPE_DECIMAL:
      {
	decimal d = COB_DECIMAL (o);
	fputs ("decimal(", stdout);
	mpz_out_str (stdout, 10, d->number);
	if (d->decimals)
	  fprintf (stdout, " * 10^-%d", d->decimals);
	fputs (")\n", stdout);
      }
      break;

    case COB_TYPE_FIELD:
      {
	int i;
	struct cob_field *p = COB_FIELD (o);
	for (i = 0; i < p->f->len; i++)
	  fputc (p->s[i], stdout);
	fputc ('\n', stdout);
      }
      break;
    }
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
cob_stack_clear (void)
{
  stack_index = -1;
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
cob_push_boolean (int flag)
{
  struct cob_boolean *p = COB_BOOLEAN (grab_object (COB_TYPE_BOOLEAN));
  p->value = (flag == 0) ? cob_true : cob_false;
}

void
cob_push_field (struct fld_desc *f, unsigned char *s)
{
  struct cob_field *p = COB_FIELD (grab_object (COB_TYPE_FIELD));
  p->f = f;
  p->s = s;
}

void
cob_push_copy (int n)
{
  cob_object src = REF (n);
  cob_object dst = grab_object (src->type);
  switch (src->type)
    {
    case COB_TYPE_BOOLEAN:
      COB_BOOLEAN (dst)->value = COB_BOOLEAN (src)->value;
      break;
    case COB_TYPE_DECIMAL:
      mpz_set (COB_DECIMAL (dst)->number, COB_DECIMAL (src)->number);
      COB_DECIMAL (dst)->decimals = COB_DECIMAL (src)->decimals;
      break;
    case COB_TYPE_FIELD:
      COB_FIELD (dst)->f = COB_FIELD (src)->f;
      COB_FIELD (dst)->s = COB_FIELD (src)->s;
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

void
cob_add (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  arrange_decimal (d1, d2);
  mpz_add (d1->number, d1->number, d2->number);
}

void
cob_sub (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  arrange_decimal (d1, d2);
  mpz_sub (d1->number, d1->number, d2->number);
}

void
cob_mul (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  d1->decimals += d2->decimals;
  mpz_mul (d1->number, d1->number, d2->number);
}

void
cob_div (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (TOP ());
  shift_decimal (d1, 15 - d1->decimals);
  d1->decimals -= d2->decimals;
  mpz_tdiv_q (d1->number, d1->number, d2->number);
}

void
cob_pow (void)
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
cob_cmp (void)
{
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (POP ());
  arrange_decimal (d1, d2);
  return mpz_cmp (d1->number, d2->number);
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
cob_push_zero (void)
{
  decimal d = grab_decimal ();
  mpz_set_ui (d->number, 0);
  d->decimals = 0;
}

void
cob_push_decimal (struct fld_desc *f, unsigned char *s)
{
  decimal d = grab_decimal ();

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: mpz_set_si (d->number, *((signed char *) s)); break;
	case 2: mpz_set_si (d->number, *((signed short *) s)); break;
	case 4: mpz_set_si (d->number, *((signed long *) s)); break;
	case 8:
	  {
	    signed long long val = *((signed long long *) s);
	    mpz_set_si (d->number, val >> 32);
	    mpz_mul_2exp (d->number, d->number, 32);
	    mpz_add_ui (d->number, d->number, val & 0xffffffff);
	    break;
	  }
	}
      break;

    case 'C':
    case 'U':
      puts ("cob_push: not implemented yet");
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
}

void
cob_set (struct fld_desc *f, char *s, int round)
{
  decimal d = COB_DECIMAL (POP ());

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
	if (f->len <= 4)
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
	      }
	  }
	else
	  {
	    signed long long val;
	    unsigned long lower = mpz_get_ui (d->number);
	    mpz_fdiv_q_2exp (d->number, d->number, 32);
	    if (!mpz_fits_sint_p (d->number))
	      goto size_error;
	    val = mpz_get_si (d->number);
	    val = (val << 32) + lower;
	    if (val < -999999999999999999 || val > 999999999999999999)
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
	    fputs ("warning: size error in numeric operation\n", stderr);
	    cob_size_error_flag = 1;
	    return;
	  }

	/* Copy string */
	memset (s, '0', f->len - size);
	memcpy (s + f->len - size, p, size);

	put_sign (f, s, sign);
	break;
      }
    }
}



int
check_condition (struct fld_desc *f1, char *s1, ...)
{
  int i, len2, len3;
  struct fld_desc *f2, *f3;
  char *s2, *s3;
  int ret = 1;			/* assume wrong */
  va_list args;

  va_start (args, s1);
  f2 = va_arg (args, struct fld_desc *);
  while (f2)
    {
      s2 = va_arg (args, char *);
      f3 = va_arg (args, struct fld_desc *);
      s3 = va_arg (args, char *);

      if (f1->type == '9' || f1->type == 'B')
	{
	  cob_push_decimal (f1, s1);
	  cob_push_decimal (f2, s2);
	  if (cob_cmp () >= 0)		/* f1 >= f2 */
	    {
	      cob_push_decimal (f1, s1);
	      cob_push_decimal (f3, s3);
	      if (cob_cmp () <= 0)	/* f1 <= f3 */
		{
		  ret = 0;
		  break;
		}
	    }
	}
      else
	{
	  len2 = f2->len;
	  len3 = f3->len;
	  for (i = 0; i < f1->len; i++)
	    {
	      if ((i < len2) && (s1[i] >= s2[i]))
		{
		  if ((i < len3) && (s1[i] <= s3[i]))
		    {
		      va_end (args);
		      return 0;
		    }
		}
	    }
	}
      f2 = va_arg (args, struct fld_desc *);
    }
  va_end (args);
  return ret;
}

int
compare (struct fld_desc *f1, char *s1, struct fld_desc *f2, char *s2)
{
  int i, maxi;

  if ((f1->type != '9' && f1->type != 'C' && f1->type != 'B') ||
      (f2->type != '9' && f2->type != 'C' && f2->type != 'B'))
    {
      if (f1->all || f2->all)
	{
	  int i, j, k, maxi;

	  maxi = (f1->len < f2->len) ? f1->len : f2->len;
	  j = 0;
	  k = 0;
	  for (i = 0; i < maxi; i++)
	    {
	      if (s1[j] == s2[k])
		continue;
	      if (s1[j] > s2[k])
		return 1;
	      if (s1[j] < s2[k])
		return -1;
	      j++;
	      k++;
	      if (f1->all && j >= f1->len)
		j = 0;
	      if (f2->all && k >= f2->len)
		k = 0;
	    }

	  if (f1->len > f2->len)
	    while (j < f1->len)
	      {
		if (s1[j++] != s2[k++])
		  return 1;
		if (k >= f2->len)
		  k = 0;
	      }
	  else
	    while (k < f2->len)
	      {
		if (s2[k++] != s1[j++])
		  return -1;
		if (j >= f1->len)
		  j = 0;
	      }
	  return 0;
	}
      maxi = (f1->len < f2->len) ? f1->len : f2->len;
      for (i = 0; i < maxi; i++)
	{
	  if (s1[i] == s2[i])
	    continue;
	  if (s1[i] > s2[i])
	    return 1;
	  if (s1[i] < s2[i])
	    return -1;
	}
      if (f1->len > f2->len)
	while (i < f1->len)
	  {
	    if (s1[i++] != ' ')
	      return 1;
	  }
      else
	while (i < f2->len)
	  {
	    if (s2[i++] != ' ')
	      return -1;
	  }
    }
  else
    {
      cob_push_decimal (f1, s1);
      cob_push_decimal (f2, s2);
      return cob_cmp ();
    }
  return 0;
}

int
cob_is_zero ()
{
  switch (COB_TYPE (TOP ()))
    {
    case COB_TYPE_DECIMAL:
      cob_push_zero ();
      return (cob_cmp () == 0) ? 1 : 0;

    case COB_TYPE_FIELD:
      {
	int i;
	struct cob_field *p = COB_FIELD (POP ());
	for (i = 0; i < p->f->len; i++)
	  if (p->s[i] != '0')
	    return 0;
	return 1;
      }

    default:
      abort ();
    }
}

int
cob_is_equal ()
{
  cob_object o1 = REF (0);
  cob_object o2 = REF (1);

  puts ("is_equal?");
  cob_debug_print (o1);
  cob_debug_print (o2);

  if (COB_TYPE (o1) == COB_TYPE (o2))
    switch (COB_TYPE (o1))
      {
      case COB_TYPE_BOOLEAN:
	DROP (2);
	return (COB_BOOLEAN (o1)->value == COB_BOOLEAN (o2)->value) ? 1 : 0;

      case COB_TYPE_DECIMAL:
	return (cob_cmp () == 0) ? 1 : 0;

      case COB_TYPE_FIELD:
	DROP (2);
	return (compare (COB_FIELD (o1)->f, COB_FIELD (o1)->s,
			 COB_FIELD (o2)->f, COB_FIELD (o2)->s) == 0) ? 1 : 0;
      }
  /* A trick that allows 88 variables */
  else if (COB_TYPE (o2) == COB_TYPE_BOOLEAN
	   && COB_BOOLEAN (o2)->value == cob_true)
    {
      DROP (2);
      return 1;
    }
  DROP (2);
  return 0;
}

int
cob_in_range ()
{
  decimal value = COB_DECIMAL (POP ());
  decimal upper = COB_DECIMAL (POP ());
  decimal lower = COB_DECIMAL (POP ());

  arrange_decimal (lower, value);
  if (mpz_cmp (lower->number, value->number) <= 0)
    {
      arrange_decimal (value, upper);
      if (mpz_cmp (value->number, upper->number) <= 0)
	return 1;
    }
  return 0;
}
