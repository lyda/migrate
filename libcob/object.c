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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <gmp.h>


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

static void
print_decimal (decimal d)
{
  fputs ("decimal(", stdout);
  mpz_out_str (stdout, 10, d->number);
  if (d->decimals)
    fprintf (stdout, " * 10^-%d", d->decimals);
  fputs (")\n", stdout);
}

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
      print_decimal (COB_DECIMAL (o));
      break;

    case COB_TYPE_FIELD:
      {
	int i;
	struct cob_field *p = COB_FIELD (o);
	for (i = 0; i < p->desc->len; i++)
	  fputc (p->data[i], stdout);
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
cob_push_field (struct cob_field fld)
{
  struct cob_field *p = COB_FIELD (grab_object (COB_TYPE_FIELD));
  p->desc = fld.desc;
  p->data = fld.data;
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
      COB_FIELD (dst)->desc = COB_FIELD (src)->desc;
      COB_FIELD (dst)->data = COB_FIELD (src)->data;
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
  int val;
  decimal d2 = COB_DECIMAL (POP ());
  decimal d1 = COB_DECIMAL (POP ());
  arrange_decimal (d1, d2);
  val = mpz_cmp (d1->number, d2->number); /* return value may not be -1,0,1 */
  return (val < 0) ? -1 : (val > 0) ? 1 : 0;
}

int
cob_between (void)
{
  decimal upper = COB_DECIMAL (POP ());
  decimal value = COB_DECIMAL (POP ());
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
cob_push_decimal (struct cob_field f)
{
  decimal d = grab_decimal ();

  switch (f.desc->type)
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
    case 'U':
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
cob_set (struct cob_field f, int round)
{
  decimal d = COB_DECIMAL (POP ());

  /* Just return if something has happened */
  if (cob_status == COB_STATUS_OVERFLOW)
    return;

  /* Append or truncate decimal digits */
  if (round && f.desc->decimals < d->decimals)
    {
      /* with rounding */
      int sign = mpz_sgn (d->number);
      if (sign != 0)
	{
	  shift_decimal (d, f.desc->decimals - d->decimals + 1);
	  if (sign > 0)
	    mpz_add_ui (d->number, d->number, 5);
	  else
	    mpz_sub_ui (d->number, d->number, 5);
	  mpz_tdiv_q_ui (d->number, d->number, 10);
	}
    }
  else
    {
      /* without rounding */
      shift_decimal (d, f.desc->decimals - d->decimals);
    }

  /* Store number */
  switch (f.desc->type)
    {
    case 'B':
      {
	int len = picCompLength (f.desc->pic);
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
    case 'U':
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

	if (f.desc->type == '9')
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
	    struct fld_desc desc =
	      {size, '9', f.desc->decimals, 0, 0, 0, 0, 0, 0};
	    struct cob_field temp = {&desc, buff};
	    unsigned char pic[10];
	    if (f.desc->decimals > 0)
	      sprintf (pic, "S\0019%cV\0019%c",
		       (char) (size - f.desc->decimals),
		       (char) f.desc->decimals);
	    else
	      sprintf (pic, "S\0019%c", (char) size);
	    desc.pic = pic;
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
cob_divide_remainder (struct cob_field q, struct cob_field r, int round)
{
  decimal d;

  /* duplicate divisor and dividend */
  cob_push_copy (1);
  cob_push_copy (1);

  /* compute quotient */
  cob_div ();
  cob_push_copy (0);		/* save the quotient */
  cob_set (q, round);

  /* truncate digits from quotient */
  d = COB_DECIMAL (TOP ());
  shift_decimal (d, q.desc->decimals - d->decimals);

  /* compute remainder */
  cob_mul ();
  cob_sub ();
  cob_set (r, 0);
}



int
check_condition (struct cob_field f1, ...)
{
  struct cob_field f2, f3;
  va_list args;

  va_start (args, f1);
  f2.desc = va_arg (args, struct fld_desc *);
  while (f2.desc)
    {
      f2.data = va_arg (args, char *);
      f3 = va_arg (args, struct cob_field);

      if (f1.desc->type == '9' || f1.desc->type == 'B')
	{
	  cob_push_decimal (f1);
	  cob_push_decimal (f2);
	  if (cob_cmp () >= 0)		/* f1 >= f2 */
	    {
	      cob_push_decimal (f1);
	      cob_push_decimal (f3);
	      if (cob_cmp () <= 0)	/* f1 <= f3 */
		goto success;
	    }
	}
      else
	{
	  int i;
	  for (i = 0; i < f1.desc->len; i++)
	    if ((i < f2.desc->len) && (f1.data[i] >= f2.data[i]))
	      if ((i < f3.desc->len) && (f1.data[i] <= f3.data[i]))
		goto success;
	}
      f2.desc = va_arg (args, struct fld_desc *);
    }
  /* fail */
  va_end (args);
  return 1;

 success:
  va_end (args);
  return 0;
}

int
cob_compare (struct cob_field f1, struct cob_field f2)
{
  int sign1, sign2;
  char type1 = f1.desc->type;
  char type2 = f2.desc->type;
  int len1 = f1.desc->len;
  int len2 = f2.desc->len;

  if ((type1 == '9' || type1 == 'C' || type1 == 'B' || type1 == 'U')
      && (type2 == '9' || type2 == 'C' || type2 == 'B' || type2 == 'U'))
    {
      cob_push_decimal (f1);
      cob_push_decimal (f2);
      return cob_cmp ();
    }

  sign1 = get_sign (f1);
  sign2 = get_sign (f2);

  if (f1.desc->all || f2.desc->all)
    {
      int i, j = 0, k = 0;
      int maxi = (len1 < len2) ? len1 : len2;
      for (i = 0; i < maxi; i++)
	{
	  if (f1.data[j] == f2.data[k])
	    continue;
	  if (f1.data[j] > f2.data[k])
	    goto positive;
	  if (f1.data[j] < f2.data[k])
	    goto negative;
	  j++;
	  k++;
	  if (f1.desc->all && j >= len1)
	    j = 0;
	  if (f2.desc->all && k >= len2)
	    k = 0;
	}

      if (len1 > len2)
	while (j < len1)
	  {
	    if (f1.data[j++] != f2.data[k++])
	      goto positive;
	    if (k >= len2)
	      k = 0;
	  }
      else
	while (k < len2)
	  {
	    if (f1.data[j++] != f2.data[k++])
	      goto negative;
	    if (j >= len1)
	      j = 0;
	  }
      goto zero;
    }

  {
    int i;
    int maxi = (len1 < len2) ? len1 : len2;
    for (i = 0; i < maxi; i++)
      {
	if (f1.data[i] == f2.data[i])
	  continue;
	if (f1.data[i] > f2.data[i])
	  goto positive;
	if (f1.data[i] < f2.data[i])
	  goto negative;
      }
    if (len1 > len2)
      {
	while (i < len1)
	  if (f1.data[i++] != ' ')
	    goto positive;
      }
    else
      {
	while (i < len2)
	  if (f2.data[i++] != ' ')
	    goto negative;
      }
    goto zero;
  }

  {
    int ret;
  positive:
    ret = 1; goto end;
  zero:
    ret = 0; goto end;
  negative:
    ret = -1; goto end;
  end:
    put_sign (f1, sign1);
    put_sign (f2, sign2);
    return ret;
  }
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
	for (i = 0; i < p->desc->len; i++)
	  if (p->data[i] != '0')
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
	return (cob_compare (*COB_FIELD (o1), *COB_FIELD (o2)) == 0) ? 1 : 0;
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
cob_in_range (struct cob_field low, struct cob_field val, struct cob_field up)
{
  if (cob_compare (low, val) <= 0 && cob_compare (val, up) <= 0)
    return 1;
  return 0;
}
