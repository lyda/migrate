/*
 * Copyright (C) 2001-2003 Keisuke Nishida
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libcob.h>

#include "cobc.h"

cb_tree
cb_build_identifier (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f;
  const char *name = r->word->name;
  cb_tree v = cb_ref (x);

  if (v == cb_error_node)
    return cb_error_node;

  /* check if it is a data name */
  if (!CB_FIELD_P (v))
    {
      cb_error_x (x, _("`%s' not data name"), name);
      return cb_error_node;
    }
  f = CB_FIELD (v);

  /* check the number of subscripts */
  if (list_length (r->subs) != f->indexes)
    {
      switch (f->indexes)
	{
	case 0:
	  cb_error_x (x, _("`%s' cannot be subscripted"), name);
	  break;
	case 1:
	  cb_error_x (x, _("`%s' requires 1 subscript"), name);
	  break;
	default:
	  cb_error_x (x, _("`%s' requires %d subscripts"), name, f->indexes);
	  break;
	}
      return cb_error_node;
    }

  /* check the range of constant subscripts */
  if (r->subs)
    {
      struct cb_field *p;
      cb_tree l = r->subs = list_reverse (r->subs);

      for (p = f; p; p = p->parent)
	if (p->flag_occurs)
	  {
	    if (CB_LITERAL_P (CB_VALUE (l)))
	      {
		int n = cb_literal_to_int (CB_LITERAL (CB_VALUE (l)));
		if (n < p->occurs_min || n > p->occurs_max)
		  cb_error_x (x, _("subscript of `%s' out of bounds: %d"),
			     name, n);
	      }
	    l = CB_CHAIN (l);
	  }

      r->subs = list_reverse (r->subs);
    }

  /* check the range of constant reference modification */
  if (r->offset && CB_LITERAL_P (r->offset))
    {
      int offset = cb_literal_to_int (CB_LITERAL (r->offset));
      if (offset < 1 || offset > f->size)
	cb_error_x (x, _("offset of `%s' out of bounds: %d"), name, offset);
      else if (r->length && CB_LITERAL_P (r->length))
	{
	  int len = cb_literal_to_int (CB_LITERAL (r->length));
	  if (len < 1 || len > f->size - offset + 1)
	    cb_error_x (x, _("length of `%s' out of bounds: %d"), name, len);
	}
    }

  return x;
}


/*
 * Numerical operation
 */

#define add_stmt(s,x) \
  CB_SEQUENCE (s)->list = list_add (CB_SEQUENCE (s)->list, x)

static cb_tree
decimal_alloc (void)
{
  cb_tree x = cb_build_decimal (current_program->decimal_index++);
  if (current_program->decimal_index > current_program->decimal_index_max)
    current_program->decimal_index_max = current_program->decimal_index;
  return x;
}

static void
decimal_free (void)
{
  current_program->decimal_index--;
}

static void
decimal_compute (cb_tree s, char op, cb_tree x, cb_tree y)
{
  const char *func;
  switch (op)
    {
    case '+': func = "cob_decimal_add"; break;
    case '-': func = "cob_decimal_sub"; break;
    case '*': func = "cob_decimal_mul"; break;
    case '/': func = "cob_decimal_div"; break;
    case '^': func = "cob_decimal_pow"; break;
    default: abort ();
    }
  add_stmt (s, cb_build_funcall_2 (func, x, y));
}

static void
decimal_expand (cb_tree s, cb_tree d, cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      {
	cb_tree e;
	if (x == cb_zero)
	  e = cb_build_funcall_2 ("cob_decimal_set_int", d, cb_int0);
	else
	  abort ();
	add_stmt (s, e);
	break;
      }
    case CB_TAG_LITERAL:
      {
	/* set d, N */
	struct cb_literal *l = CB_LITERAL (x);
	if (l->size < 10 && l->expt == 0)
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_int",
				       d, cb_build_cast_integer (x)));
	else
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_REFERENCE:
      {
	/* set d, X */
	struct cb_field *f = cb_field (x);

	/* check numeric */
	if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE))
	  if (f->usage == CB_USAGE_DISPLAY)
	    add_stmt (s, cb_build_funcall_2 ("cob_check_numeric",
					 x, cb_build_string (f->name)));

	if (cb_fits_int (x))
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_int",
				       d, cb_build_cast_integer (x)));
	else
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	if (p->op == '@')
	  {
	    decimal_expand (s, d, p->x);
	  }
	else
	  {
	    /* set d, X
	     * set t, Y
	     * OP d, t */
	    cb_tree t = decimal_alloc ();
	    decimal_expand (s, d, p->x);
	    decimal_expand (s, t, p->y);
	    decimal_compute (s, p->op, d, t);
	    decimal_free ();
	  }
	break;
      }
    default:
      abort ();
    }
}

static void
decimal_assign (cb_tree s, cb_tree x, cb_tree d, int round)
{
  const char *func =
    round ? "cob_decimal_get_field_r" : "cob_decimal_get_field";
  add_stmt (s, cb_build_funcall_2 (func, d, x));
}

static cb_tree
build_decimal_assign (cb_tree vars, char op, cb_tree val)
{
  cb_tree l;
  cb_tree s1 = make_sequence (NULL);
  cb_tree s2 = make_sequence (NULL);
  cb_tree d = decimal_alloc ();

  /* set d, VAL */
  decimal_expand (s2, d, val);

  if (op == 0)
    {
      for (l = vars; l; l = CB_CHAIN (l))
	{
	  /* set VAR, d */
	  decimal_assign (s2, CB_VALUE (l), d, CB_PURPOSE_INT (l));
	  add_stmt (s1, s2);
	  if (CB_CHAIN (l))
	    s2 = make_sequence (NULL);
	}
    }
  else
    {
      cb_tree t = decimal_alloc ();
      for (l = vars; l; l = CB_CHAIN (l))
	{
	  /* set t, VAR
	   * OP t, d
	   * set VAR, t
	   */
	  decimal_expand (s2, t, CB_VALUE (l));
	  decimal_compute (s2, op, t, d);
	  decimal_assign (s2, CB_VALUE (l), t, CB_PURPOSE_INT (l));
	  add_stmt (s1, s2);
	  if (CB_CHAIN (l))
	    s2 = make_sequence (NULL);
	}
      decimal_free ();
    }

  decimal_free ();
  return s1;
}

cb_tree
cb_build_assign (cb_tree vars, char op, cb_tree val)
{
  cb_tree l;

  for (l = vars; l; l = CB_CHAIN (l))
    if (CB_VALUE (l) == cb_error_node)
      return cb_error_node;

  if (val == cb_error_node)
    return cb_error_node;

  if (!CB_BINARY_OP_P (val))
    if (op == '+' || op == '-')
      {
	for (l = vars; l; l = CB_CHAIN (l))
	  {
	    if (op == '+')
	      CB_VALUE (l) = cb_build_add (CB_VALUE (l), val, CB_PURPOSE_INT (l));
	    else
	      CB_VALUE (l) = cb_build_sub (CB_VALUE (l), val, CB_PURPOSE_INT (l));
	  }
	return make_sequence (vars);
      }

  return build_decimal_assign (vars, op, val);
}


/*
 * ADD/SUBTRACT/MOVE CORRESPONDING
 */

cb_tree
cb_build_add (cb_tree v, cb_tree n, int round)
{
  if (cb_field (v)->usage == CB_USAGE_INDEX)
    return cb_build_move (cb_build_binary_op (v, '+', n), v);

  if (round == 0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_add_int", v, cb_build_cast_integer (n));
  if (round)
    return cb_build_funcall_2 ("cob_add_r", v, n);
  else
    return cb_build_funcall_2 ("cob_add", v, n);
}

cb_tree
cb_build_sub (cb_tree v, cb_tree n, int round)
{
  if (cb_field (v)->usage == CB_USAGE_INDEX)
    return cb_build_move (cb_build_binary_op (v, '-', n), v);

  if (round == 0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_sub_int", v, cb_build_cast_integer (n));
  if (round)
    return cb_build_funcall_2 ("cob_sub_r", v, n);
  else
    return cb_build_funcall_2 ("cob_sub", v, n);
}

static void
warning_destination (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f = CB_FIELD (r->value);
  cb_tree loc = CB_TREE (f);

  if (r->offset)
    return;

  if (f->pic)
    cb_warning_x (loc, _("`%s' defined here as PIC %s"), f->name, f->pic->orig);
  else
    cb_warning_x (loc, _("`%s' defined here as a group of length %d"),
	      f->name, f->size);
}

static int
move_error (cb_tree src, cb_tree dst, int value_flag, int flag, const char *msg)
{
  cb_tree loc = src->source_line ? src : dst;

  /* for VALUE clause */
  if (value_flag)
    {
      if (cb_standard == CB_STANDARD_COBOL2002)
	{
	  cb_error_x (loc, msg);
	  return -1;
	}
      else if (flag)
	{
	  cb_warning_x (loc, msg);
	  return 0;
	}
    }

  /* for MOVE statement */
  if (flag)
    {
      cb_warning_x (loc, msg);
      warning_destination (dst);
    }
  return 0;
}

int
validate_move (cb_tree src, cb_tree dst, int value_flag)
{
  struct cb_field *f = cb_field (dst);
  cb_tree loc = src->source_line ? src : dst;

  switch (CB_TREE_TAG (src))
    {
    case CB_TAG_CONST:
      {
	if (src == cb_space)
	  {
	    if (f->pic)
	      if (f->pic->category == CB_CATEGORY_NUMERIC
		  || f->pic->category == CB_CATEGORY_NUMERIC_EDITED)
		goto invalid;
	  }
	else if (src == cb_zero)
	  {
	    if (f->pic)
	      if (f->pic->category == CB_CATEGORY_ALPHABETIC)
		goto invalid;
	  }
	break;
      }
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (src);

	/* TODO: ALL literal */

	if (CB_TREE_CLASS (src) == CB_CLASS_NUMERIC)
	  {
	    /* Numeric literal */
	    int i;
	    int most_significant = -999;
	    int least_significant = 999;

	    /* compute the most significant figure place */
	    for (i = 0; i < l->size; i++)
	      if (l->data[i] != '0')
		break;
	    if (i != l->size)
	      most_significant = l->size + l->expt - i - 1;

	    /* compute the least significant figure place */
	    for (i = 0; i < l->size; i++)
	      if (l->data[l->size - i - 1] != '0')
		break;
	    if (i != l->size)
	      least_significant = l->expt + i;

	    /* value check */
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHANUMERIC:
	      case CB_CATEGORY_ALPHANUMERIC_EDITED:
		{
		  if (value_flag)
		    goto expect_alphanumeric;

		  if (l->expt == 0)
		    goto expect_alphanumeric;
		  else
		    goto invalid;
		}
	      case CB_CATEGORY_NUMERIC:
		{
		  if (f->pic->expt > 0)
		    {
		      /* check for PIC 9(n)P(m) */
		      if (least_significant < f->pic->expt)
			goto value_mismatch;
		    }
		  else if (f->pic->expt < -f->pic->size)
		    {
		      /* check for PIC P(n)9(m) */
		      if (most_significant >= f->pic->expt + f->pic->size)
			goto value_mismatch;
		    }
		  break;
		}
	      case CB_CATEGORY_NUMERIC_EDITED:
		{
		  if (value_flag)
		    goto expect_alphanumeric;

		  /* TODO */
		  break;
		}
	      default:
		if (value_flag)
		  goto expect_alphanumeric;
		goto invalid;
	      }

	    /* sign check */
	    if (l->sign != 0 && !f->pic->have_sign)
	      {
		if (value_flag)
		  {
		    cb_error_x (loc, _("data item not signed"));
		    return -1;
		  }
		if (cb_warn_constant)
		  {
		    cb_warning_x (loc, _("ignoring negative sign"));
		  }
	      }

	    /* size check */
	    if (least_significant < f->pic->expt)
	      goto size_overflow;
	    if (most_significant >= (f->pic->digits
				     + (f->pic->expt < 0 ? f->pic->expt : 0)))
	      goto size_overflow;
	  }
	else
	  {
	    /* Alphanumeric literal */

	    /* value check */
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHABETIC:
		{
		  int i;
		  for (i = 0; i < l->size; i++)
		    if (!isalpha (l->data[i]) && !isspace (l->data[i]))
		      goto value_mismatch;
		  break;
		}
	      case CB_CATEGORY_NUMERIC:
		goto expect_numeric;
	      case CB_CATEGORY_NUMERIC_EDITED:
		if (!value_flag)
		  goto expect_numeric;

		/* TODO: validate the value */
		break;
	      default:
		break;
	      }

	    /* size check */
	    {
	      int size = cb_field_size (dst);
	      if (size >= 0 && l->size > size)
		goto size_overflow;
	    }
	  }
	break;
      }
    case CB_TAG_FIELD:
    case CB_TAG_REFERENCE:
      {
	/* non-elementary move */
	if (cb_field (src)->children || cb_field (dst)->children)
	  break;

	/* elementary move */
	switch (CB_TREE_CATEGORY (src))
	  {
	  case CB_CATEGORY_ALPHANUMERIC:
	    break;
	  case CB_CATEGORY_ALPHABETIC:
	  case CB_CATEGORY_ALPHANUMERIC_EDITED:
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_NUMERIC:
	      case CB_CATEGORY_NUMERIC_EDITED:
		goto invalid;
	      default:
		break;
	      }
	    break;
	  case CB_CATEGORY_NUMERIC:
	  case CB_CATEGORY_NUMERIC_EDITED:
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHABETIC:
		goto invalid;
	      case CB_CATEGORY_ALPHANUMERIC:
	      case CB_CATEGORY_ALPHANUMERIC_EDITED:
		if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC
		    && cb_field (src)->pic->expt < 0)
		  goto invalid;
	      default:
		break;
	      }
	    break;
	  default:
	    abort ();
	  }
	break;
      }
    case CB_TAG_BINARY_OP:
      break;
    default:
      abort ();
    }
  return 0;

 invalid:
  if (value_flag)
    cb_error_x (loc, _("invalid VALUE clause"));
  else
    cb_error_x (loc, _("invalid MOVE statement"));
  return -1;

 expect_numeric:
  return move_error (src, dst, value_flag, cb_warn_strict_typing,
		     _("numeric value is expected"));

 expect_alphanumeric:
  return move_error (src, dst, value_flag, cb_warn_strict_typing,
		     _("alphanumeric value is expected"));

 value_mismatch:
  return move_error (src, dst, value_flag, cb_warn_constant,
		     _("value does not fit the picture string"));

 size_overflow:
  return move_error (src, dst, value_flag, cb_warn_constant,
		     _("value size exceeds data size"));
}

cb_tree
cb_build_move (cb_tree src, cb_tree dst)
{
  validate_move (src, dst, 0);
  return cb_build_funcall_2 ("@move", src, dst);
}

static cb_tree
build_corr_1 (cb_tree (*func)(), cb_tree x1, cb_tree x2, int opt, cb_tree l)
{
  struct cb_field *f1, *f2;
  for (f1 = cb_field (x1)->children; f1; f1 = f1->sister)
    if (!f1->redefines && !f1->flag_occurs)
      for (f2 = cb_field (x2)->children; f2; f2 = f2->sister)
	if (!f2->redefines && !f2->flag_occurs)
	  if (strcmp (f1->name, f2->name) == 0)
	    {
	      cb_tree t1 = copy_reference (x1, CB_TREE (f1));
	      cb_tree t2 = copy_reference (x2, CB_TREE (f2));
	      if (f1->children && f2->children)
		l = build_corr_1 (func, t1, t2, opt, l);
	      else
		{
		  if (opt < 0)
		    l = cons (func (t1, t2), l);
		  else
		    l = cons (func (t1, t2, opt), l);
		}
	    }
  return l;
}

cb_tree
cb_build_corr (cb_tree (*func)(), cb_tree x1, cb_tree x2, int opt)
{
  return make_sequence (build_corr_1 (func, x1, x2, opt, NULL));
}


/*
 * DIVIDE
 */

cb_tree
cb_build_divide (cb_tree dividend, cb_tree divisor,
		 cb_tree quotient, cb_tree remainder)
{
  cb_tree l = NULL;
  l = list_add (l, cb_build_funcall_4 ("cob_div_quotient",
				       dividend, divisor, CB_VALUE (quotient),
				       CB_PURPOSE_INT (quotient) ? cb_int1 : cb_int0));
  l = list_add (l, cb_build_funcall_1 ("cob_div_remainder", CB_VALUE (remainder)));
  return make_sequence (l);
}


/*
 * Condition
 */

static cb_tree
build_cond_88 (cb_tree x)
{
  struct cb_field *f = cb_field (x);
  cb_tree l;
  cb_tree c1 = NULL;

  /* refer to parent's data storage */
  x = copy_reference (x, CB_TREE (f->parent));

  /* build condition */
  for (l = f->values; l; l = CB_CHAIN (l))
    {
      cb_tree t = CB_VALUE (l);
      cb_tree c2;
      if (CB_PAIR_P (t))
	{
	  /* VALUE THRU VALUE */
	  c2 = cb_build_binary_op (cb_build_binary_op (CB_PAIR_X (t), '[', x),
				   '&',
				   cb_build_binary_op (x, '[', CB_PAIR_Y (t)));
	}
      else
	{
	  /* VALUE */
	  c2 = cb_build_binary_op (x, '=', t);
	}
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = cb_build_binary_op (c1, '|', c2);
    }
  return c1;
}

cb_tree
cb_build_cond (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
    case CB_TAG_FUNCALL:
      return x;
    case CB_TAG_REFERENCE:
      {
	struct cb_field *f = cb_field (x);

	/* level 88 condition */
	if (f->level == 88)
	  {
	    /* We need to build a 88 condition at every occurrence
	       instead of once at the beginning because a 88 item
	       may be subscripted (i.e., it is not a constant tree). */
	    return cb_build_cond (build_cond_88 (x));
	  }

	/* constant condition */
	if (f->storage == CB_STORAGE_CONSTANT)
	  return cb_build_cond (CB_VALUE (f->values));

	abort ();
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	switch (p->op)
	  {
	  case '@':
	    return cb_build_cond (p->x);
	  case '!':
	    p->x = cb_build_cond (p->x);
	    break;
	  case '&': case '|':
	    p->x = cb_build_cond (p->x);
	    p->y = cb_build_cond (p->y);
	    break;
	  default:
	    if (CB_INDEX_P (p->x) || CB_INDEX_P (p->y))
	      return x;
	    else if (CB_BINARY_OP_P (p->x) || CB_BINARY_OP_P (p->y))
	      {
		/* decimal comparison */
		cb_tree s = make_sequence (NULL);
		cb_tree d1 = decimal_alloc ();
		cb_tree d2 = decimal_alloc ();
		decimal_expand (s, d1, p->x);
		decimal_expand (s, d2, p->y);
		add_stmt (s, cb_build_funcall_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		p->x = s;
	      }
	    else if (CB_LITERAL_P (p->y))
	      {
		struct cb_literal *l = CB_LITERAL (p->y);
		int size = cb_field_size (p->x);

		if (CB_TREE_CLASS (p->x) == CB_CLASS_NUMERIC
		    && CB_TREE_CLASS (p->y) == CB_CLASS_NUMERIC)
		  {
		    if (cb_fits_int (p->y))
		      p->x = cb_build_funcall_2 ("cob_cmp_int",
						 p->x, cb_build_cast_integer (p->y));
		    else
		      p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
		  }
		else if (size > 0 && size >= l->size && !l->all)
		  p->x = cb_build_funcall_2 ("@memcmp", p->x, p->y);
		else
		  p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    else
	      {
		/* field comparison */
		p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    break;
	  }
	return x;
      }
    default:
      abort ();
    }
}


/*
 * EVALUATE
 */

static cb_tree
evaluate_test (cb_tree s, cb_tree o)
{
  int flag;
  cb_tree x, y;

  /* ANY is always true */
  if (o == cb_any)
    return cb_true;

  /* object TRUE or FALSE */
  if (o == cb_true)
    return s;
  if (o == cb_false)
    return cb_build_negation (s);

  flag = CB_PURPOSE_INT (o);
  x = CB_PAIR_X (CB_VALUE (o));
  y = CB_PAIR_Y (CB_VALUE (o));

  /* subject TRUE or FALSE */
  if (s == cb_true)
    return flag ? cb_build_negation (x) : x;
  if (s == cb_false)
    return flag ? x : cb_build_negation (x);

  /* x THRU y */
  if (y)
    {
      cb_tree t = cb_build_binary_op (cb_build_binary_op (x, '[', s),
				      '&',
				      cb_build_binary_op (s, '[', y));
      return flag ? cb_build_negation (t) : t;
    }

  /* regular comparison */
  if (flag)
    return cb_build_binary_op (s, '~', x);
  else
    return cb_build_binary_op (s, '=', x);
}

static cb_tree
evaluate_internal (cb_tree subject_list, cb_tree case_list)
{
  cb_tree stmt;
  cb_tree c1 = NULL;
  cb_tree subjs, whens, objs;

  if (case_list == NULL)
    return NULL;

  whens = CB_VALUE (case_list);
  stmt = CB_VALUE (whens);
  whens = CB_CHAIN (whens);

  /* for each WHEN sequence */
  for (; whens; whens = CB_CHAIN (whens))
    {
      cb_tree c2 = NULL;
      /* single WHEN test */
      for (subjs = subject_list, objs = CB_VALUE (whens);
	   subjs && objs;
	   subjs = CB_CHAIN (subjs), objs = CB_CHAIN (objs))
	{
	  cb_tree c3 = evaluate_test (CB_VALUE (subjs), CB_VALUE (objs));
	  if (c2 == NULL)
	    c2 = c3;
	  else
	    c2 = cb_build_binary_op (c2, '&', c3);
	}
      if (subjs || objs)
	cb_error (_("wrong number of WHEN parameters"));
      /* connect multiple WHEN's */
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = cb_build_binary_op (c1, '|', c2);
    }

  if (c1 == NULL)
    return stmt;
  else
    return cb_build_if (cb_build_cond (c1), stmt,
			evaluate_internal (subject_list,
					   CB_CHAIN (case_list)));
}

cb_tree
cb_build_evaluate (cb_tree subject_list, cb_tree case_list)
{
  return evaluate_internal (subject_list, case_list);
}


/*
 * SEARCH ALL
 */

static void
search_set_keys (struct cb_field *f, cb_tree x)
{
  struct cb_binary_op *p;

  if (CB_REFERENCE_P (x))
    x = build_cond_88 (x);
  
  p = CB_BINARY_OP (x);
  switch (p->op)
    {
    case '&':
      search_set_keys (f, p->x);
      search_set_keys (f, p->y);
      break;
    case '=':
      {
	int i;
	for (i = 0; i < f->nkeys; i++)
	  if (cb_field (p->x) == cb_field (f->keys[i].key))
	    {
	      f->keys[i].ref = p->x;
	      f->keys[i].val = p->y;
	      break;
	    }
	if (i == f->nkeys)
	  cb_error_x (x, _("undeclared key `%s'"), cb_field (p->x)->name);
	break;
      }
    default:
      cb_error_x (x, _("invalid SEARCH ALL condition"));
      break;
    }
}

cb_tree
cb_build_search_all (cb_tree table, cb_tree cond)
{
  int i;
  struct cb_field *f = cb_field (table);
  cb_tree c1 = NULL;

  /* set keys */
  for (i = 0; i < f->nkeys; i++)
    f->keys[i].ref = 0;
  search_set_keys (f, cond);

  /* build condition */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].ref)
      {
	cb_tree c2;
	if (f->keys[i].dir == COB_ASCENDING)
	  c2 = cb_build_binary_op (f->keys[i].ref, '=', f->keys[i].val);
	else
	  c2 = cb_build_binary_op (f->keys[i].val, '=', f->keys[i].ref);
	if (c1 == NULL)
	  c1 = c2;
	else
	  c1 = cb_build_binary_op (c1, '&', c2);
      }

  return cb_build_cond (c1);
}
