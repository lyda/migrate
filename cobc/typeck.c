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
#include "tree.h"

int
cb_get_int (cb_tree x)
{
  return cb_literal_to_int (CB_LITERAL (x));
}

const char *
cb_build_program_id (cb_tree name, cb_tree alt_name)
{
  if (alt_name)
    {
      return CB_LITERAL (alt_name)->data;
    }
  else
    {
      int converted = 0;
      char *s, *str = strdup (CB_NAME (name));
      for (s = str; *s; s++)
	if (*s == '-')
	  {
	    converted = 1;
	    *s = '_';
	  }
      if (converted)
	cb_warning_x (name, _("PROGRAM-ID is converted to `%s'"), str);
      return str;
    }
}

void
cb_define_switch_name (cb_tree name, cb_tree sname, cb_tree flag, cb_tree ref)
{
  if (name == cb_error_node || sname == cb_error_node)
    return;

  if (CB_SYSTEM_NAME (sname)->category != CB_SWITCH_NAME)
    {
      cb_error_x (ref, _("switch-name is expected `%s'"), CB_NAME (ref));
    }
  else
    {
      cb_tree switch_id = cb_int (CB_SYSTEM_NAME (sname)->token);
      cb_tree value = cb_build_funcall_1 ("cob_get_switch", switch_id);
      if (flag == cb_int0)
	value = cb_build_negation (value);
      cb_build_constant (name, value);
    }
}

cb_tree
cb_build_section_name (cb_tree name)
{
  if (name == cb_error_node)
    return cb_error_node;

  if (CB_REFERENCE (name)->word->count > 0)
    {
      cb_tree x = CB_VALUE (CB_REFERENCE (name)->word->items);
      if (/* used as a non-label name */
	  !CB_LABEL_P (x)
	  /* used as a section name */
	  || CB_LABEL (x)->section == NULL
	  /* used as the same paragraph name in the same section */
	  || CB_LABEL (x)->section == current_section)
	{
	  redefinition_error (name);
	  return cb_error_node;
	}
    }

  return name;
}

cb_tree
cb_build_identifier (cb_tree x)
{
  struct cb_reference *r;
  struct cb_field *f;
  const char *name;
  cb_tree v ;

  if (x == cb_error_node)
    return cb_error_node;

  r = CB_REFERENCE (x);
  name = r->word->name;

  /* resolve reference */
  v = cb_ref (x);
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

  /* subscript check */
  if (r->subs)
    {
      struct cb_field *p;
      cb_tree l = r->subs;
      for (p = f; p; p = p->parent)
	if (p->flag_occurs)
	  {
	    cb_tree sub = CB_VALUE (l);

	    /* compile-time check */
	    if (CB_LITERAL_P (sub))
	      {
		int n = cb_literal_to_int (CB_LITERAL (sub));
		if (n < p->occurs_min || n > p->occurs_max)
		  cb_error_x (x, _("subscript of `%s' out of bounds: %d"),
			     name, n);
	      }

	    /* run-time check */
	    if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT))
	      {
		if (p->occurs_depending)
		  {
		    int n = p->occurs_max;
		    if (CB_LITERAL_P (sub))
		      n = cb_literal_to_int (CB_LITERAL (sub));
		    if (p->occurs_min <= n && n <= p->occurs_max)
		      {
			cb_tree e1, e2;
			e1 = cb_build_funcall_4 ("cob_check_odo",
						 cb_build_cast_integer (p->occurs_depending),
						 cb_int (p->occurs_min),
						 cb_int (p->occurs_max),
						 cb_build_string0 (cb_field (p->occurs_depending)->name));
			e2 = cb_build_funcall_4 ("cob_check_subscript",
						 cb_build_cast_integer (sub),
						 cb_int (p->occurs_min),
						 cb_build_cast_integer (p->occurs_depending),
						 cb_build_string0 (p->name));
			r->check = list_add (r->check, e1);
			r->check = list_add (r->check, e2);
		      }
		  }
		else
		  {
		    if (!CB_LITERAL_P (sub))
		      {
			cb_tree e1;
			e1 = cb_build_funcall_4 ("cob_check_subscript",
						 cb_build_cast_integer (sub),
						 cb_int1,
						 cb_int (p->occurs_max),
						 cb_build_string0 (p->name));
			r->check = list_add (r->check, e1);
		      }
		  }
	      }

	    l = CB_CHAIN (l);
	  }
    }

  /* reference modification check */
  if (r->offset)
    {
      /* compile-time check */
      if (CB_LITERAL_P (r->offset))
	{
	  int offset = cb_literal_to_int (CB_LITERAL (r->offset));
	  if (offset < 1 || offset > f->size)
	    cb_error_x (x, _("offset of `%s' out of bounds: %d"),
			name, offset);
	  else if (r->length && CB_LITERAL_P (r->length))
	    {
	      int length = cb_literal_to_int (CB_LITERAL (r->length));
	      if (length < 1 || length > f->size - offset + 1)
		cb_error_x (x, _("length of `%s' out of bounds: %d"),
			    name, length);
	    }
	}

      /* run-time check */
      if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_REF_MOD))
	{
	  if (!CB_LITERAL_P (r->offset)
	      || (r->length && !CB_LITERAL_P (r->length)))
	    {
	      cb_tree e1;
	      e1 = cb_build_funcall_4 ("cob_check_ref_mod",
				       cb_build_cast_integer (r->offset),
				       r->length ? cb_build_cast_integer (r->length) : cb_int1,
				       cb_int (f->size),
				       cb_build_string0 (f->name));
	      r->check = list_add (r->check, e1);
	    }
	}
    }

  if (f->storage == CB_STORAGE_CONSTANT)
    return CB_VALUE (f->values);

  return x;
}

cb_tree
cb_build_length (cb_tree x)
{
  if (!CB_REFERENCE_P (x))
    return x;

  /* X(I:L) */
  if (CB_REFERENCE (x)->length)
    return CB_REFERENCE (x)->length;

  /* X(I:) */
  if (cb_field_size (x) == -1)
    return cb_build_binary_op (cb_int (cb_field (x)->size), '-',
			       CB_REFERENCE (x)->offset);

  /* X */
  {
    unsigned char buff[20];
    sprintf (buff, "%d", cb_field_size (x));
    return cb_build_numeric_literal (0, buff, 0);
  }
}

cb_tree
cb_build_using_list (cb_tree list)
{
  cb_tree l;
  for (l = list; l; l = CB_CHAIN (l))
    {
      if (CB_VALUE (l) == cb_error_node)
	return cb_error_node;
      else
	{
	  cb_tree x = CB_VALUE (l);
	  struct cb_field *f = CB_FIELD (cb_ref (x));
	  if (f->level != 01 && f->level != 77)
	    cb_error_x (x, _("`%s' not level 01 or 77"), cb_name (x));
	}
      }
  return list;
}


/*
 * Expressions
 */

struct expr_node {
  /* The token of this node.
   *  'x'                          - values (cb_tree)
   *  '+', '-', '*', '/', '^'      - arithmetic operators
   *  '=', '~', '<', '>', '[', ']' - conditional operators
   *  '!', '&', '|'                - logical operators
   *  '(', ')'                     - parentheses
   */
  int token;
  /* The value itself if this node is a value */
  cb_tree value;
};

static int expr_op;			/* last operator */
static cb_tree expr_lh;			/* last left hand */

static int expr_index;			/* stack index */
static int expr_stack_size;		/* stack max size */
static struct expr_node *expr_stack;	/* expr node stack */

static char expr_prio[128];

#define TOKEN(offset)	(expr_stack[expr_index + offset].token)
#define VALUE(offset)	(expr_stack[expr_index + offset].value)

void
cb_expr_init (void)
{
  static int initialized = 0;
  if (initialized == 0)
    {
      /* init priority talble */
      expr_prio['x'] = 0;
      expr_prio['^'] = 1;
      expr_prio['*'] = 2;
      expr_prio['/'] = 2;
      expr_prio['+'] = 3;
      expr_prio['-'] = 3;
      expr_prio['='] = 4;
      expr_prio['~'] = 4;
      expr_prio['<'] = 4;
      expr_prio['>'] = 4;
      expr_prio['['] = 4;
      expr_prio[']'] = 4;
      expr_prio['!'] = 5;
      expr_prio['&'] = 6;
      expr_prio['|'] = 7;
      expr_prio[')'] = 8;
      expr_prio['('] = 9;
      expr_prio[0] = 10;
      /* init stack */
      expr_stack_size = 8;
      expr_stack = malloc (sizeof (struct expr_node) * expr_stack_size);
      initialized = 1;
    }

  expr_op = 0;
  expr_lh = NULL;
  expr_index = 0;
}

static int
expr_reduce (int token)
{
  /* Example:
   * index: -3  -2  -1   0
   * token: 'x' '*' 'x' '+' ...
   */
  while (expr_index >= 2
	 && expr_prio[TOKEN (-2)] > 0
	 && expr_prio[TOKEN (-2)] <= expr_prio[token])
    {
      /* Reduce the expression depending on the last operator */
      int op = TOKEN (-2);
      switch (op)
	{
	case '+': case '-': case '*': case '/': case '^':
	  /* Arithmetic operators: 'x' op 'x' */
	  if (expr_index < 3 || TOKEN (-1) != 'x' || TOKEN (-3) != 'x')
	    return -1;
	  TOKEN (-3) = 'x';
	  VALUE (-3) = cb_build_binary_op (VALUE (-3), op, VALUE (-1));
	  expr_index -= 2;
	  break;

	case '!':
	  /* Negation: '!' 'x' */
	  if (TOKEN (-1) != 'x')
	    return -1;
	  /* 'x' '=' 'x' '|' '!' 'x' */
	  if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
	    VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
	  TOKEN (-2) = 'x';
	  VALUE (-2) = cb_build_negation (VALUE (-1));
	  expr_index -= 1;
	  break;

	case '&': case '|':
	  /* Logical AND/OR: 'x' op 'x' */
	  if (expr_index < 3 || TOKEN (-1) != 'x' || TOKEN (-3) != 'x')
	    return -1;
	  /* 'x' '=' 'x' '|' 'x' */
	  if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
	    VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
	  /* warning for complex expressions without explicit parentheses
	     (i.e., "a OR b AND c" or "a AND b OR c") */
	  if (cb_warn_parentheses && op == '|')
	    if ((CB_BINARY_OP_P (VALUE (-3))
		 && CB_BINARY_OP (VALUE (-3))->op == '&')
		|| (CB_BINARY_OP_P (VALUE (-1))
		    && CB_BINARY_OP (VALUE (-1))->op == '&'))
	      cb_warning (_("suggest parentheses around AND within OR"));
	  TOKEN (-3) = 'x';
	  VALUE (-3) = cb_build_binary_op (VALUE (-3), op, VALUE (-1));
	  expr_index -= 2;
	  break;

	case '(': case ')':
	  return 0;

	default:
	  /* Conditional operators */
	  if (expr_index < 3 || TOKEN (-1) != 'x')
	    return -1;
	  switch (TOKEN (-3))
	    {
	    case 'x':
	      /* Simple condition: 'x' op 'x' */
	      expr_lh = VALUE (-3);
	      expr_op = op;
	      TOKEN (-3) = 'x';
	      VALUE (-3) = cb_build_binary_op (expr_lh, op, VALUE (-1));
	      expr_index -= 2;
	      break;
	    case '&':
	    case '|':
	      /* Complex condition: 'x' '=' 'x' '|' op 'x' */
	      expr_op = op;
	      TOKEN (-2) = 'x';
	      VALUE (-2) = cb_build_binary_op (expr_lh, op, VALUE (-1));
	      expr_index -= 1;
	      break;
	    default:
	      return -1;
	    }
	  break;
	}
    }

  /* handle special case "op OR x AND" */
  if (token == '&'
      && expr_index >= 2
      && TOKEN (-2) == '|'
      && CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
    {
      TOKEN (-1) = 'x';
      VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
    }

  return 0;
}

void
cb_expr_shift (int token, cb_tree value)
{
 start:
  if (token == 'x')
    {
      /* sign ZERO condition */
      if (value == cb_zero)
	if (TOKEN (-1) == 'x' || TOKEN (-1) == '!')
	  {
	    cb_expr_shift_sign ('=');
	    return;
	  }

      /* class condition */
      if (CB_CLASS_NAME_P (value))
	{
	  cb_expr_shift_class (CB_CLASS_NAME (value)->cname);
	  return;
	}

      /* unary sign */
      if (expr_index > 0 && (TOKEN (-1) == '+' || TOKEN (-1) == '-'))
	if (expr_index == 1 || TOKEN (-2) != 'x')
	  {
	    if (TOKEN (-1) == '-')
	      value = cb_build_binary_op (cb_zero, '-', value);
	    expr_index--;
	  }
    }
  else
    {
      /* '<' '|' '=' --> '[' */
      /* '>' '|' '=' --> ']' */
      if (expr_index >= 2 && token == '=' && TOKEN (-1) == '|'
	  && (TOKEN (-2) == '<' || TOKEN (-2) == '>'))
	{
	  token = (TOKEN (-2) == '<') ? '[' : ']';
	  expr_index -= 2;
	}

      /* '!' '=' --> '~', etc. */
      if (expr_index >= 1 && TOKEN (-1) == '!')
	switch (token)
	  {
	  case '=': token = '~'; expr_index--; break;
	  case '~': token = '='; expr_index--; break;
	  case '<': token = ']'; expr_index--; break;
	  case '>': token = '['; expr_index--; break;
	  case '[': token = '>'; expr_index--; break;
	  case ']': token = '<'; expr_index--; break;
	  }

#if 0
      switch (token)
	{
	case '=': case '~': case '<': case '>': case '[': case ']':
	  expr_lh = VALUE (-1);
	  expr_op = token;
	}
#endif
    }

  /* reduce */
  expr_reduce (token);

  if (token == ')')
    {
      if (expr_index >= 2 && TOKEN (-2) == '(')
	{
	  token = 'x';
	  value = cb_build_parenthesis (VALUE (-1));
	  expr_index -= 2;
	  goto start;
	}
    }
  else
    {
      /* allocate sufficient stack memory */
      if (expr_index >= expr_stack_size)
	{
	  expr_stack_size *= 2;
	  expr_stack = realloc (expr_stack,
				sizeof (struct expr_node) * expr_stack_size);
	}

      /* put on the stack */
      expr_stack[expr_index].token = token;
      expr_stack[expr_index].value = value;
      expr_index++;
    }
}

void
cb_expr_shift_class (const char *name)
{
  int have_not = 0;
  if (expr_index > 0 && TOKEN (-1) == '!')
    {
      have_not = 1;
      expr_index--;
    }
  expr_reduce ('=');
  if (expr_index > 0 && TOKEN (-1) == 'x')
    {
      VALUE (-1) = cb_build_funcall_1 (name, VALUE (-1));
      if (have_not)
	VALUE (-1) = cb_build_negation (VALUE (-1));
    }
}

void
cb_expr_shift_sign (char op)
{
  int have_not = 0;
  if (expr_index > 0 && TOKEN (-1) == '!')
    {
      have_not = 1;
      expr_index--;
    }
  expr_reduce ('=');
  if (expr_index > 0 && TOKEN (-1) == 'x')
    {
      VALUE (-1) = cb_build_binary_op (VALUE (-1), op, cb_zero);
      if (have_not)
	VALUE (-1) = cb_build_negation (VALUE (-1));
    }
}

cb_tree
cb_expr_finish (void)
{
  expr_reduce (0); /* reduce all */

  if (expr_index != 1)
    {
      cb_error (_("invalid expression"));
      return cb_error_node;
    }

  return expr_stack[0].value;
}


/*
 * Numerical operation
 */

static cb_tree decimal_stack = NULL;

#define dpush(x) decimal_stack = cons (x, decimal_stack)

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
decimal_compute (char op, cb_tree x, cb_tree y)
{
  const char *func;
  switch (op)
    {
    case '+': func = "cob_decimal_add"; break;
    case '-': func = "cob_decimal_sub"; break;
    case '*': func = "cob_decimal_mul"; break;
    case '/': func = "cob_decimal_div"; break;
    case '^': func = "cob_decimal_pow"; break;
    default: ABORT ();
    }
  dpush (cb_build_funcall_2 (func, x, y));
}

static void
decimal_expand (cb_tree d, cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      {
	if (x == cb_zero)
	  dpush (cb_build_funcall_2 ("cob_decimal_set_int", d, cb_int0));
	else
	  ABORT ();
	break;
      }
    case CB_TAG_LITERAL:
      {
	/* set d, N */
	struct cb_literal *l = CB_LITERAL (x);
	if (l->size < 10 && l->scale == 0)
	  dpush (cb_build_funcall_2 ("cob_decimal_set_int",
				     d, cb_build_cast_integer (x)));
	else
	  dpush (cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_REFERENCE:
      {
	/* set d, X */
	struct cb_field *f = cb_field (x);

	/* check numeric */
	if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE))
	  if (f->usage == CB_USAGE_DISPLAY)
	    dpush (cb_build_funcall_2 ("cob_check_numeric",
				       x, cb_build_string0 (f->name)));

	if (cb_fits_int (x))
	  dpush (cb_build_funcall_2 ("cob_decimal_set_int",
				     d, cb_build_cast_integer (x)));
	else
	  dpush (cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	if (p->op == '@')
	  {
	    decimal_expand (d, p->x);
	  }
	else
	  {
	    /* set d, X
	     * set t, Y
	     * OP d, t */
	    cb_tree t = decimal_alloc ();
	    decimal_expand (d, p->x);
	    decimal_expand (t, p->y);
	    decimal_compute (p->op, d, t);
	    decimal_free ();
	  }
	break;
      }
    default:
      ABORT ();
    }
}

static void
decimal_assign (cb_tree x, cb_tree d, int round)
{
  if (round)
    dpush (cb_build_funcall_2 ("cob_decimal_get_field_round", d, x));
  else
    dpush (cb_build_funcall_2 ("cob_decimal_get_field", d, x));
}

static cb_tree
build_decimal_assign (cb_tree vars, char op, cb_tree val)
{
  cb_tree l;
  cb_tree s1 = NULL;
  cb_tree d = decimal_alloc ();

  /* set d, VAL */
  decimal_expand (d, val);

  if (op == 0)
    {
      for (l = vars; l; l = CB_CHAIN (l))
	{
	  /* set VAR, d */
	  decimal_assign (CB_VALUE (l), d, CB_PURPOSE_INT (l));
	  s1 = list_add (s1, list_reverse (decimal_stack));
	  decimal_stack = NULL;
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
	  decimal_expand (t, CB_VALUE (l));
	  decimal_compute (op, t, d);
	  decimal_assign (CB_VALUE (l), t, CB_PURPOSE_INT (l));
	  s1 = list_add (s1, list_reverse (decimal_stack));
	  decimal_stack = NULL;
	}
      decimal_free ();
    }

  decimal_free ();
  return s1;
}

cb_tree
cb_build_arithmetic (cb_tree vars, char op, cb_tree val)
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
	      CB_VALUE (l) = cb_build_add (CB_VALUE (l), val, CB_PURPOSE (l));
	    else
	      CB_VALUE (l) = cb_build_sub (CB_VALUE (l), val, CB_PURPOSE (l));
	  }
	return vars;
      }

  return build_decimal_assign (vars, op, val);
}


/*
 * MOVE statement
 */

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

  if (value_flag)
    {
      /* VALUE clause */
      cb_warning_x (loc, msg);
    }
  else
    {
      /* MOVE statement */
      if (flag)
	{
	  cb_warning_x (loc, msg);
	  warning_destination (dst);
	}
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
	      most_significant = l->size - l->scale - i - 1;

	    /* compute the least significant figure place */
	    for (i = 0; i < l->size; i++)
	      if (l->data[l->size - i - 1] != '0')
		break;
	    if (i != l->size)
	      least_significant = -l->scale + i;

	    /* value check */
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHANUMERIC:
	      case CB_CATEGORY_ALPHANUMERIC_EDITED:
		{
		  if (value_flag)
		    goto expect_alphanumeric;

		  if (l->scale == 0)
		    goto expect_alphanumeric;
		  else
		    goto invalid;
		}
	      case CB_CATEGORY_NUMERIC:
		{
		  if (f->pic->scale < 0)
		    {
		      /* check for PIC 9(n)P(m) */
		      if (least_significant < -f->pic->scale)
			goto value_mismatch;
		    }
		  else if (f->pic->scale > f->pic->size)
		    {
		      /* check for PIC P(n)9(m) */
		      if (most_significant >= f->pic->size - f->pic->scale)
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
	    if (least_significant < -f->pic->scale)
	      goto size_overflow;
	    if (most_significant >=
		f->pic->digits - (f->pic->scale > 0 ? f->pic->scale : 0))
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
		    && cb_field (src)->pic->scale > 0)
		  goto invalid;
	      default:
		break;
	      }
	    break;
	  default:
	    ABORT ();
	  }
	break;
      }
    case CB_TAG_INTEGER:
    case CB_TAG_BINARY_OP:
      /* TODO: check this */
      break;
    default:
      ABORT ();
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

static cb_tree
cb_build_memset (cb_tree x, char c)
{
  return cb_build_funcall_3 ("memset",
			     cb_build_cast_address (x),
			     cb_int (c),
			     cb_build_cast_length (x));
}

static cb_tree
cb_build_move_copy (cb_tree src, cb_tree dst)
{
  return cb_build_funcall_3 ("memcpy",
			     cb_build_cast_address (dst),
			     cb_build_cast_address (src),
			     cb_build_cast_length (dst));
}

static cb_tree
cb_build_move_call (cb_tree src, cb_tree dst)
{
  return cb_build_funcall_2 ("cob_move", src, dst);
}

static cb_tree
cb_build_move_num (cb_tree x, int high)
{
  switch (cb_field (x)->usage)
    {
    case CB_USAGE_BINARY:
      return cb_build_assign (x, cb_int (high ? -1 : 0));
    case CB_USAGE_DISPLAY:
      return cb_build_memset (x, high ? '9' : '0');
    case CB_USAGE_PACKED:
      return cb_build_memset (x, high ? 0x99 : 0x00);
    default:
      ABORT ();
    }
}

static cb_tree
cb_build_move_space (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      return cb_build_memset (x, ' ');
    default:
      return cb_build_move_call (cb_space, x);
    }
}

static cb_tree
cb_build_move_zero (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      if (cb_field (x)->flag_blank_zero)
	return cb_build_move_space (x);
      else
	return cb_build_move_num (x, 0);
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      return cb_build_memset (x, '0');
    default:
      return cb_build_move_call (cb_zero, x);
    }
}

static cb_tree
cb_build_move_high (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      return cb_build_move_num (x, 9);
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      return cb_build_memset (x, 255);
    default:
      return cb_build_move_call (cb_high, x);
    }
}

static cb_tree
cb_build_move_low (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      return cb_build_move_num (x, 0);
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      return cb_build_memset (x, 0);
    default:
      return cb_build_move_call (cb_low, x);
    }
}

static cb_tree
cb_build_move_quote (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      return cb_build_memset (x, '"');
    default:
      return cb_build_move_call (cb_quote, x);
    }
}

static cb_tree
cb_build_move_literal (cb_tree src, cb_tree dst)
{
  struct cb_literal *l = CB_LITERAL (src);
  struct cb_field *f = cb_field (dst);
  enum cb_category cat = CB_TREE_CATEGORY (dst);

  if (l->all)
    {
      int i;
      unsigned char *buff = malloc (f->size);
      for (i = 0; i < f->size; i++)
	buff[i] = l->data[i % l->size];
      return cb_build_funcall_3 ("memcpy",
				 cb_build_cast_address (dst),
				 cb_build_string (buff, f->size),
				 cb_build_cast_length (dst));
    }
  else if ((cat == CB_CATEGORY_NUMERIC
	    && f->usage == CB_USAGE_DISPLAY
	    && f->pic->scale == l->scale
	    && !f->flag_sign_leading
	    && !f->flag_sign_separate)
	   || ((cat == CB_CATEGORY_ALPHABETIC
		|| cat == CB_CATEGORY_ALPHANUMERIC)
	       && f->size < l->size + 16
	       && !cb_field_varying (f)))
    {
      unsigned char *buff = malloc (f->size);
      int diff = f->size - l->size;
      if (cat == CB_CATEGORY_NUMERIC)
	{
	  if (diff <= 0)
	    {
	      memcpy (buff, l->data - diff, f->size);
	    }
	  else
	    {
	      memset (buff, '0', diff);
	      memcpy (buff + diff, l->data, l->size);
	    }
	  if (l->sign < 0)
	    {
	      unsigned char *p = &buff[f->size - 1];
	      switch (cb_display_sign)
		{
		case COB_DISPLAY_SIGN_ASCII: *p += 0x40; break;
		case COB_DISPLAY_SIGN_ASCII10: *p += 0x10; break;
		default: ABORT ();
		}
	    }
	}
      else
	{
	  if (f->flag_justified)
	    {
	      if (diff <= 0)
		{
		  memcpy (buff, l->data - diff, f->size);
		}
	      else
		{
		  memset (buff, ' ', diff);
		  memcpy (buff + diff, l->data, l->size);
		}
	    }
	  else
	    {
	      if (diff <= 0)
		{
		  memcpy (buff, l->data, f->size);
		}
	      else
		{
		  memcpy (buff, l->data, l->size);
		  memset (buff + l->size, ' ', diff);
		}
	    }
	}
      return cb_build_funcall_3 ("memcpy",
				 cb_build_cast_address (dst),
				 cb_build_string (buff, f->size),
				 cb_build_cast_length (dst));
    }
  else if (cb_fits_int (src)
	   && (f->usage == CB_USAGE_BINARY && !f->flag_binary_swap)
	   && (f->size == 1 || f->size == 2 || f->size == 4 || f->size == 8))
    {
      int val = cb_literal_to_int (l);
      int n = f->pic->scale - l->scale;
      for (; n > 0; n--) val *= 10;
      for (; n < 0; n++) val /= 10;
      return cb_build_assign (dst, cb_int (val));
    }
  else
    {
      return cb_build_move_call (src, dst);
    }
}

static cb_tree
cb_build_move_field (cb_tree src, cb_tree dst)
{
  int src_size = cb_field_size (src);
  int dst_size = cb_field_size (dst);
  struct cb_field *src_f = cb_field (src);
  struct cb_field *dst_f = cb_field (dst);

  if ((src_size > 0 && dst_size > 0 && src_size >= dst_size)
      && (!cb_field_varying (src_f) && !cb_field_varying (dst_f)))
    switch (CB_TREE_CATEGORY (src))
      {
      case CB_CATEGORY_ALPHABETIC:
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC
	    || CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
	  if (dst_f->flag_justified == 0)
	    return cb_build_move_copy (src, dst);
	break;
      case CB_CATEGORY_ALPHANUMERIC:
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
	  if (dst_f->flag_justified == 0)
	    return cb_build_move_copy (src, dst);
	break;
      case CB_CATEGORY_NUMERIC:
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
	    && src_f->usage == CB_USAGE_DISPLAY
	    && dst_f->usage == CB_USAGE_DISPLAY
	    && src_f->pic->size == dst_f->pic->size
	    && src_f->pic->digits == dst_f->pic->digits
	    && src_f->pic->scale == dst_f->pic->scale
	    && src_f->pic->have_sign == dst_f->pic->have_sign
	    && src_f->flag_sign_leading == dst_f->flag_sign_leading
	    && src_f->flag_sign_separate == dst_f->flag_sign_separate)
	  return cb_build_move_copy (src, dst);
	else if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC
		 && src_f->pic->have_sign == 0)
	  return cb_build_move_copy (src, dst);
	break;
      default:
	break;
      }

  return cb_build_move_call (src, dst);
}

cb_tree
cb_build_move (cb_tree src, cb_tree dst)
{
  validate_move (src, dst, 0);

  if (CB_REFERENCE_P (src))
    CB_REFERENCE (src)->type = CB_SENDING_OPERAND;
  if (CB_REFERENCE_P (dst))
    CB_REFERENCE (dst)->type = CB_RECEIVING_OPERAND;

  if (CB_INDEX_P (dst))
    return cb_build_assign (dst, src);

  if (CB_INDEX_P (src) || CB_BINARY_OP_P (src))
    return cb_build_funcall_2 ("cob_set_int", dst,
			       cb_build_cast_integer (src));

  if (cb_flag_runtime_inlining)
    {
      /* convert "MOVE 0 TO X" into "MOVE ZERO TO X" */
      if (CB_NUMERIC_LITERAL_P (src)
	  && cb_fits_int (src)
	  && cb_literal_to_int (CB_LITERAL (src)) == 0)
	src = cb_zero;

      /* no optimization for binary swap and packed decimal for now */
      if (cb_field (dst)->flag_binary_swap
	  || cb_field (dst)->usage == CB_USAGE_PACKED)
	return cb_build_move_call (src, dst);

      /* output optimal code */
      if (src == cb_zero)
	return cb_build_move_zero (dst);
      else if (src == cb_space)
	return cb_build_move_space (dst);
      else if (src == cb_high)
	return cb_build_move_high (dst);
      else if (src == cb_low)
	return cb_build_move_low (dst);
      else if (src == cb_quote)
	return cb_build_move_quote (dst);
      else if (CB_LITERAL_P (src))
	return cb_build_move_literal (src, dst);
      else
	return cb_build_move_field (src, dst);
    }

  /* output runtime call */
  return cb_build_move_call (src, dst);
}


/*
 * ADD/SUBTRACT CORRESPONDING
 */

cb_tree
cb_build_add (cb_tree v, cb_tree n, cb_tree round)
{
  if (CB_INDEX_P (v))
    return cb_build_move (cb_build_binary_op (v, '+', n), v);

  if (round == cb_int0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_add_int", v, cb_build_cast_integer (n));
  if (round == cb_int1)
    return cb_build_funcall_2 ("cob_add_round", v, n);
  else
    return cb_build_funcall_2 ("cob_add", v, n);
}

cb_tree
cb_build_sub (cb_tree v, cb_tree n, cb_tree round)
{
  if (CB_INDEX_P (v))
    return cb_build_move (cb_build_binary_op (v, '-', n), v);

  if (round == cb_int0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_sub_int", v, cb_build_cast_integer (n));
  if (round == cb_int1)
    return cb_build_funcall_2 ("cob_sub_round", v, n);
  else
    return cb_build_funcall_2 ("cob_sub", v, n);
}

static cb_tree
build_corr_1 (cb_tree (*func)(), cb_tree x1, cb_tree x2, cb_tree opt, cb_tree l)
{
  struct cb_field *f1, *f2;
  for (f1 = cb_field (x1)->children; f1; f1 = f1->sister)
    if (!f1->redefines && !f1->flag_occurs)
      for (f2 = cb_field (x2)->children; f2; f2 = f2->sister)
	if (!f2->redefines && !f2->flag_occurs)
	  if (strcmp (f1->name, f2->name) == 0)
	    {
	      cb_tree t1 = cb_build_field_reference (f1, x1);
	      cb_tree t2 = cb_build_field_reference (f2, x2);
	      if (f1->children && f2->children)
		l = build_corr_1 (func, t1, t2, opt, l);
	      else
		l = cons (func (t1, t2, opt), l);
	    }
  return l;
}

cb_tree
cb_build_corr (cb_tree (*func)(), cb_tree x1, cb_tree x2, cb_tree opt)
{
  return build_corr_1 (func, x1, x2, opt, NULL);
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
  return l;
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
  x = cb_build_field_reference (f->parent, x);

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

	ABORT ();
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	switch (p->op)
	  {
	  case '@':
	    return cb_build_cond (p->x);
	  case '!':
	    return cb_build_binary_op (cb_build_cond (p->x), '!', 0);
	  case '&': case '|':
	    return cb_build_binary_op (cb_build_cond (p->x), p->op,
				       cb_build_cond (p->y));
	  default:
	    if (CB_INDEX_P (p->x) || CB_INDEX_P (p->y))
	      {
		x = cb_build_binary_op (p->x, '-', p->y);
	      }
	    else if (CB_BINARY_OP_P (p->x) || CB_BINARY_OP_P (p->y))
	      {
		/* decimal comparison */
		cb_tree d1 = decimal_alloc ();
		cb_tree d2 = decimal_alloc ();
		decimal_expand (d1, p->x);
		decimal_expand (d2, p->y);
		dpush (cb_build_funcall_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		x = list_reverse (decimal_stack);
		decimal_stack = NULL;
	      }
	    else
	      {
		if (cb_flag_runtime_inlining)
		  {
		    if (CB_LITERAL_P (p->y))
		      {
#if 0
			struct cb_literal *l = CB_LITERAL (p->y);
			int size = cb_field_size (p->x);
#endif

			if (CB_TREE_CLASS (p->x) == CB_CLASS_NUMERIC
			    && CB_TREE_CLASS (p->y) == CB_CLASS_NUMERIC
			    && cb_fits_int (p->y))
			  {
			    x = cb_build_funcall_2 ("cob_cmp_int",
						    p->x, cb_build_cast_integer (p->y));
			    break;
			  }
#if 0
			if (size > 0 && size >= l->size && !l->all)
			  {
			    x = cb_build_funcall_2 ("@memcmp", p->x, p->y);
			    break;
			  }
#endif
		      }
		  }

		/* field comparison */
		x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	  }
	return cb_build_binary_op (x, p->op, p->y);
      }
    default:
      ABORT ();
    }
}


/*
 * ACCEPT
 */

cb_tree
cb_build_accept_from (cb_tree x)
{
  switch (CB_SYSTEM_NAME (cb_ref (x))->token)
    {
    case CB_DEVICE_CONSOLE:
    case CB_DEVICE_SYSIN:
      return cb_int0;
    default:
      cb_error_x (x, _("invalid input stream `%s'"), cb_name (x));
      return cb_error_node;
    }
}

cb_tree
cb_build_accept_from_direct (cb_tree x)
{
  const char *name = CB_NAME (x);

  if (CB_REFERENCE (x)->word->count == 0)
    {
      cb_tree sys = lookup_system_name (CB_NAME (x));
      if (sys != cb_error_node)
	{
	  switch (CB_SYSTEM_NAME (sys)->token)
	    {
	    case CB_DEVICE_CONSOLE:
	    case CB_DEVICE_SYSIN:
	      cb_warning_x (x, _("`%s' undefined in SPECIAL-NAMES"), name);
	      return cb_int0;
	    default:
	      break;
	    }
	}
    }

  cb_error_x (x, _("`%s' undefined in SPECIAL-NAMES"), name);
  return cb_error_node;
}


/*
 * DISPLAY
 */

cb_tree
cb_build_display_statement (cb_tree values, cb_tree upon, cb_tree no_adv,
			    cb_tree pos)
{
  cb_tree l;

  /* screen mode */
  if (current_program->flag_screen)
    {
      for (l = values; l; l = CB_CHAIN (l))
	{
	  cb_tree x = CB_VALUE (l);
	  if (CB_FIELD_P (cb_ref (x))
	      && CB_FIELD (cb_ref (x))->storage == CB_STORAGE_SCREEN)
	    {
	      cb_tree line = CB_PAIR_X (pos);
	      cb_tree column = CB_PAIR_Y (pos);
	      CB_VALUE (l) =
		cb_build_funcall_3 ("cob_screen_display", x, line, column);
	    }
	  else
	    {
	      cb_error_x (x, "`%s' not defined in SCREEN SECTION",
			  cb_name (x));
	      return cb_error_node;
	    }
	}
      return values;
    }

  /* DISPLAY x UPON ENVIRONMENT-NAME */
  if (upon == cb_true)
    {
      if (list_length (values) != 1)
	{
	  cb_error (_("wrong number of data items"));
	  return cb_error_node;
	}
      return cb_build_funcall_1 ("cob_display_environment", CB_VALUE (values));
    }

  /* DISPLAY x ... [UPON device-name] */
  {
    int is_stdout = (upon == cb_int1);
    const char *display = is_stdout ? "cob_display" : "cob_display_error";
    const char *newline = is_stdout ? "cob_newline" : "cob_newline_error";
    for (l = values; l; l = CB_CHAIN (l))
      CB_VALUE (l) = cb_build_funcall_1 (display, CB_VALUE (l));
    if (no_adv == cb_int0)
      values = list_add (values, cb_build_funcall_0 (newline));
    return values;
  }
}

cb_tree
cb_build_display_upon (cb_tree x)
{
  if (x == cb_error_node)
    return cb_error_node;

  switch (CB_SYSTEM_NAME (cb_ref (x))->token)
    {
    case CB_DEVICE_CONSOLE:
    case CB_DEVICE_SYSOUT:
      return cb_int1;
    case CB_DEVICE_SYSERR:
      return cb_int2;
    default:
      cb_error_x (x, _("invalid output stream"));
      return cb_error_node;
    }
}

cb_tree
cb_build_display_upon_direct (cb_tree x)
{
  const char *name = CB_NAME (x);

  if (CB_REFERENCE (x)->word->count == 0)
    {
      cb_tree sys = lookup_system_name (CB_NAME (x));
      if (sys != cb_error_node)
	{
	  switch (CB_SYSTEM_NAME (sys)->token)
	    {
	    case CB_DEVICE_CONSOLE:
	    case CB_DEVICE_SYSOUT:
	      cb_warning_x (x, _("`%s' undefined in SPECIAL-NAMES"), name);
	      return cb_int1;
	    case CB_DEVICE_SYSERR:
	      cb_warning_x (x, _("`%s' undefined in SPECIAL-NAMES"), name);
	      return cb_int2;
	    default:
	      break;
	    }
	}
    }

  cb_error_x (x, _("`%s' undefined in SPECIAL-NAMES"), name);
  return cb_error_node;
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


/* validate program */

void
cb_validate_program_environment (struct cb_program *prog)
{
  /* resolve the program collating sequence */
  if (prog->collating_sequence)
    {
      cb_tree x = cb_ref (prog->collating_sequence);
      if (x == cb_error_node)
	{
	  prog->collating_sequence = NULL;
	  return;
	}

      if (!CB_ALPHABET_NAME_P (x))
	{
	  cb_error_x (prog->collating_sequence, _("`%s' not alphabet name"),
		      cb_name (prog->collating_sequence));
	  prog->collating_sequence = NULL;
	  return;
	}

      if (CB_ALPHABET_NAME (x)->type == CB_ALPHABET_CUSTOM)
	{
	  x = CB_VALUE (CB_ALPHABET_NAME (x)->custom_list);
	  if (CB_PAIR_P (x) && CB_PAIR_X (x))
	    cb_low = CB_PAIR_X (x);
	  else if (CB_LIST_P (x))
	    cb_low = CB_VALUE (x);
	  else
	    cb_low = cb_build_alphanumeric_literal (CB_LITERAL (x)->data, 1);
	}
    }
}

void
cb_validate_program_data (struct cb_program *prog)
{
  cb_tree l;

  /* build undeclared assign name */
  if (cb_assign_identifier == CB_ASSIGN_DATA)
    for (l = current_program->file_list; l; l = CB_CHAIN (l))
      {
	cb_tree assign = CB_FILE (CB_VALUE (l))->assign;
	if (CB_REFERENCE_P (assign) && CB_REFERENCE (assign)->word->count == 0)
	  {
	    cb_tree x = cb_build_implicit_field (assign, FILENAME_MAX);
	    struct cb_field *p = current_program->working_storage;
	    if (p)
	      {
		while (p->sister)
		  p = p->sister;
		p->sister = CB_FIELD (x);
	      }
	    else
	      current_program->working_storage = CB_FIELD (x);
	  }
      }

  /* resolve all references so far */
  for (l = list_reverse (prog->reference_list); l; l = CB_CHAIN (l))
    cb_ref (CB_VALUE (l));
}

void
cb_validate_program_body (struct cb_program *prog)
{
  /* resolve all labels */
  cb_tree l;
  for (l = list_reverse (prog->label_list); l; l = CB_CHAIN (l))
    {
      cb_tree x = CB_VALUE (l);
      cb_tree v = cb_ref (x);
      if (CB_LABEL_P (v))
	{
	  CB_LABEL (v)->need_begin = 1;
	  if (CB_REFERENCE (x)->length)
	    CB_LABEL (v)->need_return = 1;
	}
      else if (v != cb_error_node)
	cb_error_x (x, _("`%s' not procedure name"), cb_name (x));
    }

  prog->file_list = list_reverse (prog->file_list);
  prog->exec_list = list_reverse (prog->exec_list);
}
