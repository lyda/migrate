/*
 * Copyright (C) 2002-2003 Keisuke Nishida
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
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <libcob.h>

#include "cobc.h"
#include "tree.h"

static int param_id = 0;

static void output_stmt (cb_tree x);
static void output_integer (cb_tree x);
static void output_index (cb_tree x);
static void output_func_1 (const char *name, cb_tree x);


/*
 * Output routines
 */

static int output_indent_level = 0;
static FILE *output_target;

static void
output (const char *fmt, ...)
{
  if (output_target)
    {
      va_list ap;
      va_start (ap, fmt);
      vfprintf (output_target, fmt, ap);
      va_end (ap);
    }
}

static void
output_newline (void)
{
  if (output_target)
    fputs ("\n", output_target);
}

static void
output_prefix (void)
{
  if (output_target)
    {
      int i;
      for (i = 0; i < output_indent_level; i++)
	fputc (' ', output_target);
    }
}

static void
output_line (const char *fmt, ...)
{
  if (output_target)
    {
      va_list ap;
      va_start (ap, fmt);
      output_prefix ();
      vfprintf (output_target, fmt, ap);
      fputc ('\n', output_target);
      va_end (ap);
    }
}

static void
output_indent (const char *str)
{
  const char *p;
  int level = 2;

  for (p = str; *p == ' '; p++)
    level++;

  if (*p == '}')
    output_indent_level -= level;

  output_line (str);

  if (*p == '{')
    output_indent_level += level;
}

static void
output_string (const unsigned char *s, int size)
{
  int i;
  int printable = 1;
  for (i = 0; i < size; i++)
    if (!isprint (s[i]))
      printable = 0;

  output ("\"");
  for (i = 0; i < size; i++)
    {
      int c = s[i];
      if (printable)
	{
	  if (c == '\"' || c == '\\')
	    output ("\\%c", c);
	  else
	    output ("%c", c);
	}
      else
	output ("\\%03o", c);
    }
  output ("\"");
}

static void
output_storage (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (cb_storage_file, fmt, ap);
  va_end (ap);
}


/*
 * Field
 */

static void
output_base (struct cb_field *f)
{
  struct cb_field *f01 = cb_field_founder (f);

  if (f01->redefines)
    f01 = f01->redefines;

  if (f01->flag_external)
    {
      output ("%s", f01->cname);
    }
  else
    {
      if (!f01->flag_base)
	{
	  if (!f01->flag_local)
	    output_storage ("static ");
	  output_storage ("unsigned char b_%s[%d];\n",
			  f01->cname, f01->memory_size);
	  f01->flag_base = 1;
	}
      output ("b_%s", f01->cname);
    }
  if (f->offset > 0)
    output (" + %d", f->offset);
}

static void
output_data (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (x);
	if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
	  output ("\"%s%s\"", l->data,
		  (l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "");
	else
	  output_string (l->data, l->size);
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	/* base address */
	output_base (f);

	/* subscripts */
	if (r->subs)
	  {
	    cb_tree l = r->subs;
	    for (; f; f = f->parent)
	      if (f->flag_occurs)
		{
		  output (" + ");
		  if (f->size != 1)
		    output ("%d * ", f->size);
		  output_index (CB_VALUE (l));
		  l = CB_CHAIN (l);
		}
	  }

	/* offset */
	if (r->offset)
	  {
	    output (" + ");
	    output_index (r->offset);
	  }
	break;
      }
    default:
      abort ();
    }
}

static void
output_size (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (x);
	output ("%d", l->size + ((l->sign != 0) ? 1 : 0));
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	if (r->length)
	  {
	    output_integer (r->length);
	  }
	else if (r->offset)
	  {
	    output ("%d - ", f->size);
	    output_index (r->offset);
	  }
	else
	  {
	    struct cb_field *p = cb_field_varying (f);
	    if (p && (r->type == CB_SENDING_OPERAND
		      || !cb_field_subordinate (cb_field (p->occurs_depending), f)))
	      {
		output ("%d + ", p->offset - f->offset);
		if (p->size != 1)
		  output ("%d * ", p->size);
		output_integer (p->occurs_depending);
	      }
	    else
	      output ("%d", f->size);
	  }
	break;
      }
    default:
      abort ();
    }
}

static int
tree_type (cb_tree x)
{
  struct cb_field *f = cb_field (x);

  if (f->children)
    return COB_TYPE_GROUP;

  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_ALPHABETIC:
      return COB_TYPE_ALPHABETIC;
    case CB_CATEGORY_ALPHANUMERIC:
      return COB_TYPE_ALPHANUMERIC;
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
      return COB_TYPE_ALPHANUMERIC_EDITED;
    case CB_CATEGORY_NUMERIC:
      switch (cb_field (x)->usage)
	{
	case CB_USAGE_DISPLAY:
	  return COB_TYPE_NUMERIC_DISPLAY;
	case CB_USAGE_BINARY:
	  return COB_TYPE_NUMERIC_BINARY;
	case CB_USAGE_PACKED:
	  return COB_TYPE_NUMERIC_PACKED;
	default:
	  abort ();
	}
    case CB_CATEGORY_NUMERIC_EDITED:
      return COB_TYPE_NUMERIC_EDITED;
    default:
      abort ();
    }
}

static int
lookup_attr (char type, char digits, char expt, char flags, unsigned char *pic)
{
  static int id = 0;
  static struct attr_list {
    int id;
    char type;
    char digits;
    char expt;
    char flags;
    unsigned char *pic;
    struct attr_list *next;
  } *attr_cache = NULL;

  struct attr_list *l;

  /* search attribute cache */
  for (l = attr_cache; l; l = l->next)
    if (type == l->type
	&& digits == l->digits
	&& expt == l->expt
	&& flags == l->flags
	&& ((pic == l->pic)
	    || (pic && l->pic && strcmp (pic, l->pic) == 0)))
      return l->id;

  /* output new attribute */
  output_storage ("static cob_field_attr a_%d = ", ++id);
  output_storage ("{%d, %d, %d, %d, ", type, digits, -expt, flags);
  if (pic)
    {
      unsigned char *s;
      output_storage ("\"");
      for (s = pic; *s; s += 2)
	output_storage ("%c\\%03o", s[0], s[1]);
      output_storage ("\"");
    }
  else
    output_storage ("0");
  output_storage ("};\n");

  /* cache it */
  l = malloc (sizeof (struct attr_list));
  l->id = id;
  l->type = type;
  l->digits = digits;
  l->expt = expt;
  l->flags = flags;
  l->pic = pic;
  l->next = attr_cache;
  attr_cache = l;

  return id;
}

static void
output_attr (cb_tree x)
{
  int id;
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (x);

	if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
	  {
	    char flags = 0;
	    if (l->sign != 0)
	      flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
	    id = lookup_attr (COB_TYPE_NUMERIC_DISPLAY,
			      l->size, l->expt, flags, 0);
	  }
	else
	  {
	    if (l->all)
	      id = lookup_attr (COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, 0);
	    else
	      id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0);
	  }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	int type = tree_type (x);
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	if (r->offset)
	  id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0);
	else
	  switch (type)
	    {
	    case COB_TYPE_GROUP:
	    case COB_TYPE_ALPHANUMERIC:
	      if (f->flag_justified)
		id = lookup_attr (type, 0, 0, COB_FLAG_JUSTIFIED, 0);
	      else
		id = lookup_attr (type, 0, 0, 0, 0);
	      break;
	    default:
	      {
		char flags = 0;
		if (f->pic->have_sign)
		  flags |= COB_FLAG_HAVE_SIGN;
		if (f->flag_sign_separate)
		  flags |= COB_FLAG_SIGN_SEPARATE;
		if (f->flag_sign_leading)
		  flags |= COB_FLAG_SIGN_LEADING;
		if (f->flag_blank_zero)
		  flags |= COB_FLAG_BLANK_ZERO;
		if (f->flag_justified)
		  flags |= COB_FLAG_JUSTIFIED;

		id = lookup_attr (type, f->pic->digits, f->pic->expt,
				  flags, f->pic->str);
		break;
	      }
	    }
	break;
      }
    default:
      abort ();
    }

  output ("&a_%d", id);
}

static void
output_field (cb_tree x)
{
  output ("{");
  output_size (x);
  output (", ");
  output_data (x);
  output (", ");
  output_attr (x);
  output ("}");
}


/*
 * Literal
 */

static int
lookup_literal (cb_tree x)
{
  static int id = 0;
  static struct literal_list {
    int id;
    struct cb_literal *literal;
    struct literal_list *next;
  } *literal_cache = NULL;
  
  struct cb_literal *literal = CB_LITERAL (x);
  struct literal_list *l;

  /* search literal cache */
  for (l = literal_cache; l; l = l->next)
    if (CB_TREE_CLASS (literal) == CB_TREE_CLASS (l->literal)
	&& literal->size == l->literal->size
	&& literal->all  == l->literal->all
	&& literal->sign == l->literal->sign
	&& literal->expt == l->literal->expt
	&& strcmp (literal->data, l->literal->data) == 0)
      return l->id;

  /* output new literal */
  output_target = 0;
  output_field (x);

  output_target = cb_storage_file;
  output ("static cob_field c_%d = ", ++id);
  output_field (x);
  output (";\n");
  output_target = yyout;

  /* cache it */
  l = malloc (sizeof (struct literal_list));
  l->id = id;
  l->literal = literal;
  l->next = literal_cache;
  literal_cache = l;

  return id;
}


/*
 * Integer
 */

static void
output_integer (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      if (x == cb_zero)
	output ("0");
      else
	output ("%s", CB_CONST (x)->val);
      break;
    case CB_TAG_INTEGER:
      output ("%d", CB_INTEGER (x)->val);
      break;
    case CB_TAG_LITERAL:
      output ("%d", cb_literal_to_int (CB_LITERAL (x)));
      break;
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	output_integer (p->x);
	output (" %c ", p->op);
	output_integer (p->y);
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_field *f = cb_field (x);
	if (f->usage == CB_USAGE_INDEX)
	  {
	    if (f->level == 0)
	      {
		output ("i_%s", f->cname);
	      }
	    else
	      {
		output ("(*(int *) (");
		output_data (x);
		output ("))");
	      }
	    return;
	  }

	if (cb_flag_inline_get_int)
	  {
	    switch (f->usage)
	      {
	      case CB_USAGE_DISPLAY:
		if (f->pic->expt <= 0
		    && f->size + f->pic->expt <= 4
		    && f->pic->have_sign == 0)
		  {
		    int i, j;
		    int size = f->size + f->pic->expt;
		    output ("(");
		    for (i = 0; i < size; i++)
		      {
			output ("((");
			output_data (x);
			output (")");
			output ("[%d] - '0')", i);
			if (i + 1 < size)
			  {
			    output (" * 1");
			    for (j = 1; j < size - i; j++)
			      output ("0");
			    output (" + ");
			  }
		      }
		    output (")");
		    return;
		  }
		break;

	      case CB_USAGE_BINARY:
		output ("(*(");
		switch (f->size)
		  {
		  case 1: output ("char"); break;
		  case 2: output ("short"); break;
		  case 4: output ("long"); break;
		  case 8: output ("long long"); break;
		  }
		output (" *) (");
		output_data (x);
		output ("))");
		return;

	      default:
		break;
	      }
	  }

	output_func_1 ("cob_get_int", x);
	break;
      }
    default:
      abort ();
    }
}

static void
output_index (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_INTEGER:
      output ("%d", CB_INTEGER (x)->val - 1);
      break;
    case CB_TAG_LITERAL:
      output ("%d", cb_literal_to_int (CB_LITERAL (x)) - 1);
      break;
    default:
      output ("(");
      output_integer (x);
      output (" - 1)");
      break;
    }
}


/*
 * Parameter
 */

static void
output_param (cb_tree x, int id)
{
  char fname[5];
  sprintf (fname, "f[%d]", id);
  param_id = id;

  if (x == NULL)
    {
      output ("0");
      return;
    }

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      output ("%s", CB_CONST (x)->val);
      break;
    case CB_TAG_INTEGER:
      output_integer (x);
      break;
    case CB_TAG_STRING:
      output_string (CB_STRING (x)->data, CB_STRING (x)->size);
      break;
    case CB_TAG_CAST:
      switch (CB_CAST (x)->type)
	{
	case CB_CAST_INTEGER:
	  output_integer (CB_CAST (x)->val);
	  break;
	case CB_CAST_ADDRESS:
	  output_data (CB_CAST (x)->val);
	  break;
	case CB_CAST_LENGTH:
	  output_size (CB_CAST (x)->val);
	  break;
	}
      break;
    case CB_TAG_DECIMAL:
      output ("&d[%d]", CB_DECIMAL (x)->id);
      break;
    case CB_TAG_FILE:
      output ("&%s", CB_FILE (x)->cname);
      break;
    case CB_TAG_LITERAL:
      output ("&c_%d", lookup_literal (x));
      break;
    case CB_TAG_FIELD:
      /* TODO: remove me */
      output_param (cb_build_field_reference (CB_FIELD (x), 0), id);
      break;
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	if (!r->subs && !r->offset && !cb_field_varying (f))
	  {
	    if (!f->flag_field)
	      {
		output_target = 0;
		output_field (x);

		output_target = cb_storage_file;
		if (!f->flag_local)
		  output ("static ");
		output ("cob_field f_%s = ", f->cname);
		output_field (x);
		output (";\n");

		f->flag_field = 1;
		output_target = yyout;
	      }
	    output ("&f_%s", f->cname);
	    return;
	  }

	output ("(%s = (cob_field) ", fname);
	output_field (x);
	output (", &%s)", fname);
	break;
      }
    default:
      abort ();
    }
}


/*
 * Function call
 */

static void
output_funcall (cb_tree x)
{
  int i;
  struct cb_funcall *p = CB_FUNCALL (x);
  output ("%s (", p->name);
  for (i = 0; i < p->argc; i++)
    {
      output_param (p->argv[i], i);
      if (i + 1 < p->argc)
	output (", ");
    }
  output (")");
}

static void
output_func_1 (const char *name, cb_tree x)
{
  output ("%s (", name);
  output_param (x, param_id);
  output (")");
}


/*
 * Condition
 */

static void
output_cond (cb_tree x, int save_flag)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      {
	if (x == cb_true)
	  output ("1");
	else
	  abort ();
	break;
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	switch (p->op)
	  {
	  case '!':
	    output ("!");
	    output_cond (p->x, save_flag);
	    break;

	  case '&': case '|':
	    output ("(");
	    output_cond (p->x, save_flag);
	    output (p->op == '&' ? " && " : " || ");
	    output_cond (p->y, save_flag);
	    output (")");
	    break;

	  case '=': case '<': case '[': case '>': case ']': case '~':
	    output ("(");
	    output_cond (p->x, save_flag);
	    switch (p->op)
	      {
	      case '=': output (" == 0"); break;
	      case '<': output (" <  0"); break;
	      case '[': output (" <= 0"); break;
	      case '>': output (" >  0"); break;
	      case ']': output (" >= 0"); break;
	      case '~': output (" != 0"); break;
	      }
	    output (")");
	    break;

	  default:
	    output_integer (x);
	    break;
	  }
	break;
      }
    case CB_TAG_FUNCALL:
      {
	if (save_flag)
	  output ("(ret = ");
	output_funcall (x);
	if (save_flag)
	  output (")");
	break;
      }
    case CB_TAG_LIST:
      {
	if (save_flag)
	  output ("(ret = ");
	output_indent ("({");
	for (; x; x = CB_CHAIN (x))
	  output_stmt (CB_VALUE (x));
	output_indent ("})");
	if (save_flag)
	  output (")");
	break;
      }
    default:
      abort ();
    }
}


/*
 * MOVE
 */

static void
output_move (cb_tree src, cb_tree dst)
{
  output_stmt (cb_build_move (src, dst));
}


/*
 * INITIALIZE
 */

static int
initialize_any (struct cb_initialize *p, struct cb_field *f)
{
  if (f->redefines)
    return 0;

  if (p->def)
    return 1;

  if (p->val && f->values)
    return 1;

  if (f->children)
    {
      for (f = f->children; f; f = f->sister)
	if (initialize_any (p, f))
	  return 1;
    }
  else
    {
      cb_tree l;
      for (l = p->rep; l; l = CB_CHAIN (l))
	if (CB_PURPOSE_INT (l) == CB_TREE_CATEGORY (f))
	  return 1;
    }

  return 0;
}

static int
field_uniform_char (struct cb_field *f)
{
  if (f->children)
    {
      int c = field_uniform_char (f->children);
      for (f = f->children->sister; f; f = f->sister)
	if (!f->redefines)
	  if (c != field_uniform_char (f))
	    return -1;
      return c;
    }
  else
    {
      switch (tree_type (CB_TREE (f)))
	{
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_PACKED:
	  return 0;
	case COB_TYPE_NUMERIC_DISPLAY:
	  return '0';
	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
	  return ' ';
	default:
	  return -1;
	}
    }
}

static void
output_initialize_uniform (cb_tree x, int c, int size)
{
  output_prefix ();
  output ("memset (");
  output_data (x);
  output (", %d, %d);\n", c, size);
}

static void
output_initialize_value (cb_tree x)
{
  struct cb_field *f = cb_field (x);
  cb_tree value = CB_VALUE (f->values);

  if (CB_CONST_P (value)
      || CB_TREE_CLASS (value) == CB_CLASS_NUMERIC
      || CB_LITERAL (value)->all)
    {
      /* figurative literal, numeric literal, or ALL literal */
      output_move (value, x);
    }
  else
    {
      /* alphanumeric literal */
      /* We do not use output_move here because
	 we do not want to have the value be edited. */
      char buff[f->size];
      char *str = CB_LITERAL (value)->data;
      int size = CB_LITERAL (value)->size;
      if (size >= f->size)
	{
	  memcpy (buff, str, f->size);
	}
      else
	{
	  memcpy (buff, str, size);
	  memset (buff + size, ' ', f->size - size);
	}
      output_prefix ();
      output ("memcpy (");
      output_data (x);
      output (", ");
      output_string (buff, f->size);
      output (", %d);\n", f->size);
    }
}

static int
output_initialize_one (struct cb_initialize *p, cb_tree x)
{
  struct cb_field *f = cb_field (x);
  cb_tree l;

  /* initialize by value */
  if (p->val && f->values)
    {
      output_initialize_value (x);
      return 0;
    }

  /* initialize replacing */
  if (!f->children)
    for (l = p->rep; l; l = CB_CHAIN (l))
      if (CB_PURPOSE_INT (l) == CB_TREE_CATEGORY (x))
	{
	  output_move (CB_VALUE (l), x);
	  return 0;
	}

  /* initialize by defualt */
  if (p->def)
    switch (CB_TREE_CATEGORY (x))
      {
      case CB_CATEGORY_NUMERIC_EDITED:
	output_move (cb_zero, x);
	return 0;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
	output_move (cb_space, x);
	return 0;
      default:
	{
	  int c = field_uniform_char (f);
	  if (c != -1)
	    {
	      output_initialize_uniform (x, c, f->size * f->occurs_max);
	      return 0;
	    }
	  return -1;
	}
      }

  return f->children ? -1 : 0;
}

static void
output_initialize_compound (struct cb_initialize *p, cb_tree x)
{
  struct cb_field *f = cb_field (x);

  for (f = f->children; f; f = f->sister)
    if (initialize_any (p, f))
      {
	cb_tree c = cb_build_field_reference (f, x);

	if (f->flag_occurs)
	  {
	    /* begin occurs loop */
	    int i = f->indexes;
	    output_line ("for (i%d = 1; i%d <= %d; i%d++)",
			 i, i, f->occurs_max, i);
	    output_indent ("  {");
	    CB_REFERENCE (c)->subs = cons (cb_i[i], CB_REFERENCE (c)->subs);
	  }

	/* process output */
	if (output_initialize_one (p, c) != 0)
	  output_initialize_compound (p, c);

	if (f->flag_occurs)
	  {
	    /* close loop */
	    CB_REFERENCE (c)->subs = CB_CHAIN (CB_REFERENCE (c)->subs);
	    output_indent ("  }");
	  }
      }
}

static void
output_initialize (struct cb_initialize *p)
{
  if (output_initialize_one (p, p->var) != 0)
    output_initialize_compound (p, p->var);
}


/*
 * SEARCH
 */

static void
output_occurs (struct cb_field *p)
{
  if (p->occurs_depending)
    output_integer (p->occurs_depending);
  else
    output ("%d", p->occurs_max);
}

static void
output_search_whens (cb_tree table, cb_tree var, cb_tree stmt, cb_tree whens)
{
  cb_tree l;
  struct cb_field *p = cb_field (table);
  cb_tree idx = NULL;

  /* determine the index to use */
  if (var)
    for (l = p->index_list; l; l = CB_CHAIN (l))
      if (cb_ref (CB_VALUE (l)) == cb_ref (var))
	idx = var;
  if (!idx)
    idx = CB_VALUE (p->index_list);

  /* start loop */
  output_line ("while (1)");
  output_indent ("  {");

  /* end test */
  output_prefix ();
  output ("if (");
  output_integer (idx);
  output (" > ");
  output_occurs (p);
  output (")\n");
  output_indent ("  {");
  if (stmt)
    output_stmt (stmt);
  output_line ("break;");
  output_indent ("  }");

  /* WHEN test */
  output_stmt (whens);
  output_line ("else");
  output_indent ("  {");
  output_prefix ();
  output_integer (idx);
  output ("++;\n");
  if (var && var != idx)
    output_move (idx, var);
  output_line ("continue;");
  output_indent ("  }");
  output_line ("break;");
  output_indent ("  }");
}

static void
output_search_all (cb_tree table, cb_tree stmt, cb_tree cond, cb_tree when)
{
  struct cb_field *p = cb_field (table);
  cb_tree idx = CB_VALUE (p->index_list);

  /* header */
  output_indent ("{");
  output_line ("int ret;");
  output_line ("int head = %d - 1;", p->occurs_min);
  output_prefix ();
  output ("int tail = ");
  output_occurs (p);
  output (" + 1;\n");

  /* start loop */
  output_line ("while (1)");
  output_indent ("  {");

  /* end test */
  output_line ("if (head >= tail - 1)");
  output_indent ("  {");
  if (stmt)
    output_stmt (stmt);
  output_line ("break;");
  output_indent ("  }");

  /* next index */
  output_prefix ();
  output_integer (idx);
  output (" = (head + tail) / 2;\n");

  /* WHEN test */
  output ("if (");
  output_cond (cond, 1);
  output (")\n");
  output_indent_level += 2;
  output_stmt (when);
  output_indent_level -= 2;
  output_line ("else");
  output_indent ("  {");
  output_line ("if (ret < 0)");
  output_prefix ();
  output ("  head = ");
  output_integer (idx);
  output (";\n");
  output_line ("else");
  output_prefix ();
  output ("  tail = ");
  output_integer (idx);
  output (";\n");
  output_line ("continue;");
  output_indent ("  }");
  output_line ("break;");
  output_indent ("  }");
  output_indent ("}");
}

static void
output_search (struct cb_search *p)
{
  if (p->flag_all)
    output_search_all (p->table, p->end_stmt,
		       CB_IF (p->whens)->test, CB_IF (p->whens)->stmt1);
  else
    output_search_whens (p->table, p->var, p->end_stmt, p->whens);
}


/*
 * CALL
 */

static void
output_call (struct cb_call *p)
{
  int n;
  int dynamic_link = 1;
  cb_tree l;

  if (cb_flag_call_static && CB_LITERAL_P (p->name))
    dynamic_link = 0;

  /* local variables */
  output_indent ("{");
  if (dynamic_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++)
    {
      cb_tree x = CB_VALUE (l);
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_CONTENT:
	  output_prefix ();
	  output ("char content_%d[", n);
	  output_size (x);
	  output ("];\n");
	  break;
	case CB_CALL_BY_LENGTH:
	  output_prefix ();
	  output ("int length_%d = ", n);
	  output_size (x);
	  output (";\n");
	}
    }
  for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++)
    {
      cb_tree x = CB_VALUE (l);
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_CONTENT:
	  output_prefix ();
	  output ("memcpy (content_%d, ", n);
	  output_data (x);
	  output (", ");
	  output_size (x);
	  output (");\n");
	}
    }

  /* function name */
  output_prefix ();
  if (!dynamic_link)
    {
      /* static link */
      output ("cob_return_code = %s", CB_LITERAL (p->name)->data);
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      if (CB_LITERAL_P (p->name))
	output ("cob_resolve (\"%s\")", CB_LITERAL (p->name)->data);
      else
	output_funcall (cb_build_funcall_1 ("cob_call_resolve", p->name));
      output (";\n");
      output_line ("if (func == NULL)");
      output_indent_level += 2;
      output_stmt (p->stmt1);
      output_indent_level -= 2;
      output_line ("else");
      output_indent ("  {");
      output_prefix ();
      output ("cob_return_code = func");
    }

  /* arguments */
  output (" (");
  for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++)
    {
      cb_tree x = CB_VALUE (l);
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_REFERENCE:
	  output_data (x);
	  break;
	case CB_CALL_BY_CONTENT:
	  output ("content_%d", n);
	  break;
	case CB_CALL_BY_LENGTH:
	  output ("&length_%d", n);
	  break;
	case CB_CALL_BY_VALUE:
	  switch (CB_TREE_TAG (x))
	    {
	    case CB_TAG_LITERAL:
	      if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
		output ("%d", cb_literal_to_int (CB_LITERAL (x)));
	      else
		output ("%d", CB_LITERAL (x)->data[0]);
	      break;
	    default:
	      if (cb_field (x)->usage == CB_USAGE_BINARY
		  || cb_field (x)->usage == CB_USAGE_INDEX)
		{
		  output_integer (x);
		}
	      else
		{
		  output ("*");
		  output_data (x);
		}
	      break;
	    }
	  break;
	}
      if (CB_CHAIN (l))
	output (", ");
    }
  output (");\n");
  if (p->stmt2)
    output_stmt (p->stmt2);
  if (dynamic_link)
    output_indent ("  }");
  output_indent ("}");
}


/*
 * GO TO
 */

static void
output_goto_1 (cb_tree x)
{
  output_line ("goto lb_%s;", CB_LABEL (cb_ref (x))->cname);
}

static void
output_goto (struct cb_goto *p)
{
  if (p->depending)
    {
      int i = 1;
      cb_tree l;
      output_prefix ();
      output ("switch (");
      output_integer (p->depending);
      output (")\n");
      output_indent ("  {");
      for (l = p->target; l; l = CB_CHAIN (l))
	{
	  output_indent_level -= 2;
	  output_line ("case %d:", i++);
	  output_indent_level += 2;
	  output_goto_1 (CB_VALUE (l));
	}
      output_indent ("  }");
    }
  else if (p->target == NULL)
    output_line ("goto exit_program;");
  else
    output_goto_1 (p->target);
}


/*
 * PERFORM
 */

static void
output_perform_call (struct cb_label *lb, struct cb_label *le)
{
  static int id = 1;
  output_line ("/* PERFORM %s THRU %s */", lb->name, le->name);
  output_line ("frame_stack[++frame_index] = (struct frame) {%d, &&l_%d};",
	       le->id, id);
  output_line ("goto lb_%s;", lb->cname);
  output_line ("l_%d:", id++);
  output_line ("frame_index--;");
}

static void
output_perform_exit (struct cb_label *l)
{
  output_line ("if (frame_stack[frame_index].perform_through == %d)", l->id);
  output_line ("  goto *frame_stack[frame_index].return_address;");
}

static void
output_perform_once (struct cb_perform *p)
{
  if (p->body && CB_PAIR_P (p->body))
    output_perform_call (CB_LABEL (cb_ref (CB_PAIR_X (p->body))),
			 CB_LABEL (cb_ref (CB_PAIR_Y (p->body))));
  else
    output_stmt (p->body);
}

static void
output_perform_until (struct cb_perform *p, struct cb_perform_varying *v)
{
  if (!v)
    {
      /* perform body at the end */
      output_perform_once (p);
      return;
    }

  output_line ("while (1)");
  output_indent ("  {");

  if (v->next && v->next->name)
    output_move (v->next->from, v->next->name);

  if (p->test == CB_AFTER)
    output_perform_until (p, v->next);

  output_prefix ();
  output ("if (");
  output_cond (v->until, 0);
  output (")\n");
  output_line ("  break;");

  if (p->test == CB_BEFORE)
    output_perform_until (p, v->next);

  if (v->step)
    output_stmt (v->step);

  output_indent ("  }");
}

static void
output_perform (struct cb_perform *p)
{
  static int loop_counter = 0;

  switch (p->type)
    {
    case CB_PERFORM_EXIT:
      if (CB_LABEL (p->data)->need_return)
	output_perform_exit (CB_LABEL (p->data));
      break;
    case CB_PERFORM_ONCE:
      output_perform_once (p);
      break;
    case CB_PERFORM_TIMES:
      output_prefix ();
      output ("for (n[%d] = ", loop_counter);
      output_integer (p->data);
      output ("; n[%d] > 0; n[%d]--)\n", loop_counter, loop_counter);
      output_indent ("  {");
      output_perform_once (p);
      output_indent ("  }");
      loop_counter++;
      break;
    case CB_PERFORM_UNTIL:
      if (p->varying->name)
	output_move (p->varying->from, p->varying->name);
      output_perform_until (p, p->varying);
      break;
    }
}


/*
 * Output statement
 */

static void
output_stmt (cb_tree x)
{
  if (x == NULL)
    {
      output_line (";");
      return;
    }

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_STATEMENT:
      {
	static int last_line = 0;
	struct cb_statement *p = CB_STATEMENT (x);

	output_line ("/* %s */", p->name);
	if (x->source_file && last_line != x->source_line)
	  {
	    if (cb_flag_line_directive)
	      output ("#line %d \"%s\"\n", x->source_line, x->source_file);
	    if (cb_flag_source_location)
	      {
		output_line ("cob_source_file = \"%s\";", x->source_file);
		output_line ("cob_source_line = %d;", x->source_line);
	      }
	    last_line = x->source_line;
	  }

	if (p->handler1 || p->handler2
	    || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O)))
	  output_line ("cob_exception_code = 0;");

	if (p->body)
	  output_stmt (p->body);

	if (p->handler1 || p->handler2
	    || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O)))
	  {
	    int code = CB_EXCEPTION_CODE (p->handler_id);
	    if (p->handler1)
	      {
		if ((code & 0x00ff) == 0)
		  output_line ("if ((cob_exception_code & 0xff00) == 0x%04x)",
			       code);
		else
		  output_line ("if (cob_exception_code == 0x%04x)", code);
		output_stmt (p->handler1);
		if (p->handler2 || p->file)
		  output_line ("else");
	      }
	    if (p->file)
	      {
		output_line ("if (cob_exception_code)");
		output_indent ("  {");
		output_perform_call (CB_FILE (p->file)->handler,
				     CB_FILE (p->file)->handler);
		output_indent ("  }");
		if (p->handler2)
		  output_line ("else");
	      }
	    if (p->handler2)
	      {
		if (p->handler1 == 0 && p->file == 0)
		  output_line ("if (!cob_exception_code)");
		output_stmt (p->handler2);
	      }
	  }
	break;
      }
    case CB_TAG_LABEL:
      {
	struct cb_label *p = CB_LABEL (x);
	output_newline ();
	output_line ("/* %s: */", p->name);
	if (p->need_begin)
	  output_line ("lb_%s:", p->cname);
	break;
      }
    case CB_TAG_FUNCALL:
      {
	output_prefix ();
	output_funcall (x);
	output (";\n");
	break;
      }
    case CB_TAG_ASSIGN:
      {
	struct cb_assign *p = CB_ASSIGN (x);
	output_prefix ();
	output_integer (p->var);
	output (" = ");
	output_integer (p->val);
	output (";\n");
	break;
      }
    case CB_TAG_INITIALIZE:
      {
	output_initialize (CB_INITIALIZE (x));
	break;
      }
    case CB_TAG_SEARCH:
      {
	output_search (CB_SEARCH (x));
	break;
      }
    case CB_TAG_CALL:
      {
	output_call (CB_CALL (x));
	break;
      }
    case CB_TAG_GOTO:
      {
	output_goto (CB_GOTO (x));
	break;
      }
    case CB_TAG_IF:
      {
	struct cb_if *p = CB_IF (x);
	output_prefix ();
	output ("if (");
	output_cond (p->test, 0);
	output (")\n");
	if (p->stmt1)
	  {
	    output_indent_level += 2;
	    output_stmt (p->stmt1);
	    output_indent_level -= 2;
	  }
	else
	  output_line ("  /* nothing */;");
	if (p->stmt2)
	  {
	    output_line ("else");
	    output_indent_level += 2;
	    output_stmt (p->stmt2);
	    output_indent_level -= 2;
	  }
	break;
      }
    case CB_TAG_PERFORM:
      {
	output_perform (CB_PERFORM (x));
	break;
      }
    case CB_TAG_LIST:
      {
	output_indent ("{");
	for (; x; x = CB_CHAIN (x))
	  output_stmt (CB_VALUE (x));
	output_indent ("}");
	break;
      }
    default:
      abort ();
    }
}


/*
 * File definition
 */

static void
output_file_definition (struct cb_file *f)
{
  int nkeys = 1;

  /* output RELATIVE/RECORD KEY's */
  if (f->organization == COB_ORG_RELATIVE
      || f->organization == COB_ORG_INDEXED)
    {
      struct cb_alt_key *l;
      output ("static cob_file_key %s_keys[] = {\n", f->cname);
      output ("  {");
      output_param (f->key, -1);
      output (", 0},\n");
      for (l = f->alt_key_list; l; l = l->next)
	{
	  nkeys++;
	  output ("  {");
	  output_param (l->key, -1);
	  output (", %d},\n", l->duplicates);
	}
      output ("};\n");
    }

  /* output the file descriptor */
  output ("static cob_file %s = {", f->cname);
  /* organization, access_mode, open_mode, flag_optional */
  output ("%d, %d, 0, %d, ", f->organization, f->access_mode, f->optional);
  /* file_status */
  if (f->file_status)
    output_data (f->file_status);
  else
    output ("0");
  output (", ");
  /* assign */
  output_param (f->assign, -1);
  output (", ");
  /* record */
  output_param (CB_TREE (f->record), -1);
  output (", ");
  /* record_size */
  output_param (f->record_depending, -1);
  output (", ");
  /* record_min, record_max */
  output ("%d, %d, ", f->record_min, f->record_max);
  /* nkeys, keys */
  if (f->organization == COB_ORG_RELATIVE
      || f->organization == COB_ORG_INDEXED)
    output ("%d, %s_keys, ", nkeys, f->cname);
  else
    output ("0, 0, ");
  /* flags */
  output ("0, 0, 0, 0, 0, ");
  /* file */
  output ("0};\n\n");
}


/*
 * Screen definition
 */

static void
output_screen_definition (struct cb_field *p)
{
  int type = (p->children   ? COB_SCREEN_TYPE_GROUP :
	      p->values     ? COB_SCREEN_TYPE_VALUE :
	      (p->size > 0) ? COB_SCREEN_TYPE_FIELD :
			      COB_SCREEN_TYPE_ATTRIBUTE);

  if (p->sister)
    output_screen_definition (p->sister);
  if (p->children)
    output_screen_definition (p->children);

  output ("struct cob_screen s_%s = {%d, ", p->cname, type);

  output ("(union cob_screen_data) ");
  switch (type)
    {
    case COB_SCREEN_TYPE_GROUP:
      output ("&s_%s", p->children->cname);
      break;
    case COB_SCREEN_TYPE_VALUE:
      output_string (CB_LITERAL (CB_VALUE (p->values))->data,
		     CB_LITERAL (CB_VALUE (p->values))->size);
      break;
      break;
    case COB_SCREEN_TYPE_FIELD:
      output ("&f_%s", p->cname);
      break;
    case COB_SCREEN_TYPE_ATTRIBUTE:
      output ("0");
      break;
    }
  output (", ");

  if (p->sister)
    output ("&s_%s, ", p->sister->cname);
  else
    output ("0, ");
  if (p->screen_from)
    output ("&f_%s, ", p->screen_from->cname);
  else
    output ("0, ");
  if (p->screen_to)
    output ("&f_%s, ", p->screen_to->cname);
  else
    output ("0, ");
  output_integer (p->screen_line);
  output (", ");
  output_integer (p->screen_column);
  output (", %d};\n", p->screen_flag);
}


/*
 * Alphabet-name
 */

static int
literal_value (cb_tree x)
{
  if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
    return cb_literal_to_int (CB_LITERAL (x));
  else
    return CB_LITERAL (x)->data[0];
}

static void
output_alphabet_name_definition (struct cb_alphabet_name *p)
{
  int i, n = 0;
  int table[256];
  cb_tree l;

  /* reset by -1 */
  for (i = 0; i < 256; i++)
    table[i] = -1;

  for (l = p->custom_list; l; l = CB_CHAIN (l))
    {
      cb_tree x = CB_VALUE (l);
      if (CB_PAIR_P (x))
	{
	  /* X THRU Y */
	  int lower = literal_value (CB_PAIR_X (x));
	  int upper = literal_value (CB_PAIR_Y (x));
	  for (i = lower; i <= upper; i++)
	    table[i] = n++;
	}
      else if (CB_LIST_P (x))
	{
	  /* X ALSO Y ... */
	  cb_tree ls;
	  for (ls = x; ls; ls = CB_CHAIN (ls))
	    table[literal_value (CB_VALUE (ls))] = n;
	  n++;
	}
      else
	{
	  /* literal */
	  if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
	    table[cb_literal_to_int (CB_LITERAL (x))] = n++;
	  else
	    {
	      size_t size = CB_LITERAL (x)->size;
	      unsigned char *data = CB_LITERAL (x)->data;
	      for (i = 0; i < size; i++)
		table[data[i]] = n++;
	    }
	}
    }

  /* fill the rest of characters */
  for (i = 0; i < 256; i++)
    if (table[i] == -1)
      table[i] = n++;

  /* output the table */
  output ("static unsigned char %s[256] = {\n", p->cname);
  for (i = 0; i < 256; i++)
    {
      output (" %d,", table[i]);
      if (i % 16 == 15)
	output_newline ();
    }
  output ("};\n\n");
}


/*
 * Class definition
 */

static void
output_class_name_definition (struct cb_class_name *p)
{
  cb_tree l;

  output_line ("static int");
  output_line ("%s (cob_field *f)", p->cname);
  output_indent ("{");
  output_line ("int i;");
  output_line ("for (i = 0; i < f->size; i++)");
  output_prefix ();
  output ("  if (!(");
  for (l = p->list; l; l = CB_CHAIN (l))
    {
      cb_tree x = CB_VALUE (l);
      if (CB_PAIR_P (x))
	{
	  char lower = CB_LITERAL (CB_PAIR_X (x))->data[0];
	  char upper = CB_LITERAL (CB_PAIR_Y (x))->data[0];
	  output ("(%d <= f->data[i] && f->data[i] <= %d)", lower, upper);
	}
      else
	{
	  int i;
	  size_t size = CB_LITERAL (x)->size;
	  unsigned char *data = CB_LITERAL (x)->data;
	  for (i = 0; i < size; i++)
	    {
	      output ("f->data[i] == %d", data[i]);
	      if (i + 1 < size)
		output (" || ");
	    }
	}
      if (CB_CHAIN (l))
	output (" || ");
    }
  output ("))\n");
  output_line ("    return 0;");
  output_line ("return 1;");
  output_indent ("}");
  output_newline ();
}


static void
output_initial_values (struct cb_field *p)
{
  cb_tree def = cb_flag_auto_initialize ? cb_true : NULL;
  for (; p; p = p->sister)
    {
      cb_tree x = cb_build_field_reference (p, 0);
      output_stmt (cb_build_initialize (x, cb_true, NULL, def));
    }
}

static void
output_internal_function (struct cb_program *prog, cb_tree parameter_list)
{
  int i;
  cb_tree l;

  /* program function */
  output_line ("static int");
  output ("__%s (int entry", prog->program_id);
  for (l = parameter_list; l; l = CB_CHAIN (l))
    output (", unsigned char *b_%s", cb_field (CB_VALUE (l))->cname);
  output (")\n");
  output_indent ("{");

  /* local variables */
  output_line ("static int initialized = 0;");
  output_line ("static cob_decimal d[%d];", prog->decimal_index_max);
  output_line ("static cob_module module = {'%c', '%c', '%c', %s, 0};",
	       prog->decimal_point,
	       prog->currency_symbol,
	       prog->numeric_separator,
	       (prog->collating_sequence
		? CB_ALPHABET_NAME (cb_ref (prog->collating_sequence))->cname
		: "0"));
  output_newline ();
  output_line ("int i;");
  output_line ("int i1, i2, i3, i4, i5, i6, i7;");
  output_line ("int n[%d];", prog->loop_counter);
  output_line ("int frame_index;");
  output_line ("struct frame { int perform_through; void *return_address; } "
	       "frame_stack[24];");
  output_line ("cob_field f[4];");
  output_newline ();

  output ("#include \"%s\"\n\n", cb_storage_file_name);

  /* files */
  if (prog->file_list)
    {
      output ("/* Files */\n\n");
      for (l = prog->file_list; l; l = CB_CHAIN (l))
	output_file_definition (CB_FILE (CB_VALUE (l)));
      output_newline ();
    }

  /* screens */
  if (prog->screen_storage)
    {
      struct cb_field *f;
      output ("/* Screens */\n\n");
      for (f = prog->screen_storage; f; f = f->sister)
	output_screen_definition (f);
      output_newline ();
    }

  output_line ("cob_module_enter (&module);");
  output_newline ();

  /* initialization */
  output_line ("if (!initialized)");
  output_indent ("  {");
  output_line ("/* initialize decimal numbers */");
  output_line ("for (i = 0; i < %d; i++)", prog->decimal_index_max);
  output_line ("  cob_decimal_init (&d[i]);");
  output_newline ();
  if (!prog->flag_initial)
    output_initial_values (prog->working_storage);
  output_line ("initialized = 1;");
  output_indent ("  }");
  if (prog->flag_initial)
    output_initial_values (prog->working_storage);
  output_initial_values (prog->local_storage);
  output_newline ();

  output_line ("/* initialize frame stack */");
  output_line ("frame_index = 0;");
  output_line ("frame_stack[0].perform_through = -1;");
  output_newline ();

  /* entry dispatch */
  output_line ("switch (entry)");
  output_line ("  {");
  for (i = 0, l = prog->entry_list; l; l = CB_CHAIN (l))
    {
      output_line ("  case %d:", i++);
      output_line ("    goto lb_%s;", CB_LABEL (CB_PURPOSE (l))->cname);
    }
  output_line ("  }");
  output_newline ();

  /* error handlers */
  output_line ("/* error handlers */");
  output_stmt (CB_TREE (cb_standard_error_handler));
  output_line ("switch (cob_error_file->last_open_mode)");
  output_line ("  {");
  for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++)
    if (prog->file_handler[i])
      {
	output_line ("  case %d:", i);
	output ("    ");
	output_perform_call (prog->file_handler[i], prog->file_handler[i]);
	output_line ("    break;");
      }
  output_line ("  default:");
  output_line ("    cob_default_error_handle ();");
  output_line ("    break;");
  output_line ("  }");
  output_perform_exit (cb_standard_error_handler);
  output_newline ();

  /* PROCEDURE DIVISION */
  output_line ("/* PROCEDURE DIVISION */");
  for (l = prog->exec_list; l; l = CB_CHAIN (l))
    output_stmt (CB_VALUE (l));
  output_newline ();

  output_line ("exit_program:");
  output_line ("cob_module_leave (&module);");
  output_line ("return cob_return_code;");
  output_indent ("}");
  output_newline ();
}

static void
output_entry_function (struct cb_program *prog,
		       cb_tree entry,
		       cb_tree parameter_list)
{
  static int id = 0;

  const char *entry_name = CB_LABEL (CB_PURPOSE (entry))->name;
  cb_tree using_list = CB_VALUE (entry);
  cb_tree l, l1, l2;

  output ("int\n");
  output ("%s (", entry_name);
  if (!using_list)
    output ("void");
  else
    for (l = using_list; l; l = CB_CHAIN (l))
      {
	output ("unsigned char *b_%s", cb_field (CB_VALUE (l))->cname);
	if (CB_CHAIN (l))
	  output (", ");
      }
  output (")\n");
  output ("{\n");

  output ("  return __%s (%d", prog->program_id, id++);
  for (l1 = parameter_list; l1; l1 = CB_CHAIN (l1))
    {
      for (l2 = using_list; l2; l2 = CB_CHAIN (l2))
	if (strcmp (cb_field (CB_VALUE (l1))->cname,
		    cb_field (CB_VALUE (l2))->cname) == 0)
	  {
	    output (", b_%s", cb_field (CB_VALUE (l1))->cname);
	    break;
	  }
      if (l2 == NULL)
	output (", 0");
    }
  output (");\n");
  output ("}\n\n");
}

static void
output_main_function (struct cb_program *prog)
{
  output_line ("int");
  output_line ("main (int argc, char **argv)");
  output_indent ("{");
  output_line ("cob_init (argc, argv);");
  if (prog->flag_screen)
    output_line ("cob_screen_init ();");
  output_line ("%s ();", prog->program_id);
  if (prog->flag_screen)
    output_line ("cob_screen_clear ();");
  output_line ("return cob_return_code;");
  output_indent ("}");
}

void
codegen (struct cb_program *prog)
{
  cb_tree l;
  cb_tree parameter_list = NULL;

  if (cb_flag_main)
    prog->flag_initial = 1;

  output_target = yyout;

  output ("/* Generated from %s by cobc %s */\n\n",
	  cb_source_file, PACKAGE_VERSION);
  output ("#include <stdio.h>\n");
  output ("#include <stdlib.h>\n");
  output ("#include <string.h>\n");
  output ("#include <libcob.h>\n\n");

  /* fields */
  output ("/* Fields */\n\n");
  output ("#define i_RETURN_CODE    cob_return_code\n");
  output ("#define i_LINAGE_COUNTER cob_linage_counter\n\n");
  for (l = prog->index_list; l; l = CB_CHAIN (l))
    output ("static int i_%s;\n", CB_FIELD (CB_VALUE (l))->cname);
  output_newline ();

  /* alphabet-names */
  for (l = prog->alphabet_name_list; l; l = CB_CHAIN (l))
    output_alphabet_name_definition (CB_ALPHABET_NAME (CB_VALUE (l)));

  /* class-names */
  for (l = prog->class_name_list; l; l = CB_CHAIN (l))
    output_class_name_definition (CB_CLASS_NAME (CB_VALUE (l)));

  /* build parameter list */
  for (l = prog->entry_list; l; l = CB_CHAIN (l))
    {
      cb_tree using_list = CB_VALUE (l);
      cb_tree l1, l2;
      for (l1 = using_list; l1; l1 = CB_CHAIN (l1))
	{
	  for (l2 = parameter_list; l2; l2 = CB_CHAIN (l2))
	    if (strcmp (cb_field (CB_VALUE (l1))->cname,
			cb_field (CB_VALUE (l2))->cname) == 0)
	      break;
	  if (l2 == NULL)
	    parameter_list = list_add (parameter_list, CB_VALUE (l1));
	}
    }

  /* internal function */
  output_internal_function (prog, parameter_list);

  /* entry functions */
  for (l = prog->entry_list; l; l = CB_CHAIN (l))
    output_entry_function (prog, l, parameter_list);

  /* main function */
  if (cb_flag_main)
    output_main_function (prog);
}
