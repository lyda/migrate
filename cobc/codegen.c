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
static void output_data (cb_tree x);
static void output_integer (cb_tree x);
static void output_index (cb_tree x);
static void output_func_1 (const char *name, cb_tree a1);


/*
 * Output routine
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
output_quoted_string (unsigned char *s, int size)
{
  int i;
  output ("\"");
  for (i = 0; i < size; i++)
    {
      int c = s[i];
      if (c == '\"' || c == '\\')
	output ("\\%c", c);
      else if (isprint (c))
	output ("%c", c);
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
 * Output field
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
	  output_quoted_string (l->data, l->size);
	break;
      }
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	int i = f->indexes;

	/* base address */
	output_base (f);

	/* subscripts */
	for (; f; f = f->parent)
	  if (f->flag_occurs)
	    output (" + %d * i%d", f->size, i--);
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
	    cb_tree l = r->subs = list_reverse (r->subs);

	    for (; f; f = f->parent)
	      if (f->flag_occurs)
		{
		  output (" + %d * ", f->size);
		  output_index (CB_VALUE (l));
		  l = CB_CHAIN (l);
		}

	    r->subs = list_reverse (r->subs);
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
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	output ("%d", f->size);
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
		output ("%d + %d * ", p->offset - f->offset, p->size);
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
  output_storage ("{%d, %d, %d, %d, ", type, digits, expt, flags);
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
	    output ("&a_%d", lookup_attr (COB_TYPE_NUMERIC_DISPLAY,
					  l->size, l->expt, flags, 0));
	  }
	else
	  {
	    if (l->all)
	      output ("&cob_all_attr");
	    else
	      output ("&cob_alnum_attr");
	  }
	break;
      }
    case CB_TAG_FIELD:
      {
	int type = tree_type (x);
	struct cb_field *f = CB_FIELD (x);

	switch (type)
	  {
	  case COB_TYPE_GROUP:
	    output ("&cob_group_attr");
	    break;
	  case COB_TYPE_ALPHANUMERIC:
	    if (f->flag_justified)
	      output ("&cob_just_attr");
	    else
	      output ("&cob_alnum_attr");
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

	      output ("&a_%d", lookup_attr (type, f->pic->digits, f->pic->expt,
					    flags, f->pic->str));
	      break;
	    }
	  }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	if (r->offset)
	  output ("&cob_alnum_attr");
	else
	  output_attr (r->value);
	break;
      }
    default:
      abort ();
    }
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
      output ("\"%s\"", CB_STRING (x)->str);
      break;
    case CB_TAG_CAST_INTEGER:
      output_integer (CB_CAST_INTEGER (x)->val);
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
      {
	struct cb_field *f = CB_FIELD (x);
	if (f->indexes > 0)
	  {
	    output ("(%s = (cob_field) ", fname);
	    output_field (x);
	    output (", &%s)", fname);
	  }
	else
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
	  }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f;

	if (!CB_FIELD_P (r->value)
	    || (!r->subs && !r->offset && !cb_field_varying (cb_field (x))))
	  {
	    output_param (r->value, id);
	    return;
	  }

	f = CB_FIELD (r->value);
	output_line ("/* %s */", cb_name (x));
	output ("({");

	/* subscript check */
	if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT) && r->subs)
	  {
	    struct cb_field *p;
	    cb_tree l = r->subs = list_reverse (r->subs);

	    for (p = f; p; p = p->parent)
	      if (p->flag_occurs)
		{
		  cb_tree x = CB_VALUE (l);
		  if (p->occurs_depending)
		    {
		      int n = p->occurs_max;
		      if (CB_LITERAL_P (x))
			n = cb_literal_to_int (CB_LITERAL (x));
		      if (p->occurs_min <= n && n <= p->occurs_max)
			{
			  output_prefix ();
			  output ("cob_check_subscript_depending (");
			  output_integer (x);
			  output (", %d, %d, ", p->occurs_min, p->occurs_max);
			  output_integer (p->occurs_depending);
			  output (", \"%s\", \"%s\");\n", p->name,
				  cb_field (p->occurs_depending)->name);
			}
		    }
		  else
		    {
		      if (!CB_LITERAL_P (x))
			{
			  output_prefix ();
			  output ("cob_check_subscript (");
			  output_integer (x);
			  output (", %d, \"%s\");\n", p->occurs_max, p->name);
			}
		    }
		  l = CB_CHAIN (l);
		}

	    r->subs = list_reverse (r->subs);
	  }

	/* reference modifier check */
	if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_REF_MOD) && r->offset)
	  {
	    if (!CB_LITERAL_P (r->offset)
		|| (r->length && !CB_LITERAL_P (r->length)))
	      {
		output ("cob_check_ref_mod (");
		output_integer (r->offset);
		output (", ");
		if (r->length)
		  output_integer (r->length);
		else
		  output ("1");
		output (", %d, \"%s\");\n", f->size, f->name);
	      }
	  }

	output ("%s = (cob_field) ", fname);
	output_field (x);
	output ("; &%s; })", fname);
	break;
      }
    default:
      abort ();
    }
}

static void
output_func_1 (const char *name, cb_tree a1)
{
  output ("%s (", name);
  output_param (a1, param_id);
  output (")");
}


/*
 * Convert to integer
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
    default:
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
    }
}

static void
output_index (cb_tree x)
{
  output ("(");
  output_integer (x);
  output (" - 1)");
}


/*
 * Output condition
 */

static void
output_cond (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      {
	output ("%s", CB_CONST (x)->val);
	break;
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	switch (p->op)
	  {
	  case '!':
	    output ("!");
	    output_cond (p->x);
	    break;

	  case '&': case '|':
	    output ("(");
	    output_cond (p->x);
	    output (p->op == '&' ? " && " : " || ");
	    output_cond (p->y);
	    output (")");
	    break;

	  case '=': case '<': case '[': case '>': case ']': case '~':
	    output ("(");
	    if (CB_INDEX_P (p->x) || CB_INDEX_P (p->y))
	      {
		output_integer (p->x);
		switch (p->op)
		  {
		  case '=': output (" == "); break;
		  case '<': output (" < "); break;
		  case '[': output (" <= "); break;
		  case '>': output (" > "); break;
		  case ']': output (" >= "); break;
		  case '~': output (" != "); break;
		  }
		output_integer (p->y);
	      }
	    else
	      {
		output ("(");
		if (CB_TREE_TAG (p->x) != CB_TAG_SEQUENCE)
		  output ("{");
		output_stmt (p->x);
		if (CB_TREE_TAG (p->x) != CB_TAG_SEQUENCE)
		  output ("}");
		output (")");
		switch (p->op)
		  {
		  case '=': output (" == 0"); break;
		  case '<': output (" <  0"); break;
		  case '[': output (" <= 0"); break;
		  case '>': output (" >  0"); break;
		  case ']': output (" >= 0"); break;
		  case '~': output (" != 0"); break;
		  }
	      }
	    output (")");
	    break;

	  default:
	    abort ();
	  }
	break;
      }
    case CB_TAG_FUNCALL:
      {
	struct cb_funcall *p = CB_FUNCALL (x);
	output_func_1 (p->name, p->argv[0]);
	break;
      }
    default:
      abort ();
    }
}


/*
 * Recursion
 */

static void
output_recursive (void (*func) (struct cb_field *), struct cb_field *f)
{
  if (f->level != 01 && f->level != 77 && f->redefines)
    return;

  if (f->flag_occurs)
    {
      /* begin occurs loop */
      int i = f->indexes;
      output_indent ("{");
      output_line ("int i%d;", i);
      output_line ("for (i%d = 0; i%d < %d; i%d++)", i, i, f->occurs_max, i);
      output_indent ("  {");
    }

  /* process output */
  func (f);

  if (f->flag_occurs)
    {
      /* close loop */
      output_indent ("  }");
      output_indent ("}");
    }
}


/*
 * Exception handler
 */

static void output_perform_call (struct cb_label *lb, struct cb_label *le);

static void
output_handler (int id, cb_tree st1, cb_tree st2, struct cb_label *l)
{
  int code = CB_EXCEPTION_CODE (id);
  if (st1)
    {
      if ((code & 0x00ff) == 0)
	output_line ("if ((cob_exception_code & 0xff00) == 0x%04x)", code);
      else
	output_line ("if (cob_exception_code == 0x%04x)", code);
      output_stmt (st1);
      if (st2 || l)
	output_line ("else");
    }
  if (l)
    {
      output_line ("if (cob_exception_code)");
      output_indent ("  {");
      output_perform_call (l, l);
      output_indent ("  }");
      if (st2)
	output_line ("else");
    }
  if (st2)
    {
      if (st1 == 0 && l == 0)
	output_line ("if (!cob_exception_code)");
      output_stmt (st2);
    }
}


/*
 * Inline functions
 */

static void
output_memcmp (cb_tree x, cb_tree y)
{
  size_t size = cb_field_size (x);
  unsigned char buff[size];
  struct cb_literal *l = CB_LITERAL (y);

  memset (buff, ' ', size);
  memcpy (buff, l->data, l->size);

  output_prefix ();
  output ("cob_cmp_result = memcmp (");
  output_data (x);
  output (", ");
  output_quoted_string (buff, size);
  output (", %d);\n", size);
}

static void
output_memset (cb_tree x, char c)
{
  output_prefix ();
  output ("memset (");
  output_data (x);
  output (", ");
  output (isprint (c) ? "'%c'" : "%d", c);
  output (", ");
  output_size (x);
  output (");\n");
}

static void
output_memcpy (cb_tree x, char *s)
{
  output_prefix ();
  output ("memcpy (");
  output_data (x);
  output (", ");
  output_quoted_string (s, cb_field (x)->size);
  output (", ");
  output_size (x);
  output (");\n");
}

static void
output_native_assign (cb_tree x, int val)
{
  output_prefix ();
  output_integer (x);
  output (" = %d;\n", val);
}


/*
 * GO TO
 */

static void
output_goto (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_label *l = CB_LABEL (r->value);
  output_line ("goto lb_%s;", l->cname);
}

static void
output_goto_depending (cb_tree labels, cb_tree index)
{
  int i = 1;
  cb_tree l;
  output_prefix ();
  output ("switch (");
  output_integer (index);
  output (")\n");
  output_indent ("  {");
  for (l = labels; l; l = CB_CHAIN (l))
    {
      output_indent_level -= 2;
      output_line ("case %d:", i++);
      output_indent_level += 2;
      output_goto (CB_VALUE (l));
    }
  output_indent ("  }");
}

static void
output_exit_program (void)
{
  output_line ("goto exit_program;");
}


/*
 * MOVE
 */

static void
output_move_call (cb_tree src, cb_tree dst)
{
  output_stmt (cb_build_funcall_2 ("cob_move", src, dst));
}

static void
output_move_num (cb_tree x, int high)
{
  switch (cb_field (x)->usage)
    {
    case CB_USAGE_BINARY:
      output_native_assign (x, high ? -1 : 0);
      break;
    case CB_USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0');
      break;
    case CB_USAGE_PACKED:
      output_memset (x, high ? 0x99 : 0x00);
      break;
    default:
      abort ();
    }
}

static void
output_move_space (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      output_memset (x, ' ');
      break;
    default:
      output_move_call (cb_space, x);
      break;
    }
}

static void
output_move_zero (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      if (cb_field (x)->flag_blank_zero)
	output_move_space (x);
      else
	output_move_num (x, 0);
      break;
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      output_memset (x, '0');
      break;
    default:
      output_move_call (cb_zero, x);
      break;
    }
}

static void
output_move_high (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      output_move_num (x, 9);
      break;
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      output_memset (x, 255);
      break;
    default:
      output_move_call (cb_high, x);
      break;
    }
}

static void
output_move_low (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
      output_move_num (x, 0);
      break;
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      output_memset (x, 0);
      break;
    default:
      output_move_call (cb_low, x);
      break;
    }
}

static void
output_move_quote (cb_tree x)
{
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC:
    case CB_CATEGORY_ALPHABETIC:
    case CB_CATEGORY_ALPHANUMERIC:
      output_memset (x, '"');
      break;
    default:
      output_move_call (cb_quote, x);
      break;
    }
}

static void
output_move_literal (cb_tree src, cb_tree dst)
{
  struct cb_literal *l = CB_LITERAL (src);
  struct cb_field *f = cb_field (dst);

  if (l->all)
    {
      int i;
      unsigned char buff[f->size];
      for (i = 0; i < f->size; i++)
	buff[i] = l->data[i % l->size];
      output_memcpy (dst, buff);
    }
  else if (f->usage == CB_USAGE_BINARY && cb_fits_int (src))
    {
      int val = cb_literal_to_int (l);
      int n = l->expt - f->pic->expt;
      for (; n > 0; n--) val *= 10;
      for (; n < 0; n++) val /= 10;
      output_native_assign (dst, val);
    }
  else
    {
      output_move_call (src, dst);
    }
}

static void
output_move_index (cb_tree src, cb_tree dst)
{
  output_prefix ();
  output_integer (dst);
  output (" = ");
  output_integer (src);
  output (";\n");
}

static void
output_move (cb_tree src, cb_tree dst)
{
  if (CB_INDEX_P (dst))
    return output_move_index (src, dst);

  if (CB_INDEX_P (src))
    return output_stmt (cb_build_funcall_2 ("cob_set_int", dst,
					    cb_build_cast_integer (src)));

  if (cb_flag_inline_move)
    {
      if (src == cb_zero)
	return output_move_zero (dst);
      else if (src == cb_space)
	return output_move_space (dst);
      else if (src == cb_high)
	return output_move_high (dst);
      else if (src == cb_low)
	return output_move_low (dst);
      else if (src == cb_quote)
	return output_move_quote (dst);
      else if (CB_LITERAL_P (src))
	return output_move_literal (src, dst);
      else
	{
	  int simple_copy = 0;
	  int src_size = cb_field_size (src);
	  int dst_size = cb_field_size (dst);
	  struct cb_field *src_f = cb_field (src);
	  struct cb_field *dst_f = cb_field (dst);

	  if (src_size > 0 && dst_size > 0 && src_size >= dst_size)
	    switch (CB_TREE_CATEGORY (src))
	      {
	      case CB_CATEGORY_ALPHABETIC:
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC
		    || CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
		  if (dst_f->flag_justified == 0)
		    simple_copy = 1;
		break;
	      case CB_CATEGORY_ALPHANUMERIC:
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
		  if (dst_f->flag_justified == 0)
		    simple_copy = 1;
		break;
	      case CB_CATEGORY_NUMERIC:
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
		    && src_f->usage == CB_USAGE_DISPLAY
		    && dst_f->usage == CB_USAGE_DISPLAY
		    && src_f->pic->size == dst_f->pic->size
		    && src_f->pic->digits == dst_f->pic->digits
		    && src_f->pic->expt == dst_f->pic->expt
		    && src_f->pic->have_sign == dst_f->pic->have_sign
		    && src_f->flag_sign_leading == dst_f->flag_sign_leading
		    && src_f->flag_sign_separate == dst_f->flag_sign_separate)
		  simple_copy = 1;
		else if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC
			 && src_f->pic->have_sign == 0)
		  simple_copy = 1;
		break;
	      default:
		break;
	      }
	  if (simple_copy)
	    {
	      output_prefix ();
	      output ("memcpy (");
	      output_data (dst);
	      output (", ");
	      output_data (src);
	      output (", ");
	      output_size (dst);
	      output (");\n");
	      return;
	    }
	}
    }

  return output_move_call (src, dst);
}


/*
 * INITIALIZE
 */

static cb_tree initialize_replacing_list;

static void output_initialize_internal (struct cb_field *f);

static int
field_uniform_type (struct cb_field *f)
{
  if (f->children)
    {
      int type = field_uniform_type (f->children);
      for (f = f->children->sister; f; f = f->sister)
	if (!f->redefines)
	  if (type != field_uniform_type (f))
	    return COB_TYPE_UNKNOWN;
      return type;
    }
  else
    {
      switch (tree_type (CB_TREE (f)))
	{
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_PACKED:
	  return COB_TYPE_NUMERIC_BINARY;
	case COB_TYPE_NUMERIC_DISPLAY:
	  return COB_TYPE_NUMERIC_DISPLAY;
	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
	  return COB_TYPE_ALPHANUMERIC;
	default:
	  return COB_TYPE_UNKNOWN;
	}
    }
}

static void
output_initialize_uniform (struct cb_field *f, int type, int size, int flag)
{
  if (flag && f->flag_occurs)
    {
      output_indent ("{");
      output_line ("int i%d = 0;", f->indexes);
    }

  output_prefix ();
  output ("memset (");
  output_data (CB_TREE (f));
  output (", ");
  switch (type)
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      output ("'0'");
      break;
    case COB_TYPE_NUMERIC_BINARY:
      output ("0");
      break;
    case COB_TYPE_ALPHANUMERIC:
      output ("' '");
      break;
    }
  output (", %d);\n", size);

  if (flag && f->flag_occurs)
    {
      output_indent ("}");
    }
}

static void
output_initialize_compound (struct cb_field *f)
{
  cb_tree x = CB_TREE (f);
  switch (CB_TREE_CATEGORY (x))
    {
    case CB_CATEGORY_NUMERIC_EDITED:
      output_move (cb_zero, x);
      break;
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
    case CB_CATEGORY_NATIONAL_EDITED:
      output_move (cb_space, x);
      break;
    default:
      output_initialize_internal (f);
      break;
    }
}

static void
output_initialize_internal (struct cb_field *f)
{
  int last_type = COB_TYPE_UNKNOWN;
  struct cb_field *p;
  struct cb_field *first_field = NULL;

  /* initialize all children, combining uniform sequence into one */
  for (p = f->children; p; p = p->sister)
    {
      /* check if this child is uniform */
      int type = field_uniform_type (p);
      if (type == COB_TYPE_UNKNOWN || type != last_type)
	{
	  /* if not, or if this child's category is different from
	     the previous one, initialize the last uniform sequence */
	  if (first_field && last_type != COB_TYPE_ALPHANUMERIC)
	    output_initialize_uniform (first_field, last_type,
				       p->offset - first_field->offset, 1);
	  /* if not uniform, initialize the children */
	  if (type == COB_TYPE_UNKNOWN)
	    output_recursive (output_initialize_compound, p);
	  last_type = type;
	  first_field = (type != COB_TYPE_UNKNOWN) ? p : NULL;
	}
    }
  /* initialize the final uniform sequence */
  if (first_field && last_type != COB_TYPE_ALPHANUMERIC)
    output_initialize_uniform (first_field, last_type,
			       f->offset + f->size - first_field->offset, 1);
}

static void
output_initialize_replacing (struct cb_field *f)
{
  if (f->children)
    {
      for (f = f->children; f; f = f->sister)
	output_recursive (output_initialize_replacing, f);
    }
  else
    {
      cb_tree l;
      for (l = initialize_replacing_list; l; l = CB_CHAIN (l))
	if (CB_PURPOSE_INT (l) == f->pic->category)
	  {
	    output_move (CB_VALUE (l), CB_TREE (f));
	    break;
	  }
    }
}

static void
output_initialize (cb_tree x, cb_tree l)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f = CB_FIELD (r->value);

  /* output fixed indexes */
  if (r->subs)
    {
      int i = 1;
      cb_tree subs;
      output_indent ("{");
      for (subs = r->subs; subs; subs = CB_CHAIN (subs))
	{
	  /* FIXME: need boundary check */
	  output_prefix ();
	  output ("int i%d = ", i++);
	  output_index (CB_VALUE (subs));
	  output (";\n");
	}
    }

  if (l != NULL)
    {
      /* INITIALIZE REPLACING */
      initialize_replacing_list = l;
      output_initialize_replacing (f);
    }
  else
    {
      /* INITIALIZE */
      int type = field_uniform_type (f);
      if (type != COB_TYPE_UNKNOWN)
	{
	  /* if field is uniform (i.e., all children are
	     in the same category), initialize it at once */
	  output_initialize_uniform (f, type, f->size, 0);
	}
      else
	{
	  /* otherwise, fill the field by spaces first */
	  output_initialize_uniform (f, COB_TYPE_ALPHANUMERIC, f->size, 0);
	  /* then initialize the children recursively */
	  output_initialize_compound (f);
	}
    }

  if (r->subs)
    output_indent ("}");
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
output_search (cb_tree table, cb_tree var, cb_tree stmt, cb_tree whens)
{
  cb_tree l;
  struct cb_field *p = cb_field (table);
  cb_tree idx = NULL;

  /* determine the index to use */
  var = var ? CB_TREE (cb_field (var)) : NULL;
  for (l = p->index_list; l; l = CB_CHAIN (l))
    if (CB_VALUE (l) == var)
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
output_search_all (cb_tree table, cb_tree stmt, cb_tree when)
{
  struct cb_field *p = cb_field (table);
  cb_tree idx = CB_TREE (CB_VALUE (p->index_list));

  /* header */
  output_indent ("{");
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
  output_stmt (when);
  output_line ("else");
  output_indent ("  {");
  output_line ("if (cob_cmp_result < 0)");
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


/*
 * SORT
 */

static void
output_sort_init (cb_tree file, cb_tree keys)
{
  cb_tree l;

  output_indent ("{");
  output_line ("static cob_file_key keys[] = {");
  for (l = keys; l; l = CB_CHAIN (l))
    {
      output_prefix ();
      output ("  {");
      output_param (CB_VALUE (l), -1);
      output (", %d},\n", CB_PURPOSE_INT (l));
    }
  output_line ("};");
  output_prefix ();
  output ("cob_sort_init (");
  output_param (file, 0);
  output (", %d, keys);\n", list_length (keys));
  output_indent ("}");
}


/*
 * CALL
 */

static void
output_call (cb_tree name, cb_tree args, cb_tree st1, cb_tree st2)
{
  int n;
  int dynamic_link = 1;
  cb_tree l;

  if (cb_flag_call_static && CB_LITERAL_P (name))
    dynamic_link = 0;

  /* local variables */
  output_indent ("{");
  if (dynamic_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = args, n = 1; l; l = CB_CHAIN (l), n++)
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
  for (l = args, n = 1; l; l = CB_CHAIN (l), n++)
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
      output ("cob_return_code = %s", CB_LITERAL (name)->data);
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      if (CB_LITERAL_P (name))
	output ("cob_resolve (\"%s\")", CB_LITERAL (name)->data);
      else
	output_func_1 ("cob_call_resolve", name);
      output (";\n");
      output_line ("if (func == NULL)");
      output_indent_level += 2;
      output_stmt (st1);
      output_indent_level -= 2;
      output_line ("else");
      output_indent ("  {");
      output_prefix ();
      output ("cob_return_code = func");
    }

  /* arguments */
  output (" (");
  for (l = args, n = 1; l; l = CB_CHAIN (l), n++)
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
  if (st2)
    output_stmt (st2);
  if (dynamic_link)
    output_indent ("  }");
  output_indent ("}");
}


/*
 * Function call
 */

static struct inline_func {
  const char *name;
  void (*func) ();
} inline_table[] = {
  {"@handler", output_handler},
  {"@memcmp", output_memcmp},
  {"@goto", output_goto},
  {"@goto-depending", output_goto_depending},
  {"@exit-program", output_exit_program},
  {"@move", output_move},
  {"@initialize", output_initialize},
  {"@search", output_search},
  {"@search-all", output_search_all},
  {"@sort-init", output_sort_init},
  {"@call", output_call},
  {0, 0}
};

static void
output_funcall (struct cb_funcall *p)
{
  if (p->name[0] == '@')
    {
      /* inline function */
      int i;
      for (i = 0; inline_table[i].name; i++)
	if (strcmp (p->name, inline_table[i].name) == 0)
	  {
	    void (*func) () = inline_table[i].func;
	    switch (p->argc)
	      {
	      case 0: func (); break;
	      case 1: func (p->argv[0]); break;
	      case 2: func (p->argv[0], p->argv[1]); break;
	      case 3: func (p->argv[0], p->argv[1], p->argv[2]); break;
	      case 4: func (p->argv[0], p->argv[1], p->argv[2], p->argv[3]); break;
	      }
	    break;
	  }
    }
  else
    {
      /* regular function call */
      int i;
      output_prefix ();
      output ("%s (", p->name);
      for (i = 0; i < p->argc; i++)
	{
	  output_param (p->argv[i], i);
	  if (i + 1 < p->argc)
	    output (", ");
	}
      output (");\n");
    }
}


/*
 * PERFORM
 */

static void
output_perform_call (struct cb_label *lb, struct cb_label *le)
{
  static int id = 1;
  output_line ("/* PERFORM %s THRU %s */", lb->name, le->name);
  output_line ("frame_stack[++frame_index] = (struct frame) {le_%s, &&l_%d};",
	       le->cname, id);
  output_line ("goto lb_%s;", lb->cname);
  output_line ("l_%d:", id++);
  output_line ("frame_index--;");
}

static void
output_perform_exit (struct cb_label *l)
{
  output_line ("if (frame_stack[frame_index].perform_through == le_%s)",
	       l->cname);
  output_line ("  goto *frame_stack[frame_index].return_address;");
}

static void
output_perform_once (struct cb_perform *p)
{
  if (CB_PAIR_P (p->body))
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
  output_cond (v->until);
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
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_STATEMENT:
      {
	static int last_line = 0;
	if (x->source_file && last_line != x->source_line)
	  {
	    struct cb_statement *p = CB_STATEMENT (x);
	    if (cb_flag_line_directive)
	      output ("#line %d \"%s\"\n", x->source_line, x->source_file);
	    output_line ("/* %s */;", p->name);
	    if (cb_flag_source_location)
	      {
		output_line ("cob_source_file = \"%s\";", x->source_file);
		output_line ("cob_source_line = %d;", x->source_line);
	      }
	    last_line = x->source_line;
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
	output_funcall (CB_FUNCALL (x));
	break;
      }
    case CB_TAG_IF:
      {
	struct cb_if *p = CB_IF (x);
	output_prefix ();
	output ("if (");
	output_cond (p->test);
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
    case CB_TAG_SEQUENCE:
      {
	struct cb_sequence *p = CB_SEQUENCE (x);
	cb_tree l = p->list;
	output_indent ("{");
	if (p->save_status && l && CB_CHAIN (l))
	  {
	    /* output with combining multiple cob_exception_code */
	    output_line ("int code = 0;");
	    for (; l; l = CB_CHAIN (l))
	      {
		output_stmt (CB_VALUE (l));
		output_line ("code |= cob_exception_code;");
	      }
	    output_line ("cob_exception_code = code;");
	  }
	else
	  {
	    /* output without using cob_exception_code */
	    for (; l; l = CB_CHAIN (l))
	      output_stmt (CB_VALUE (l));
	  }
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
      output_quoted_string (CB_LITERAL (CB_VALUE (p->values))->data,
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
      if (CB_PAIR_P (x) && CB_PAIR_X (x))
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


/*
 * Initial value
 */

/* return 1 if any child has VALUE clause */
static int
have_value (struct cb_field *p)
{
  if (p->values)
    return 1;
  for (p = p->children; p; p = p->sister)
    if (have_value (p))
      return 1;
  return 0;
}

static void
output_value (struct cb_field *f)
{
  if (f->values)
    {
      cb_tree value = CB_VALUE (f->values);
      if (CB_CONST_P (value)
	  || CB_TREE_CLASS (value) == CB_CLASS_NUMERIC
	  || CB_LITERAL (value)->all)
	{
	  /* figurative literal, numeric literal, or ALL literal */
	  output_move (value, CB_TREE (f));
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
	  output_memcpy (CB_TREE (f), buff);
	}
    }
  else
    {
      for (f = f->children; f; f = f->sister)
	if (have_value (f))
	  output_recursive (output_value, f);
    }
}

static void
output_init_values (struct cb_field *p)
{
  for (; p; p = p->sister)
    if (have_value (p))
      output_recursive (output_value, p);
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
    output_init_values (prog->working_storage);
  output_line ("initialized = 1;");
  output_indent ("  }");
  if (prog->flag_initial)
    output_init_values (prog->working_storage);
  output_init_values (prog->local_storage);
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
  output ("#define i_SWITCH         cob_switch\n");
  output ("#define i_RETURN_CODE    cob_return_code\n");
  output ("#define i_LINAGE_COUNTER cob_linage_counter\n\n");
  for (l = prog->index_list; l; l = CB_CHAIN (l))
    output ("static int i_%s;\n", CB_FIELD (CB_VALUE (l))->cname);
  output_newline ();

  /* labels */
  output ("/* Labels */\n\n");
  output ("enum {\n");
  output ("  le_standard_error_handler,\n");
  for (l = prog->exec_list; l; l = CB_CHAIN (l))
    if (CB_LABEL_P (CB_VALUE (l)) && CB_LABEL (CB_VALUE (l))->need_return)
      output ("  le_%s,\n", CB_LABEL (CB_VALUE (l))->cname);
  output ("};\n\n");

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
