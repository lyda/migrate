/*
 * Copyright (C) 2002 Keisuke Nishida
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
#include "scanner.h"
#include "codegen.h"

static void output_stmt (cobc_tree x);
static void output_data (cobc_tree x);
static void output_func_1 (const char *name, cobc_tree a1);

static char *
field_name (cobc_tree x)
{
  static char name[512];

  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (x);
	sprintf (name, "f[%d]", l->id - 1);
	break;
      }
    case cobc_tag_reference:
      {
	struct cobc_reference *r = COBC_REFERENCE (x);
	if (r->id > 0)
	  sprintf (name, "f[%d]", r->id - 1);
	else
	  field_name (r->value);
	break;
      }
    case cobc_tag_field:
      {
	struct cobc_field *f = COBC_FIELD (x);
	if (f->f.screen)
	  sprintf (name, "s_%s", f->cname);
	else
	  sprintf (name, "f_%s", f->cname);
	break;
      }
    case cobc_tag_file:
      {
	struct cobc_file *f = COBC_FILE (x);
	sprintf (name, "%s", f->cname);
	break;
      }
    }

  return name;
}

static char
get_type (struct cobc_field *p)
{
  switch (p->usage)
    {
    case COBC_USAGE_BINARY:
    case COBC_USAGE_INDEX:
      return COB_TYPE_NUMERIC_BINARY;
    default:
      return p->pic->category;
    }
}


/*
 * Output routine
 */

static int output_indent_level = 0;

static void
output (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (cobc_out, fmt, ap);
  va_end (ap);
}

static void
output_newline (void)
{
  fputs ("\n", cobc_out);
}

static void
output_prefix (void)
{
  int i;
  for (i = 0; i < output_indent_level; i++)
    fputc (' ', cobc_out);
}

static void
output_line (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  output_prefix ();
  vfprintf (cobc_out, fmt, ap);
  fputc ('\n', cobc_out);
  va_end (ap);
}

static void
output_indent (char *str)
{
  char *p;
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
output_line_directive (cobc_tree x)
{
  static int last_line = 0;
  if (x->loc.text && last_line != x->loc.first_line)
    {
      if (cobc_flags.line_directive)
	output ("#line %d \"%s\"\n", x->loc.first_line, x->loc.text);
      if (cobc_flags.source_location)
	output_line ("cob_source_line = %d;", x->loc.first_line);
      last_line = x->loc.first_line;
    }
}


/*
 * Convert to int32
 */

static void
output_int32 (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      if (x == cobc_zero)
	output ("0");
      else
	output ("%s", COBC_CONST (x)->val);
      break;
    case cobc_tag_integer:
      output ("%d", COBC_INTEGER (x)->val);
      break;
    case cobc_tag_literal:
      output ("%d", (int) literal_to_int (COBC_LITERAL (x)));
      break;
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	output_int32 (p->x);
	output (" %c ", p->op);
	output_int32 (p->y);
	break;
      }
    default:
      {
	struct cobc_field *p = field (x);
	switch (p->usage)
	  {
	  case COBC_USAGE_DISPLAY:
	    output_func_1 ("cob_to_int", x);
	    break;
	  case COBC_USAGE_BINARY:
	  case COBC_USAGE_INDEX:
	    if (p->level == 0)
	      {
		output ("i_%s", p->cname);
	      }
	    else
	      {
		output ("(*(");
		switch (p->size)
		  {
		  case 1: output ("char"); break;
		  case 2: output ("short"); break;
		  case 4: output ("long"); break;
		  case 8: output ("long long"); break;
		  }
		output (" *) (");
		output_data (x);
		output ("))");
	      }
	  }
	break;
      }
    }
}


/*
 * Output parameter
 */

static void
output_param (cobc_tree x)
{
  if (x == NULL)
    {
      output ("0");
      return;
    }

  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      output ("%s", COBC_CONST (x)->val);
      break;
    case cobc_tag_integer:
      output_int32 (x);
      break;
    case cobc_tag_string:
      output ("\"%s\"", COBC_STRING (x)->str);
      break;
    case cobc_tag_cast_int32:
      output_int32 (COBC_CAST_INT32 (x)->val);
      break;
    case cobc_tag_decimal:
      output ("&d[%d]", COBC_DECIMAL (x)->id);
      break;
    case cobc_tag_literal:
    case cobc_tag_field:
    case cobc_tag_file:
    case cobc_tag_reference:
      output ("&%s", field_name (x));
      break;
    default:
      abort ();
    }
}


/*
 * Inline functions
 */

static void
output_base (struct cobc_field *p)
{
  struct cobc_field *p01 = field_founder (p);

  if (p01->redefines)
    p01 = p01->redefines;

  output ("f_%s_data", p01->cname);
  if (p->offset > 0)
    output (" + %d", p->offset);
}

static void
output_offset (struct cobc_reference *r)
{
  struct cobc_field *f = COBC_FIELD (r->value);
  output ("cob_index (");
  output_int32 (r->offset);
  output (", %d, \"%s\")", f->size, f->name);
}

static void
output_data (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_literal:
      output_quoted_string (COBC_LITERAL (x)->data, COBC_LITERAL (x)->size);
      break;
    case cobc_tag_field:
      {
	struct cobc_field *f = COBC_FIELD (x);
	int i = f->indexes;
	output_base (f);
	for (; f; f = f->parent)
	  if (f->f.have_occurs)
	    output (" + %d * i%d", f->size, i--);
	break;
      }
    case cobc_tag_reference:
      {
	struct cobc_reference *r = COBC_REFERENCE (x);
	struct cobc_field *f = COBC_FIELD (r->value);

	/* base address */
	output ("%s.data", field_name (COBC_TREE (f)));

	/* subscript reference */
	if (r->subs)
	  {
	    struct cobc_field *p;
	    struct cobc_list *l = r->subs = list_reverse (r->subs);

	    for (p = f; p; p = p->parent)
	      if (p->f.have_occurs)
		{
		  output (" + %d * ", p->size);
		  if (p->occurs_depending)
		    {
		      output ("cob_index_depending (");
		      output_int32 (l->item);
		      output (", %d, %d, ", p->occurs_min, p->occurs);
		      output_int32 (p->occurs_depending);
		      output (", \"%s\", \"%s\")", p->name,
			      field (p->occurs_depending)->name);
		    }
		  else
		    {
		      output ("cob_index (");
		      output_int32 (l->item);
		      output (", %d, \"%s\")", p->occurs, p->name);
		    }
		  l = l->next;
		}

	    r->subs = list_reverse (r->subs);
	  }

	/* offset */
	if (r->offset)
	  {
	    output (" + ");
	    output_offset (r);
	  }
	break;
      }
    }
}

static void
output_size (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_literal:
      output ("%d", COBC_LITERAL (x)->size);
      break;
    case cobc_tag_field:
      output ("%d", COBC_FIELD (x)->size);
      break;
    case cobc_tag_reference:
      {
	struct cobc_reference *r = COBC_REFERENCE (x);
	struct cobc_field *f = COBC_FIELD (r->value);

	if (r->length)
	  {
	    output ("cob_index (");
	    output_int32 (r->length);
	    output (", %d - ", f->size);
	    output_offset (r);
	    output (", \"%s\") + 1", f->name);
	  }
	else if (r->offset)
	  {
	    output ("%d - ", f->size);
	    output_offset (r);
	  }
	else
	  {
	    output ("%d", f->size);
	  }
	break;
      }
    }
}

static void
output_memset (cobc_tree x, char c)
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
output_memcpy (cobc_tree x, char *s)
{
  output_prefix ();
  output ("memcpy (");
  output_data (x);
  output (", ");
  output_quoted_string (s, field (x)->size);
  output (", ");
  output_size (x);
  output (");\n");
}

static void
output_native_assign (cobc_tree x, long long val)
{
  output_prefix ();
  output_int32 (x);
  output (" = %lldLL;\n", val);
}


/*
 * Field
 */

static void
output_field (cobc_tree x, int id)
{
  char fname[5];
  sprintf (fname, "f[%d]", id - 1);

  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (x);
	char class = COBC_TREE_CLASS (x);

	l->id = id;

	if (class == COB_TYPE_NUMERIC)
	  {
	    output_indent ("{");
	    output_line ("static cob_field_attr attr = {%d, %d, %d, 1};",
			 COB_TYPE_NUMERIC_BINARY, l->size, l->decimals);
	    output_line ("static long long n = %lldLL;", literal_to_int (l));
	    output_line ("%s = (cob_field) {8, (void *) &n, &attr};", fname);
	    output_indent ("}");
	  }
	else
	  {
	    output_prefix ();
	    output ("%s = (cob_field) {%d, ", fname, l->size);
	    output_quoted_string (l->data, l->size);
	    output (", ");
	    if (l->all)
	      output ("&cob_all_attr");
	    else
	      output ("&cob_alnum_attr");
	    output ("};\n");
	  }
	break;
      }
    case cobc_tag_reference:
      {
	struct cobc_reference *r = COBC_REFERENCE (x);
	struct cobc_field *f;

	if (r->subs == NULL && r->offset == NULL)
	  return;

	r->id = id;
	f = COBC_FIELD (r->value);
	output_line ("/* %s */", tree_to_string (x));

	/* size */
	output_prefix ();
	output ("%s.size = ", fname);
	output_size (x);
	output (";\n");

	/* data */
	output_prefix ();
	output ("%s.data = ", fname);
	output_data (x);
	output (";\n");

	/* attr */
	if (r->offset)
	  output_line ("%s.attr = &cob_group_attr;", fname);
	else
	  output_line ("%s.attr = %s.attr;", fname, field_name (COBC_TREE (f)));

	break;
      }
    }
}

static void
output_func_1 (const char *name, cobc_tree a1)
{
  output ("%s (", name);
  if (COBC_LITERAL_P (a1) || COBC_REFERENCE_P (a1))
    {
      output ("({\n");
      output_field (a1, 1);
      output_param (a1);
      output (";})");
    }
  else
    output_param (a1);
  output (")");
}


/*
 * Output condition
 */

static void
output_cond (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      {
	output ("%s", COBC_CONST (x)->val);
	break;
      }
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
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
	    if ((COBC_REFERENCE_P (p->x)
		 && field (p->x)->usage == COBC_USAGE_INDEX)
		|| (COBC_REFERENCE_P (p->y)
		    && field (p->y)->usage == COBC_USAGE_INDEX))
	      {
		output_int32 (p->x);
		switch (p->op)
		  {
		  case '=': output (" == "); break;
		  case '<': output (" < "); break;
		  case '[': output (" <= "); break;
		  case '>': output (" > "); break;
		  case ']': output (" >= "); break;
		  case '~': output (" != "); break;
		  }
		output_int32 (p->y);
	      }
	    else
	      {
		output ("({");
		output_stmt (p->x);
		output ("})");
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
    case cobc_tag_funcall:
      {
	struct cobc_funcall *p = COBC_FUNCALL (x);
	output ("%s (", p->name);
	output_param (p->argv[0]);
	output (")");
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
output_recursive (void (*func) (struct cobc_field *), struct cobc_field *f)
{
  if (f->level != 01 && f->level != 77 && f->redefines)
    return;

  if (f->f.have_occurs)
    {
      /* begin occurs loop */
      int i = f->indexes;
      output_indent ("{");
      output_line ("int i%d;", i);
      output_line ("for (i%d = 0; i%d < %d; i%d++)", i, i, f->occurs, i);
      output_indent ("  {");
    }

  /* process output */
  func (f);

  if (f->f.have_occurs)
    {
      /* close loop */
      output_indent ("  }");
      output_indent ("}");
    }
}


/*
 * Exception handler
 */

static void output_perform_call (struct cobc_label *lb, struct cobc_label *le);

static void
output_handler (int ec, cobc_tree st1, cobc_tree st2, struct cobc_label *l)
{
  if (st1)
    {
      if ((ec & 0x00ff) == 0)
	output_line ("if ((cob_error_code & 0xff00) == 0x%04x)", ec);
      else
	output_line ("if (cob_error_code == 0x%04x)", ec);
      output_stmt (st1);
      if (st2 || l)
	output_line ("else");
    }
  if (l)
    {
      output_line ("if (cob_error_code)");
      output_indent ("  {");
      output_perform_call (l, l);
      output_indent ("  }");
      if (st2)
	output_line ("else");
    }
  if (st2)
    {
      if (st1 == 0 && l == 0)
	output_line ("if (!cob_error_code)");
      output_stmt (st2);
    }
}


/*
 * Comparison
 */

static void
output_memcmp (cobc_tree x, cobc_tree y)
{
  size_t size = field_size (x);
  unsigned char buff[size];
  struct cobc_literal *l = COBC_LITERAL (y);

  memset (buff, ' ', size);
  memcpy (buff, l->data, l->size);

  output_prefix ();
  output ("cob_cmp_result = memcmp (");
  output_data (x);
  output (", ");
  output_quoted_string (buff, size);
  output (", %d);\n", size);
}


/*
 * GO TO
 */

static void
output_goto (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_label *l = COBC_LABEL (r->value);
  output_line ("goto lb_%s;", l->cname);
}

static void
output_goto_depending (struct cobc_list *labels, cobc_tree index)
{
  int i = 1;
  struct cobc_list *l;
  output_prefix ();
  output ("switch (");
  output_int32 (index);
  output (")\n");
  output_indent ("  {");
  for (l = labels; l; l = l->next)
    {
      output_indent_level -= 2;
      output_line ("case %d:", i++);
      output_indent_level += 2;
      output_goto (l->item);
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

static int
category (cobc_tree x)
{
  if (field (x)->pic)
    return field (x)->pic->category;
  else
    return COB_TYPE_ALPHANUMERIC;
}

static void
output_advance_move (cob_field *f, cobc_tree dst)
{
  struct cobc_field *p = field (dst);
  cob_field_attr attr;
  unsigned char data[p->size];
  cob_field fld = {p->size, data, &attr};

  if (p->children || p->rename_thru
      || (p->level == 66 && p->redefines->children))
    {
      attr.type = COB_TYPE_GROUP;
      attr.justified = 0;
    }
  else
    {
      attr.type = get_type (p);
      attr.digits = p->pic->digits;
      attr.decimals = p->pic->decimals;
      attr.have_sign = p->pic->have_sign;
      attr.sign_separate = p->f.sign_separate;
      attr.sign_leading = p->f.sign_leading;
      attr.blank_zero = p->f.blank_zero;
      attr.justified = p->f.justified;
      attr.pic = p->pic->str;
    }

  cob_move (f, &fld);
  output_memcpy (dst, data);
}

static void
output_move_num (cobc_tree x, int high)
{
  switch (field (x)->usage)
    {
    case COBC_USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0');
      break;
    case COBC_USAGE_BINARY:
    case COBC_USAGE_INDEX:
      output_native_assign (x, high ? -1 : 0);
      break;
    }
}

static void
output_move_all (cobc_tree x, char c)
{
  struct cobc_field *p = field (x);
  unsigned char data[p->size];
  cob_field fld = {p->size, data, &cob_alnum_attr};
  memset (data, c, p->size);
  output_advance_move (&fld, x);
}

static void
output_move_space (cobc_tree x)
{
  switch (category (x))
    {
    case COB_TYPE_NUMERIC:
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, ' ');
      break;
    default:
      output_move_all (x, ' ');
      break;
    }
}

static void
output_move_zero (cobc_tree x)
{
  switch (category (x))
    {
    case COB_TYPE_NUMERIC:
      if (field (x)->f.blank_zero)
	output_move_space (x);
      else
	output_move_num (x, 0);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, '0');
      break;
    default:
      output_move_all (x, '0');
      break;
    }
}

static void
output_move_high (cobc_tree x)
{
  switch (category (x))
    {
    case COB_TYPE_NUMERIC:
      output_move_num (x, 9);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, 255);
      break;
    default:
      output_move_all (x, 255);
      break;
    }
}

static void
output_move_low (cobc_tree x)
{
  switch (category (x))
    {
    case COB_TYPE_NUMERIC:
      output_move_num (x, 0);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, 0);
      break;
    default:
      output_move_all (x, 0);
      break;
    }
}

static void
output_move_quote (cobc_tree x)
{
  switch (category (x))
    {
    case COB_TYPE_NUMERIC:
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, '"');
      break;
    default:
      output_move_all (x, '"');
      break;
    }
}

static void
output_move_literal (struct cobc_literal *l, cobc_tree dst)
{
  struct cobc_field *f = field (dst);

  if (l->all)
    {
      int i;
      unsigned char buff[f->size];
      for (i = 0; i < f->size; i++)
	buff[i] = l->data[i % l->size];
      output_memcpy (dst, buff);
    }
  else if (f->usage == COBC_USAGE_BINARY || f->usage == COBC_USAGE_INDEX)
    {
      long long val = literal_to_int (l);
      int decs = f->pic->decimals;
      if (decs > l->decimals)
	val *= cob_exp10[decs - l->decimals];
      else if (decs < l->decimals)
	val /= cob_exp10[l->decimals - decs];
      output_native_assign (dst, val);
    }
  else
    {
      cob_field_attr attr =
	{COBC_TREE_CLASS (l), l->size, l->decimals, l->sign ? 1 : 0};
      cob_field fld = {l->size, l->data, &attr};
      if (l->sign < 0)
	l->data[l->size - 1] += 0x10;
      output_advance_move (&fld, dst);
      if (l->sign < 0)
	l->data[l->size - 1] -= 0x10;
    }
}

static void
output_move_index (cobc_tree src, cobc_tree dst)
{
  output_prefix ();
  output_int32 (dst);
  output (" = ");
  output_int32 (src);
  output (";\n");
}

static void
output_move (cobc_tree src, cobc_tree dst)
{
  if (src == cobc_zero)
    output_move_zero (dst);
  else if (src == cobc_space)
    output_move_space (dst);
  else if (src == cobc_high)
    output_move_high (dst);
  else if (src == cobc_low)
    output_move_low (dst);
  else if (src == cobc_quote)
    output_move_quote (dst);
  else if (src == cobc_true || src == cobc_false)
    output_move_index (src, dst);
  else if (COBC_LITERAL_P (src))
    output_move_literal (COBC_LITERAL (src), dst);
  else if (field (dst)->usage == COBC_USAGE_INDEX)
    output_move_index (src, dst);
  else if (field (src)->usage == COBC_USAGE_INDEX)
    output_stmt (make_funcall_2 ("cob_set_int", dst, make_cast_int32 (src)));
  else
    output_stmt (make_funcall_2 ("cob_move", src, dst));
}


/*
 * INITIALIZE
 */

static struct cobc_list *initialize_replacing_list;

static void output_initialize_internal (struct cobc_field *f);

static int
field_uniform_class (struct cobc_field *f)
{
  if (f->children)
    {
      int class = field_uniform_class (f->children);
      for (f = f->children->sister; f; f = f->sister)
	if (!f->redefines)
	  if (class != field_uniform_class (f))
	    return COB_TYPE_UNKNOWN;
      return class;
    }
  else
    {
      switch (f->pic->category)
	{
	case COB_TYPE_NUMERIC:
	  switch (f->usage)
	    {
	    case COBC_USAGE_DISPLAY:
	      return COB_TYPE_NUMERIC;
	    case COBC_USAGE_BINARY:
	    case COBC_USAGE_INDEX:
	      return COB_TYPE_NUMERIC_BINARY;
	    default:
	      return COB_TYPE_UNKNOWN;
	    }
	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
	  return COB_TYPE_ALPHANUMERIC;
	default:
	  return COB_TYPE_UNKNOWN;
	}
    }
}

static void
output_initialize_uniform (struct cobc_field *f, int class, int size)
{
  if (f->f.have_occurs)
    {
      output_indent ("{");
      output_line ("int i%d = 0;", f->indexes);
    }

  output_prefix ();
  output ("memset (");
  output_data (COBC_TREE (f));
  output (", ");
  switch (class)
    {
    case COB_TYPE_NUMERIC_BINARY:
      output ("0");
      break;
    case COB_TYPE_NUMERIC:
      output ("'0'");
      break;
    case COB_TYPE_ALPHANUMERIC:
      output ("' '");
      break;
    }
  output (", %d);\n", size);

  if (f->f.have_occurs)
    {
      output_indent ("}");
    }
}

static void
output_initialize_compound (struct cobc_field *f)
{
  cobc_tree x = COBC_TREE (f);
  switch (category (x))
    {
    case COB_TYPE_NUMERIC_EDITED:
      output_move_zero (x);
      break;
    case COB_TYPE_ALPHANUMERIC_EDITED:
    case COB_TYPE_NATIONAL_EDITED:
      output_move_space (x);
      break;
    default:
      output_initialize_internal (f);
      break;
    }
}

static void
output_initialize_internal (struct cobc_field *f)
{
  int last_class = COB_TYPE_UNKNOWN;
  struct cobc_field *p;
  struct cobc_field *first_field = NULL;

  /* initialize all children, combining uniform sequence into one */
  for (p = f->children; p; p = p->sister)
    {
      /* check if this child is uniform */
      int class = field_uniform_class (p);
      if (class == COB_TYPE_UNKNOWN || class != last_class)
	{
	  /* if not, or if this child is in a different category,
	     initialize the last uniform sequence */
	  if (first_field && last_class != COB_TYPE_ALPHANUMERIC)
	    output_initialize_uniform (first_field, last_class,
				       p->offset - first_field->offset);
	  /* if not uniform, initialize the children */
	  if (class == COB_TYPE_UNKNOWN)
	    output_recursive (output_initialize_compound, p);
	  last_class = class;
	  first_field = (class != COB_TYPE_UNKNOWN) ? p : NULL;
	}
    }
  /* initialize the final uniform sequence */
  if (first_field && last_class != COB_TYPE_ALPHANUMERIC)
    output_initialize_uniform (first_field, last_class,
			       f->offset + f->size - first_field->offset);
}

static void
output_initialize_replacing (struct cobc_field *f)
{
  if (f->children)
    {
      for (f = f->children; f; f = f->sister)
	output_recursive (output_initialize_replacing, f);
    }
  else
    {
      struct cobc_list *l;
      for (l = initialize_replacing_list; l; l = l->next)
	{
	  struct cobc_parameter *p = l->item;
	  if (p->type == f->pic->category)
	    {
	      output_move (p->x, COBC_TREE (f));
	      break;
	    }
	}
    }
}

static void
output_initialize (cobc_tree x, struct cobc_list *l)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_field *f = COBC_FIELD (r->value);

  /* output fixed indexes */
  if (r->subs)
    {
      int i = 1;
      struct cobc_list *l;
      output_indent ("{");
      for (l = r->subs; l; l = l->next)
	{
	  /* FIXME: need boundary check */
	  output_prefix ();
	  output ("int i%d = ", i++);
	  output_int32 (l->item);
	  output (" - 1;\n");
	}
    }

  if (l != NULL)
    {
      /* INITIALIZE REPLACING */
      initialize_replacing_list = l;
      output_recursive (output_initialize_replacing, f);
    }
  else
    {
      /* INITIALIZE */
      int class = field_uniform_class (f);
      if (class != COB_TYPE_UNKNOWN)
	{
	  /* if field is uniform (i.e., all children are
	     in the same category), initialize it at once */
	  output_initialize_uniform (f, class, f->size);
	}
      else
	{
	  /* otherwise, fill the field by spaces first */
	  output_initialize_uniform (f, COB_TYPE_ALPHANUMERIC, f->size);
	  /* then initialize the children recursively */
	  output_recursive (output_initialize_compound, f);
	}
    }

  if (r->subs)
    output_indent ("}");
}


/*
 * SEARCH
 */

static void
output_occurs (struct cobc_field *p)
{
  if (p->occurs_depending)
    output_int32 (p->occurs_depending);
  else
    output ("%d", p->occurs);
}

static void
output_search (cobc_tree table, cobc_tree var, cobc_tree stmt, cobc_tree whens)
{
  struct cobc_list *l;
  struct cobc_field *p = field (table);
  cobc_tree idx = NULL;

  /* determine the index to use */
  var = var ? COBC_TREE (field (var)) : NULL;
  for (l = p->index_list; l; l = l->next)
    if (l->item == var)
      idx = var;
  if (!idx)
    idx = COBC_TREE (p->index_list->item);

  /* start loop */
  output_line ("while (1)");
  output_indent ("  {");

  /* end test */
  output_prefix ();
  output ("if (");
  output_int32 (idx);
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
  output_int32 (idx);
  output ("++;\n");
  if (var && var != idx)
    output_move (idx, var);
  output_line ("continue;");
  output_indent ("  }");
  output_line ("break;");
  output_indent ("  }");
}

static void
output_search_all (cobc_tree table, cobc_tree stmt, cobc_tree when)
{
  struct cobc_field *p = field (table);
  cobc_tree idx = COBC_TREE (p->index_list->item);

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
  output_int32 (idx);
  output (" = (head + tail) / 2;\n");

  /* WHEN test */
  output_stmt (when);
  output_line ("else");
  output_indent ("  {");
  output_line ("if (cob_cmp_result < 0)");
  output_prefix ();
  output ("  head = ");
  output_int32 (idx);
  output (";\n");
  output_line ("else");
  output_prefix ();
  output ("  tail = ");
  output_int32 (idx);
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
output_sort_init (cobc_tree file, struct cobc_list *keys)
{
  struct cobc_list *l;

  output_indent ("{");
  output_line ("static cob_file_key keys[] = {");
  for (l = keys; l; l = l->next)
    {
      struct cobc_parameter *p = l->item;
      output_prefix ();
      output ("  {");
      output_param (p->x);
      output (", %d},\n", p->type);
    }
  output_line ("};");
  output_prefix ();
  output ("cob_sort_init (");
  output_param (file);
  output (", %d, keys);\n", list_length (keys));
  output_indent ("}");
}


/*
 * CALL
 */

static void
output_call (cobc_tree name, struct cobc_list *args,
	     cobc_tree st1, cobc_tree st2)
{
  int n;
  int dynamic_link = 1;
  struct cobc_list *l;

  if (cobc_flags.static_call && COBC_LITERAL_P (name))
    dynamic_link = 0;

  /* local variables */
  output_indent ("{");
  if (dynamic_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      cobc_tree x = p->x;
      switch (p->type)
	{
	case COBC_CALL_BY_CONTENT:
	  output_prefix ();
	  output ("char content_%d[", n);
	  output_size (x);
	  output ("];\n");
	  break;
	case COBC_CALL_BY_LENGTH:
	  output_prefix ();
	  output ("int length_%d = ", n);
	  output_size (x);
	  output (";\n");
	}
    }
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      cobc_tree x = p->x;
      switch (p->type)
	{
	case COBC_CALL_BY_CONTENT:
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
      output ("cob_return_code = %s", COBC_LITERAL (name)->data);
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      if (COBC_LITERAL_P (name))
	output ("cob_resolve (\"%s\")", COBC_LITERAL (name)->data);
      else
	output_func_1 ("cob_call_resolve", name);
      output (";\n");
      output_line ("if (func == NULL)");
      output_indent_level += 2;
      if (st1)
	output_stmt (st1);
      else
	output_line ("cob_call_error ();");
      output_indent_level -= 2;
      output_line ("else");
      output_indent ("  {");
      output_prefix ();
      output ("cob_return_code = func");
    }

  /* arguments */
  output (" (");
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      cobc_tree x = p->x;
      switch (p->type)
	{
	case COBC_CALL_BY_REFERENCE:
	  output_data (x);
	  break;
	case COBC_CALL_BY_CONTENT:
	  output ("content_%d", n);
	  break;
	case COBC_CALL_BY_LENGTH:
	  output ("&length_%d", n);
	  break;
	case COBC_CALL_BY_VALUE:
	  switch (COBC_TREE_TAG (x))
	    {
	    case cobc_tag_literal:
	      if (COBC_TREE_CLASS (x) == COB_TYPE_NUMERIC)
		output ("%lld", literal_to_int (COBC_LITERAL (x)));
	      else
		output ("%d", COBC_LITERAL (x)->data[0]);
	      break;
	    default:
	      switch (COBC_FIELD (x)->usage)
		{
		case COBC_USAGE_BINARY:
		case COBC_USAGE_INDEX:
		  output_int32 (x);
		  break;
		default:
		  output ("*");
		  output_data (x);
		  break;
		}
	      break;
	    }
	  break;
	}
      if (l->next)
	output (", ");
    }
  output (");\n");
  output_line ("init_environment ();");
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
  char *name;
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
output_funcall (struct cobc_funcall *p)
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

      /* reference */
      for (i = 0; i < p->argc; i++)
	output_field (p->argv[i], i + 1);

      /* function call */
      output_prefix ();
      output ("%s (", p->name);
      for (i = 0; i < p->argc; i++)
	{
	  output_param (p->argv[i]);
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
output_perform_call (struct cobc_label *lb, struct cobc_label *le)
{
  static int id = 1;
  output_line ("cob_perform (%d, lb_%s, le_%s);", id++, lb->cname, le->cname);
}

static void
output_perform_once (struct cobc_perform *p)
{
  if (COBC_PARAMETER_P (p->body))
    {
      cobc_tree lb = COBC_REFERENCE (COBC_PARAMETER (p->body)->x)->value;
      cobc_tree le = COBC_REFERENCE (COBC_PARAMETER (p->body)->y)->value;
      output_perform_call (COBC_LABEL (lb), COBC_LABEL (le));
    }
  else
    output_stmt (p->body);
}

static void
output_perform_until (struct cobc_perform *p, struct cobc_perform_varying *v)
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

  if (p->test == COBC_AFTER)
    output_perform_until (p, v->next);

  output_prefix ();
  output ("if (");
  output_cond (v->until);
  output (")\n");
  output_line ("  break;");

  if (p->test == COBC_BEFORE)
    output_perform_until (p, v->next);

  if (v->step)
    output_stmt (v->step);

  output_indent ("  }");
}

static void
output_perform (struct cobc_perform *p)
{
  static int loop_counter = 0;

  switch (p->type)
    {
    case COBC_PERFORM_EXIT:
      if (COBC_LABEL (p->data)->need_return)
	output_line ("cob_exit (le_%s);", COBC_LABEL (p->data)->cname);
      break;
    case COBC_PERFORM_ONCE:
      output_perform_once (p);
      break;
    case COBC_PERFORM_TIMES:
      output_prefix ();
      output ("for (n[%d] = ", loop_counter);
      output_int32 (p->data);
      output ("; n[%d] > 0; n[%d]--)\n", loop_counter, loop_counter);
      output_indent ("  {");
      output_perform_once (p);
      output_indent ("  }");
      loop_counter++;
      break;
    case COBC_PERFORM_UNTIL:
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
output_stmt (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_label:
      {
	struct cobc_label *p = COBC_LABEL (x);
	output_newline ();
	output_line ("/* %s: */", p->name);
	if (p->need_begin)
	  output_line ("lb_%s:", p->cname);
	break;
      }
    case cobc_tag_funcall:
      {
	output_funcall (COBC_FUNCALL (x));
	break;
      }
    case cobc_tag_if:
      {
	struct cobc_if *p = COBC_IF (x);
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
    case cobc_tag_perform:
      {
	output_perform (COBC_PERFORM (x));
	break;
      }
    case cobc_tag_sequence:
      {
	struct cobc_sequence *p = COBC_SEQUENCE (x);
	struct cobc_list *l = p->list;
	output_line_directive (x);
	output_indent ("{");
	if (p->save_status && l && l->next)
	  {
	    /* output with combining multiple cob_error_code */
	    output_line ("int error_code = 0;");
	    for (; l; l = l->next)
	      {
		output_stmt (l->item);
		output_line ("error_code |= cob_error_code;");
	      }
	    output_line ("cob_error_code = error_code;");
	  }
	else
	  {
	    /* output without using cob_error_code */
	    for (; l; l = l->next)
	      output_stmt (l->item);
	  }
	output_indent ("}");
	break;
      }
    default:
      abort ();
    }
}


/*
 * Field definition
 */

static void
output_field_definition (struct cobc_field *p, struct cobc_field *p01,
			 int gen_data, int gen_filler)
{
  char *fname = field_name (COBC_TREE (p));
  char attr_buff[1024], *attr = attr_buff;

  if (!gen_filler)
    gen_filler = !COBC_FILLER_P (COBC_TREE (p));

  /* attribute */
  if (p->children || p->rename_thru
      || (p->level == 66 && p->redefines->children))
    {
      attr = "cob_group_attr";
    }
  else if (p->f.used && gen_filler)
    {
      char type = get_type (p);
      if (type == COB_TYPE_ALPHANUMERIC)
	{
	  if (p->f.justified)
	    attr = "cob_just_attr";
	  else
	    attr = "cob_alnum_attr";
	}
      else
	{
	  sprintf (attr_buff, "%s_attr", fname);
	  output ("static cob_field_attr %s = ", attr);
	  output ("{%d, %d, %d, %d, %d, %d, %d, %d, ",
		  type, p->pic->digits, p->pic->decimals,
		  p->pic->have_sign, p->f.sign_separate, p->f.sign_leading,
		  p->f.blank_zero, p->f.justified);
	  if (p->pic->str[0] != 0)
	    {
	      unsigned char *s;
	      output ("\"");
	      for (s = p->pic->str; *s; s += 2)
		output ("%c\\%03o", s[0], s[1]);
	      output ("\"");
	    }
	  else
	    output ("0");
	  output ("};\n");
	}
    }

  /* data */
  if (p == p01 && !p->redefines)
    {
      /* level 01 */
      if (field_used_any_child (p) && gen_data)
	{
	  if (p->f.external)
	    {
	      output ("unsigned char %s[%d];\n", p->cname, p->memory_size);
	      output ("#define %s_data %s\n", fname, p->cname);
	    }
	  else
	    {
	      output ("static unsigned char %s_data[%d];\n",
		      fname, p->memory_size);
	    }
	}
    }

  /* field */
  if (p->f.used && gen_filler)
    {
      if (gen_data)
	output ("static ");
      output ("cob_field %s = {%d, ", fname, p->size);
      output_base (p);
      output (", &%s};\n", attr);
    }
  if (p->f.used)
    output_newline ();

  /* children */
  for (p = p->children; p; p = p->sister)
    output_field_definition (p, p01, gen_data, gen_filler);
}


/*
 * File definition
 */

static void
output_file_definition (struct cobc_file *f)
{
  int nkeys = 1;
  struct cobc_field *p;

  /* output record definition */
  for (p = f->record; p; p = p->sister)
    output_field_definition (p, p, 1, 0);

  /* output file name */
  if (COBC_LITERAL_P (f->assign))
    {
      struct cobc_literal *l = COBC_LITERAL (f->assign);
      output ("static cob_field %s_file = {%d, ", f->cname, l->size);
      output_quoted_string (l->data, l->size);
      output (", &cob_alnum_attr};\n");
    }

  /* output RELATIVE/RECORD KEY's */
  if (f->organization == COB_ORG_RELATIVE
      || f->organization == COB_ORG_INDEXED)
    {
      struct cobc_alt_key *l;
      output ("static cob_file_key %s_keys[] = {\n", f->cname);
      output ("  {");
      output_param (f->key);
      output (", 0},\n");
      for (l = f->alt_key_list; l; l = l->next)
	{
	  nkeys++;
	  output ("  {");
	  output_param (l->key);
	  output (", %d},\n", l->duplicates);
	}
      output ("};\n");
    }

  /* output the file descriptor */
  output ("static cob_file %s = {", f->cname);
  /* organization, access_mode, open_mode */
  output ("%d, %d, 0, ", f->organization, f->access_mode);
  /* file_status */
  if (f->file_status)
    output_base (field (f->file_status));
  else
    output ("cob_dummy_status");
  output (", ");
  /* assign */
  if (COBC_LITERAL_P (f->assign))
    output ("&%s_file", f->cname);
  else
    output_param (f->assign);
  output (", ");
  /* record */
  output_param (COBC_TREE (f->record));
  output (", ");
  /* record_size */
  output_param (f->record_depending);
  output (", ");
  /* record_min, record_max */
  output ("%d, %d, ", f->record_min, f->record_max);
  /* flags */
  output ("{%d, 0, 0, 0, 0, 0}, ", f->optional);
  /* nkeys, keys */
  if (f->organization == COB_ORG_RELATIVE
      || f->organization == COB_ORG_INDEXED)
    output ("%d, %s_keys, ", nkeys, f->cname);
  else
    output ("0, 0, ");
  /* file */
  output ("0};\n\n");
}


/*
 * Screen definition
 */

static void
output_screen_definition (struct cobc_field *p)
{
  int type = (p->children   ? COB_SCREEN_TYPE_GROUP :
	      p->value      ? COB_SCREEN_TYPE_VALUE :
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
      output_quoted_string (COBC_LITERAL (p->value)->data,
			    COBC_LITERAL (p->value)->size);
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
  output_int32 (p->screen_line);
  output (", ");
  output_int32 (p->screen_column);
  output (", %d};\n", p->screen_flag);
}


/*
 * Class definition
 */

static void
output_class_definition (struct cobc_class *p)
{
  struct cobc_list *l;

  output_line ("static int");
  output_line ("%s (cob_field *f)", p->cname);
  output_indent ("{");
  output_line ("int i;");
  output_line ("for (i = 0; i < f->size; i++)");
  output_prefix ();
  output ("  if (!(");
  for (l = p->list; l; l = l->next)
    {
      cobc_tree x = l->item;
      if (COBC_PARAMETER_P (x))
	{
	  struct cobc_parameter *p = COBC_PARAMETER (x);
	  char x = COBC_LITERAL (p->x)->data[0];
	  char y = COBC_LITERAL (p->y)->data[0];
	  output ("(%d <= f->data[i] && f->data[i] <= %d)", x, y);
	}
      else
	{
	  int i;
	  size_t size = COBC_LITERAL (x)->size;
	  unsigned char *data = COBC_LITERAL (x)->data;
	  for (i = 0; i < size; i++)
	    {
	      output ("f->data[i] == %d", data[i]);
	      if (i + 1 < size)
		output (" || ");
	    }
	}
      if (l->next)
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
have_value (struct cobc_field *p)
{
  if (p->value)
    return 1;
  for (p = p->children; p; p = p->sister)
    if (have_value (p))
      return 1;
  return 0;
}

static void
output_value (struct cobc_field *f)
{
  if (!field_used_any_child (f) && !field_used_any_parent (f))
    return;

  if (f->value)
    {
      if (COBC_CONST_P (f->value)
	  || COBC_TREE_CLASS (f->value) == COB_TYPE_NUMERIC
	  || COBC_LITERAL (f->value)->all)
	{
	  /* figurative literal, numeric literal, or ALL literal */
	  output_move (f->value, COBC_TREE (f));
	}
      else
	{
	  /* alphanumeric literal */
	  /* We do not use output_move here because
	     we do not want to have the value be edited. */
	  char buff[f->size];
	  char *str = COBC_LITERAL (f->value)->data;
	  int size = COBC_LITERAL (f->value)->size;
	  if (size >= f->size)
	    {
	      memcpy (buff, str, f->size);
	    }
	  else
	    {
	      memcpy (buff, str, size);
	      memset (buff + size, ' ', f->size - size);
	    }
	  output_memcpy (COBC_TREE (f), buff);
	}
    }
  else
    {
      for (f = f->children; f; f = f->sister)
	if (have_value (f))
	  output_recursive (output_value, f);
    }
}


void
codegen (struct cobc_program_spec *spec)
{
  int i;
  struct cobc_list *l;
  struct cobc_field *p;

  output ("/* Generated from %s by cobc %s */\n\n",
	  cobc_source_file, COBC_VERSION);
  output ("#include <stdio.h>\n");
  output ("#include <stdlib.h>\n");
  output ("#include <string.h>\n");
  output ("#include <libcob.h>\n\n");

  if (cobc_flags.main)
    spec->initial_program = 1;

  output ("#define cob_perform(id,from,until) \\\n");
  output ("  do { \\\n");
  output ("    frame_index++; \\\n");
  output ("    frame_stack[frame_index].perform_through = until; \\\n");
  output ("    frame_stack[frame_index].return_address = &&l_##id; \\\n");
  output ("    goto from; \\\n");
  output ("    l_##id: \\\n");
  output ("    frame_index--; \\\n");
  output ("  } while (0)\n");
  output ("\n");
  output ("#define cob_exit(label) \\\n");
  output ("  if (frame_stack[frame_index].perform_through == label) \\\n");
  output ("    goto *frame_stack[frame_index].return_address\n");

  /* fields */
  output ("/* Fields */\n\n");
  output ("#define i_SWITCH      cob_switch\n");
  output ("#define i_RETURN_CODE cob_return_code\n\n");
  for (p = spec->working_storage; p; p = p->sister)
    output_field_definition (p, p, 1, 0);
  for (l = spec->index_list; l; l = l->next)
    output ("static int i_%s;\n", COBC_FIELD (l->item)->cname);
  output_newline ();

  /* files */
  if (spec->file_list)
    {
      output ("/* Files */\n\n");
      for (l = spec->file_list; l; l = l->next)
	output_file_definition (l->item);
      output_newline ();
    }

  /* screens */
  if (spec->screen_storage)
    {
      output ("/* Screens */\n\n");
      for (p = spec->screen_storage; p; p = p->sister)
	{
	  output_field_definition (p, p, 1, 1);
	  output_screen_definition (p);
	}
      output_newline ();
    }

  /* labels */
  output ("/* Labels */\n\n");
  output ("enum {\n");
  output ("  le_standard_error_handler,\n");
  for (l = spec->exec_list; l; l = l->next)
    if (COBC_LABEL_P (l->item) && COBC_LABEL (l->item)->need_return)
      output ("  le_%s,\n", COBC_LABEL (l->item)->cname);
  output ("};\n\n");

  /* classes */
  for (l = spec->class_list; l; l = l->next)
    output_class_definition (l->item);

  /* environment */
  output ("static void\n");
  output ("init_environment (void)\n");
  output ("{\n");
  output ("  cob_source_file = \"%s\";\n", cobc_source_file);
  output ("  cob_decimal_point = '%c';\n", cob_decimal_point);
  output ("  cob_currency_symbol = '%c';\n", cob_currency_symbol);
  output ("}\n\n");

  /* initialize values */
  output_line ("static void");
  output_line ("%s_init (void)", spec->program_id);
  output_indent ("{");
  for (p = spec->working_storage; p; p = p->sister)
    if (have_value (p))
      output_recursive (output_value, p);
  output_indent ("}");
  output_newline ();

  /* program function */
  output_line ("int");
  output ("%s (", spec->program_id);
  if (!spec->using_list)
    output ("void");
  else
    for (l = spec->using_list; l; l = l->next)
      {
	output ("unsigned char *f_%s_data", COBC_FIELD (l->item)->cname);
	if (l->next)
	  output (", ");
      }
  output (")\n");
  output_indent ("{");

  /* local variables */
  if (!spec->initial_program)
    output_line ("static int initialized = 0;\n");
  output_line ("int i;");
  output_line ("int n[%d];", spec->loop_counter);
  output_line ("int frame_index;");
  output_line ("struct { int perform_through; void *return_address; } "
	       "frame_stack[24];");
  output_line ("cob_field f[4];");
  output_line ("cob_decimal d[%d];", spec->decimal_index_max);
  output_newline ();
  for (p = spec->linkage_storage; p; p = p->sister)
    output_field_definition (p, p, 0, 0);
  output_newline ();

  /* initialization */
  output_line ("/* ensure initializing libcob */");
  output_line ("cob_module_init ();");
  output_newline ();
  output_line ("/* initialize frame stack */");
  output_line ("frame_index = 0;");
  output_line ("frame_stack[0].perform_through = -1;");
  output_newline ();
  output_line ("/* initialize decimal numbers */");
  output_line ("for (i = 0; i < %d; i++)", spec->decimal_index_max);
  output_line ("  cob_decimal_init (&d[i]);");
  output_newline ();
  output_line ("/* initialize %s */", spec->program_id);
  output_line ("init_environment ();");
  if (!spec->initial_program)
    {
      output_line ("if (!initialized)");
      output_indent ("  {");
    }
  output_line ("%s_init ();", spec->program_id);
  if (!spec->initial_program)
    {
      output_line ("initialized = 1;");
      output_indent ("  }");
    }
  output_line ("goto lb_main;");
  output_newline ();

  /* error handlers */
  output_line ("/* error handlers */");
  output_line ("lb_standard_error_handler:");
  output_line ("switch (cob_error_file->open_mode)");
  output_line ("  {");
  for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++)
    if (spec->file_handler[i])
      {
	output_line ("  case %d:", i);
	output ("    ");
	output_perform_call (spec->file_handler[i], spec->file_handler[i]);
	output_line ("    break;");
      }
  output_line ("  default:");
  output_line ("    cob_default_error_handle (cob_error_file);");
  output_line ("    break;");
  output_line ("  }");
  output_line ("cob_exit (le_standard_error_handler);");
  output_newline ();

  /* PROCEDURE DIVISION */
  output_line ("/* PROCEDURE DIVISION */");
  for (l = spec->exec_list; l; l = l->next)
    output_stmt (l->item);
  output_newline ();

  output_line ("exit_program:");
  output_line ("return cob_return_code;");
  output_indent ("}");
  output_newline ();

  /* main function */
  if (cobc_flags.main)
    {
      output_line ("int");
      output_line ("main (int argc, char **argv)");
      output_indent ("{");
      output_line ("cob_init (argc, argv);");
      if (spec->enable_screen)
	output_line ("cob_screen_init ();");
      output_line ("%s ();", spec->program_id);
      if (spec->enable_screen)
	output_line ("cob_screen_clear ();");
      output_line ("return cob_return_code;");
      output_indent ("}");
    }
}
