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

#ifndef PRId64
#ifdef __MINGW32__
#define PRId64 "I64d"
#else
#define PRId64 "lld"
#endif
#endif /* !PRId64 */

FILE *storage_file;
char *storage_file_name;

static void output_stmt (cb_tree x);
static void output_data (cb_tree x);
static void output_int32 (cb_tree x);
static void output_index (cb_tree x);
static void output_func_1 (const char *name, cb_tree a1);


/*
 * Output routine
 */

static int output_indent_level = 0;
static FILE *output_target;

static void
output (char *fmt, ...)
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
output_line (char *fmt, ...)
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
output_storage (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (storage_file, fmt, ap);
  va_end (ap);
}


/*
 * Output field
 */

static void
output_base (struct cb_field *f)
{
  struct cb_field *f01 = field_founder (f);

  if (f01->redefines)
    f01 = f01->redefines;

  if (f01->flag_external)
    {
      output ("%s", f01->cname);
    }
  else
    {
      if (!f01->flag_base && f01->storage != CB_STORAGE_LINKAGE)
	{
	  output_storage ("static unsigned char b_%s[%d];\n",
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
    case cb_tag_literal:
      {
	struct cb_literal *l = CB_LITERAL (x);
	if (CB_TREE_CLASS (x) == COB_TYPE_NUMERIC)
	  output ("\"%s%s\"", l->data,
		  (l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "");
	else
	  output_quoted_string (l->data, l->size);
	break;
      }
    case cb_tag_field:
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
    case cb_tag_reference:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	/* base address */
	output_base (f);

	/* subscripts */
	if (r->subs)
	  {
	    struct cb_list *l = r->subs = list_reverse (r->subs);

	    for (; f; f = f->parent)
	      if (f->flag_occurs)
		{
		  output (" + %d * ", f->size);
		  output_index (l->item);
		  l = l->next;
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
    case cb_tag_literal:
      {
	struct cb_literal *l = CB_LITERAL (x);
	output ("%d", l->size + ((l->sign != 0) ? 1 : 0));
	break;
      }
    case cb_tag_field:
      {
	struct cb_field *f = CB_FIELD (x);
	output ("%d", f->size);
	break;
      }
    case cb_tag_reference:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	if (r->length)
	  {
	    output_int32 (r->length);
	  }
	else if (r->offset)
	  {
	    output ("%d - ", f->size);
	    output_index (r->offset);
	  }
	else
	  {
	    output ("%d", f->size);
	  }
	break;
      }
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
    case cb_tag_literal:
      {
	struct cb_literal *l = CB_LITERAL (x);

	if (CB_TREE_CLASS (x) == COB_TYPE_NUMERIC)
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
    case cb_tag_field:
      {
	struct cb_field *f = CB_FIELD (x);

	switch (CB_TREE_TYPE (x))
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

	      output ("&a_%d", lookup_attr (CB_TREE_TYPE (f),
					    f->pic->digits, f->pic->expt,
					    flags, f->pic->str));
	      break;
	    }
	  }
	break;
      }
    case cb_tag_reference:
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
    if (CB_TREE_TYPE (literal) == CB_TREE_TYPE (l->literal)
	&& literal->size == l->literal->size
	&& literal->all  == l->literal->all
	&& literal->sign == l->literal->sign
	&& literal->expt == l->literal->expt
	&& strcmp (literal->data, l->literal->data) == 0)
      return l->id;

  /* output new literal */
  output_target = 0;
  output_field (x);

  output_target = storage_file;
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

  if (x == NULL)
    {
      output ("0");
      return;
    }

  switch (CB_TREE_TAG (x))
    {
    case cb_tag_const:
      output ("%s", CB_CONST (x)->val);
      break;
    case cb_tag_integer:
      output_int32 (x);
      break;
    case cb_tag_string:
      output ("\"%s\"", CB_STRING (x)->str);
      break;
    case cb_tag_cast_int32:
      output_int32 (CB_CAST_INT32 (x)->val);
      break;
    case cb_tag_decimal:
      output ("&d[%d]", CB_DECIMAL (x)->id);
      break;
    case cb_tag_file:
      output ("&%s", CB_FILE (x)->cname);
      break;
    case cb_tag_literal:
      output ("&c_%d", lookup_literal (x));
      break;
    case cb_tag_field:
      {
	struct cb_field *f = CB_FIELD (x);
	if (f->indexes > 0 || f->storage == CB_STORAGE_LINKAGE)
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

		output_target = storage_file;
		output ("static cob_field f_%s = ", f->cname);
		output_field (x);
		output (";\n");

		f->flag_field = 1;
		output_target = yyout;
	      }
	    output ("&f_%s", f->cname);
	  }
	break;
      }
    case cb_tag_reference:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f;

	if (!CB_FIELD_P (r->value) || (!r->subs && !r->offset))
	  {
	    output_param (r->value, id);
	    return;
	  }

	f = CB_FIELD (r->value);
	output_line ("/* %s */", tree_name (x));
	output ("({");

	/* subscript check */
	if (r->subs)
	  {
	    struct cb_field *p;
	    struct cb_list *l = r->subs = list_reverse (r->subs);

	    for (p = f; p; p = p->parent)
	      if (p->flag_occurs)
		{
		  if (p->occurs_depending)
		    {
		      int n = p->occurs;
		      if (CB_LITERAL_P (l->item))
			n = literal_to_int (CB_LITERAL (l->item));
		      if (p->occurs_min <= n && n <= p->occurs)
			{
			  output_prefix ();
			  output ("cob_check_subscript_depending (");
			  output_int32 (l->item);
			  output (", %d, %d, ", p->occurs_min, p->occurs);
			  output_int32 (p->occurs_depending);
			  output (", \"%s\", \"%s\");\n", p->name,
				  field (p->occurs_depending)->name);
			}
		    }
		  else
		    {
		      if (!CB_LITERAL_P (l->item))
			{
			  output_prefix ();
			  output ("cob_check_subscript (");
			  output_int32 (l->item);
			  output (", %d, \"%s\");\n", p->occurs, p->name);
			}
		    }
		  l = l->next;
		}

	    r->subs = list_reverse (r->subs);
	  }

	/* reference modifier check */
	if (r->offset)
	  {
	    if (!CB_LITERAL_P (r->offset)
		|| (r->length && !CB_LITERAL_P (r->length)))
	      {
		output ("cob_check_ref_mod (");
		output_int32 (r->offset);
		output (", ");
		if (r->length)
		  output_int32 (r->length);
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
  output_param (a1, 0);
  output (")");
}


/*
 * Convert to int32
 */

static void
output_int32 (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case cb_tag_const:
      if (x == cb_zero)
	output ("0");
      else
	output ("%s", CB_CONST (x)->val);
      break;
    case cb_tag_integer:
      output ("%d", CB_INTEGER (x)->val);
      break;
    case cb_tag_literal:
      output ("%d", (int) literal_to_int (CB_LITERAL (x)));
      break;
    case cb_tag_binary_op:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	output_int32 (p->x);
	output (" %c ", p->op);
	output_int32 (p->y);
	break;
      }
    default:
      {
	struct cb_field *f = field (x);
	switch (f->usage)
	  {
	  case CB_USAGE_BINARY:
	  case CB_USAGE_INDEX:
	    if (f->level == 0)
	      {
		output ("i_%s", f->cname);
	      }
	    else
	      {
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
	      }
	    break;
	  default:
	    output_func_1 ("cob_get_int", x);
	    break;
	  }
	break;
      }
    }
}

static void
output_index (cb_tree x)
{
  output ("(");
  output_int32 (x);
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
    case cb_tag_const:
      {
	output ("%s", CB_CONST (x)->val);
	break;
      }
    case cb_tag_binary_op:
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
	    if ((CB_REFERENCE_P (p->x)
		 && field (p->x)->usage == CB_USAGE_INDEX)
		|| (CB_REFERENCE_P (p->y)
		    && field (p->y)->usage == CB_USAGE_INDEX))
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
    case cb_tag_funcall:
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
      output_line ("for (i%d = 0; i%d < %d; i%d++)", i, i, f->occurs, i);
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
output_handler (int ec, cb_tree st1, cb_tree st2, struct cb_label *l)
{
  if (st1)
    {
      if ((ec & 0x00ff) == 0)
	output_line ("if ((cob_exception_code & 0xff00) == 0x%04x)", ec);
      else
	output_line ("if (cob_exception_code == 0x%04x)", ec);
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
  size_t size = field_size (x);
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
  output_quoted_string (s, field (x)->size);
  output (", ");
  output_size (x);
  output (");\n");
}

static void
output_native_assign (cb_tree x, long long val)
{
  output_prefix ();
  output_int32 (x);
  output (" = %" PRId64 "LL;\n", val);
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
output_goto_depending (struct cb_list *labels, cb_tree index)
{
  int i = 1;
  struct cb_list *l;
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

static void
output_move_num (cb_tree x, int high)
{
  switch (field (x)->usage)
    {
    case CB_USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0');
      break;
    case CB_USAGE_PACKED:
      output_memset (x, high ? 0x99 : 0x00);
      break;
    case CB_USAGE_BINARY:
    case CB_USAGE_INDEX:
      output_native_assign (x, high ? -1 : 0);
      break;
    default:
      abort ();
    }
}

static void
output_move_all (cb_tree src, cb_tree dst)
{
  output_stmt (make_funcall_2 ("cob_move_all", src, dst));
}

static void
output_move_space (cb_tree x)
{
  switch (CB_TREE_TYPE (x))
    {
    case COB_TYPE_NUMERIC:
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, ' ');
      break;
    default:
      output_move_all (cb_space, x);
      break;
    }
}

static void
output_move_zero (cb_tree x)
{
  switch (CB_TREE_TYPE (x))
    {
    case COB_TYPE_NUMERIC:
      if (field (x)->flag_blank_zero)
	output_move_space (x);
      else
	output_move_num (x, 0);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, '0');
      break;
    default:
      output_move_all (cb_zero, x);
      break;
    }
}

static void
output_move_high (cb_tree x)
{
  switch (CB_TREE_TYPE (x))
    {
    case COB_TYPE_NUMERIC:
      output_move_num (x, 9);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, 255);
      break;
    default:
      output_move_all (cb_high, x);
      break;
    }
}

static void
output_move_low (cb_tree x)
{
  switch (CB_TREE_TYPE (x))
    {
    case COB_TYPE_NUMERIC:
      output_move_num (x, 0);
      break;
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, 0);
      break;
    default:
      output_move_all (cb_low, x);
      break;
    }
}

static void
output_move_quote (cb_tree x)
{
  switch (CB_TREE_TYPE (x))
    {
    case COB_TYPE_NUMERIC:
    case COB_TYPE_ALPHABETIC:
    case COB_TYPE_ALPHANUMERIC:
      output_memset (x, '"');
      break;
    default:
      output_move_all (cb_quote, x);
      break;
    }
}

static void
output_move_literal (struct cb_literal *l, cb_tree dst)
{
  struct cb_field *f = field (dst);

  if (l->all)
    {
      int i;
      unsigned char buff[f->size];
      for (i = 0; i < f->size; i++)
	buff[i] = l->data[i % l->size];
      output_memcpy (dst, buff);
    }
  else if (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_INDEX)
    {
      long long val = literal_to_int (l);
      int n = f->pic->expt - l->expt;
      for (; n > 0; n--) val /= 10;
      for (; n < 0; n++) val *= 10;
      output_native_assign (dst, val);
    }
  else
    {
      output_stmt (make_funcall_2 ("cob_move", CB_TREE (l), dst));
    }
}

static void
output_move_index (cb_tree src, cb_tree dst)
{
  output_prefix ();
  output_int32 (dst);
  output (" = ");
  output_int32 (src);
  output (";\n");
}

static void
output_move (cb_tree src, cb_tree dst)
{
  if (field (dst)->usage == CB_USAGE_INDEX)
    output_move_index (src, dst);
  else if (src == cb_zero)
    output_move_zero (dst);
  else if (src == cb_space)
    output_move_space (dst);
  else if (src == cb_high)
    output_move_high (dst);
  else if (src == cb_low)
    output_move_low (dst);
  else if (src == cb_quote)
    output_move_quote (dst);
  else if (src == cb_true || src == cb_false)
    output_move_index (src, dst);
  else if (CB_LITERAL_P (src))
    output_move_literal (CB_LITERAL (src), dst);
  else if (field (src)->usage == CB_USAGE_INDEX)
    output_stmt (make_funcall_2 ("cob_set_int", dst, make_cast_int32 (src)));
  else
    output_stmt (make_funcall_2 ("cob_move", src, dst));
}


/*
 * INITIALIZE
 */

static struct cb_list *initialize_replacing_list;

static void output_initialize_internal (struct cb_field *f);

static int
field_uniform_class (struct cb_field *f)
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
      switch (CB_TREE_TYPE (f))
	{
	case COB_TYPE_NUMERIC:
	case COB_TYPE_NUMERIC_BINARY:
	  return CB_TREE_TYPE (f);
	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
	  return COB_TYPE_ALPHANUMERIC;
	default:
	  return COB_TYPE_UNKNOWN;
	}
    }
}

static void
output_initialize_uniform (struct cb_field *f, int class, int size, int flag)
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

  if (flag && f->flag_occurs)
    {
      output_indent ("}");
    }
}

static void
output_initialize_compound (struct cb_field *f)
{
  cb_tree x = CB_TREE (f);
  switch (CB_TREE_TYPE (x))
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
output_initialize_internal (struct cb_field *f)
{
  int last_class = COB_TYPE_UNKNOWN;
  struct cb_field *p;
  struct cb_field *first_field = NULL;

  /* initialize all children, combining uniform sequence into one */
  for (p = f->children; p; p = p->sister)
    {
      /* check if this child is uniform */
      int class = field_uniform_class (p);
      if (class == COB_TYPE_UNKNOWN || class != last_class)
	{
	  /* if not, or if this child's category is different from
	     the previous one, initialize the last uniform sequence */
	  if (first_field && last_class != COB_TYPE_ALPHANUMERIC)
	    output_initialize_uniform (first_field, last_class,
				       p->offset - first_field->offset, 1);
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
      struct cb_list *l;
      for (l = initialize_replacing_list; l; l = l->next)
	{
	  struct cb_parameter *p = l->item;
	  if (p->type == f->pic->category)
	    {
	      output_move (p->x, CB_TREE (f));
	      break;
	    }
	}
    }
}

static void
output_initialize (cb_tree x, struct cb_list *l)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f = CB_FIELD (r->value);

  /* output fixed indexes */
  if (r->subs)
    {
      int i = 1;
      struct cb_list *l;
      output_indent ("{");
      for (l = r->subs; l; l = l->next)
	{
	  /* FIXME: need boundary check */
	  output_prefix ();
	  output ("int i%d = ", i++);
	  output_index (l->item);
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
      int class = field_uniform_class (f);
      if (class != COB_TYPE_UNKNOWN)
	{
	  /* if field is uniform (i.e., all children are
	     in the same category), initialize it at once */
	  output_initialize_uniform (f, class, f->size, 0);
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
    output_int32 (p->occurs_depending);
  else
    output ("%d", p->occurs);
}

static void
output_search (cb_tree table, cb_tree var, cb_tree stmt, cb_tree whens)
{
  struct cb_list *l;
  struct cb_field *p = field (table);
  cb_tree idx = NULL;

  /* determine the index to use */
  var = var ? CB_TREE (field (var)) : NULL;
  for (l = p->index_list; l; l = l->next)
    if (l->item == var)
      idx = var;
  if (!idx)
    idx = CB_TREE (p->index_list->item);

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
output_search_all (cb_tree table, cb_tree stmt, cb_tree when)
{
  struct cb_field *p = field (table);
  cb_tree idx = CB_TREE (p->index_list->item);

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
output_sort_init (cb_tree file, struct cb_list *keys)
{
  struct cb_list *l;

  output_indent ("{");
  output_line ("static cob_file_key keys[] = {");
  for (l = keys; l; l = l->next)
    {
      struct cb_parameter *p = l->item;
      output_prefix ();
      output ("  {");
      output_param (p->x, -1);
      output (", %d},\n", p->type);
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
output_call (cb_tree name, struct cb_list *args,
	     cb_tree st1, cb_tree st2)
{
  int n;
  int dynamic_link = 1;
  struct cb_list *l;

  if (cb_flag_call_static && CB_LITERAL_P (name))
    dynamic_link = 0;

  /* local variables */
  output_indent ("{");
  if (dynamic_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cb_parameter *p = l->item;
      cb_tree x = p->x;
      switch (p->type)
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
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cb_parameter *p = l->item;
      cb_tree x = p->x;
      switch (p->type)
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
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cb_parameter *p = l->item;
      cb_tree x = p->x;
      switch (p->type)
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
	    case cb_tag_literal:
	      if (CB_TREE_CLASS (x) == COB_TYPE_NUMERIC)
		output ("%" PRId64, literal_to_int (CB_LITERAL (x)));
	      else
		output ("%d", CB_LITERAL (x)->data[0]);
	      break;
	    default:
	      switch (CB_FIELD (x)->usage)
		{
		case CB_USAGE_BINARY:
		case CB_USAGE_INDEX:
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
  if (CB_PARAMETER_P (p->body))
    {
      cb_tree lb = CB_REFERENCE (CB_PARAMETER (p->body)->x)->value;
      cb_tree le = CB_REFERENCE (CB_PARAMETER (p->body)->y)->value;
      output_perform_call (CB_LABEL (lb), CB_LABEL (le));
    }
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
      output_int32 (p->data);
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
    case cb_tag_location:
      {
	static int last_line = 0;
	if (x->source_file && last_line != x->source_line)
	  {
	    if (cb_flag_line_directive)
	      output ("#line %d \"%s\"\n", x->source_line, x->source_file);
	    output_line ("cob_source_file = \"%s\";", x->source_file);
	    output_line ("cob_source_line = %d;", x->source_line);
	    last_line = x->source_line;
	  }
	break;
      }
    case cb_tag_label:
      {
	struct cb_label *p = CB_LABEL (x);
	output_newline ();
	output_line ("/* %s: */", p->name);
	if (p->need_begin)
	  output_line ("lb_%s:", p->cname);
	break;
      }
    case cb_tag_funcall:
      {
	output_funcall (CB_FUNCALL (x));
	break;
      }
    case cb_tag_if:
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
    case cb_tag_perform:
      {
	output_perform (CB_PERFORM (x));
	break;
      }
    case cb_tag_sequence:
      {
	struct cb_sequence *p = CB_SEQUENCE (x);
	struct cb_list *l = p->list;
	output_indent ("{");
	if (p->save_status && l && l->next)
	  {
	    /* output with combining multiple cob_exception_code */
	    output_line ("int code = 0;");
	    for (; l; l = l->next)
	      {
		output_stmt (l->item);
		output_line ("code |= cob_exception_code;");
	      }
	    output_line ("cob_exception_code = code;");
	  }
	else
	  {
	    /* output without using cob_exception_code */
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
      output_quoted_string (CB_LITERAL (p->values->item)->data,
			    CB_LITERAL (p->values->item)->size);
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
output_class_definition (struct cb_class *p)
{
  struct cb_list *l;

  output_line ("static int");
  output_line ("%s (cob_field *f)", p->cname);
  output_indent ("{");
  output_line ("int i;");
  output_line ("for (i = 0; i < f->size; i++)");
  output_prefix ();
  output ("  if (!(");
  for (l = p->list; l; l = l->next)
    {
      cb_tree x = l->item;
      if (CB_PARAMETER_P (x))
	{
	  struct cb_parameter *p = CB_PARAMETER (x);
	  char x = CB_LITERAL (p->x)->data[0];
	  char y = CB_LITERAL (p->y)->data[0];
	  output ("(%d <= f->data[i] && f->data[i] <= %d)", x, y);
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
      cb_tree value = f->values->item;
      if (CB_CONST_P (value)
	  || CB_TREE_CLASS (value) == COB_TYPE_NUMERIC
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
output_internal_function (struct cb_program *prog,
			  struct cb_list *parameter_list)
{
  int i;
  struct cb_list *l;

  /* program function */
  output_line ("static int");
  output ("internal_main (int entry");
  for (l = parameter_list; l; l = l->next)
    output (", unsigned char *b_%s", CB_FIELD (l->item)->cname);
  output (")\n");
  output_indent ("{");

  /* local variables */
  output_line ("static int initialized = 0;");
  output_line ("static cob_decimal d[%d];", prog->decimal_index_max);
  output_line ("static cob_environment env;");
  output_newline ();
  output_line ("int i;");
  output_line ("int n[%d];", prog->loop_counter);
  output_line ("int frame_index;");
  output_line ("struct frame { int perform_through; void *return_address; } "
	       "frame_stack[24];");
  output_line ("cob_field f[4];");
  output_newline ();

  /* initialization */
  output_line ("if (!initialized)");
  output_indent ("  {");
  output_line ("/* ensure initializing libcob */");
  output_line ("cob_module_init ();");
  output_newline ();
  output_line ("/* initialize decimal numbers */");
  output_line ("for (i = 0; i < %d; i++)", prog->decimal_index_max);
  output_line ("  cob_decimal_init (&d[i]);");
  output_newline ();
  output_line ("/* initialize environment */");
  output_line ("env.decimal_point = '%c';", prog->decimal_point);
  output_line ("env.currency_symbol = '%c';", prog->currency_symbol);
  output_line ("env.numeric_separator = '%c';", prog->numeric_separator);
  output_newline ();
  if (!prog->flag_initial)
    output_init_values (prog->working_storage);
  output_line ("initialized = 1;");
  output_indent ("  }");
  output_newline ();

  output_line ("/* initialize frame stack */");
  output_line ("frame_index = 0;");
  output_line ("frame_stack[0].perform_through = -1;");
  output_newline ();
  output_line ("/* initialize program */");
  output_line ("cob_push_environment (&env);");
  if (prog->flag_initial)
    output_init_values (prog->working_storage);
  output_newline ();

  /* entry dispatch */
  output_line ("switch (entry)");
  output_line ("  {");
  for (i = 0, l = prog->entry_list; l; l = l->next)
    {
      struct cb_parameter *p = CB_PARAMETER (l->item);
      output_line ("  case %d:", i++);
      output_line ("    goto lb_%s;", CB_LABEL (p->x)->cname);
    }
  output_line ("  }");
  output_newline ();

  /* error handlers */
  output_line ("/* error handlers */");
  output_stmt (CB_TREE (cb_standard_error_handler));
  output_line ("switch (cob_error_file->open_mode)");
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
  for (l = prog->exec_list; l; l = l->next)
    output_stmt (l->item);
  output_newline ();

  output_line ("exit_program:");
  output_line ("cob_pop_environment ();");
  output_line ("return cob_return_code;");
  output_indent ("}");
  output_newline ();
}

static void
output_entry_function (struct cb_parameter *entry,
		       struct cb_list *parameter_list)
{
  static int id = 0;

  const char *entry_name = CB_LABEL (entry->x)->name;
  struct cb_list *using_list = (struct cb_list *) entry->y;
  struct cb_list *l, *l1, *l2;

  output ("int\n");
  output ("%s (", entry_name);
  if (!using_list)
    output ("void");
  else
    for (l = using_list; l; l = l->next)
      {
	output ("unsigned char *b_%s", CB_FIELD (l->item)->cname);
	if (l->next)
	  output (", ");
      }
  output (")\n");
  output ("{\n");

  output ("  return internal_main (%d", id++);
  for (l1 = parameter_list; l1; l1 = l1->next)
    {
      for (l2 = using_list; l2; l2 = l2->next)
	if (strcmp (CB_FIELD (l1->item)->cname,
		    CB_FIELD (l2->item)->cname) == 0)
	  {
	    output (", b_%s", CB_FIELD (l1->item)->cname);
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
  struct cb_list *l;
  struct cb_list *parameter_list = NULL;

  if (cb_flag_main)
    prog->flag_initial = 1;

  output_target = yyout;

  output ("/* Generated from %s by cobc %s */\n\n",
	  cb_source_file, CB_VERSION);
  output ("#include <stdio.h>\n");
  output ("#include <stdlib.h>\n");
  output ("#include <string.h>\n");
  output ("#include <libcob.h>\n\n");

  output ("#include \"%s\"\n\n", storage_file_name);

  /* fields */
  output ("/* Fields */\n\n");
  output ("#define i_SWITCH      cob_switch\n");
  output ("#define i_RETURN_CODE cob_return_code\n\n");
  for (l = prog->index_list; l; l = l->next)
    output ("static int i_%s;\n", CB_FIELD (l->item)->cname);
  output_newline ();

  /* files */
  if (prog->file_list)
    {
      output ("/* Files */\n\n");
      for (l = prog->file_list; l; l = l->next)
	output_file_definition (l->item);
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

  /* labels */
  output ("/* Labels */\n\n");
  output ("enum {\n");
  output ("  le_standard_error_handler,\n");
  for (l = prog->exec_list; l; l = l->next)
    if (CB_LABEL_P (l->item) && CB_LABEL (l->item)->need_return)
      output ("  le_%s,\n", CB_LABEL (l->item)->cname);
  output ("};\n\n");

  /* classes */
  for (l = prog->class_list; l; l = l->next)
    output_class_definition (l->item);

  /* build parameter list */
  for (l = prog->entry_list; l; l = l->next)
    {
      struct cb_parameter *p = CB_PARAMETER (l->item);
      struct cb_list *using_list = (struct cb_list *) p->y;
      struct cb_list *l1, *l2;
      for (l1 = using_list; l1; l1 = l1->next)
	{
	  for (l2 = parameter_list; l2; l2 = l2->next)
	    if (strcmp (CB_FIELD (l1->item)->cname,
			CB_FIELD (l2->item)->cname) == 0)
	      break;
	  if (l2 == NULL)
	    parameter_list = list_add (parameter_list, l1->item);
	}
    }

  /* internal function */
  output_internal_function (prog, parameter_list);

  /* entry functions */
  for (l = prog->entry_list ; l; l = l->next)
    output_entry_function (CB_PARAMETER (l->item), parameter_list);

  /* main function */
  if (cb_flag_main)
    output_main_function (prog);
}
