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

#include "cobc.h"
#include "tree.h"
#include "scanner.h"
#include "codegen.h"
#include "libcob.h"

static void output_tree (cobc_tree x);


/*
 * Output routine
 */

static int output_indent_level = 0;

static void
output (char *fmt, ...)
{
  va_list argptr;
  va_start (argptr, fmt);
  vfprintf (cobc_out, fmt, argptr);
  va_end (argptr);
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
  va_list argptr;
  va_start (argptr, fmt);
  output_prefix ();
  vfprintf (cobc_out, fmt, argptr);
  fputc ('\n', cobc_out);
  va_end (argptr);
}

static void
output_indent (char *str, int level)
{
  if (level < 0)
    output_indent_level += level;
  output_line (str);
  if (level > 0)
    output_indent_level += level;
}

static void
output_quoted_string (char *s, int size)
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
	output ("\\%03o", (unsigned char) c);
    }
  output ("\"");
}

static void
output_line_directive (cobc_tree x)
{
  if (x->loc.text)
    {
      //output ("#line %d \"%s\"\n", x->loc.first_line, x->loc.text);
      output_line ("cob_source_line = %d;", x->loc.first_line);
    }
}


/*
 * Function calls
 */

static void
output_func_0 (const char *name)
{
  output ("%s ()", name);
}

static void
output_func_1 (const char *name, cobc_tree a1)
{
  output ("%s (", name);
  output_tree (a1);
  output (")");
}

static void
output_func_2 (const char *name, cobc_tree a1, cobc_tree a2)
{
  output ("%s (", name);
  output_tree (a1); output (", ");
  output_tree (a2);
  output (")");
}

static void
output_func_3 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3)
{
  output ("%s (", name);
  output_tree (a1); output (", ");
  output_tree (a2); output (", ");
  output_tree (a3);
  output (")");
}

static void
output_func_4 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3, cobc_tree a4)
{
  output ("%s (", name);
  output_tree (a1); output (", ");
  output_tree (a2); output (", ");
  output_tree (a3); output (", ");
  output_tree (a4);
  output (")");
}

static void
output_func_5 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3, cobc_tree a4, cobc_tree a5)
{
  output ("%s (", name);
  output_tree (a1); output (", ");
  output_tree (a2); output (", ");
  output_tree (a3); output (", ");
  output_tree (a4); output (", ");
  output_tree (a5);
  output (")");
}

static void
output_call_0 (const char *name)
{
  output_prefix ();
  output_func_0 (name);
  output (";\n");
}

static void
output_call_1 (const char *name, cobc_tree a1)
{
  output_prefix ();
  output_func_1 (name, a1);
  output (";\n");
}

static void
output_call_2 (const char *name, cobc_tree a1, cobc_tree a2)
{
  output_prefix ();
  output_func_2 (name, a1, a2);
  output (";\n");
}

static void
output_call_3 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3)
{
  output_prefix ();
  output_func_3 (name, a1, a2, a3);
  output (";\n");
}

static void
output_call_4 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3, cobc_tree a4)
{
  output_prefix ();
  output_func_4 (name, a1, a2, a3, a4);
  output (";\n");
}

static void
output_call_5 (const char *name, cobc_tree a1, cobc_tree a2, cobc_tree a3, cobc_tree a4, cobc_tree a5)
{
  output_prefix ();
  output_func_5 (name, a1, a2, a3, a4, a5);
  output (";\n");
}


/*
 * Subscripts
 */

static void output_index (cobc_tree x);

static char *
field_subscripts (struct cobc_field *p)
{
  int i;
  static char subscripts[256];
  if (p->indexes > 0)
    {
      char *s = subscripts + 1;
      strcpy (subscripts, "(");
      for (i = 1; i < p->indexes; i++)
	s += sprintf (s, "i%d,", i);
      sprintf (s, "i%d)", i);
    }
  else
    strcpy (subscripts, "");
  return subscripts;
}

static void
output_subscripts (cobc_tree x)
{
  if (COBC_REFMOD_P (x))
    x = COBC_REFMOD (x)->field;
  if (COBC_SUBREF_P (x))
    {
      struct cobc_list *l;
      struct cobc_list *subs = COBC_SUBREF (x)->subs;
      output (" (");
      for (l = subs; l; l = l->next)
	{
	  output_index (l->item);
	  if (l->next)
	    output (", ");
	}
      output (")");
    }
  else
    output ("%s", field_subscripts (COBC_FIELD (x)));
}


/*
 * Location/Length
 */

static void
output_location (cobc_tree x)
{
  if (COBC_LITERAL_P (x))
    {
      output_quoted_string (COBC_LITERAL (x)->str, COBC_LITERAL (x)->size);
    }
  else
    {
      output ("f_%s_data", COBC_FIELD (x)->cname);
      output_subscripts (x);
      if (COBC_REFMOD_P (x))
	{
	  output (" + ");
	  output_index (COBC_REFMOD (x)->offset);
	}
    }
}

static void
output_length (cobc_tree x)
{
  if (COBC_LITERAL_P (x))
    output ("%d", COBC_LITERAL (x)->size);
  else if (COBC_REFMOD_P (x))
    {
      if (COBC_REFMOD (x)->length)
	output_index (COBC_REFMOD (x)->length);
      else
	{
	  output ("%d - ", COBC_FIELD (x)->size);
	  output_index (COBC_REFMOD (x)->offset);
	}
    }
  else
    output ("%d", COBC_FIELD (x)->size);
}


/*
 * Index
 */

static void
output_index (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      output ("%s", COBC_CONST (x)->val);
      break;
    case cobc_tag_integer:
      output ("%d", COBC_INTEGER (x)->val);
      break;
    case cobc_tag_literal:
      output ("%lld", literal_to_int (COBC_LITERAL (x)));
      break;
    case cobc_tag_expr:
      {
	struct cobc_expr *p = COBC_EXPR (x);
	output_index (p->left);
	output (" %c ", p->op);
	output_index (p->right);
	break;
      }
    default:
      {
	struct cobc_field *p = COBC_FIELD (x);
	switch (p->usage)
	  {
	  case USAGE_DISPLAY:
	  case USAGE_PACKED:
	    output_func_1 ("cob_to_int", x);
	    break;
	  case USAGE_BINARY:
	  case USAGE_INDEX:
	    output ("i_%s", p->cname);
	    output_subscripts (x);
	    break;
	  }
	break;
      }
    }
}


/*
 * Inline functions
 */

static void
output_memset (cobc_tree x, char c, int size)
{
  output_prefix ();
  output ("memset (");
  output_location (x);
  output (", %d, %d);\n", c, size);
}

static void
output_memcpy (cobc_tree x, char *s, int size)
{
  output_prefix ();
  output ("memcpy (");
  output_location (x);
  output (", ");
  output_quoted_string (s, size);
  output (", %d);\n", size);
}


/*
 * Expression
 */

static void
output_expr (cobc_tree x, int id)
{
#ifdef COB_DEBUG
  if (!is_numeric (x))
    {
      printf ("output_expr: invalid expr: %s\n", tree_to_string (x));
      abort ();
    }
#endif

  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_expr:
      {
	struct cobc_expr *p = COBC_EXPR (x);
	output_expr (p->left, id);
	output_expr (p->right, id + 1);
	output_prefix ();
	switch (p->op)
	  {
	  case '+': output ("cob_decimal_add"); break;
	  case '-': output ("cob_decimal_sub"); break;
	  case '*': output ("cob_decimal_mul"); break;
	  case '/': output ("cob_decimal_div"); break;
	  case '^': output ("cob_decimal_pow"); break;
	  }
	output (" (cob_d%d, cob_d%d);\n", id, id + 1);
	break;
      }
    case cobc_tag_integer:
      {
	output_line ("cob_decimal_set_int (cob_d%d, %d, 0);",
		     id, COBC_INTEGER (x)->val);
	break;
      }
    case cobc_tag_literal:
      {
	struct cobc_literal *p = COBC_LITERAL (x);
	if (p->size < 10)
	  output_line ("cob_decimal_set_int (cob_d%d, %lld, %d);",
		       id, literal_to_int (p), p->decimals);
	else
	  output_line ("cob_decimal_set_int64 (cob_d%d, %lldLL, %d);",
		       id, literal_to_int (p), p->decimals);
	break;
      }
    default:
      {
	if (COBC_FIELD_P (x) && COBC_FIELD (x)->usage == USAGE_BINARY)
	  {
	    output_prefix ();
	    if (COBC_FIELD (x)->size <= 4)
	      output ("cob_decimal_set_int");
	    else
	      output ("cob_decimal_set_int64");
	    output (" (cob_d%d, ", id);
	    output_index (x);
	    output (", %d);\n", COBC_FIELD (x)->pic->decimals);
	  }
	else
	  {
	    if (cobc_failsafe_flag && !COBC_CONST_P (x))
	      output_call_1 ("cob_check_numeric", x);
	    output_prefix ();
	    output ("cob_decimal_set (cob_d%d, ", id);
	    output_tree (x);
	    output (");\n");
	  }
      }
    }
}


/*
 * Assignment
 */

static void
output_native_assign (cobc_tree x, long long val)
{
  output_prefix ();
  output_index (x);
  output (" = %lldLL;\n", val);
}

static void
output_assign (struct cobc_assign *p)
{
  if (COBC_FIELD (p->field)->usage == USAGE_INDEX)
    {
      /* Index */
      output_prefix ();
      output_index (p->field);
      output (" = ");
      output_index (p->value);
      output (";\n");
      return;
    }

  if (COBC_EXPR_P (p->value))
    {
      struct cobc_expr *e = COBC_EXPR (p->value);
      if (e->left == p->field
	  && (e->op == '+' || e->op == '-')
	  && !COBC_EXPR_P (e->right))
	{
	  /* X = X +/- Y */
	  cobc_tree rounded = p->rounded ? cobc_int1 : cobc_int0;
	  switch (COBC_TREE_TAG (e->right))
	    {
	    case cobc_tag_integer:
	      {
		char *func = (e->op == '+') ? "cob_add_int" : "cob_sub_int";
		output_call_4 (func, p->field, e->right, cobc_int0, rounded);
		break;
	      }
	    case cobc_tag_literal:
	      {
		struct cobc_literal *l = COBC_LITERAL (e->right);
		output_prefix ();
		if (l->size < 10)
		  {
		    if (e->op == '+')
		      output ("cob_add_int (");
		    else
		      output ("cob_sub_int (");
		    output_tree (p->field);
		    output (", %d, %d, %d);\n", (int) literal_to_int (l),
			    l->decimals, p->rounded);
		  }
		else
		  {
		    if (e->op == '+')
		      output ("cob_add_int64 (");
		    else
		      output ("cob_sub_int64 (");
		    output_tree (p->field);
		    output (", %lldLL, %d, %d);\n", literal_to_int (l),
			    l->decimals, p->rounded);
		  }
		break;
	      }
	    default:
	      {
		char *func = (e->op == '+') ? "cob_add" : "cob_sub";
		output_call_3 (func, p->field, e->right, rounded);
		break;
	      }
	    }
	  return;
	}
    }

  /* default */
  output_expr (p->value, 1);
  output_prefix ();
  if (p->rounded)
    output ("cob_decimal_get_rounded");
  else
    output ("cob_decimal_get");
  output (" (cob_d1, ");
  output_tree (p->field);
  output (");\n");
}


/*
 * Condition
 */

static void
output_compare (cobc_tree s1, int op, cobc_tree s2)
{
  output ("(");

  if (COBC_INDEX_NAME_P (s1) || COBC_INDEX_NAME_P (s2) || s1 == cobc_status)
    {
      output_func_2 ("cob_cmp", make_index (s1), make_index (s2));
    }
  else if (is_numeric (s1) && is_numeric (s2))
    {
      /* numeric comparison */
      output_indent ("({", 2);
      output_expr (s1, 1);
      output_expr (s2, 2);
      output_line ("cob_decimal_cmp (cob_d1, cob_d2);");
      output_indent ("})", -2);
    }
  else if (COBC_CONST_P (s1) || COBC_CONST_P (s2))
    {
      /* non-numeric figurative comparison */
      unsigned char c, *neg;
      cobc_tree x, y;
      if (COBC_CONST_P (s2))
	x = s1, y = s2, neg = "";
      else
	x = s2, y = s1, neg = "-";
      c = ((y == cobc_zero) ? '0' :
	   (y == cobc_space) ? ' ' :
	   (y == cobc_low) ? '\0' :
	   (y == cobc_high) ? '\xff' :
	   (y == cobc_quote) ? '\"' : 0);
      output ("%scob_cmp_all (", neg);
      output_location (x);
      output (", %d, ", c);
      output_length (x);
      output (")");
    }
  else if (COBC_LITERAL_P (s1) || COBC_LITERAL_P (s2))
    {
      /* non-numeric literal comparison */
      cobc_tree x = COBC_LITERAL_P (s1) ? s2 : s1;
      cobc_tree y = COBC_LITERAL_P (s1) ? s1 : s2;
      if (COBC_LITERAL_P (s1))
	output ("-");
      output ("cob_cmp_str (");
      output_tree (x);
      output (", ");
      output_location (y);
      output (", ");
      output_length (y);
      output (")");
    }
  else
    {
      /* non-numeric comparison */
      output ("cob_str_cmp (");
      output_tree (s1);
      output (", ");
      output_tree (s2);
      output (")");
    }

  switch (op)
    {
    case COBC_COND_EQ: output (" == 0"); break;
    case COBC_COND_LT: output (" <  0"); break;
    case COBC_COND_LE: output (" <= 0"); break;
    case COBC_COND_GT: output (" >  0"); break;
    case COBC_COND_GE: output (" >= 0"); break;
    case COBC_COND_NE: output (" != 0"); break;
    }

  output (")");
}

static void
output_condition (cobc_tree x)
{
  enum cobc_cond_type type = COND_TYPE (x);
  cobc_tree l = COND_LEFT (x);
  cobc_tree r = COND_RIGHT (x);

  switch (type)
    {
    case COBC_COND_CLASS:
      output ("%s (", (char *) r);
      output_tree (l);
      output (")");
      break;
    case COBC_COND_NOT:
      output ("!");
      output_condition (l);
      break;
    case COBC_COND_AND:
    case COBC_COND_OR:
      output ("(");
      output_condition (l);
      output (type == COBC_COND_AND ? " && " : " || ");
      output_condition (r);
      output (")");
      break;
    default:
      output_compare (l, type, r);
      break;
    }
}


/*
 * Recursion
 */

static void
output_recursive (void (*func) (struct cobc_field *), cobc_tree x)
{
  struct cobc_field *p = COBC_FIELD (x);

  if (p->redefines)
    return;

  if (COBC_SUBREF_P (x))
    {
      /* fixed indexes */
      int i = 1;
      struct cobc_list *l;
      output_indent ("{", 2);
      for (l = COBC_SUBREF (x)->subs; l; l = l->next)
	{
	  output_prefix ();
	  output ("int i%d = ", i++);
	  output_index (l->item);
	  output (";\n");
	}
    }
  else if (p->occurs > 1)
    {
      /* begin occurs loop */
      int i = p->indexes;
      output_indent ("{", 2);
      output_line ("int i%d;", i);
      output_line ("for (i%d = 1; i%d <= %d; i%d++)", i, i, p->occurs, i);
      output_indent ("  {", 4);
    }

  /* process output */
  func (p);

  /* close loop */
  if (COBC_SUBREF_P (x))
    {
      output_indent ("}", -2);
    }
  else if (p->occurs > 1)
    {
      output_indent ("  }", -4);
      output_indent ("}", -2);
    }
}


/*
 * Field definition
 */

static char
get_type (struct cobc_field *p)
{
  int type = p->category;
  if (p->usage == USAGE_BINARY || p->usage == USAGE_INDEX)
    type = 'B';
  else if (p->usage == USAGE_PACKED)
    type = 'C';
  return type;
}

static void
output_field_definition (struct cobc_field *p, struct cobc_field *p01,
			 int linkage)
{
  char *subscripts;

  /* descriptor */
  if (p->f.used && !COBC_FILLER_P (COBC_TREE (p)))
    {
      output ("static struct cob_field_desc f_%s_desc = ", p->cname);
      if (p->children || p->rename_thru)
	{
	  /* field group */
	  output ("{%d, 'G'};\n", p->size);
	}
      else
	{
	  /* regular field */
	  output ("{%d, '%c', %d, %d, %d, %d, %d, %d, %d, ",
		  p->size, get_type (p), p->pic->digits, p->pic->decimals,
		  p->pic->have_sign, p->f.sign_separate, p->f.sign_leading,
		  p->f.blank_zero, p->f.justified);
	  if (p->category == COB_NUMERIC_EDITED
	      || p->category == COB_ALPHANUMERIC_EDITED)
	    {
	      char *s;
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
  subscripts = field_subscripts (p);
  if (p == p01 && !p->redefines)
    {
      /* level 01 */
      if (field_used_any_child (p) && !linkage)
	{
	  if (p->f.external)
	    {
	      output ("unsigned char %s[%d];\n", p->cname, p->size);
	      output ("#define f_%s_data %s\n", p->cname, p->cname);
	    }
	  else
	    {
	      output ("static unsigned char f_%s_data[%d];\n",
		      p->cname, p->size);
	    }
	}
    }
  else if (p->indexes == 0)
    {
      /* field without subscripts */
      char *p01_cname = p01->redefines ? p01->redefines->cname : p01->cname;
      output ("#define f_%s_data (f_%s_data + %d)\n",
	      p->cname, p01_cname, p->offset);
    }
  else
    {
      /* field with subscripts */
      int i = p->indexes;
      struct cobc_field *f;
      output ("#define f_%s_data%s (f_%s_data + %d",
	      p->cname, subscripts, p01->cname, p->offset);
      for (f = p; f; f = f->parent)
	if (f->f.have_occurs)
	  {
	    if (cobc_failsafe_flag)
	      output (" + cob_index (i%d, %d) * %d", i--, f->occurs, f->size);
	    else
	      output (" + (i%d - 1) * %d", i--, f->size);
	  }
      output (")\n");
    }

  /* macro */
  if (p->f.used && !COBC_FILLER_P (COBC_TREE (p)))
    output ("#define f_%s%s ((struct cob_field) {&f_%s_desc, f_%s_data%s})\n",
	    p->cname, subscripts, p->cname, p->cname, subscripts);
  if (p->usage == USAGE_BINARY || p->usage == USAGE_INDEX)
    {
      output ("#define i_%s%s (*(", p->cname, subscripts);
      switch (p->size)
	{
	case 1: output ("char"); break;
	case 2: output ("short"); break;
	case 4: output ("long"); break;
	case 8: output ("long long"); break;
	}
      output (" *) f_%s_data%s)\n", p->cname, subscripts);
    }

  /* reference modifier */
  if (p->f.referenced)
    {
      output ("#define f_%s_mod%s ({ \\\n", p->cname, subscripts);
      output ("  struct cob_field_desc f_%s_desc = {cob_ref_len, \'X\'}; \\\n",
	      p->cname);
      output ("  ((struct cob_field) ");
      output ("{&f_%s_desc, f_%s_data%s + cob_ref_off}); \\\n",
	      p->cname, p->cname, subscripts);
      output ("})\n");
    }

  if (p->f.used)
    output_newline ();

  /* children */
  for (p = p->children; p; p = p->sister)
    output_field_definition (p, p01, linkage);
}


/*
 * File name
 */

static void
output_file_name (struct cobc_file_name *f)
{
  struct cobc_field *p;

  for (p = f->record; p; p = p->sister)
    output_field_definition (p, p, 0);

  if (f->organization == COB_ORG_INDEXED)
    {
      struct cobc_alt_key *p;
      output ("static struct altkey_desc %s_altkey_descs[] = {\n", f->cname);
      for (p = f->alt_key_list; p; p = p->next)
	output ("  {%d, &f_%s_desc, %d, 0},\n",
		COBC_FIELD (p->key)->offset - f->record->offset,
		COBC_FIELD (p->key)->cname, p->duplicates);
      output ("  {-1, 0, 0, 0}\n");
      output ("};\n");
    }
  if (COBC_LITERAL_P (f->assign))
    {
      output ("static struct cob_field_desc fn_%s_desc = {%d, 'X'};\n",
	      f->cname, COBC_LITERAL (f->assign)->size);
    }
  output ("static struct cob_file_desc %s_desc = {", f->cname);
  if (COBC_LITERAL_P (f->assign))
    {
      output ("&fn_%s_desc, ", f->cname);
      output_quoted_string (COBC_LITERAL (f->assign)->str,
			    COBC_LITERAL (f->assign)->size);
    }
  else
    {
      output ("&f_%s_desc, ", COBC_FIELD (f->assign)->cname);
      output_location (f->assign);
    }
  output (", %d, f_%s_data, %d, %d, ",
	  f->record->size, f->record->cname, f->organization, f->access_mode);
  if (f->status)
    output_location (f->status);
  else
    output ("0");
  output (", 0, 0, 0, %d, 0", f->optional);
  if (f->organization == COB_ORG_INDEXED)
    {
      output (", %d, &f_%s_desc, 0, %s_altkey_descs",
	      COBC_FIELD (f->key)->offset, COBC_FIELD (f->key)->cname,
	      f->cname);
    }
  output ("};\n\n");
}


/*
 * EVALUATE
 */

static void
output_evaluate_test (cobc_tree s, cobc_tree o)
{
  /* extract NOT option */
  if (COND_P (o) && COND_TYPE (o) == COBC_COND_NOT)
    {
      output ("!");
      o = COND_LEFT (o);
    }

  /* ANY is always true */
  if (o == cobc_any)
    {
      output ("1");
      return;
    }

  /* boolean comparison */
  if (COBC_TREE_CLASS (s) == COB_BOOLEAN
      || COBC_TREE_CLASS (o) == COB_BOOLEAN)
    {
      if (COBC_TREE_CLASS (s) != COB_BOOLEAN
	  || COBC_TREE_CLASS (o) != COB_BOOLEAN)
	{
	  yyerror_loc (&COBC_TREE (o)->loc, "type mismatch");
	  output ("0");
	  return;
	}
      output ("(");
      output_tree (s);
      output (" == ");
      output_tree (o);
      output (")");
      return;
    }

  /* x THRU y */
  if (COBC_PAIR_P (o))
    {
      cobc_tree l = COBC_PAIR (o)->x;
      cobc_tree u = COBC_PAIR (o)->y;
      output_condition (make_cond (make_cond (l, COBC_COND_LE, s),
				   COBC_COND_AND,
				   make_cond (s, COBC_COND_LE, u)));
      return;
    }

  /* regular comparison */
  output_condition (make_cond (s, COBC_COND_EQ, o));
}

static void
output_evaluate (struct cobc_evaluate *p)
{
  struct cobc_list *sbjs, *cases, *whens, *objs;

  /* for each case (i.e., WHEN ... WHEN ... statement ...) */
  for (cases = p->case_list; cases; cases = cases->next)
    {
      cobc_tree stmt;
      whens = cases->item;
      stmt = whens->item;
      whens = whens->next;

      /* output a single case */
      output_prefix ();
      if (cases != p->case_list)
	output ("else ");
      if (!whens)
	{
	  /* WHEN OTHER */
	  output ("\n");
	  output_indent_level += 2;
	  output_tree (stmt);
	  output_indent_level -= 2;
	}
      else
	{
	  output ("if (");
	  output_indent_level += 4;
	  /* for each WHEN */
	  for (; whens; whens = whens->next)
	    {
	      output ("(");
	      /* output condition test */
	      for (sbjs = p->subject_list, objs = whens->item;
		   sbjs && objs;
		   sbjs = sbjs->next, objs = objs->next)
		{
		  output_evaluate_test (sbjs->item, objs->item);
		  if (sbjs->next)
		    output (" && ");
		}
	      if (sbjs || objs)
		yyerror ("wrong number of WHEN parameters");
	      output (")");
	      /* connect multiple WHEN's by || */
	      if (whens->next)
		{
		  output_newline ();
		  output_prefix ();
		  output ("|| ");
		}
	    }
	  output (")\n");
	  /* output imperative statemtnt */
	  output_indent_level -= 2;
	  output_tree (stmt);
	  output_indent_level -= 2;
	}
    }
}


/*
 * PERFORM
 */

static void
output_perform_once (struct cobc_perform *p)
{
  if (COBC_SEQUENCE_P (p->body))
    {
      output_tree (p->body);
    }
  else
    {
      static int perform_label = 1;
      struct cobc_pair *pair = COBC_PAIR (p->body);
      struct cobc_label_name *from = COBC_LABEL_NAME (pair->x);
      struct cobc_label_name *until =
	pair->y ? COBC_LABEL_NAME (pair->y) : from;
      output_line ("cob_perform (lp%d, lb_%s, le_%s);",
		   perform_label++, from->cname, until->cname);
    }
}

static void
output_perform_varying (struct cobc_perform *p, struct cobc_perform_varying *v)
{
  cobc_tree init = NULL;
  cobc_tree step = NULL;
  cobc_tree cond;

  /* perform body at the end */
  if (!v)
    {
      output_perform_once (p);
      return;
    }

  /* build operations */
  if (v->name)
    {
      init = make_call_2 (COB_MOVE, v->from, v->name);
      step = make_op_assign (v->name, '+', v->by);
    }
  cond = make_unary_cond (v->until, COBC_COND_NOT);

  /* initialize */
  if (init)
    output_tree (init);

  /* loop */
  output_prefix ();
  if (p->test == COBC_BEFORE)
    {
      output ("while (");
      output_condition (cond);
      output (")\n");
    }
  else
    {
      output ("do\n");
    }
  output_indent ("  {", 4);

  /* body */
  output_perform_varying (p, v->next);
  if (step)
    output_tree (step);

  /* end loop */
  output_indent ("  }", -4);
  if (p->test == COBC_AFTER)
    {
      output ("while (");
      output_condition (cond);
      output (");\n");
    }
}

static void
output_perform (struct cobc_perform *p)
{
  switch (p->type)
    {
    case COBC_PERFORM_EXIT:
      output_line ("cob_exit (le_%s);", COBC_LABEL_NAME (p->data)->cname);
      break;
    case COBC_PERFORM_ONCE:
      output_perform_once (p);
      break;
    case COBC_PERFORM_TIMES:
      output_indent ("{", 2);
      output_prefix ();
      output ("int i, n = ");
      output_index (p->data);
      output (";\n");
      output_line ("for (i = 0; i < n; i++)");
      output_indent ("  {", 4);
      output_perform_once (p);
      output_indent ("  }", -4);
      output_indent ("}", -2);
      break;
    case COBC_PERFORM_UNTIL:
      output_perform_varying (p, p->varying);
      break;
    }
}


/*
 * CALL
 */

#include "inline.c"

#undef COBC_DEFINE_FUNCTION
#undef COBC_DEFINE_INLINE
#define COBC_DEFINE_FUNCTION(tag,name,argc) {argc, name, 0},
#define COBC_DEFINE_INLINE(tag,func,argc) {argc, 0, func},
struct {
  int argc;
  const char *name;
  void (*func) ();
} function_table[] = {
#include "functions.h"
  {0, 0, 0}
};

static void
output_call (struct cobc_call *p)
{
  const char *name;
  void (*func)();

#ifdef COB_DEBUG
  /* check the number of arguments */
  if (p->argc != function_table[p->tag].argc)
    {
      puts ("output_call: argc does not match");
      abort ();
    }
#endif

  name = function_table[p->tag].name;
  func = function_table[p->tag].func;

  if (func)
    /* call inline function if exists */
    switch (p->argc)
      {
      case 0: func (); break;
      case 1: func (p->argv[0]); break;
      case 2: func (p->argv[0], p->argv[1]); break;
      case 3: func (p->argv[0], p->argv[1], p->argv[2]); break;
      case 4: func (p->argv[0], p->argv[1], p->argv[2], p->argv[3]); break;
      case 5: func (p->argv[0], p->argv[1], p->argv[2], p->argv[3], p->argv[4]); break;
      }
  else
    {
      /* regular function call */
      switch (p->argc)
	{
	case 0: output_call_0 (name); break;
	case 1: output_call_1 (name, p->argv[0]); break;
	case 2: output_call_2 (name, p->argv[0], p->argv[1]); break;
	case 3: output_call_3 (name, p->argv[0], p->argv[1], p->argv[2]); break;
	case 4: output_call_4 (name, p->argv[0], p->argv[1], p->argv[2], p->argv[3]); break;
	case 5: output_call_5 (name, p->argv[0], p->argv[1], p->argv[2], p->argv[3], p->argv[4]); break;
	}
    }
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
output_value (struct cobc_field *p)
{
  if (!field_used_any_child (p) && !field_used_any_parent (p))
    return;

  if (p->value)
    {
      switch (COBC_TREE_TAG (p->value))
	{
	case cobc_tag_const:
	  {
	    output_move (p->value, COBC_TREE (p));
	    break;
	  }
	case cobc_tag_literal:
	  {
	    if (COBC_TREE_CLASS (p->value) == COB_NUMERIC
		|| COBC_LITERAL (p->value)->all)
	      {
		/* numeric literal or ALL literal */
		output_move (p->value, COBC_TREE (p));
	      }
	    else
	      {
		/* non-numeric literal */
		/* We do not use output_move here because
		   we do not want the value to be edited. */
		char buff[p->size];
		char *str = COBC_LITERAL (p->value)->str;
		int size = COBC_LITERAL (p->value)->size;
		if (size >= p->size)
		  {
		    memcpy (buff, str, p->size);
		  }
		else
		  {
		    memcpy (buff, str, size);
		    memset (buff + size, ' ', p->size - size);
		  }
		output_memcpy (COBC_TREE (p), buff, p->size);
	      }
	    break;
	  }
	}
    }
  else
    {
      for (p = p->children; p; p = p->sister)
	if (have_value (p))
	  output_recursive (output_value, COBC_TREE (p));
    }
}


/*
 * Output tree
 */

static void
output_tree (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
    case cobc_tag_integer:
      {
	output_index (x);
	break;
      }
    case cobc_tag_index:
      {
	output_index (COBC_INDEX (x)->val);
	break;
      }
    case cobc_tag_literal:
      {
	struct cobc_literal *p = COBC_LITERAL (x);
	if (p->sign)
	  {
	    struct cob_field_desc src_desc =
	      {p->size, COBC_TREE_CLASS (p), p->size, p->decimals};
	    struct cob_field src_fld = {&src_desc, p->str};
	    src_desc.have_sign = 1;
	    put_sign (src_fld, (p->sign < 0) ? 1 : 0);
	  }
	output ("({ struct cob_field_desc desc = {%d, '%c', %d, %d, %d}; ",
		p->size, COBC_TREE_CLASS (p),
		p->size, p->decimals, p->sign ? 1 : 0);
	output ("(struct cob_field) {&desc, ");
	output_quoted_string (p->str, p->size);
	output ("}; })");
	break;
      }
    case cobc_tag_field:
      {
	output ("f_%s", COBC_FIELD (x)->cname);
	break;
      }
    case cobc_tag_subref:
      {
	struct cobc_subref *p = COBC_SUBREF (x);
	output_tree (p->field);
	output_subscripts (x);
	break;
      }
    case cobc_tag_refmod:
      {
	struct cobc_refmod *p = COBC_REFMOD (x);
	/* macro */
	if (p->length)
	  output ("cob_ref");
	else
	  output ("cob_ref_rest");
	output (" (f_%s_mod", COBC_FIELD (p->field)->cname);
	/* subscripts */
	output_subscripts (p->field);
	output (", ");
	/* offset */
	output_index (p->offset);
	output (", ");
	/* length */
	if (p->length)
	  output_index (p->length);
	else
	  output ("%d", COBC_FIELD (p->field)->size);
	output (")");
	break;
      }
    case cobc_tag_file_name:
      {
	struct cobc_file_name *p = COBC_FILE_NAME (x);
	output ("&%s_desc", p->cname);
	break;
      }
    case cobc_tag_label_name:
      {
	struct cobc_label_name *p = COBC_LABEL_NAME (x);
	output_newline ();
	output_line ("lb_%s:", p->cname);
	break;
      }
    case cobc_tag_expr:
      {
	output_expr (x, 1);
	break;
      }
    case cobc_tag_cond:
      {
	output_condition (x);
	break;
      }
    case cobc_tag_if:
      {
	struct cobc_if *p = COBC_IF (x);
	output_prefix ();
	output ("if (");
	output_tree (p->test);
	output (")\n");
	if (p->stmt1)
	  {
	    output_indent_level += 2;
	    output_tree (p->stmt1);
	    output_indent_level -= 2;
	  }
	else
	  output_line ("  /* nothing */;");
	if (p->stmt2)
	  {
	    output_line ("else");
	    output_indent_level += 2;
	    output_tree (p->stmt2);
	    output_indent_level -= 2;
	  }
	break;
      }
    case cobc_tag_evaluate:
      {
	output_evaluate (COBC_EVALUATE (x));
	break;
      }
    case cobc_tag_assign:
      {
	output_assign (COBC_ASSIGN (x));
	break;
      }
    case cobc_tag_call:
      {
	output_line_directive (x);
	output_call (COBC_CALL (x));
	break;
      }
    case cobc_tag_sequence:
      {
	struct cobc_sequence *p = COBC_SEQUENCE (x);
	struct cobc_list *l = p->list;
	output_indent ("{", 2);
	if (!l)
	  {
	    /* nothing */
	  }
	else if (!p->save_status)
	  {
	    /* output without using cob_status */
	    for (; l; l = l->next)
	      output_tree (l->item);
	  }
	else if (!l->next)
	  {
	    /* output with initializing cob_status */
	    output_line ("cob_status = 0;");
	    output_tree (l->item);
	  }
	else
	  {
	    /* output with combining multiple cob_status */
	    output_line ("int status = 0;");
	    for (; l; l = l->next)
	      {
		output_line ("cob_status = 0;");
		output_tree (l->item);
		output_line ("status |= cob_status;");
	      }
	    output_line ("cob_status = status;");
	  }
	output_indent ("}", -2);
	break;
      }
    case cobc_tag_perform:
      {
	output_perform (COBC_PERFORM (x));
	break;
      }
    default:
      {
	printf ("output_tree: unknown tree type: %d\n", COBC_TREE_TAG (x));
	abort ();
      }
    }
}


void
codegen (struct program_spec *spec)
{
  struct cobc_list *l;
  struct cobc_field *p;

  output ("/* Generated from %s by OpenCOBOL %s */\n\n",
	  cobc_source_file, COBC_VERSION);
  output ("#include <stdio.h>\n");
  output ("#include <stdlib.h>\n");
  output ("#include <string.h>\n");
  output ("#include <libcob.h>\n\n");

  if (!cobc_module_flag)
    spec->initial_program = 1;

  /* fields */
  if (spec->working_storage)
    output ("/* Fields */\n\n");
  for (p = spec->working_storage; p; p = p->sister)
    output_field_definition (p, p, 0);
  for (p = spec->linkage_storage; p; p = p->sister)
    output_field_definition (p, p, 1);
  for (l = spec->index_list; l; l = l->next)
    {
      struct cobc_field *p = l->item;
      output ("static int i_%s;\n", p->cname);
    }

  /* files */
  if (spec->file_name_list)
    output ("/* Files */\n\n");
  for (l = spec->file_name_list; l; l = l->next)
    output_file_name (l->item);

  /* labels */
  output ("/* Labels */\n\n");
  output ("enum {\n");
  output ("  COB_INITIAL_LABEL,\n");
  for (l = spec->label_list; l; l = l->next)
    output ("  le_%s,\n", COBC_LABEL_NAME (l->item)->cname);
  output ("};\n\n");

  /* classes */
  for (l = spec->class_list; l; l = l->next)
    {
      struct cobc_class *p = l->item;
      output_line ("static int");
      output_line ("%s (struct cob_field x)", p->cname);
      output_indent ("{", 2);
      output_prefix ();
      output ("return ");
      output_condition (p->cond);
      output (";\n");
      output_indent ("}", -2);
      output_newline ();
    }

  /* environment */
  output_line ("static void");
  output_line ("init_environment (void)");
  output_indent ("{", 2);
  output_line ("cob_source_file = \"%s\";", cobc_source_file);
  output_line ("cob_decimal_point = '%c';", cob_decimal_point);
  output_line ("cob_currency_symbol = '%c';", cob_currency_symbol);
  output_line ("cob_reloading_flag = %d;", cob_reloading_flag);
  output_indent ("}", -2);
  output_newline ();

  /* initial values */
  output_line ("static void");
  output_line ("init_values (void)");
  output_indent ("{", 2);
  for (p = spec->working_storage; p; p = p->sister)
    if (have_value (p))
      output_recursive (output_value, COBC_TREE (p));
  output_indent ("}", -2);
  output_newline ();

  /* main function */
  if (cobc_module_flag)
    {
      output_line ("int");
      output ("%s (", spec->program_id);
      for (l = spec->using_list; l; l = l->next)
	{
	  output ("unsigned char *f_%s_data", COBC_FIELD (l->item)->cname);
	  if (l->next)
	    output (", ");
	}
      output (")\n");
    }
  else
    {
      output_line ("int");
      output_line ("main (int argc, char **argv)");
    }
  output_indent ("{", 2);

  /* local variables */
  if (!spec->initial_program)
    output_line ("static int initialized = 0;\n");
  output_line ("int frame_index;");
  output_line ("struct cob_frame frame_stack[24];");
  output_newline ();

  if (!cobc_module_flag)
    output_line ("cob_init (argc, argv);");
  output_line ("init_environment ();");
  output_newline ();

  /* initial values */
  output_line ("/* initial values */");
  if (!spec->initial_program)
    {
      output_line ("if (!initialized)");
      output_indent ("  {", 4);
    }
  output_line ("init_values ();");
  if (!spec->initial_program)
    {
      output_line ("initialized = 1;");
      output_indent ("  }", -4);
    }
  output_newline ();

  /* frame stack */
  output_line ("/* initialize frame stack */");
  output_line ("frame_index = 0;");
  output_line ("frame_stack[0].perform_through = COB_INITIAL_LABEL;");
  output_newline ();

  output_line ("/* PROCEDURE DIVISION */");
  for (l = spec->exec_list; l; l = l->next)
    output_tree (l->item);
  output_newline ();

  output_line ("return 0;");
  output_indent ("}", -2);
}
