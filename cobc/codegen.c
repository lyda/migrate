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
output_quoted_string (char *s)
{
  output ("\"");
  for (; *s; s++)
    {
      int c = *s;
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
//  if (x->loc.file)
//    {
//      output ("#line %d \"%s\"\n", x->loc.line, x->loc.file);
//      output_line ("cob_source_line = %d;", x->loc.line);
//    }
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
  output ("f_%s_data", COBC_FIELD (x)->cname);
  output_subscripts (x);
  if (COBC_REFMOD_P (x))
    {
      output (" + ");
      output_index (COBC_REFMOD (x)->offset);
    }
}

static void
output_length (cobc_tree x)
{
  if (COBC_REFMOD_P (x))
    output_index (COBC_REFMOD (x)->length);
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
      if (x == cobc_zero)
	output ("0");
      else if (x == cobc_status)
	output ("cob_status");
      else
	yyerror ("non-numeric value");
      break;

    case cobc_tag_integer:
      output ("%d", COBC_INTEGER (x)->val);
      break;

    case cobc_tag_literal:
      output ("%lld", literal_to_int (COBC_LITERAL (x)));
      break;

    case cobc_tag_expr:
      while (COBC_EXPR_P (x))
	{
	  output_index (COBC_EXPR (x)->left);
	  output (" %c ", COBC_EXPR (x)->op);
	  x = COBC_EXPR (x)->right;
	}
      output_index (x);
      break;

    default:
      {
	struct cobc_field *p = COBC_FIELD (x);
	switch (p->usage)
	  {
	  case USAGE_DISPLAY:
	  case USAGE_PACKED:
	    output ("get_index (");
	    output_tree (x);
	    output (")");
	    break;
	  case USAGE_BINARY:
	  case USAGE_INDEX:
	    output ("(*");
	    switch (p->size)
	      {
	      case 1: output ("(char *)"); break;
	      case 2: output ("(short *)"); break;
	      case 4: output ("(long *)"); break;
	      case 8: output ("(long long *)"); break;
	      }
	    output_location (x);
	    output (")");
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
output_memset (cobc_tree x, char c)
{
  output_prefix ();
  output ("memset (");
  output_location (x);
  output (", %d, %d);\n", c, COBC_FIELD (x)->size);
}

static void
output_memcpy (cobc_tree x, char *s)
{
  output_prefix ();
  output ("memcpy (");
  output_location (x);
  output (", ");
  output_quoted_string (s);
  output (", %d);\n", strlen (s));
}

static void
output_native_assign (cobc_tree x, long long val)
{
  output_prefix ();
  output_index (x);
  output (" = %lldLL;\n", val);
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


/*
 * Expression
 */

static void
output_expr (cobc_tree x)
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
	output_expr (p->left);
	output_expr (p->right);
	switch (p->op)
	  {
	  case '+': output_call_0 ("cob_num_add"); break;
	  case '-': output_call_0 ("cob_num_sub"); break;
	  case '*': output_call_0 ("cob_num_mul"); break;
	  case '/': output_call_0 ("cob_num_div"); break;
	  case '^': output_call_0 ("cob_num_pow"); break;
	  }
	break;
      }
    case cobc_tag_integer:
      output_line ("cob_push_int (%d, 0);", COBC_INTEGER (x)->val);
      break;
    case cobc_tag_literal:
      {
	struct cobc_literal *p = COBC_LITERAL (x);
	if (p->size < 10)
	  output_line ("cob_push_int (%lld, %d);",
		       literal_to_int (COBC_LITERAL (x)), p->decimals);
	else
	  output_line ("cob_push_str (\"%s%s\", %d);",
		       (p->sign < 0) ? "-" : "", p->str, p->decimals);
	break;
      }
    default:
      output_call_1 ("cob_push_decimal", x);
    }
}


/*
 * Condition
 */

static void
output_compare_zero (cobc_tree s1, cobc_tree s2)
{
  cobc_tree x = (s1 == cobc_zero) ? s2 : s1;
  if (s1 == cobc_zero)
    output ("-");
  output ("cob_cmp_zero (");
  output_location (x);
  output (", ");
  output_length (x);
  output (")");
}

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
      output ("({");
      output_expr (s1);
      output_expr (s2);
      output_call_0 ("cob_num_cmp");
      output ("})");
    }
  else if (s1 == cobc_zero || s2 == cobc_zero)
    {
      output_compare_zero (s1, s2);
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

void
output_condition (cobc_tree x)
{
  cobc_tree l, r;
  enum cobc_cond_type type;

 again:
  l = COND_LEFT (x);
  r = COND_RIGHT (x);
  type = COND_TYPE (x);

  switch (type)
    {
    case COBC_COND_NUMERIC:
      output_func_1 ("cob_is_numeric", l);
      break;
    case COBC_COND_ALPHABETIC:
      output_func_1 ("cob_is_alpha", l);
      break;
    case COBC_COND_LOWER:
      output_func_1 ("cob_is_lower", l);
      break;
    case COBC_COND_UPPER:
      output_func_1 ("cob_is_upper", l);
      break;
    case COBC_COND_POSITIVE:
      x = make_cond (l, COBC_COND_GT, cobc_zero);
      goto again;
    case COBC_COND_NEGATIVE:
      x = make_cond (l, COBC_COND_LT, cobc_zero);
      goto again;
    case COBC_COND_ZERO:
      x = make_cond (l, COBC_COND_EQ, cobc_zero);
      goto again;
    case COBC_COND_CLASS:
      puts ("class-name is not implemented");
      output ("1");
      break;
    case COBC_COND_NOT:
      output ("!");
      x = l;
      goto again;
    case COBC_COND_AND:
    case COBC_COND_OR:
      output ("(");
      output_condition (l);
      if (type == COBC_COND_AND)
	output (" && ");
      else
	output (" || ");
      output_condition (r);
      output (")");
      break;
    default:
      output_compare (l, type, r);
      return;
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
  if (p->children || p->rename_thru)
    {
      /* field group */
      output ("static struct cob_field_desc f_%s_desc = {%d, 'G'};\n",
	      p->cname, p->size);
    }
  else if (!COBC_FILLER_P (COBC_TREE (p)))
    {
      /* regular field */
      char *s;
      output ("static struct cob_field_desc f_%s_desc = ", p->cname);
      output ("{%d, '%c', %d, %d, %d, %d, %d, %d, \"",
	      p->size, get_type (p), p->pic->decimals, p->f.justified,
	      p->pic->have_sign, p->f.sign_separate, p->f.sign_leading,
	      p->f.blank_zero);
      for (s = p->pic->str; *s; s += 2)
	output ("%c\\%03o", s[0], s[1]);
      output ("\"};\n");
    }

  /* data */
  subscripts = field_subscripts (p);
  if (p == p01 && !p->redefines)
    {
      /* level 01 */
      if (linkage)
	/* linkage fields are given as function parameters,
	 * so nothing to be done here */;
      else
	output ("static unsigned char f_%s_data[%d];\n", p->cname, p->size);
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
      /* TODO: check subscripts overflow */
      output ("#define f_%s_data%s (f_%s_data + %d",
	      p->cname, subscripts, p01->cname, p->offset);
      for (f = p; f; f = f->parent)
	if (f->f.have_occurs)
	  output (" + ((i%d) - 1) * %d", i--, f->size);
      output (")\n");
    }

  /* macro */
  if (!COBC_FILLER_P (COBC_TREE (p)))
    output ("#define f_%s%s ((struct cob_field) {&f_%s_desc, f_%s_data%s})\n\n",
	    p->cname, subscripts, p->cname, p->cname, subscripts);

  /* reference modifier */
  if (p->f.referenced)
    {
      output ("#define f_%s_mod%s ({ \\\n", p->cname, subscripts);
      output ("  struct cob_field_desc f_%s_desc = {cob_ref_len, \'X\'}; \\\n",
	      p->cname);
      output ("  ((struct cob_field) ");
      output ("{&f_%s_desc, f_%s_data%s + cob_ref_off}); \\\n",
	      p->cname, p->cname, subscripts);
      output ("})\n\n");
    }

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
      output_quoted_string (COBC_LITERAL (f->assign)->str);
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
output_perform (struct cobc_perform *p)
{
  switch (p->type)
    {
    case COBC_PERFORM_EXIT:
      output_line ("cob_exit_section (le_%s);",
		   COBC_LABEL_NAME (p->cond)->cname);
      break;
    case COBC_PERFORM_ONCE:
      output_perform_once (p);
      break;
    case COBC_PERFORM_TIMES:
      output_indent ("{", 2);
      output_prefix ();
      output ("int i, n = ");
      output_index (p->cond);
      output (";\n");
      output_line ("for (i = 0; i < n; i++)");
      output_indent ("  {", 4);
      output_perform_once (p);
      output_indent ("  }", -4);
      output_indent ("}", -2);
      break;
    case COBC_PERFORM_UNTIL:
      if (p->init)
	output_tree (p->init);
      output_prefix ();
      if (p->test == COBC_BEFORE)
	{
	  output ("while (!");
	  output_condition (p->cond);
	  output (")\n");
	}
      else
	{
	  output ("do\n");
	}
      output_indent ("  {", 4);
      output_perform_once (p);
      if (p->step)
	output_tree (p->step);
      output_indent ("  }", -4);
      if (p->test == COBC_AFTER)
	{
	  output ("while (!");
	  output_condition (p->cond);
	  output (");\n");
	}
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
      }
  else
    {
      /* regular function call */
      switch (p->argc)
	{
	case 0: output_call_0 (name); break;
	case 1: output_call_1 (name, p->argv[0]); break;
	case 2: output_call_2 (name, p->argv[0], p->argv[1]); break;
	case 3: output_call_3 (name, p->argv[0], p->argv[1], p->argv[2]);
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
	    if (COBC_TREE_CLASS (p) == COB_NUMERIC)
	      {
		/* numeric literal */
		output_move (p->value, COBC_TREE (p));
	      }
	    else
	      {
		/* non-numeric literal */
		/* We do not use output_move here because
		   we do not want the value to be edited. */
		char *str = COBC_LITERAL (p->value)->str;
		char buff[p->size + 1];
		int len = strlen (str);
		if (len >= p->size)
		  {
		    memcpy (buff, str, p->size);
		  }
		else
		  {
		    memcpy (buff, str, len);
		    memset (buff + len, ' ', p->size - len);
		  }
		buff[p->size] = 0;
		output_memcpy (COBC_TREE (p), buff);
	      }
	    break;
	  }
	}
    }
  else
    {
      struct cobc_field *c;
      for (c = p->children; c; c = c->sister)
	if (have_value (c))
	  output_recursive (output_value, COBC_TREE (c));
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
      {
	if (x == cobc_true)
	  output ("1");
	else if (x == cobc_false)
	  output ("0");
	else if (x == cobc_zero)
	  output ("COB_ZERO");
	else if (x == cobc_space)
	  output ("COB_SPACE");
	else if (x == cobc_high)
	  output ("COB_HIGH");
	else if (x == cobc_low)
	  output ("COB_LOW");
	else if (x == cobc_quote)
	  output ("COB_QUOTE");
	break;
      }
    case cobc_tag_integer:
      {
	output ("%d", COBC_INTEGER (x)->val);
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
	      {p->size, COBC_TREE_CLASS (p), p->decimals};
	    struct cob_field src_fld = {&src_desc, p->str};
	    src_desc.have_sign = 1;
	    put_sign (src_fld, (p->sign < 0) ? 1 : 0);
	  }
	output ("({ struct cob_field_desc desc = {%d, '%c', %d, 0, %d}; ",
		p->size, COBC_TREE_CLASS (p), p->decimals, p->sign ? 1 : 0);
	output ("(struct cob_field) {&desc, ");
	output_quoted_string (p->str);
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
	output_expr (x);
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
	struct cobc_assign *p = COBC_ASSIGN (x);
	output_tree (p->value);
	if (p->rounded)
	  output_call_1 ("cob_round", p->field);
	output_call_1 ("cob_set", p->field);
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
    output_field_definition (l->item, l->item, 0);

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

  /* environment */
  output_line ("static void");
  output_line ("init_environment (void)");
  output_indent ("{", 2);
  output_line ("cob_source_file = \"%s\";", cobc_source_file);
  output_line ("cob_decimal_point = '%c';", cob_decimal_point);
  output_line ("cob_currency_symbol = '%c';", cob_currency_symbol);
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
