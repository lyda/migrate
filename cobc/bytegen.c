/*
 * Copyright (C) 2003 Keisuke Nishida
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

static void output_integer (cb_tree x);
static void output_index (cb_tree x);


/*
 * Output routine
 */

static void
output (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (yyout, fmt, ap);
  va_end (ap);
}

static void
output_newline (void)
{
  fputs ("\n", yyout);
}

static void
output_line (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (yyout, fmt, ap);
  output_newline ();
  va_end (ap);
}

static void
output_int (int i)
{
  output_line ("\tipush\t%d", i);
}

static void
output_add (void)
{
  output_line ("\tadd");
}

static void
output_sub (void)
{
  output_line ("\tsub");
}

static void
output_mul (void)
{
  output_line ("\tmul");
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

  output_line ("\tbpush\tb%s", f01->cname);
}

static void
output_offset (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	int i = f->indexes;

	/* base offset */
	output_int (f->offset);

	/* subscripts */
	for (; f; f = f->parent)
	  if (f->flag_occurs)
	    {
	      output_int (f->size);
	      output_line ("\tlpush\ti%d", i--);
	      output_mul ();
	      output_add ();
	    }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);

	/* base offset */
	output_int (f->offset);

	/* subscripts */
	if (r->subs)
	  {
	    cb_tree l = r->subs = list_reverse (r->subs);

	    for (; f; f = f->parent)
	      if (f->flag_occurs)
		{
		  output_int (f->size);
		  output_index (CB_VALUE (l));
		  output_mul ();
		  output_add ();
		  l = CB_CHAIN (l);
		}

	    r->subs = list_reverse (r->subs);
	  }

	/* offset */
	if (r->offset)
	  {
	    output_index (r->offset);
	    output_add ();
	  }
	break;
      }
    default:
      abort ();
    }
}

static void
output_length (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	output_int (f->size);
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
	    output_int (f->size);
	    output_index (r->offset);
	    output_sub ();
	  }
	else
	  {
	    struct cb_field *p = cb_field_varying (f);
	    if (p && (r->type == CB_SENDING_OPERAND
		      || !cb_field_subordinate (cb_field (p->occurs_depending), f)))
	      {
		output_int (p->offset - f->offset);
		output_int (p->size);
		output_integer (p->occurs_depending);
		output_mul ();
		output_add ();
	      }
	    else
	      output_int (f->size);
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

static void
output_attr_1 (char type, char digits, char expt, char flags, char *pic)
{
  output ("\tapush\t%d:%d:%d:%d:", type, digits, expt, flags);
  if (pic)
    {
      unsigned char *s;
      for (s = pic; *s; s += 2)
	output ("%c(%d)", s[0], s[1]);
    }
  else
    output ("null");
  output_newline ();
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
	    output_attr_1 (COB_TYPE_NUMERIC_DISPLAY,
			   l->size, l->expt, flags, 0);
	  }
	else
	  {
	    if (l->all)
	      output_attr_1 (COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, 0);
	    else
	      output_attr_1 (COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0);
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
	    output_attr_1 (COB_TYPE_GROUP, 0, 0, 0, 0);
	    break;
	  case COB_TYPE_ALPHANUMERIC:
	    if (f->flag_justified)
	      output_attr_1 (COB_TYPE_ALPHANUMERIC, 0, 0, COB_FLAG_JUSTIFIED, 0);
	    else
	      output_attr_1 (COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0);
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

	      output_attr_1 (type, f->pic->digits, f->pic->expt,
			     flags, f->pic->str);
	      break;
	    }
	  }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	if (r->offset)
	  output_attr_1 (COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0);
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
  output_line ("\tnew\tField");
  output_base (cb_field (x));
  output_offset (x);
  output_length (x);
  output_attr (x);
  output_line ("\tinit\tField");
}


static void
output_integer (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      if (x == cb_zero)
	output_int (0);
      else
	abort ();
      break;
    case CB_TAG_INTEGER:
      output_int (CB_INTEGER (x)->val);
      break;
    case CB_TAG_LITERAL:
      output_int (cb_literal_to_int (CB_LITERAL (x)));
      break;
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	output_integer (p->x);
	output_integer (p->y);
	if (p->op == '+')
	  output_add ();
	else
	  output_sub ();
	break;
      }
    default:
      {
	struct cb_field *f = cb_field (x);
	if (f->usage == CB_USAGE_INDEX)
	  {
	    if (f->level == 0)
	      {
		output_line ("i_%s", f->cname);
	      }
	    else
	      {
		output_line ("(*(int *) (");
		output_offset (x);
		output_line ("))");
	      }
	    return;
	  }

	// output_func_1 ("cob_get_int", x);
	abort ();
	break;
      }
    }
}

static void
output_index (cb_tree x)
{
  output_integer (x);
  output_int (1);
  output_sub ();
}

static void
output_param (cb_tree x)
{
  if (x == NULL)
    {
      output_int (0);
      return;
    }

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      output_line ("\txpush\t%s", CB_CONST (x)->val);
      break;
    case CB_TAG_INTEGER:
      output_integer (x);
      break;
    case CB_TAG_STRING:
      output_line ("\tspush\t\"%s\"", CB_STRING (x)->str);
      break;
    case CB_TAG_CAST:
      switch (CB_CAST (x)->type)
	{
	case CB_CAST_INTEGER:
	  output_integer (CB_CAST (x)->val);
	  break;
	default:
	  abort ();
	}
      break;
#if 0
    case CB_TAG_DECIMAL:
      output ("&d[%d]", CB_DECIMAL (x)->id);
      break;
    case CB_TAG_FILE:
      output ("&%s", CB_FILE (x)->cname);
      break;
#endif
    case CB_TAG_LITERAL:
      output_line ("\tnew\tField");
      output_line ("\tspush\t\"%s\"", CB_LITERAL (x)->data);
      output_line ("\tinit\tLiteral");
      break;
    case CB_TAG_FIELD:
    case CB_TAG_REFERENCE:
      output_field (x);
      break;
    default:
      abort ();
    }
}


/*
 * Function call
 */

static struct inline_func {
  const char *name;
  void (*func) ();
} inline_table[] = {
//  {"@handler", output_handler},
//  {"@memcmp", output_memcmp},
//  {"@goto", output_goto},
//  {"@goto-depending", output_goto_depending},
//  {"@exit-program", output_exit_program},
//  {"@initialize", output_initialize},
//  {"@search", output_search},
//  {"@search-all", output_search_all},
//  {"@sort-init", output_sort_init},
//  {"@call", output_call},
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
      for (i = 0; i < p->argc; i++)
	output_param (p->argv[i]);
      output_line ("\tcall\t%s", p->name);
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
	    output_line ("; %s:%d: %s",
			 x->source_file, x->source_line, p->name);
	    if (cb_flag_source_location)
	      output_line (".source\t\"%s\" %d",
			   x->source_file, x->source_line);
	    last_line = x->source_line;
	  }
	break;
      }
    case CB_TAG_LABEL:
      {
	struct cb_label *p = CB_LABEL (x);
	output_newline ();
	output_line ("; %s:", p->name);
	if (p->need_begin)
	  output_line ("%s:", p->cname);
	break;
      }
    case CB_TAG_FUNCALL:
      {
	output_funcall (CB_FUNCALL (x));
	break;
      }
    case CB_TAG_SEQUENCE:
      {
	struct cb_sequence *p = CB_SEQUENCE (x);
	cb_tree l = p->list;
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
	break;
      }
    default:
      abort ();
    }
}


static void
output_internal_method (struct cb_program *prog, cb_tree parameter_list)
{
  cb_tree l;

  output_line (".method\t%s", prog->program_id);

  /* PROCEDURE DIVISION */
  for (l = prog->exec_list; l; l = CB_CHAIN (l))
    output_stmt (CB_VALUE (l));

  output_line ("\treturn");
}

void
bytegen (struct cb_program *prog)
{
  cb_tree l;
  cb_tree parameter_list = NULL;
  struct cb_field *f;

  if (cb_flag_main)
    prog->flag_initial = 1;

  output_line ("; Generated from %s by cobc %s\n",
	       cb_source_file, PACKAGE_VERSION);
  output_line (".file\t\"%s\"", cb_source_file);
  output_line (".class\t%c%s",
	       toupper (prog->program_id[0]), prog->program_id + 1);

  output_line (".static");
  for (f = prog->working_storage; f; f = f->sister)
    output_line ("\tbase\tb%s %d", f->cname, f->memory_size);
  output_line ("\treturn");
  output_newline ();

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

  /* internal method */
  output_internal_method (prog, parameter_list);

  /* main method */
  if (cb_flag_main)
    output_line (".main");
}