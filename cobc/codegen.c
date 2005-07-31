/*
 * Copyright (C) 2002-2004 Keisuke Nishida
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
#include "call.def"

static int param_id = 0;
static int stack_id = 0;
static int num_cob_fields = 0;
static int loop_counter = 0;
static int progid = 0;
static int last_line = 0;
static int needs_exit_prog = 0;
static int need_double = 0;

int i_counters[8] = {0, 0, 0, 0, 0, 0, 0, 0};
int entry_number = 0;

static void output (const char *fmt, ...)
     __attribute__ ((__format__ (__printf__, 1, 2)));
static void output_line (const char *fmt, ...)
     __attribute__ ((__format__ (__printf__, 1, 2)));
static void output_storage (const char *fmt, ...)
     __attribute__ ((__format__ (__printf__, 1, 2)));

static void output_stmt (cb_tree x);
static void output_integer (cb_tree x);
static void output_index (cb_tree x);
static void output_func_1 (const char *name, cb_tree x);


/*
 * Output routines
 */

static int output_indent_level = 0;
static FILE *output_target;

static struct attr_list {
	struct attr_list	*next;
	int			id;
	char			type;
	char			digits;
	char			scale;
	char			flags;
	unsigned char		*pic;
} *attr_cache = NULL;

static struct literal_list {
	struct literal_list	*next;
	int			id;
	struct cb_literal	*literal;
	cb_tree			x;
} *literal_cache = NULL;

static struct field_list {
	struct field_list	*next;
	struct cb_field		*f;
	cb_tree			x;
} *field_cache = NULL;

static struct call_list {
	struct call_list	*next;
	const char		*callname;
	const char		*callorig;
} *call_cache = NULL;

static void
lookup_call (const char *p, const char *q)
{
	struct call_list	*clp;

	for ( clp = call_cache; clp; clp = clp->next ) {
		if ( strcmp (p, clp->callname) == 0 ) {
			return;
		}
	}
	clp = cob_malloc( sizeof(struct call_list) );
	clp->callname = p;
	clp->callorig = q;
	clp->next = call_cache;
	call_cache = clp;
}

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
  if (output_target) {
    fputs ("\n", output_target);
  }
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

  if (*p == '}' && strcmp(str, "})") != 0)
    output_indent_level -= level;

  output_line (str);

  if (*p == '{' && strcmp(str, ")}") != 0)
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
  char name[CB_MAX_CNAME];

  if (f01->redefines)
    f01 = f01->redefines;

  /* base name */
  if (f01->flag_external)
    {
      char *p;
      strcpy (name, f01->name);
      for (p = name; *p; p++)
	if (*p == '-')
	  *p = '_';
    }
  else
    {
      sprintf (name, "%d", f01->id);
    }

  if (!f01->flag_base)
    {
	if ( !f01->flag_external ) {
		if (!f01->flag_local)
			output_storage ("static ");
#if defined (__GNUC__) && defined (__i386__)
		if ( f01->memory_size > 63 ) {
			output_storage ("unsigned char %s%s[%d]\t__attribute__ ((__aligned__(64)));",
				CB_PREFIX_BASE, name, f01->memory_size);
		} else {
			output_storage ("unsigned char %s%s[%d];",
				CB_PREFIX_BASE, name, f01->memory_size);
		}
#else
		output_storage ("unsigned char %s%s[%d];",
				CB_PREFIX_BASE, name, f01->memory_size);
#endif
		output_storage ("  /* %s */\n", f01->name);
	}
	f01->flag_base = 1;
    }
  output ("%s%s", CB_PREFIX_BASE, name);

  if (cb_field_variable_address (f))
    {
      struct cb_field *p;
      for (p = f->parent; p; f = f->parent, p = f->parent)
	for (p = p->children; p != f; p = p->sister)
	  {
	    struct cb_field *v = cb_field_variable_size (p);
	    if (v)
	      {
		output (" + %d + ", v->offset - p->offset);
		if (v->size != 1)
		  output ("%d * ", v->size);
		output_integer (v->occurs_depending);
	      }
	    else
	      output (" + %d", p->size);
	  }
    }
  else if (f->offset > 0)
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
	  output ("(unsigned char *)\"%s%s\"", l->data,
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
/* Leave in
    case CB_TAG_CAST:
      {
	output("&");
	output_param(x,0);
	break;
      }
*/
    default:
      ABORT ();
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
	    struct cb_field *p = cb_field_variable_size (f);
	    struct cb_field *q = f;

	  again:
	    if (p && (r->type == CB_SENDING_OPERAND
		      || !cb_field_subordinate (cb_field (p->occurs_depending), q)))
	      {
		if (p->offset - q->offset > 0)
		  output ("%d + ", p->offset - q->offset);
		if (p->size != 1)
		  output ("%d * ", p->size);
		output_integer (p->occurs_depending);
		q = p;
	      }
	    else
	      output ("%d", q->size);

	    for (; q != f; q = q->parent)
	      if (q->sister && !q->sister->redefines)
		{
		  q = q->sister;
		  p = q->occurs_depending ? q : cb_field_variable_size (q);
		  output (" + ");
		  goto again;
		}
	  }
	break;
      }
    default:
	fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG(x));
	fflush(stderr);
      ABORT ();
    }
}

static int
lookup_attr (char type, char digits, char scale, char flags, unsigned char *pic)
{
  struct attr_list *l;

  /* search attribute cache */
  for (l = attr_cache; l; l = l->next)
    if (type == l->type
	&& digits == l->digits
	&& scale == l->scale
	&& flags == l->flags
	&& ((pic == l->pic)
	    || (pic && l->pic && strcmp (pic, l->pic) == 0)))
      return l->id;

  /* output new attribute */

  /* cache it */
  l = cob_malloc (sizeof (struct attr_list));
  l->id = cb_id;
  l->type = type;
  l->digits = digits;
  l->scale = scale;
  l->flags = flags;
  l->pic = pic;
  l->next = attr_cache;
  attr_cache = l;

  return cb_id++;
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
			      l->size, l->scale, flags, 0);
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
	int type = cb_tree_type (x);
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
		  {
		    flags |= COB_FLAG_HAVE_SIGN;
		    if (f->flag_sign_separate)
		      flags |= COB_FLAG_SIGN_SEPARATE;
		    if (f->flag_sign_leading)
		      flags |= COB_FLAG_SIGN_LEADING;
		  }
		if (f->flag_blank_zero)
		  flags |= COB_FLAG_BLANK_ZERO;
		if (f->flag_justified)
		  flags |= COB_FLAG_JUSTIFIED;
		if (f->flag_binary_swap)
		  flags |= COB_FLAG_BINARY_SWAP;

		id = lookup_attr (type, f->pic->digits, f->pic->scale,
				  flags, f->pic->str);
		break;
	      }
	    }
	break;
      }
    default:
      ABORT ();
    }

  output ("&%s%d", CB_PREFIX_ATTR, id);
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
  
  struct cb_literal *literal = CB_LITERAL (x);
  struct literal_list *l;

  /* search literal cache */
  for (l = literal_cache; l; l = l->next)
    if (CB_TREE_CLASS (literal) == CB_TREE_CLASS (l->literal)
	&& literal->size == l->literal->size
	&& literal->all  == l->literal->all
	&& literal->sign == l->literal->sign
	&& literal->scale == l->literal->scale
	&& memcmp (literal->data, l->literal->data, literal->size) == 0)
      return l->id;

  /* output new literal */
  output_target = 0;
  output_field (x);

  output_target = yyout;

  /* cache it */
  l = cob_malloc (sizeof (struct literal_list));
  l->id = cb_id;
  l->literal = literal;
  l->x = x;
  l->next = literal_cache;
  literal_cache = l;

  return cb_id++;
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
      else if ( x == cb_null )
	output ("(unsigned char *)NULL");
      else
	output ("%s", CB_CONST (x)->val);
      break;
    case CB_TAG_INTEGER:
      output ("%d", CB_INTEGER (x)->val);
      break;
    case CB_TAG_LITERAL:
      output ("%d", cb_get_int (x));
      break;
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	if (p->op == '^')
	  {
	    output ("(int) pow (");
	    output_integer (p->x);
	    output (", ");
	    output_integer (p->y);
	    output (")");
	  }
	else
	  {
	    output ("(");
	    if ( need_double )
		output("(double)");
	    output_integer (p->x);
	    output (" %c ", p->op);
	    if ( need_double )
		output("(double)");
	    output_integer (p->y);
	    output (")");
	  }
	break;
      }
    case CB_TAG_CAST:
      {
	struct cb_cast *p = CB_CAST (x);
	switch (p->type)
	  {
	  case CB_CAST_ADDRESS:
	    output ("(");
	    output_data (p->val);
	    output (")");
	    break;
	  default:
	    ABORT ();
	  }
	break;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_field *f = cb_field (x);
	switch (f->usage)
	  {
	  case CB_USAGE_INDEX:
	  case CB_USAGE_LENGTH:
	    output ("(*(int *) (");
	    output_data (x);
	    output ("))");
	    return;

	  case CB_USAGE_POINTER:
	    output ("(*(unsigned char **) (");
	    output_data (x);
	    output ("))");
	    return;

	  case CB_USAGE_DISPLAY:
	    if (cb_flag_runtime_inlining
		&& f->pic->scale >= 0
		&& f->size - f->pic->scale <= 4
		&& f->pic->have_sign == 0)
	      {
		int i, j;
		int size = f->size - f->pic->scale;
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
	  case CB_USAGE_COMP_5:
	  case CB_USAGE_COMP_X:
	    if (cb_flag_runtime_inlining
		&& !f->flag_binary_swap
		&& (f->size == 1 || f->size == 2
		    || f->size == 4 || f->size == 8))
	      {
		output ("(*(");
		switch (f->size)
		  {
		  case 1: output ("char"); break;
		  case 2: output ("short"); break;
		  case 4: output ("int"); break;
		  case 8: output ("long long"); break;
		  }
		output (" *) (");
		output_data (x);
		output ("))");
		return;
	      }
	    break;

	  default:
	    break;
	  }

	output_func_1 ("cob_get_int", x);
	break;
      }
    default:
      ABORT ();
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
      output ("%d", cb_get_int (x) - 1);
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
  char fname[8];

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
    case CB_TAG_ALPHABET_NAME:
      {
	struct cb_alphabet_name *p = CB_ALPHABET_NAME (x);
	switch (p->type)
	  {
	  case CB_ALPHABET_NATIVE:
	  case CB_ALPHABET_STANDARD_1:
	  case CB_ALPHABET_STANDARD_2:
	    output ("0");
	    break;
	  case CB_ALPHABET_EBCDIC:
	    output ("cob_a2e");
	    break;
	  case CB_ALPHABET_CUSTOM:
	    output ("%s%s", CB_PREFIX_SEQUENCE, p->cname);
	    break;
	  }
	break;
      }
    case CB_TAG_CAST:
      {
	struct cb_cast *p = CB_CAST (x);
	switch (p->type)
	  {
	  case CB_CAST_INTEGER:
	    output_integer (p->val);
	    break;
	  case CB_CAST_ADDRESS:
	    output_data (p->val);
	    break;
	  case CB_CAST_LENGTH:
	    output_size (p->val);
	    break;
	  }
	break;
      }
    case CB_TAG_DECIMAL:
      output ("&d[%d]", CB_DECIMAL (x)->id);
      break;
    case CB_TAG_FILE:
      output ("&%s%s", CB_PREFIX_FILE, CB_FILE (x)->cname);
      break;
    case CB_TAG_LITERAL:
      output ("&%s%d", CB_PREFIX_CONST, lookup_literal (x));
      break;
    case CB_TAG_FIELD:
      /* TODO: remove me */
      output_param (cb_build_field_reference (CB_FIELD (x), 0), id);
      break;
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);
	int	extrefs = 0;
	struct cb_field *pechk;

	if (r->check)
	  {
	    cb_tree l;
	    output_indent (" ({");
	    for (l = r->check; l; l = CB_CHAIN (l))
	      output_stmt (CB_VALUE (l));
	  }

	for ( pechk = f->parent; pechk; pechk = pechk->parent ) {
		if ( pechk->flag_external ) {
			extrefs = 1;
		}
	}
	if (!r->subs && !r->offset && !f->flag_local && f->count > 0
	    && !f->flag_external && !extrefs
	    && !cb_field_variable_size (f) && !cb_field_variable_address (f))
	  {
	    if (!f->flag_field)
	      {
		struct field_list *l;

		output_target = 0;
		output_field (x);

		l = cob_malloc (sizeof (struct field_list));
		l->x = x;
		l->f = f;
		l->next = field_cache;
		field_cache = l;

		f->flag_field = 1;
		output_target = yyout;
	      }
	    output ("&%s%d", CB_PREFIX_FIELD, f->id);
	  }
	else
	  {
	    if ( id >= num_cob_fields ) {
		num_cob_fields = id + 1;
	    }
/* Worse code
	    output ("(%s = (cob_field) ", fname);
	    output_field (x);
	    output (", &%s)", fname);
*/
	    if ( stack_id >= num_cob_fields ) {
		num_cob_fields = stack_id + 1;
	    }
	    sprintf (fname, "f[%d]", stack_id++);
	    output ("(%s.size = ", fname);
	    output_size (x);
	    output (", %s.data = ", fname);
	    output_data (x);
	    output (", %s.attr = ", fname);
	    output_attr (x);
	    output (", &%s)", fname);
	  }

	if (r->check)
	  output ("; })");
	break;
      }
/* RXW */
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);

	output("cob_intr_binop ( ");
	output_param(p->x, id);
	output(", ");
	output("%d", p->op);
	output(", ");
	output_param(p->y, id);
	output(")");
	break;
      }
    case CB_TAG_INTRINSIC:
      {
	int			n = 0;
	cb_tree			l;
	struct cb_intrinsic	*i = CB_INTRINSIC(x);

	output("%s (", i->intr_tab->intr_routine);
	if ( i->intr_field ) {
		if ( i->intr_field == cb_int0 ) {
			output ("NULL");
		} else if ( i->intr_field == cb_int1 ) {
			for ( l = i->args; l; l = CB_CHAIN(l) ) {
				n++;
			}
			output ("%d", n);
		} else {
			output_param (i->intr_field, id);
		}
		if ( i->args ) {
			output(", ");
		}
	}
	for ( l = i->args; l; l = CB_CHAIN(l) ) {
		output_param(CB_VALUE(l), id);
		id++;
		param_id++;
		/* Hack until sorted out */
		if ( i->intr_tab->intr_enum == CB_INTR_NUMVAL_C ) {
			break;
		}
		if ( CB_CHAIN(l) ) {
			output(", ");
		}
	}
	output(")");
	break;
      }
    default:
      fprintf(stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
      fflush(stderr);
      ABORT ();
    }
}


/*
 * Function call
 */

static void
output_funcall (cb_tree x)
{
  int i;
  cb_tree l;
  struct cb_funcall *p = CB_FUNCALL (x);

  output ("%s (", p->name);
  for (i = 0; i < p->argc; i++)
    {
	if ( p->varcnt && i + 1 == p->argc ) {
		output ("%d, ", p->varcnt);
		for (l = p->argv[i]; l; l = CB_CHAIN (l)) {
			output_param (CB_VALUE (l), i);
			i++;
			if ( CB_CHAIN (l) ) {
				output (", ");
			}
		}
	} else {
		output_param (p->argv[i], i);
		if (i + 1 < p->argc) {
			output (", ");
		}
	} 
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
	else if (x == cb_false)
	  output ("0");
	else
	  ABORT ();
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
      ABORT ();
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

#define INITIALIZE_NONE		0
#define INITIALIZE_ONE		1
#define INITIALIZE_DEFAULT	2
#define INITIALIZE_COMPOUND	3
#define INITIALIZE_EXTERNAL	4

static int
initialize_type (struct cb_initialize *p, struct cb_field *f, int topfield)
{
  if (f->flag_external)
    return INITIALIZE_EXTERNAL;

  if (f->redefines && (!topfield || !p->flag_statement))
    return INITIALIZE_NONE;

  if (p->val && f->values)
    return INITIALIZE_ONE;

  if ( *f->name == '$' && p->flag_statement && !f->children )
    return INITIALIZE_NONE;

  if (f->children)
    {
      int type = initialize_type (p, f->children, 0);
      if (type == INITIALIZE_ONE)
	return INITIALIZE_COMPOUND;
      for (f = f->children->sister; f; f = f->sister)
	if (type != initialize_type (p, f, 0))
	  return INITIALIZE_COMPOUND;
      return type;
    }
  else
    {
      cb_tree l;
      for (l = p->rep; l; l = CB_CHAIN (l))
	if (CB_PURPOSE_INT (l) == CB_TREE_CATEGORY (f))
	  return INITIALIZE_ONE;
    }

  if (p->def) {
    if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
        return INITIALIZE_ONE;
    }
    switch (CB_TREE_CATEGORY (f))
      {
      case CB_CATEGORY_NUMERIC_EDITED:
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
	return INITIALIZE_ONE;
      default:
	if (cb_tree_type (CB_TREE (f)) == COB_TYPE_NUMERIC_PACKED)
	  return INITIALIZE_ONE;
	else
	  return INITIALIZE_DEFAULT;
      }
  }

  return INITIALIZE_NONE;
}

static int
initialize_uniform_char (struct cb_field *f)
{
  if (f->children)
    {
      int c = initialize_uniform_char (f->children);
      for (f = f->children->sister; f; f = f->sister)
	if (!f->redefines)
	  if (c != initialize_uniform_char (f))
	    return -1;
      return c;
    }
  else
    {
      switch (cb_tree_type (CB_TREE (f)))
	{
	case COB_TYPE_NUMERIC_BINARY:
	  return 0;
	case COB_TYPE_NUMERIC_DISPLAY:
	  return '0';
	case COB_TYPE_ALPHANUMERIC:
	  return ' ';
	default:
	  return -1;
	}
    }
}

static void
output_initialize_fp (cb_tree x, struct cb_field *f)
{
  output_prefix ();
  if ( f->usage == CB_USAGE_FLOAT ) {
	output ("{float temp = 0.0;");
  } else {
	output ("{double temp = 0.0;");
  }
  output (" memcpy(");
  output_data (x);
  output (", (char *)&temp, sizeof(temp));}\n");
}

static void
output_initialize_external (cb_tree x, struct cb_field *f)
{
  char	*p;
  char	name[CB_MAX_CNAME];

  output_prefix ();
  output_data (x);
  if ( f->ename ) {
    output (" = cob_external_addr (\"%s\", %d);\n", f->ename, f->size);
  } else {
    strcpy(name, f->name);
    for ( p = name; *p; p++ ) {
	if ( islower(*p) ) {
		*p = toupper(*p);
	}
    }
    output (" = cob_external_addr (\"%s\", %d);\n", name, f->size);
  }
}

static void
output_initialize_uniform (cb_tree x, int c, int size)
{
  output_prefix ();
  if ( size == 1 ) {
	output ("*(unsigned char *)(");
	output_data (x);
	output (") = %d;\n", c);
  } else {
	output ("memset (");
	output_data (x);
	output (", %d, %d);\n", c, size);
  }
}

static void
output_initialize_one (struct cb_initialize *p, cb_tree x)
{
  struct cb_field *f = cb_field (x);
  cb_tree l;

  /* initialize by value */
  if (p->val && f->values)
    {
      struct cb_field *f = cb_field (x);
      cb_tree value = CB_VALUE (f->values);

      if (value == cb_space)
	{
	  /* SPACE */
	  /* FIXME: This is to avoid an error when a numeric-edited item
	     has VALUE SPACE because cob_build_move doubly checks the value.
	     We should instead check the value only once.  */
	  output_prefix ();
	  if ( f->size == 1 ) {
		output ("*(unsigned char *)(");
		output_data (x);
		output (") = ' ';\n");
	  } else {
		output ("memset (");
		output_data (x);
		output (", ' ', %d);\n", f->size);
	  }
	}
      else if (CB_CONST_P (value)
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
	  struct cb_literal *l = CB_LITERAL (value);
	  if (l->size >= f->size)
	    {
	      memcpy (buff, l->data, f->size);
	    }
	  else
	    {
	      memcpy (buff, l->data, l->size);
	      memset (buff + l->size, ' ', f->size - l->size);
	    }
	  output_prefix ();
	  if (f->size == 1)
	    {
		output ("*(unsigned char *) (");
		output_data (x);
		output (") = %d;\n", *(unsigned char *) buff);
	    }
	  else
	    {
		output ("memcpy (");
		output_data (x);
		output (", ");
		output_string (buff, f->size);
		output (", %d);\n", f->size);
	    }
	}
      return;
    }

  /* initialize replacing */
  if (!f->children)
    for (l = p->rep; l; l = CB_CHAIN (l))
      if (CB_PURPOSE_INT (l) == CB_TREE_CATEGORY (x))
	{
	  output_move (CB_VALUE (l), x);
	  return;
	}

  /* initialize by default */
  if (p->def) {
    if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
      output_initialize_fp(x, f);
      return;
    }
    switch (CB_TREE_CATEGORY (x))
      {
      case CB_CATEGORY_NUMERIC:
      case CB_CATEGORY_NUMERIC_EDITED:
	output_move (cb_zero, x);
	break;
      case CB_CATEGORY_ALPHANUMERIC_EDITED:
      case CB_CATEGORY_NATIONAL_EDITED:
	output_move (cb_space, x);
	break;
      default:
	ABORT ();
      }
  }
}

static void
output_initialize_compound (struct cb_initialize *p, cb_tree x)
{
  struct cb_field *ff = cb_field (x);
  struct cb_field *f;

  for (f = ff->children; f; f = f->sister)
    {
      int type = initialize_type (p, f, 0);
      cb_tree c = cb_build_field_reference (f, x);
      switch (type)
	{
	case INITIALIZE_NONE:
	  break;
	case INITIALIZE_DEFAULT:
	  {
	    struct cb_field *last_field = f;
	    int last_char = initialize_uniform_char (f);
	    if (last_char != -1)
	      {
		size_t size;

		if (f->flag_occurs)
		  CB_REFERENCE (c)->subs =
		    cb_cons (cb_int1, CB_REFERENCE (c)->subs);

		for (; f->sister; f = f->sister)
		  if (!f->sister->redefines)
		    if (initialize_type (p, f->sister, 0) != INITIALIZE_DEFAULT
			|| initialize_uniform_char (f->sister) != last_char)
		      break;

		if (f->sister)
		  size = f->sister->offset - last_field->offset;
		else
		  size = ff->offset + ff->size - last_field->offset;

		output_initialize_uniform (c, last_char, size);
		break;
	      }
	    /* fall through */
	  }
	default:
	  if (f->flag_occurs)
	    {
	      /* begin occurs loop */
	      int i = f->indexes;
	      output_line ("for (i%d = 1; i%d <= %d; i%d++)",
			   i, i, f->occurs_max, i);
	      output_indent ("  {");
	      CB_REFERENCE (c)->subs = cb_cons (cb_i[i], CB_REFERENCE (c)->subs);
	    }

	  if (type == INITIALIZE_ONE)
	    output_initialize_one (p, c);
	  else
	    output_initialize_compound (p, c);

	  if (f->flag_occurs)
	    {
	      /* close loop */
	      CB_REFERENCE (c)->subs = CB_CHAIN (CB_REFERENCE (c)->subs);
	      output_indent ("  }");
	    }
	}
    }
}

static void
output_initialize (struct cb_initialize *p)
{
  struct cb_field *f = cb_field (p->var);
  switch (initialize_type (p, f, 1))
    {
    case INITIALIZE_NONE:
      break;
    case INITIALIZE_ONE:
      output_initialize_one (p, p->var);
      break;
    case INITIALIZE_EXTERNAL:
	  output_initialize_external (p->var, f);
      break;
    case INITIALIZE_DEFAULT:
      {
	int c = initialize_uniform_char (f);
	if (c != -1)
	  output_initialize_uniform (p->var, c, f->size);
	else
	  output_initialize_compound (p, p->var);
	break;
      }
    case INITIALIZE_COMPOUND:
      output_initialize_compound (p, p->var);
      break;
    }
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
  char	*callp;
  int dynamic_link = 1;
  cb_tree l;

  /* User defined entry points */
  if (CB_LITERAL_P (p->name))
  {
    for ( n = 0; n < CALLTABSIZE && calltab[n]; n++ )
    {
      if ( strcmp(CB_LITERAL(p->name)->data, calltab[n]) == 0 )
      {
        dynamic_link = 0;
        break;
      }
    }
  }

  if ((cb_flag_static_call == 1) && CB_LITERAL_P (p->name))
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
	  output ("unsigned char content_%d[", n);
	  if (CB_NUMERIC_LITERAL_P (x) || CB_BINARY_OP_P (x) || x == cb_null)
	    output ("4");
	  else
	    output_size (x);
	  output ("];\n");
	  break;
	}
    }
  for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++)
    {
      cb_tree x = CB_VALUE (l);
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_CONTENT:
	  output_prefix ();
	  if (CB_NUMERIC_LITERAL_P (x) || x == cb_null
		|| ( CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
			&& cb_field (x)->usage == CB_USAGE_LENGTH ))
	    {
	      output ("*(int *)content_%d = ", n);
	      output_integer (x);
	      output (";\n");
	    }
	  else
	    {
	      output ("memcpy (content_%d, ", n);
	      output_data (x);
	      output (", ");
	      output_size (x);
	      output (");\n");
	    }
	}
    }

  /* function name */
  output_prefix ();
  n = 0;
  for (l = p->args; l; l = CB_CHAIN (l))
    {
	n++;
    }
  output ("cob_call_params = %d;\n", n);
  output_prefix ();
  if (!dynamic_link)
    {
      /* static link */
      output_integer (cb_return_code);
      output (" = %s", cb_encode_program_id (CB_LITERAL (p->name)->data));
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      if ( !p->stmt1 ) {
	if (CB_LITERAL_P (p->name)) {
	   if ( cb_flag_static_call == 2 ) {
		callp = cb_encode_program_id (CB_LITERAL (p->name)->data);
		lookup_call(callp, CB_LITERAL(p->name)->data);
		output ("call_%s", callp);
	   } else {
		output ("cob_resolve_1 (\"%s\")", CB_LITERAL (p->name)->data);
	   }
	} else {
		output_funcall (cb_build_funcall_1 ("cob_call_resolve_1", p->name));
	}
	output (";\n");
      } else {
	if (CB_LITERAL_P (p->name)) {
		output ("cob_resolve (\"%s\")", CB_LITERAL (p->name)->data);
	} else {
		output_funcall (cb_build_funcall_1 ("cob_call_resolve", p->name));
	}
	output (";\n");
#if defined (__GNUC__) && (__GNUC__ >= 3)
	output_line ("if ( __builtin_expect((func == NULL), 0) )");
#else
	output_line ("if (func == NULL)");
#endif
	output_indent_level += 2;
	output_stmt (p->stmt1);
	output_indent_level -= 2;
	output_line ("else");
	output_indent ("  {");
      }
      output_prefix ();
      output_integer (cb_return_code);
      output (" = func");
    }

  /* arguments */
  output (" (");
  for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++)
    {
      cb_tree x = CB_VALUE (l);
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_REFERENCE:
	  if (CB_REFERENCE_P (x) && CB_FILE_P (cb_ref (x)))
	    output_param (cb_ref (x), -1);
	  else
	    output_data (x);
	  break;
	case CB_CALL_BY_CONTENT:
	  output ("content_%d", n);
	  break;
	case CB_CALL_BY_VALUE:
	  switch (CB_TREE_TAG (x))
	    {
	    case CB_TAG_LITERAL:
	      if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
		output ("%d", cb_get_int (x));
	      else
		output ("%d", CB_LITERAL (x)->data[0]);
	      break;
	    default:
	      switch (cb_field (x)->usage)
		{
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_INDEX:
		  output_integer (x);
		  break;
		case CB_USAGE_LENGTH:
			output_integer (x);
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
      if (CB_CHAIN (l))
	output (", ");
    }
  output (");\n");
  if (p->stmt2)
    output_stmt (p->stmt2);
  if (dynamic_link && p->stmt1)
    output_indent ("  }");
  output_indent ("}");
}


/*
 * GO TO
 */

static void
output_goto_1 (cb_tree x)
{
  output_line ("goto %s%d;", CB_PREFIX_LABEL, CB_LABEL (cb_ref (x))->id);
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
      output_param (cb_build_cast_integer (p->depending), 0);
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
    {
       needs_exit_prog = 1;
       output_line ("goto exit_program;");
    }
  else
    output_goto_1 (p->target);
}


/*
 * PERFORM
 */

static void
output_perform_call (struct cb_label *lb, struct cb_label *le)
{
  output_line ("/* PERFORM %s THRU %s */", lb->name, le->name);
  output_line ("frame_index++;");
  output_line ("frame_stack[frame_index].perform_through = %d;",
	       le->id);
  output_line ("frame_stack[frame_index].return_address = &&%s%d;",
	       CB_PREFIX_LABEL, cb_id);
  output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
  output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
  cb_id++;
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
  if (p->cycle_label)
    output_stmt (cb_ref (p->cycle_label));
}

static void
output_perform_until (struct cb_perform *p, cb_tree l)
{
  struct cb_perform_varying *v;
  cb_tree next;

  if (l == NULL)
    {
      /* perform body at the end */
      output_perform_once (p);
      return;
    }

  v = CB_PERFORM_VARYING (CB_VALUE (l));
  next = CB_CHAIN (l);

  output_line ("while (1)");
  output_indent ("  {");

  if (next && CB_PERFORM_VARYING (CB_VALUE (next))->name)
    output_move (CB_PERFORM_VARYING (CB_VALUE (next))->from,
		 CB_PERFORM_VARYING (CB_VALUE (next))->name);

  if (p->test == CB_AFTER)
    output_perform_until (p, next);

  output_prefix ();
  output ("if (");
  output_cond (v->until, 0);
  output (")\n");
  output_line ("  break;");

  if (p->test == CB_BEFORE)
    output_perform_until (p, next);

  if (v->step)
    output_stmt (v->step);

  output_indent ("  }");
}

static void
output_perform (struct cb_perform *p)
{

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
      output_param (cb_build_cast_integer (p->data), 0);
      output ("; n[%d] > 0; n[%d]--)\n", loop_counter, loop_counter);
      loop_counter++;
      output_indent ("  {");
      output_perform_once (p);
      output_indent ("  }");
      break;
    case CB_PERFORM_UNTIL:
      {
	struct cb_perform_varying *v =
	  CB_PERFORM_VARYING (CB_VALUE (p->varying));
	if (v->name)
	  output_move (v->from, v->name);
	output_perform_until (p, p->varying);
	break;
      }
    }
  if (p->exit_label)
    output_stmt (cb_ref (p->exit_label));
}


/*
 * Output statement
 */

static void
output_stmt (cb_tree x)
{
  stack_id = 0;
  if (x == NULL)
    {
      output_line (";");
      return;
    }

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_STATEMENT:
      {
	struct cb_statement *p = CB_STATEMENT (x);

	/* output source location as a comment */
	if (p->name)
	  {
	    output_line ("/* %s:%d: %s */",
			 x->source_file, x->source_line, p->name);
	  }
	/* output source location as a code */
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
	    || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O))) {

	  output_line ("cob_exception_code = 0;");
/* RXW - Dirty must be some other way */
		if ( p->handler_id == COB_EC_I_O_EOP ) {
			output_line ("cob_check_eop = 1;");
		} else {
			output_line ("cob_check_eop = 0;");
		}
	}

	if (p->body)
	  output_stmt (p->body);

	if (p->handler1 || p->handler2
	    || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O)))
	  {
	    int code = CB_EXCEPTION_CODE (p->handler_id);
	    if (p->handler1)
	      {
		if ((code & 0x00ff) == 0)
#if defined (__GNUC__) && (__GNUC__ >= 3)
		  output_line ("if ( __builtin_expect(((cob_exception_code & 0xff00) == 0x%04x), 0) )",
#else
		  output_line ("if ((cob_exception_code & 0xff00) == 0x%04x)",
#endif
			       code);
		else
#if defined (__GNUC__) && (__GNUC__ >= 3)
		  output_line ("if ( __builtin_expect((cob_exception_code == 0x%04x), 0) )", code);
#else
		  output_line ("if (cob_exception_code == 0x%04x)", code);
#endif
		output_stmt (p->handler1);
		if (p->handler2 || p->file)
		  output_line ("else");
	      }
	    if (p->file)
	      {
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("if ( __builtin_expect(cob_exception_code, 0) )");
#else
		output_line ("if (cob_exception_code)");
#endif
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
	  output_line ("%s%d:;", CB_PREFIX_LABEL, p->id);
	if (cb_flag_trace) {
	  output_line ("puts (\"%s\");", p->name);
	  output_line ("fflush (stdout);");
	}
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
      ABORT ();
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
      output ("  static cob_file_key %s%s[] = {\n", CB_PREFIX_KEYS, f->cname);
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
  output ("  static cob_file %s%s = {", CB_PREFIX_FILE, f->cname);
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
    output ("%d, %s%s, ", nkeys, CB_PREFIX_KEYS, f->cname);
  else
    output ("0, 0, ");
  /* file */
    output ("0, ");
  /* LINAGE */
  if ( f->linage ) {
	output_param (f->linage, -1);
	output (", ");
	output_param (f->linage_ctr, -1);
	output (", ");
	if ( f->latfoot ) {
		output_param (f->latfoot, -1);
	} else {
		output ("NULL");
	}
	output (", ");
	if ( f->lattop ) {
		output_param (f->lattop, -1);
	} else {
		output ("NULL");
	}
	output (", ");
	if ( f->lattop ) {
		output_param (f->latbot, -1);
	} else {
		output ("NULL");
	}
	output (", ");
  } else {
	output ("NULL, NULL, NULL, NULL, NULL, ");
  }
  /* LINAGE  Current values */
  output ("0, 0, 0, 0, ");
  /* flags */
  output ("0, 0, 0, 0, 0, ");
  /* has file status flag */
  if (f->file_status)
    output ("1, ");
  else
    output ("0, ");
  /* LS close needs NL / Linage needs top */
  output ("0, 0};\n\n");
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

  output ("struct cob_screen s_%d = {%d, ", p->id, type);

  output ("(union cob_screen_data) ");
  switch (type)
    {
    case COB_SCREEN_TYPE_GROUP:
      output ("&s_%d", p->children->id);
      break;
    case COB_SCREEN_TYPE_VALUE:
      output_string (CB_LITERAL (CB_VALUE (p->values))->data,
		     CB_LITERAL (CB_VALUE (p->values))->size);
      break;
      break;
    case COB_SCREEN_TYPE_FIELD:
      output ("&f_%d", p->id);
      break;
    case COB_SCREEN_TYPE_ATTRIBUTE:
      output ("0");
      break;
    }
  output (", ");

  if (p->sister)
    output ("&s_%d, ", p->sister->id);
  else
    output ("0, ");
  if (p->screen_from)
    output ("&f_%d, ", cb_field (p->screen_from)->id);
  else
    output ("0, ");
  if (p->screen_to)
    output ("&f_%d, ", cb_field (p->screen_to)->id);
  else
    output ("0, ");
  output_integer (p->screen_line);
  output (", ");
  output_integer (p->screen_column);
  output (", %ld};\n", p->screen_flag);
}


/*
 * Alphabet-name
 */

static int
literal_value (cb_tree x)
{
  if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
    return cb_get_int (x) - 1;
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
	    table[literal_value (x)] = n++;
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
  output ("static const unsigned char %s%s[256] = {\n",
	  CB_PREFIX_SEQUENCE, p->cname);
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
  cb_tree def = cb_auto_initialize ? cb_true : NULL;
  for (; p; p = p->sister)
/*
    if (!p->flag_external)
*/
      {
	cb_tree x = cb_build_field_reference (p, 0);
	output_stmt (cb_build_initialize (x, cb_true, NULL, def, 0));
      }
}

static void
output_internal_function (struct cb_program *prog, cb_tree parameter_list)
{
  int i;
  cb_tree l;
  struct cb_field *f;
  struct call_list	*clp;

  /* program function */
  output ("static int\n%s_ (int entry", prog->program_id);
  for (l = parameter_list; l; l = CB_CHAIN (l))
	output (", unsigned char *%s%d",
		CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id);
  output (")\n");
  output_indent ("{");

  /* local variables */
  output_line ("static int initialized = 0;");
  if (prog->decimal_index_max)
	output_line ("static cob_decimal d[%d];", prog->decimal_index_max);

  output_prefix ();
  output ("static cob_module module = { NULL, ");
  if (prog->collating_sequence)
	output_param (cb_ref (prog->collating_sequence), -1);
  else    
	output ("NULL");

  /* Note 2 spare bytes at end */
  output (", %d, '%c', '%c', '%c', %d, %d, %d, 0, 0};\n",
	cb_display_sign, prog->decimal_point, prog->currency_symbol,
	prog->numeric_separator, cb_filename_mapping, cb_binary_truncate,
	cb_pretty_display);
  output_newline ();

  /* External items */
  for (f = prog->working_storage; f; f = f->sister) {
	if ( f->flag_external ) {
		char name[CB_MAX_CNAME];
		char *p;

		strcpy (name, f->name);
		for (p = name; *p; p++) {
			if (*p == '-')
				*p = '_';
		}
		output ("  static unsigned char *%s%s = NULL;",
			CB_PREFIX_BASE, name);
		output ("  /* %s */\n", f->name);
	}
  }
  output_newline ();

  output ("#include \"%s\"\n\n", cb_storage_file_name);
  output_newline ();

  /* files */
  if (prog->file_list)
    {
      output ("  /* Files */\n\n");
      for (l = prog->file_list; l; l = CB_CHAIN (l))
	output_file_definition (CB_FILE (CB_VALUE (l)));
      output_newline ();
    }

  for ( i = 0; i < 8; i++ ) {
	if ( i_counters[i] ) {
		output_line ("int i%d;", i);
	}
  }

  if ( prog->loop_counter )
	output_line ("int n[%d];", prog->loop_counter);

/*
  if ( num_cob_fields ) {
	output_line ("cob_field f[%d];", num_cob_fields + 1);
  }
*/
  output_newline ();

  /* linkage section */
  for (f = prog->linkage_storage; f; f = f->sister)
    {
      for (l = parameter_list; l; l = CB_CHAIN (l))
	if (f == cb_field (CB_VALUE (l)))
	  break;
      if (l == NULL)
	output_line ("unsigned char *%s%d = NULL;", CB_PREFIX_BASE, f->id);
    }
  output_newline ();

  /* screens */
  if (prog->screen_storage)
    {
      output ("/* Screens */\n\n");
      for (f = prog->screen_storage; f; f = f->sister)
	output_screen_definition (f);
      output_newline ();
    }

  output_line ("/* perform frame stack */");
  output_line ("int frame_index;");
  output_line ("struct frame { int perform_through; void *return_address; } "
	       "frame_stack[254];");
  output_newline ();

  output_line ("/* Start of function code */");
  if ( cb_flag_static_call == 2 ) {
	output_line("auto void init$%s(void);", prog->program_id);
  }
  output_newline ();
  output_line ("module.next = cob_current_module;");
  output_line ("cob_current_module = &module;");
  output_newline ();

  /* initialization */
#if defined (__GNUC__) && (__GNUC__ >= 3)
  output_line ("if ( __builtin_expect(!initialized, 0) )");
#else
  output_line ("if (!initialized)");
#endif
  output_indent ("  {");
  /* output_stmt (cb_build_assign (cb_return_code, cb_int0)); */
  output_line ("if (!cob_initialized) {");
  output_line ("  fputs(\"cob_init() has not been called\\n\", stderr);");
  output_line ("  exit (1);");
  output_line ("}");
  output_line ("cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
  if ( cb_flag_static_call == 2 ) {
	output_line("init$%s();", prog->program_id);
  }
  if (prog->decimal_index_max) {
	output_line ("/* initialize decimal numbers */");
	output_line ("{ int i;");
	output_line ("for (i = 0; i < %d; i++)", prog->decimal_index_max);
	output_line ("  cob_decimal_init (&d[i]);");
	output_line ("}");
	output_newline ();
  }
  if (!prog->flag_initial) {
    output_initial_values (prog->working_storage);
    output_newline ();
  }

  output_line ("initialized = 1;");
  output_indent ("  }");
  if (prog->flag_initial)
    output_initial_values (prog->working_storage);
  output_initial_values (prog->local_storage);
  output_newline ();

  output_line ("/* initialize frame stack */");
  output_line ("frame_index = -1;");
  output_line ("frame_stack[0].perform_through = -1;");
  output_newline ();

  output_line ("/* initialize number of call params */");
  output ("  ");
  output_integer (cb_call_params);
  output_line (" = cob_call_params;");
  output_newline ();

  /* entry dispatch */
  if ( entry_number > 1 ) {
	output_line ("switch (entry)");
	output_line ("  {");
	for (i = 0, l = prog->entry_list; l; l = CB_CHAIN (l))
	  {
	    output_line ("  case %d:", i++);
	    output_line ("    goto %s%d;",
			 CB_PREFIX_LABEL, CB_LABEL (CB_PURPOSE (l))->id);
	  }
	output_line ("  }");
	output_newline ();
  } else {
	l = prog->entry_list;
	output_line ("goto %s%d;",
		CB_PREFIX_LABEL, CB_LABEL (CB_PURPOSE (l))->id);
	output_newline ();
  }

  /* PROCEDURE DIVISION */
  output_line ("/* PROCEDURE DIVISION */");
  for (l = prog->exec_list; l; l = CB_CHAIN (l))
    output_stmt (CB_VALUE (l));
  output_newline ();

  if ( needs_exit_prog ) {
	output_line ("exit_program:");
  }
  output_line ("cob_current_module = cob_current_module->next;");
  output_prefix ();
  output ("return ");
  output_integer (cb_return_code);
  output (";\n\n");

  /* error handlers */
  output_line ("/* error handlers */");
  output_stmt (cb_standard_error_handler);
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
  output_line ("    if ( !cob_error_file->flag_has_status ) {");
  output_line ("        cob_default_error_handle ();");
  output_line ("        exit(1);");
  output_line ("    }");
  output_line ("    break;");
  output_line ("  }");
  output_perform_exit (CB_LABEL (cb_standard_error_handler));
  output_line ("fprintf(stderr, \"Codegen error\\n\");");
  output_line ("exit(1);");
  output_newline ();

  if ( cb_flag_static_call == 2 ) {
	output_line("void init$%s(void)", prog->program_id);
	output_line("{");
	for ( clp = call_cache; clp; clp = clp->next ) {
	output_line("	call_%s = cob_resolve(\"%s\");",
			clp->callname, clp->callorig);
	}
	output_line("}");
  }
	
  output_indent ("}");
  output_newline ();
}

static void
output_entry_function (struct cb_program *prog,
		       cb_tree entry,
		       cb_tree parameter_list)
{

  const char *entry_name = CB_LABEL (CB_PURPOSE (entry))->name;
  cb_tree using_list = CB_VALUE (entry);
  cb_tree l, l1, l2;

  output ("int\n");
  output ("%s (", entry_name);
  for (l = using_list; l; l = CB_CHAIN (l))
    {
      struct cb_field *f = cb_field (CB_VALUE (l));
      switch (CB_PURPOSE_INT (l))
	{
	case CB_CALL_BY_REFERENCE:
	case CB_CALL_BY_CONTENT:
	  output ("unsigned char *%s%d", CB_PREFIX_BASE, f->id);
	  break;
	case CB_CALL_BY_VALUE:
	  if (CB_TREE_CLASS (CB_VALUE (l)) == CB_CLASS_NUMERIC)
	    output ("int i_%d", f->id);
	  else
	    output ("unsigned char i_%d", f->id);
	  break;
	}
      if (CB_CHAIN (l))
	output (", ");
    }
  output (")\n");
  output ("{\n");
  for (l = using_list; l; l = CB_CHAIN (l))
    if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT)
      {
	struct cb_field *f = cb_field (CB_VALUE (l));
	output_line ("unsigned char copy_%d[%d];", f->id, f->size);
      }
  for (l = using_list; l; l = CB_CHAIN (l))
    if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT)
      {
	struct cb_field *f = cb_field (CB_VALUE (l));
	output_line ("memcpy (copy_%d, %s%d, %d);",
		     f->id, CB_PREFIX_BASE, f->id, f->size);
      }

  output ("  return %s_ (%d", prog->program_id, progid++);
  for (l1 = parameter_list; l1; l1 = CB_CHAIN (l1))
    {
      for (l2 = using_list; l2; l2 = CB_CHAIN (l2))
	if (strcasecmp (cb_field (CB_VALUE (l1))->name,
			cb_field (CB_VALUE (l2))->name) == 0)
	  {
	    struct cb_field *f = cb_field (CB_VALUE (l2));
	    switch (CB_PURPOSE_INT (l2))
	      {
	      case CB_CALL_BY_REFERENCE:
		output (", b_%d", f->id);
		break;
	      case CB_CALL_BY_CONTENT:
		output (", copy_%d", f->id);
		break;
	      case CB_CALL_BY_VALUE:
		output (", (unsigned char *) &i_%d", f->id);
		break;
	      }
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
  output_line ("int ret;");
  output_line ("cob_init (argc, argv);");
  if (prog->flag_screen)
    output_line ("cob_screen_init ();");
  output_line ("ret = %s ();", prog->program_id);
  if (prog->flag_screen)
    output_line ("cob_screen_clear ();");
  output_line ("return ret;");
  output_indent ("}\n");
}

void
codegen (struct cb_program *prog)
{
  cb_tree l;
  cb_tree parameter_list = NULL;
  struct attr_list *j;
  struct literal_list *m;
  struct field_list *k;
  struct call_list *clp;

  param_id = 0;
  stack_id = 0;
  num_cob_fields = 0;
  progid = 0;
  loop_counter = 0;
  output_indent_level = 0;
  last_line = 0;
  needs_exit_prog = 0;
  field_cache = NULL;
  attr_cache = NULL;
  literal_cache = NULL;
  call_cache = NULL;

  if (cb_flag_main)
    prog->flag_initial = 1;

  output_target = yyout;

  output ("/* Generated from %s by cobc version %s patch level %d */\n\n",
	  cb_source_file, PACKAGE_VERSION, PATCH_LEVEL);
  output ("#include <stdio.h>\n");
  output ("#include <stdlib.h>\n");
  output ("#include <string.h>\n");
  output ("#include <math.h>\n");
  output ("#include <libcob.h>\n\n");

  output ("#define COB_SOURCE_FILE		\"%s\"\n", cb_source_file);
  output ("#define COB_PACKAGE_VERSION	\"%s\"\n", PACKAGE_VERSION);
  output ("#define COB_PATCH_LEVEL		%d\n\n", PATCH_LEVEL);

/*
  output ("#include \"%s\"\n\n", cb_storage_file_name);
*/

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
	    if (strcasecmp (cb_field (CB_VALUE (l1))->name,
			    cb_field (CB_VALUE (l2))->name) == 0)
	      break;
	  if (l2 == NULL)
	    parameter_list = cb_list_add (parameter_list, CB_VALUE (l1));
	}
    }

  /* prototype */
  output ("static int %s_ (int", prog->program_id);
  for (l = parameter_list; l; l = CB_CHAIN (l))
	output (", unsigned char *");
  output (");\n\n");

  /* main function */
  if (cb_flag_main) {
	output ("int %s (void);\n\n", prog->program_id);
	output_main_function (prog);
  }

  /* functions */
  for (l = prog->entry_list; l; l = CB_CHAIN (l))
    output_entry_function (prog, l, parameter_list);
  output_internal_function (prog, parameter_list);

  output_target = cb_storage_file;
  output_storage ("\n");
  for (j = attr_cache; j; j = j->next) {
	output_storage ("static cob_field_attr %s%d\t= ", CB_PREFIX_ATTR, j->id);
	output_storage ("{%d, %d, %d, %d, ", j->type, j->digits, j->scale, j->flags);
	if (j->pic)
	    {
	      unsigned char *s;
	      output_storage ("\"");
	      for (s = j->pic; *s; s += 2)
		output_storage ("%c\\%03o", s[0], s[1]);
	      output_storage ("\"");
	    }
	else
		output_storage ("0");
	output_storage ("};\n");
  }

  output_storage ("\n");
  for (k = field_cache; k; k = k->next) {
	output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, k->f->id);
	output_field (k->x);
	output (";\t/* %s */\n", k->f->name);
  }

  output_storage ("\n");
  for (m = literal_cache; m; m = m->next) {
	output ("static cob_field %s%d\t= ", CB_PREFIX_CONST, m->id);
	output_field (m->x);
	output (";\n");
  }

  output_storage ("\n");
  for (clp = call_cache; clp; clp = clp->next) {
	output ("static int (*call_%s)();\n", clp->callname);
  }

  output_storage ("\n");
  if ( num_cob_fields ) {
	output ("cob_field f[%d];\n", num_cob_fields);
  }

  output_target = yyout;

}
