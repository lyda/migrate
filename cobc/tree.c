/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#include "tree.h"
#include "scanner.h"


/*
 * List
 */

struct cobc_list *
cons (void *x, struct cobc_list *l)
{
  struct cobc_list *p = malloc (sizeof (struct cobc_list));
  p->item = x;
  p->next = l;
  return p;
}

struct cobc_list *
list (void *x)
{
  return cons (x, NULL);
}

struct cobc_list *
list_last (struct cobc_list *l)
{
  if (l != NULL)
    for (; l->next != NULL; l = l->next);
  return l;
}

struct cobc_list *
list_add (struct cobc_list *l, void *x)
{
  return list_append (l, list (x));
}

struct cobc_list *
list_append (struct cobc_list *l1, struct cobc_list *l2)
{
  if (l1 == NULL)
    {
      return l2;
    }
  else
    {
      list_last (l1)->next = l2;
      return l1;
    }
}

struct cobc_list *
list_reverse (struct cobc_list *l)
{
  struct cobc_list *next, *last = NULL;
  for (; l; l = next)
    {
      next = l->next;
      l->next = last;
      last = l;
    }
  return last;
}

int
list_length (struct cobc_list *l)
{
  int n = 0;
  for (; l; l = l->next)
    n++;
  return n;
}


/*
 * Word table
 */

#define HASH_SIZE	133

static struct cobc_word *word_table[HASH_SIZE];

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return val % HASH_SIZE;
}

static struct cobc_word *
make_word (const char *name)
{
  struct cobc_word *p = malloc (sizeof (struct cobc_word));
  memset (p, 0, sizeof (struct cobc_word));
  p->name  = strdup (name);
  return p;
}

struct cobc_word *
lookup_word (const char *name)
{
  struct cobc_word *p;
  int val = hash (name);

  /* find existing symbol */
  for (p = word_table[val]; p; p = p->next)
    if (strcasecmp (p->name, name) == 0)
      return p;

  /* create new symbol */
  p = make_word (name);
  p->next = word_table[val];
  word_table[val] = p;
  return p;
}

void
init_word_table (void)
{
  int i;
  for (i = 0; i < HASH_SIZE; i++)
    word_table[i] = NULL;
}


/*
 * Tree
 */

static void *
make_tree (int tag, char class, int size)
{
  cobc_tree x = malloc (size);
  memset (x, 0, size);
  x->tag = tag;
  x->class = class;
  return x;
}

static int
tree_name_1 (char *s, cobc_tree x)
{
  char *orig = s;

  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      if (x == cobc_any)
	strcpy (s, "ANY");
      else if (x == cobc_true)
	strcpy (s, "TRUE");
      else if (x == cobc_false)
	strcpy (s, "FALSE");
      else if (x == cobc_zero)
	strcpy (s, "ZERO");
      else if (x == cobc_space)
	strcpy (s, "SPACE");
      else if (x == cobc_low)
	strcpy (s, "LOW-VALUE");
      else if (x == cobc_high)
	strcpy (s, "HIGH-VALUE");
      else if (x == cobc_quote)
	strcpy (s, "QUOTE");
      else
	strcpy (s, "#<unknown constant>");
      break;

    case cobc_tag_literal:
      if (COBC_TREE_CLASS (x) == COB_TYPE_NUMERIC)
	strcpy (s, COBC_LITERAL (x)->data);
      else
	sprintf (s, "\"%s\"", COBC_LITERAL (x)->data);
      break;

    case cobc_tag_field:
      strcpy (s, COBC_FIELD (x)->name);
      break;

    case cobc_tag_reference:
      {
	struct cobc_reference *p = COBC_REFERENCE (x);
	if (p->value)
	  s += tree_name_1 (s, p->value);
	else
	  s += sprintf (s, "#<reference %s>", p->word->name);
	if (p->subs)
	  {
	    struct cobc_list *l;
	    s += sprintf (s, "(");
	    for (l = p->subs; l; l = l->next)
	      {
		s += tree_name_1 (s, l->item);
		s += sprintf (s, l->next ? ", " : ")");
	      }
	  }
	if (p->offset)
	  {
	    s += sprintf (s, "(");
	    s += tree_name_1 (s, p->offset);
	    s += sprintf (s, ":");
	    if (p->length)
	      s += tree_name_1 (s, p->length);
	    strcpy (s, ")");
	  }
      }
      break;

    case cobc_tag_label:
      sprintf (s, "%s:", COBC_LABEL (x)->name);
      break;

    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	if (p->op == '!')
	  {
	    s += sprintf (s, "!");
	    s += tree_name_1 (s, p->x);
	  }
	else
	  {
	    s += sprintf (s, "(");
	    s += tree_name_1 (s, p->x);
	    s += sprintf (s, " %c ", p->op);
	    s += tree_name_1 (s, p->y);
	    strcpy (s, ")");
	  }
	break;
      }

    case cobc_tag_funcall:
      {
	int i;
	struct cobc_funcall *p = COBC_FUNCALL (x);
	s += sprintf (s, "%s", p->name);
	for (i = 0; i < p->argc; i++)
	  {
	    s += sprintf (s, (i == 0) ? "(" : ", ");
	    s += tree_name_1 (s, p->argv[i]);
	  }
	s += sprintf (s, ")");
	break;
      }

    default:
      sprintf (s, "#<unknown %d %p>", COBC_TREE_TAG (x), x);
    }

  return strlen (orig);
}

char *
tree_name (cobc_tree x)
{
  static char buff[BUFSIZ];
  tree_name_1 (buff, x);
  return buff;
}


/*
 * Location
 */

cobc_tree
make_location (char *file, int line)
{
  struct cobc_tree_common *p =
    make_tree (cobc_tag_location, COB_TYPE_UNKNOWN, sizeof (struct cobc_tree_common));
  p->source_file = file;
  p->source_line = line;
  return COBC_TREE (p);
}


/*
 * Constants
 */

cobc_tree cobc_any;
cobc_tree cobc_true;
cobc_tree cobc_false;
cobc_tree cobc_zero;
cobc_tree cobc_space;
cobc_tree cobc_low;
cobc_tree cobc_high;
cobc_tree cobc_quote;
cobc_tree cobc_return_code;
cobc_tree cobc_switch[8];
cobc_tree cobc_int0;
cobc_tree cobc_int1;
cobc_tree cobc_int2;

struct cobc_label *cobc_main_label;
struct cobc_label *cobc_standard_error_handler;

static cobc_tree
make_constant (char class, char *val)
{
  struct cobc_const *p =
    make_tree (cobc_tag_const, class, sizeof (struct cobc_const));
  p->val = val;
  return COBC_TREE (p);
}

static struct cobc_label *
make_constant_label (char *name)
{
  struct cobc_label *l =
    COBC_LABEL (make_label (make_reference (make_word (name)), NULL));
  free (l->cname);
  l->cname = name;
  l->need_begin = 1;
  return l;
}

void
init_constants (void)
{
  int i;
  cobc_any         = make_constant (COB_TYPE_UNKNOWN, 0);
  cobc_true        = make_constant (COB_TYPE_BOOLEAN, "1");
  cobc_false       = make_constant (COB_TYPE_BOOLEAN, "0");
  cobc_return_code = make_constant (COB_TYPE_NUMERIC, "cob_return_code");
  cobc_zero        = make_constant (COB_TYPE_NUMERIC, "&cob_zero");
  cobc_space       = make_constant (COB_TYPE_ALPHANUMERIC, "&cob_space");
  cobc_low         = make_constant (COB_TYPE_ALPHANUMERIC, "&cob_low");
  cobc_high        = make_constant (COB_TYPE_ALPHANUMERIC, "&cob_high");
  cobc_quote       = make_constant (COB_TYPE_ALPHANUMERIC, "&cob_quote");
  cobc_int0        = make_integer (0);
  cobc_int1        = make_integer (1);
  cobc_int2        = make_integer (2);
  for (i = 0; i < 8; i++)
    {
      char buff[16];
      sprintf (buff, "switch[%d]", i);
      cobc_switch[i] = make_field_x (buff, "9", COBC_USAGE_INDEX);
    }

  cobc_main_label = make_constant_label ("main");
  cobc_standard_error_handler = make_constant_label ("standard_error_handler");
}


/*
 * Integer
 */

cobc_tree
make_integer (int val)
{
  struct cobc_integer *p =
    make_tree (cobc_tag_integer, COB_TYPE_NUMERIC, sizeof (struct cobc_integer));
  p->val = val;
  return COBC_TREE (p);
}


/*
 * String
 */

cobc_tree
make_string (unsigned char *str)
{
  struct cobc_string *p =
    make_tree (cobc_tag_string, COB_TYPE_NUMERIC, sizeof (struct cobc_string));
  p->str = str;
  return COBC_TREE (p);
}


/*
 * Literal
 */

static struct cobc_literal *
make_literal (int class, unsigned char *str)
{
  struct cobc_literal *p =
    make_tree (cobc_tag_literal, class, sizeof (struct cobc_literal));
  p->size = strlen (str);
  p->data = strdup (str);
  return p;
}

cobc_tree
make_numeric_literal (int sign, unsigned char *digits, int decimals)
{
  struct cobc_literal *p = make_literal (COB_TYPE_NUMERIC, digits);
  p->sign = sign;
  p->decimals = decimals;
  return COBC_TREE (p);
}

cobc_tree
make_nonnumeric_literal (unsigned char *str)
{
  return COBC_TREE (make_literal (COB_TYPE_ALPHANUMERIC, str));
}

long long
literal_to_int (struct cobc_literal *l)
{
  long long val = 0;
  unsigned char *s = l->data;
  while (*s)
    val = val * 10 + *s++ - '0';
  if (l->sign < 0)
    val = -val;
  return val;
}


/*
 * Decimal
 */

cobc_tree
make_decimal (char id)
{
  struct cobc_decimal *p =
    make_tree (cobc_tag_decimal, COB_TYPE_NUMERIC, sizeof (struct cobc_decimal));
  p->id = id;
  return COBC_TREE (p);
}


/*
 * Field
 */

struct cobc_picture *
make_picture (void)
{
  struct cobc_picture *p = malloc (sizeof (struct cobc_picture));
  memset (p, 0, sizeof (struct cobc_picture));
  p->str = "";
  return p;
}

cobc_tree
make_field (cobc_tree name)
{
  struct cobc_field *p =
    make_tree (cobc_tag_field, COB_TYPE_ALPHANUMERIC, sizeof (struct cobc_field));
  p->name = associate (name, COBC_TREE (p));
  return COBC_TREE (p);
}

cobc_tree
make_field_3 (cobc_tree name, char *pic, int usage)
{
  cobc_tree x = make_field (name);
  COBC_FIELD (x)->pic = yylex_picture (pic);
  COBC_FIELD (x)->usage = usage;
  finalize_field_tree (COBC_FIELD (x));
  return x;
}

cobc_tree
make_field_x (char *name, char *pic, int usage)
{
  return make_field_3 (make_reference (make_word (name)), pic, usage);
}

struct cobc_field *
field (cobc_tree x)
{
  if (COBC_REFERENCE_P (x))
    return COBC_FIELD (COBC_REFERENCE (x)->value);
  else
    return COBC_FIELD (x);
}

int
field_size (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_literal:
      {
	return COBC_LITERAL (x)->size;
      }
    case cobc_tag_reference:
      {
	struct cobc_reference *r = COBC_REFERENCE (x);
	struct cobc_field *f = COBC_FIELD (r->value);
	if (r->length)
	  {
	    if (COBC_LITERAL_P (r->length))
	      return literal_to_int (COBC_LITERAL (r->length));
	    else
	      return -1;
	  }
	else if (r->offset)
	  {
	    if (COBC_LITERAL_P (r->offset))
	      return f->size - literal_to_int (COBC_LITERAL (r->offset));
	    else
	      return -1;
	  }
	else
	  {
	    return f->size;
	  }
      }
    default:
      abort ();
    }
}

struct cobc_field *
field_founder (struct cobc_field *p)
{
  while (p->parent)
    p = p->parent;
  return p;
}

int
field_used_any_parent (struct cobc_field *p)
{
  for (; p; p = p->parent)
    if (p->f.used)
      return 1;
  return 0;
}

int
field_used_any_child (struct cobc_field *p)
{
  if (p->f.used)
    return 1;
  for (p = p->children; p; p = p->sister)
    if (field_used_any_child (p))
      return 1;
  return 0;
}

static char *
to_cname (char *s)
{
  char *p;
  s = strdup (s);
  for (p = s; *p; p++)
    *p = (*p == '-') ? '_' : toupper (*p);
  return s;
}

static void
setup_parameters (struct cobc_field *p)
{
  /* setup cname */
  char name[BUFSIZ] = "";
  if (p->parent)
    sprintf (name, "%s$", p->parent->cname);
  strcat (name, p->name);
  p->cname = to_cname (name);

  /* determine the class */
  if (p->children)
    {
      /* group field */
      COBC_TREE_CLASS (p) = COB_TYPE_ALPHANUMERIC;

      for (p = p->children; p; p = p->sister)
	setup_parameters (p);
    }
  else if (p->level == 66)
    {
      COBC_TREE_CLASS (p) = COBC_TREE_CLASS (p->redefines);
    }
  else
    {
      /* regular field */
      if (p->usage == COBC_USAGE_INDEX)
	{
	  COBC_TREE_CLASS (p) = COB_TYPE_NUMERIC;
	  p->pic = yylex_picture ("S9(9)");
	}
      else
	switch (p->pic->category)
	  {
	  case COB_TYPE_ALPHABETIC:
	    COBC_TREE_CLASS (p) = COB_TYPE_ALPHABETIC;
	    break;
	  case COB_TYPE_NUMERIC:
	    COBC_TREE_CLASS (p) = COB_TYPE_NUMERIC;
	    break;
	  case COB_TYPE_NUMERIC_EDITED:
	  case COB_TYPE_ALPHANUMERIC:
	  case COB_TYPE_ALPHANUMERIC_EDITED:
	    COBC_TREE_CLASS (p) = COB_TYPE_ALPHANUMERIC;
	    break;
	  case COB_TYPE_NATIONAL:
	  case COB_TYPE_NATIONAL_EDITED:
	    COBC_TREE_CLASS (p) = COB_TYPE_NATIONAL;
	    break;
	  case COB_TYPE_BOOLEAN:
	    COBC_TREE_CLASS (p) = COB_TYPE_BOOLEAN;
	    break;
	  }
    }
}

static int
compute_size (struct cobc_field *p)
{
  if (p->level == 66)
    {
      /* rename */
      if (p->rename_thru)
	p->size =
	  p->rename_thru->offset + p->rename_thru->size - p->redefines->offset;
      else
	p->size = p->redefines->size;
      return p->size;
    }
  else if (p->children)
    {
      /* groups */
      int size = 0;
      struct cobc_field *c = p->children;
      for (; c; c = c->sister)
	{
	  if (c->redefines)
	    {
	      c->offset = c->redefines->offset;
	      compute_size (c);
	    }
	  else
	    {
	      if (c->f.synchronized)
		{
		  int csize = compute_size (c);
		  int border = (csize <= 4) ? 4 : 8;
		  if (size % border)
		    size += border - csize % border;
		  c->offset = p->offset + size;
		  size += border;
		}
	      else
		{
		  c->offset = p->offset + size;
		  size += compute_size (c) * c->occurs;
		}
	    }
	}
      p->size = size;
      return size;
    }
  else
    {
      /* terminals */
      switch (p->usage)
	{
	case COBC_USAGE_DISPLAY:
	  {
	    p->size = p->pic->size;
	    if (p->pic->category == COB_TYPE_NUMERIC && p->f.sign_separate)
	      p->size++;
	    break;
	  }
	case COBC_USAGE_BINARY:
	case COBC_USAGE_INDEX:
	  {
	    int len = p->pic->size;
	    if (len <= 2)
	      p->size = 1;
	    else if (len <= 4)
	      p->size = 2;
	    else if (len <= 9)
	      p->size = 4;
	    else
	      p->size = 8;
	  }
	  break;
	}
      return p->size;
    }
}

void
finalize_field_tree (struct cobc_field *p)
{
  setup_parameters (p);

  /* compute size */
  compute_size (p);
  if (!p->redefines)
    p->memory_size = p->size;
  else if (p->redefines->memory_size < p->size)
    p->redefines->memory_size = p->size;
}


/*
 * File
 */

cobc_tree
make_file (cobc_tree name)
{
  struct cobc_file *p =
    make_tree (cobc_tag_file, COB_TYPE_UNKNOWN, sizeof (struct cobc_file));
  p->name = associate (name, COBC_TREE (p));
  p->cname = to_cname (p->name);
  return COBC_TREE (p);
}


/*
 * Reference
 */

cobc_tree
make_reference (struct cobc_word *word)
{
  struct cobc_reference *p =
    make_tree (cobc_tag_reference, COB_TYPE_UNKNOWN, sizeof (struct cobc_reference));
  p->word = word;
  return COBC_TREE (p);
}

cobc_tree
copy_reference (cobc_tree ref, cobc_tree value)
{
  struct cobc_reference *r = COBC_REFERENCE (ref);
  cobc_tree x = make_reference (r->word);
  memcpy (x, ref, sizeof (struct cobc_reference));
  set_value (x, value);
  return x;
}

void
set_value (cobc_tree ref, cobc_tree value)
{
  COBC_REFERENCE (ref)->value = value;
  COBC_TREE_CLASS (ref) = COBC_TREE_CLASS (value);
}

cobc_tree
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_reference (make_word (name));
}

char *
associate (cobc_tree name, cobc_tree val)
{
  struct cobc_word *w = COBC_REFERENCE (name)->word;
  COBC_TREE_CLASS (name) = COBC_TREE_CLASS (val);
  w->items = list_add (w->items, val);
  w->count++;
  val->source_file = name->source_file;
  val->source_line = name->source_line;
  return w->name;
}


/*
 * Expression
 */

cobc_tree
make_binary_op (cobc_tree left, char op, cobc_tree right)
{
  struct cobc_binary_op *p =
    make_tree (cobc_tag_binary_op, COB_TYPE_UNKNOWN, sizeof (struct cobc_binary_op));
  p->op = op;
  p->x = left;
  p->y = right;
  switch (op)
    {
    case '+': case '-': case '*': case '/': case '^':
      /* numeric expression */
      COBC_TREE_CLASS (p) = COB_TYPE_NUMERIC;
      if (COBC_TREE_CLASS (left) != COB_TYPE_NUMERIC
	  || COBC_TREE_CLASS (right) != COB_TYPE_NUMERIC)
	goto invalid;
      break;

    case '=': case '~': case '<': case '>': case '[': case ']':
      /* comparison conditional */
      COBC_TREE_CLASS (p) = COB_TYPE_BOOLEAN;
      break;

    case '!': case '&': case '|':
      /* compound conditional */
      COBC_TREE_CLASS (p) = COB_TYPE_BOOLEAN;
      if (COBC_TREE_CLASS (left) != COB_TYPE_BOOLEAN
	  || (right && COBC_TREE_CLASS (right) != COB_TYPE_BOOLEAN))
	goto invalid;
      break;

    default:
    invalid:
      yyerror ("invalid binary-op: %s", tree_name (COBC_TREE (p)));
      abort ();
    }
  return COBC_TREE (p);
}


/*
 * Function call
 */

cobc_tree
make_funcall (const char *name, int argc,
	      void *a1, void *a2, void *a3, void *a4)
{
  struct cobc_funcall *p =
    make_tree (cobc_tag_funcall, COB_TYPE_UNKNOWN, sizeof (struct cobc_funcall));
  p->name = name;
  p->argc = argc;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  p->argv[3] = a4;
  return COBC_TREE (p);
}


/*
 * Cast to int32
 */

cobc_tree
make_cast_int32 (cobc_tree val)
{
  struct cobc_cast_int32 *p =
    make_tree (cobc_tag_cast_int32, COB_TYPE_NUMERIC, sizeof (struct cobc_cast_int32));
  p->val = val;
  return COBC_TREE (p);
}


/*
 * Label
 */

cobc_tree
make_label (cobc_tree name, struct cobc_label *section)
{
  char buff[BUFSIZ];
  struct cobc_label *p =
    make_tree (cobc_tag_label, COB_TYPE_UNKNOWN, sizeof (struct cobc_label));
  p->name = associate (name, COBC_TREE (p));
  p->section = section;
  if (section)
    sprintf (buff, "%s$%s", section->cname, p->name);
  else
    sprintf (buff, "%s", p->name);
  p->cname = to_cname (buff);
  return COBC_TREE (p);
}


/*
 * IF
 */

cobc_tree
make_if (cobc_tree test, cobc_tree stmt1, cobc_tree stmt2)
{
  struct cobc_if *p =
    make_tree (cobc_tag_if, COB_TYPE_UNKNOWN, sizeof (struct cobc_if));
  p->test  = test;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  return COBC_TREE (p);
}


/*
 * PERFORM
 */

cobc_tree
make_perform (int type)
{
  struct cobc_perform *p =
    make_tree (cobc_tag_perform, COB_TYPE_UNKNOWN, sizeof (struct cobc_perform));
  p->type = type;
  return COBC_TREE (p);
}

cobc_tree
make_perform_once (cobc_tree body)
{
  cobc_tree x = make_perform (COBC_PERFORM_ONCE);
  COBC_PERFORM (x)->body = body;
  return x;
}

cobc_tree
make_perform_exit (struct cobc_label *label)
{
  cobc_tree x = make_perform (COBC_PERFORM_EXIT);
  COBC_PERFORM (x)->data = COBC_TREE (label);
  return x;
}

void
add_perform_varying (struct cobc_perform *perf, cobc_tree name,
		     cobc_tree from, cobc_tree step, cobc_tree until)
{
  struct cobc_perform_varying *p =
    malloc (sizeof (struct cobc_perform_varying));
  p->name = name;
  p->from = from;
  p->step = step;
  p->until = until;
  p->next = NULL;
  if (perf->varying == NULL)
    perf->varying = p;
  else
    {
      struct cobc_perform_varying *l = perf->varying;
      while (l->next)
	l = l->next;
      l->next = p;
    }
}


/*
 * Sequence
 */

cobc_tree
make_sequence (struct cobc_list *list)
{
  struct cobc_sequence *p =
    make_tree (cobc_tag_sequence, COB_TYPE_UNKNOWN, sizeof (struct cobc_sequence));
  p->list = list;
  return COBC_TREE (p);
}


/*
 * Class
 */

cobc_tree
make_class (cobc_tree name, struct cobc_list *list)
{
  char buff[BUFSIZ];
  struct cobc_class *p =
    make_tree (cobc_tag_class, COB_TYPE_NUMERIC, sizeof (struct cobc_class));
  p->name = associate (name, COBC_TREE (p));
  sprintf (buff, "is_%s", to_cname (p->name));
  p->cname = strdup (buff);
  p->list = list;
  return COBC_TREE (p);
}


/*
 * Bulitin
 */

cobc_tree
make_builtin (int id)
{
  struct cobc_builtin *p =
    make_tree (cobc_tag_builtin, COB_TYPE_NUMERIC, sizeof (struct cobc_builtin));
  p->id = id;
  return COBC_TREE (p);
}


/*
 * Parameter
 */

cobc_tree
make_parameter (int type, cobc_tree x, cobc_tree y)
{
  struct cobc_parameter *p =
    make_tree (cobc_tag_parameter, COB_TYPE_UNKNOWN, sizeof (struct cobc_parameter));
  p->type = type;
  p->x = x;
  p->y = y;
  return COBC_TREE (p);
}
