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
 * Tree
 */

struct cobc_location cobc_location = { 0 };

static void *
make_tree (int tag, char class, int size)
{
  cobc_tree x = malloc (size);
  memset (x, 0, size);
  x->tag = tag;
  x->class = class;
  x->loc = cobc_location;
  return x;
}

static int
tree_to_string_1 (char *s, cobc_tree x)
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
      if (COBC_TREE_CLASS (x) == COB_NUMERIC)
	strcpy (s, COBC_LITERAL (x)->str);
      else
	sprintf (s, "\"%s\"", COBC_LITERAL (x)->str);
      break;

    case cobc_tag_field:
      strcpy (s, COBC_FIELD (x)->word->name);
      break;

    case cobc_tag_subref:
      {
	struct cobc_list *l;
	struct cobc_subref *p = COBC_SUBREF (x);
	s += tree_to_string_1 (s, p->field);
	s += sprintf (s, "(");
	for (l = p->subs; l; l = l->next)
	  {
	    s += tree_to_string_1 (s, l->item);
	    s += sprintf (s, l->next ? ", " : ")");
	  }
      }
      break;

    case cobc_tag_refmod:
      {
	struct cobc_refmod *p = COBC_REFMOD (x);
	s += sprintf (s, "%s", COBC_FIELD (x)->word->name);
	s += sprintf (s, "(");
	s += tree_to_string_1 (s, p->offset);
	s += sprintf (s, ":");
	if (p->length)
	  s += tree_to_string_1 (s, p->length);
	strcpy (s, ")");
      }
      break;

    case cobc_tag_label_name:
      sprintf (s, "%s:", COBC_LABEL_NAME (x)->word->name);
      break;

    case cobc_tag_expr:
      {
	struct cobc_expr *p = COBC_EXPR (x);
	s += sprintf (s, "(%c ", p->op);
	s += tree_to_string_1 (s, p->left);
	s += sprintf (s, " ");
	s += tree_to_string_1 (s, p->right);
	strcpy (s, ")");
      }
      break;

    case cobc_tag_cond:
      {
	cobc_tree l = COBC_COND (x)->left;
	cobc_tree r = COBC_COND (x)->right;

	if (COBC_COND (x)->type == COBC_COND_NOT)
	  {
	    s += sprintf (s, "!");
	    s += tree_to_string_1 (s, l);
	    break;
	  }

	s += sprintf (s, "(");
	s += tree_to_string_1 (s, l);
	switch (COBC_COND (x)->type)
	  {
	  case COBC_COND_EQ: s += sprintf (s, " = "); break;
	  case COBC_COND_LT: s += sprintf (s, " < "); break;
	  case COBC_COND_GT: s += sprintf (s, " > "); break;
	  case COBC_COND_LE: s += sprintf (s, " <= "); break;
	  case COBC_COND_GE: s += sprintf (s, " >= "); break;
	  case COBC_COND_AND: s += sprintf (s, " && "); break;
	  case COBC_COND_OR: s += sprintf (s, " || "); break;
	  default:
	    s += sprintf (s, " %d ", COBC_COND (x)->type);
	    break;
	  }
	if (r)
	  s += tree_to_string_1 (s, r);
	strcpy (s, ")");
	break;
      }

    default:
      sprintf (s, "#<unknown %d %p>", COBC_TREE_TAG (x), x);
    }

  return strlen (orig);
}

char *
tree_to_string (cobc_tree x)
{
  static char buff[BUFSIZ];
  tree_to_string_1 (buff, x);
  return buff;
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
cobc_tree cobc_dt;
cobc_tree cobc_status;
cobc_tree cobc_return_code;
cobc_tree cobc_switch[8];
cobc_tree cobc_int0;
cobc_tree cobc_int1;
cobc_tree cobc_int2;

struct cobc_label_name *cobc_default_error_handler;
struct cobc_label_name *cobc_standard_error_handler;

static cobc_tree
make_constant (char class, char *val)
{
  struct cobc_const *p =
    make_tree (cobc_tag_const, class, sizeof (struct cobc_const));
  p->val = val;
  return COBC_TREE (p);
}

cobc_tree
make_builtin (int id)
{
  struct cobc_builtin *p =
    make_tree (cobc_tag_builtin, COB_NUMERIC, sizeof (struct cobc_builtin));
  p->id = id;
  return COBC_TREE (p);
}

void
init_constants (void)
{
  int i;
  cobc_any         = make_constant (COB_VOID, 0);
  cobc_true        = make_constant (COB_BOOLEAN, "1");
  cobc_false       = make_constant (COB_BOOLEAN, "0");
  cobc_dt          = make_constant (COB_NUMERIC, "cob_dt");
  cobc_status      = make_constant (COB_NUMERIC, "cob_status");
  cobc_return_code = make_constant (COB_NUMERIC, "cob_retrun_code");
  cobc_zero        = make_constant (COB_NUMERIC, "cob_zero");
  cobc_space       = make_constant (COB_ALPHANUMERIC, "cob_space");
  cobc_low         = make_constant (COB_ALPHANUMERIC, "cob_low");
  cobc_high        = make_constant (COB_ALPHANUMERIC, "cob_high");
  cobc_quote       = make_constant (COB_ALPHANUMERIC, "cob_quote");
  cobc_int0        = make_integer (0);
  cobc_int1        = make_integer (1);
  cobc_int2        = make_integer (2);
  for (i = 0; i < 8; i++)
    {
      char buff[16];
      sprintf (buff, "switch[%d]", i);
      cobc_switch[i] =
	make_field_3 (make_word (buff), "9", COBC_USAGE_BINARY);
    }

  cobc_default_error_handler = COBC_LABEL_NAME (make_label_name_nodef (0, 0));
  cobc_default_error_handler->cname = "default_error_handler";
  cobc_standard_error_handler = COBC_LABEL_NAME (make_label_name_nodef (0, 0));
  cobc_standard_error_handler->cname = "standard_error_handler";
}


/*
 * Integer
 */

cobc_tree
make_integer (int val)
{
  struct cobc_integer *p =
    make_tree (cobc_tag_integer, COB_NUMERIC, sizeof (struct cobc_integer));
  p->val = val;
  return COBC_TREE (p);
}


/*
 * Index
 */

cobc_tree
make_index (cobc_tree val)
{
  struct cobc_index *p =
    make_tree (cobc_tag_index, COB_NUMERIC, sizeof (struct cobc_index));
  p->val = val;
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
  p->str = strdup (str);
  p->size = strlen (str);
  return p;
}

cobc_tree
make_numeric_literal (int sign, unsigned char *digits, int decimals)
{
  struct cobc_literal *p = make_literal (COB_NUMERIC, digits);
  p->sign = sign;
  p->decimals = decimals;
  return COBC_TREE (p);
}

cobc_tree
make_nonnumeric_literal (unsigned char *str)
{
  return COBC_TREE (make_literal (COB_ALPHANUMERIC, str));
}

long long
literal_to_int (struct cobc_literal *p)
{
  long long val = 0;
  char *s = p->str;
  while (*s)
    val = val * 10 + *s++ - '0';
  if (p->sign < 0)
    val = -val;
  return val;
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
make_field (struct cobc_word *word)
{
  struct cobc_field *p =
    make_tree (cobc_tag_field, COB_ALPHANUMERIC, sizeof (struct cobc_field));
  p->word = set_word_item (word, COBC_TREE (p));
  return COBC_TREE (p);
}

cobc_tree
make_field_3 (struct cobc_word *word, char *pic, int usage)
{
  cobc_tree x = make_field (word);
  COBC_FIELD (x)->pic = yylex_picture (pic);
  COBC_FIELD (x)->usage = usage;
  finalize_field_tree (COBC_FIELD (x));
  return x;
}

cobc_tree
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_field (make_word (name));
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
  if (p->word->count == 1)
    {
      /* there are no other field with the same name,
	 so just use the data name */
      p->cname = to_cname (p->word->name);
    }
  else
    {
      /* otherwise, use parent's cname as a prefix */
      char name[BUFSIZ] = "";
      if (p->parent)
	sprintf (name, "%s$", p->parent->cname);
      strcat (name, p->word->name);
      p->cname = to_cname (name);
    }

  /* determine the class */
  if (p->children)
    {
      /* group field */
      COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;

      for (p = p->children; p; p = p->sister)
	setup_parameters (p);
    }
  else if (p->level == 66)
    {
      COBC_TREE_CLASS (p) = COBC_TREE_CLASS (p->redefines);
    }
  else if (p->level == 88)
    {
      /* conditional field */
      COBC_TREE_CLASS (p) = COB_BOOLEAN;
    }
  else
    {
      /* regular field */
      if (p->usage == COBC_USAGE_INDEX)
	{
	  COBC_TREE_CLASS (p) = COB_NUMERIC;
	  p->pic = yylex_picture ("S9(9)");
	}
      else
	switch (p->pic->category)
	  {
	  case COB_ALPHABETIC:
	    COBC_TREE_CLASS (p) = COB_ALPHABETIC;
	    break;
	  case COB_NUMERIC:
	    COBC_TREE_CLASS (p) = COB_NUMERIC;
	    break;
	  case COB_NUMERIC_EDITED:
	  case COB_ALPHANUMERIC:
	  case COB_ALPHANUMERIC_EDITED:
	    COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;
	    break;
	  case COB_NATIONAL:
	  case COB_NATIONAL_EDITED:
	    COBC_TREE_CLASS (p) = COB_NATIONAL;
	    break;
	  case COB_BOOLEAN:
	    COBC_TREE_CLASS (p) = COB_BOOLEAN;
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
	    if (p->pic->category == COB_NUMERIC && p->f.sign_separate)
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
 * Predefined name
 */

cobc_tree
make_predefined (struct cobc_list *words)
{
  struct cobc_predefined *p =
    make_tree (cobc_tag_predefined, COB_VOID, sizeof (struct cobc_predefined));
  p->words = words;
  return COBC_TREE (p);
}


/*
 * File name
 */

cobc_tree
make_file_name (struct cobc_word *word)
{
  struct cobc_file_name *p =
    make_tree (cobc_tag_file_name, COB_VOID, sizeof (struct cobc_file_name));
  p->word = set_word_item (word, COBC_TREE (p));
  p->cname = to_cname (word->name);
  return COBC_TREE (p);
}


/*
 * Label name
 */

cobc_tree
make_label_name_nodef (struct cobc_word *word, struct cobc_word *in_word)
{
  struct cobc_label_name *p =
    make_tree (cobc_tag_label_name, COB_VOID, sizeof (struct cobc_label_name));
  p->word = word;
  p->in_word = in_word;
  return COBC_TREE (p);
}

cobc_tree
make_label_name (struct cobc_word *word)
{
  cobc_tree x = make_label_name_nodef (word, NULL);
  set_word_item (word, x);
  return x;
}

void
finalize_label_name (struct cobc_label_name *p)
{
  char name[BUFSIZ] = "";
  if (p->section)
    sprintf (name, "%s$", p->section->cname);
  strcat (name, p->word->name);
  p->cname = to_cname (name);
}


/*
 * Subscript references
 */

cobc_tree
make_subref (cobc_tree field, struct cobc_list *subs)
{
  struct cobc_subref *p =
    make_tree (cobc_tag_subref, COB_VOID, sizeof (struct cobc_subref));
  COBC_TREE_CLASS (p) = COBC_TREE_CLASS (field);
  p->field = field;
  p->subs  = subs;
  return COBC_TREE (p);
}


/*
 * Reference modifier
 */

cobc_tree
make_refmod (cobc_tree field, cobc_tree offset, cobc_tree length)
{
  struct cobc_refmod *p =
    make_tree (cobc_tag_refmod, COB_ALPHANUMERIC, sizeof (struct cobc_refmod));
  COBC_FIELD (field)->f.referenced = 1;
  p->field = field;
  p->offset = offset;
  p->length = length;
  return COBC_TREE (p);
}


/*
 * Register
 */

cobc_tree
make_register ()
{
  struct cobc_register *p =
    make_tree (cobc_tag_register, COB_NUMERIC, sizeof (struct cobc_register));
  p->id = 0;
  return COBC_TREE (p);
}


/*
 * Expression
 */

cobc_tree
make_expr (cobc_tree left, char op, cobc_tree right)
{
  struct cobc_expr *p =
    make_tree (cobc_tag_expr, COB_NUMERIC, sizeof (struct cobc_expr));
  p->op = op;
  p->left = left;
  p->right = right;
  return COBC_TREE (p);
}

int
is_numeric (cobc_tree x)
{
  if (COBC_EXPR_P (x))
    if (is_numeric (COBC_EXPR (x)->left)
	&& is_numeric (COBC_EXPR (x)->right))
      return 1;

  if (COBC_TREE_CLASS (x) == COB_NUMERIC)
    return 1;

  return 0;
}


/*
 * Class
 */

cobc_tree
make_class (struct cobc_word *word, struct cobc_list *list)
{
  char name[BUFSIZ];
  struct cobc_class *p =
    make_tree (cobc_tag_class, COB_NUMERIC, sizeof (struct cobc_class));
  sprintf (name, "is_%s", to_cname (word->name));
  p->cname = strdup (name);
  p->list = list;
  set_word_item (word, COBC_TREE (p));
  return COBC_TREE (p);
}


/*
 * Condition
 */

cobc_tree
make_cond (cobc_tree x, enum cobc_cond_type type, cobc_tree y)
{
  struct cobc_cond *p =
    make_tree (cobc_tag_cond, COB_BOOLEAN, sizeof (struct cobc_cond));
  p->type  = type;
  p->left  = x;
  p->right = y;
  return COBC_TREE (p);
}

cobc_tree
make_negative (cobc_tree x)
{
  return make_cond (x, COBC_COND_NOT, 0);
}


/*
 * If
 */

cobc_tree
make_if (cobc_tree test, cobc_tree stmt1, cobc_tree stmt2)
{
  struct cobc_if *p =
    make_tree (cobc_tag_if, COB_VOID, sizeof (struct cobc_if));
  p->test  = test;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  return COBC_TREE (p);
}


/*
 * Evaluate
 */

cobc_tree
make_evaluate (struct cobc_list *subject_list, struct cobc_list *case_list)
{
  struct cobc_evaluate *p =
    make_tree (cobc_tag_evaluate, COB_VOID, sizeof (struct cobc_evaluate));
  p->subject_list = subject_list;
  p->case_list = case_list;
  return COBC_TREE (p);
}


/*
 * Pair
 */

cobc_tree
make_pair (void *x, void *y)
{
  struct cobc_pair *p =
    make_tree (cobc_tag_pair, COB_VOID, sizeof (struct cobc_pair));
  p->x = x;
  p->y = y;
  return COBC_TREE (p);
}


/*
 * Call
 */

cobc_tree
make_call (const char *name, void (*func)(),
	   int argc, void *a1, void *a2, void *a3, void *a4)
{
  struct cobc_call *p =
    make_tree (cobc_tag_call, COB_VOID, sizeof (struct cobc_call));
  p->name = name;
  p->func = func;
  p->argc = argc;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  p->argv[3] = a4;
  return COBC_TREE (p);
}


/*
 * Assignment
 */

cobc_tree
make_assign (cobc_tree field, cobc_tree value, int rounded)
{
  struct cobc_assign *p =
    make_tree (cobc_tag_assign, COB_VOID, sizeof (struct cobc_assign));
  p->field = field;
  p->value = value;
  p->rounded = rounded;
  return COBC_TREE (p);
}

cobc_tree
make_op_assign (cobc_tree field, char op, cobc_tree value)
{
  return make_assign (field, make_expr (field, op, value), 0);
}


/*
 * Sequence
 */

cobc_tree
make_sequence (struct cobc_list *list)
{
  struct cobc_sequence *p =
    make_tree (cobc_tag_sequence, COB_VOID, sizeof (struct cobc_sequence));
  p->list = list;
  p->save_status = 0;
  return COBC_TREE (p);
}

cobc_tree
make_status_sequence (struct cobc_list *list)
{
  struct cobc_sequence *p = COBC_SEQUENCE (make_sequence (list));
  p->save_status = 1;
  return COBC_TREE (p);
}


/*
 * Perform
 */

cobc_tree
make_perform (int type)
{
  struct cobc_perform *p =
    make_tree (cobc_tag_perform, COB_VOID, sizeof (struct cobc_perform));
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

void
add_perform_varying (struct cobc_perform *perf, cobc_tree name,
		     cobc_tree from, cobc_tree by, cobc_tree until)
{
  struct cobc_perform_varying *p =
    malloc (sizeof (struct cobc_perform_varying));
  p->name = name;
  p->from = from;
  p->by = by;
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

struct cobc_word *
make_word (const char *name)
{
  struct cobc_word *p = malloc (sizeof (struct cobc_word));
  memset (p, 0, sizeof (struct cobc_word));
  p->name  = strdup (name);
  return p;
}

struct cobc_word *
set_word_item (struct cobc_word *word, cobc_tree item)
{
  if (!word->item)
    {
      word->item = item;
    }
  else
    {
      /* Create new word */
      struct cobc_word *new_word = make_word (word->name);
      new_word->item = item;
      new_word->link = word->link;
      word->link = new_word;
    }

  word->count++;
  return word;
}

struct cobc_word *
lookup_user_word (const char *name)
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

struct cobc_word *
lookup_qualified_word (struct cobc_word *word, struct cobc_field *parent)
{
  struct cobc_field *p;
  for (; word; word = word->link)
    if (word->item && COBC_FIELD_P (word->item))
      for (p = COBC_FIELD (word->item)->parent; p; p = p->parent)
	if (p == parent)
	  return word;
  return NULL;
}

void
init_word_table (void)
{
  int i;
  for (i = 0; i < HASH_SIZE; i++)
    word_table[i] = NULL;
}


/*
 * Generic item
 */

struct cobc_generic *
make_generic (int type, cobc_tree x, cobc_tree y)
{
  struct cobc_generic *p = malloc (sizeof (struct cobc_generic));
  p->type = type;
  p->x = x;
  p->y = y;
  return p;
}
