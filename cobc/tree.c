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

#include "tree.h"
#include "libcob.h"


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
list_add (struct cobc_list *l, void *x)
{
  return list_append (l, list (x));
}

struct cobc_list *
list_append (struct cobc_list *l1, struct cobc_list *l2)
{
  if (l1 == NULL)
    return l2;
  else
    {
      /* this is not the most efficient but fine with our compiler */
      struct cobc_list *p;
      for (p = l1; p->next != NULL; p = p->next);
      p->next = l2;
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

static void *
make_tree (int tag, char class, int size)
{
  cobc_tree x = malloc (size);
  x->tag = tag;
  x->class = class;
  x->loc.first_line = 0;
  x->loc.first_column = 0;
  x->loc.last_line = 0;
  x->loc.last_column = 0;
  x->loc.text = 0;
  return x;
}

static int
tree_to_string_1 (char *s, cobc_tree x)
{
  char *orig = s;

  switch (COBC_TREE_TAG (x))
    {
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
	cobc_tree l = COND_LEFT (x);
	cobc_tree r = COND_RIGHT (x);

	switch (COND_TYPE (x))
	  {
	  case COBC_COND_EQ:
	    s += sprintf (s, "(= ");
	    s += tree_to_string_1 (s, l);
	    s += sprintf (s, " ");
	    s += tree_to_string_1 (s, r);
	    strcpy (s, ")");
	    break;

	  case COBC_COND_AND:
	    s += sprintf (s, "(and ");
	    s += tree_to_string_1 (s, l);
	    s += sprintf (s, " ");
	    s += tree_to_string_1 (s, r);
	    strcpy (s, ")");
	    break;

	  default:
	    s += sprintf (s, "(cond %d ", COND_TYPE (x));
	    s += tree_to_string_1 (s, l);
	    if (r)
	      {
		s += sprintf (s, " ");
		s += tree_to_string_1 (s, r);
	      }
	    strcpy (s, ")");
	  }
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
cobc_tree cobc_param;
cobc_tree cobc_status;
cobc_tree cobc_int0;
cobc_tree cobc_int1;

static cobc_tree
make_constant (char class, char *val)
{
  struct cobc_const *p =
    make_tree (cobc_tag_const, class, sizeof (struct cobc_const));
  p->val = val;
  return COBC_TREE (p);
}

void
init_constants (void)
{
  cobc_any    = make_constant (COB_VOID, 0);
  cobc_true   = make_constant (COB_BOOLEAN, "1");
  cobc_false  = make_constant (COB_BOOLEAN, "0");
  cobc_param  = make_constant (COB_NUMERIC, "x");
  cobc_status = make_constant (COB_NUMERIC, "cob_status");
  cobc_zero   = make_constant (COB_NUMERIC, "cob_zero");
  cobc_space  = make_constant (COB_ALPHANUMERIC, "cob_space");
  cobc_low    = make_constant (COB_ALPHANUMERIC, "cob_low");
  cobc_high   = make_constant (COB_ALPHANUMERIC, "cob_high");
  cobc_quote  = make_constant (COB_ALPHANUMERIC, "cob_quote");
  cobc_int0   = make_integer (0);
  cobc_int1   = make_integer (1);
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
  p->all = 0;
  p->str = strdup (str);
  p->size = strlen (str);
  p->sign = 0;
  p->decimals = 0;
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
  p->size = 0;
  p->str = "";
  p->category = 0;
  p->digits = 0;
  p->decimals = 0;
  p->have_sign = 0;
  return p;
}

cobc_tree 
make_field (struct cobc_word *word)
{
  struct cobc_field *p =
    make_tree (cobc_tag_field, COB_ALPHANUMERIC, sizeof (struct cobc_field));
  p->size = 0;
  p->offset = 0;
  p->level = 0;
  p->occurs = 0;
  p->indexes = 0;
  p->usage = 0;
  p->category = 0;
  p->cname = NULL;
  p->file = NULL;
  p->value = NULL;
  p->cond = NULL;
  p->word = set_word_item (word, COBC_TREE (p));
  p->index_list = NULL;
  p->parent = NULL;
  p->children = NULL;
  p->sister = NULL;
  p->redefines = NULL;
  p->rename_thru = NULL;
  p->pic = NULL;
  p->f.blank_zero = 0;
  p->f.justified = 0;
  p->f.sign_separate = 0;
  p->f.sign_leading = 0;
  p->f.synchronized = 0;
  p->f.have_occurs = 0;
  p->f.used = 0;
  p->f.referenced = 0;
  return COBC_TREE (p);
}

cobc_tree 
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_field (make_word (strdup (name)));
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
setup_cname (struct cobc_field *p)
{
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

  for (p = p->children; p; p = p->sister)
    setup_cname (p);
}

static int
compute_size (struct cobc_field *p)
{
  if (p->level == 66)
    {
      /* rename */
      if (!p->rename_thru)
	{
	  p->size = p->redefines->size;
	}
      else
	{
	  struct cobc_field *c = p->redefines;
	  p->size = 0;
	  while (c != p->rename_thru)
	    {
	      p->size += c->size;
	      if (c->sister)
		c = c->sister;
	      else if (c->parent)
		c = c->parent;
	      else
		return p->size;
	    }
	  p->size += c->size;
	}
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
	case USAGE_DISPLAY:
	  {
	    p->size = p->pic->size;
	    if (p->f.sign_separate)
	      p->size++;
	    break;
	  }
	case USAGE_BINARY:
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
	case USAGE_INDEX:
	  {
	    p->size = sizeof (unsigned int);
	    break;
	  }
	case USAGE_PACKED:
	  {
	    p->size = p->pic->size / 2 + 1;
	    break;
	  }
	}
      return p->size;
    }
}

void
finalize_field_tree (struct cobc_field *p)
{
  setup_cname (p);
  compute_size (p);
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
  p->record = NULL;
  p->assign = NULL;
  p->optional = 0;
  p->organization = 0;
  p->access_mode = 0;
  p->status = NULL;
  p->key = NULL;
  p->alt_key_list = NULL;
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
  p->cname = NULL;
  p->word = word;
  p->in_word = in_word;
  p->section = NULL;
  p->children = NULL;
  return COBC_TREE (p);
}

cobc_tree
make_label_name (struct cobc_word *word)
{
  cobc_tree x = make_label_name_nodef (NULL, NULL);
  COBC_LABEL_NAME (x)->word = set_word_item (word, x);
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
  COBC_TREE (p)->loc = left->loc;
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
make_class (struct cobc_word *word, cobc_tree cond)
{
  char name[BUFSIZ];
  struct cobc_class *p =
    make_tree (cobc_tag_class, COB_NUMERIC, sizeof (struct cobc_class));
  sprintf (name, "is_%s", to_cname (word->name));
  p->cname = strdup (name);
  p->cond = cond;
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
make_unary_cond (cobc_tree x, enum cobc_cond_type type)
{
  return make_cond (x, type, 0);
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

static cobc_tree
make_call (int tag, int argc, cobc_tree a1, cobc_tree a2, cobc_tree a3)
{
  struct cobc_call *p =
    make_tree (cobc_tag_call, COB_VOID, sizeof (struct cobc_call));
  p->tag = tag;
  p->argc = argc;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  return COBC_TREE (p);
}

cobc_tree
make_call_0 (int tag)
{
  return make_call (tag, 0, 0, 0, 0);
}

cobc_tree
make_call_1 (int tag, cobc_tree a1)
{
  return make_call (tag, 1, a1, 0, 0);
}

cobc_tree
make_call_2 (int tag, cobc_tree a1, cobc_tree a2)
{
  return make_call (tag, 2, a1, a2, 0);
}

cobc_tree
make_call_3 (int tag, cobc_tree a1, cobc_tree a2, cobc_tree a3)
{
  return make_call (tag, 3, a1, a2, a3);
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
  p->test = 0;
  p->body = NULL;
  p->init = NULL;
  p->step = NULL;
  p->cond = NULL;
  return COBC_TREE (p);
}


/*
 * Various containers
 */

struct call_item *
make_call_item (int mode, cobc_tree var)
{
  struct call_item *p = malloc (sizeof (struct call_item));
  p->mode = mode;
  p->var = var;
  return p;
}

struct inspect_item *
make_inspect_item (int type, cobc_tree sy1, cobc_tree sy2, struct cobc_list *list)
{
  struct inspect_item *p = malloc (sizeof (struct inspect_item));
  p->type = type;
  p->sy1  = sy1;
  p->sy2  = sy2;
  p->list = list;
  return p;
}

struct string_item *
make_string_item (int type, cobc_tree sy)
{
  struct string_item *p = malloc (sizeof (struct string_item));
  p->type = type;
  p->sy   = sy;
  return p;
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
make_word (char *name)
{
  struct cobc_word *p = malloc (sizeof (struct cobc_word));
  p->name     = name;
  p->count    = 0;
  p->item     = NULL;
  p->link     = NULL;
  p->next     = NULL;
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
  p = make_word (strdup (name));
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
