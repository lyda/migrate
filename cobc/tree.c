/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

#include "cobc.h"
#include "error.h"


/*
 * List
 */

struct cb_list *
cons (void *x, struct cb_list *l)
{
  struct cb_list *p = malloc (sizeof (struct cb_list));
  p->item = x;
  p->next = l;
  return p;
}

struct cb_list *
list (void *x)
{
  return cons (x, NULL);
}

struct cb_list *
list_last (struct cb_list *l)
{
  if (l != NULL)
    for (; l->next != NULL; l = l->next);
  return l;
}

struct cb_list *
list_add (struct cb_list *l, void *x)
{
  return list_append (l, list (x));
}

struct cb_list *
list_append (struct cb_list *l1, struct cb_list *l2)
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

struct cb_list *
list_reverse (struct cb_list *l)
{
  struct cb_list *next, *last = NULL;
  for (; l; l = next)
    {
      next = l->next;
      l->next = last;
      last = l;
    }
  return last;
}

int
list_length (struct cb_list *l)
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

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return val % HASH_SIZE;
}

static struct cb_word *
lookup_word (const char *name)
{
  struct cb_word *p;
  int val = hash (name);

  /* find the existing word */
  if (current_program)
    for (p = current_program->word_table[val]; p; p = p->next)
      if (strcasecmp (p->name, name) == 0)
	return p;

  /* create new word */
  p = malloc (sizeof (struct cb_word));
  memset (p, 0, sizeof (struct cb_word));
  p->name = strdup (name);

  /* insert it into the table */
  if (current_program)
    {
      p->next = current_program->word_table[val];
      current_program->word_table[val] = p;
    }

  return p;
}

static struct cb_word **
make_word_table (void)
{
  size_t size = sizeof (struct cb_word *) * HASH_SIZE;
  struct cb_word **p = malloc (size);
  memset (p, 0, size);
  return p;
}


/*
 * Tree
 */

static void *
make_tree (int tag, enum cb_category category, int size)
{
  cb_tree x = malloc (size);
  memset (x, 0, size);
  x->tag = tag;
  x->category = category;
  return x;
}

static int
cb_name_1 (char *s, cb_tree x)
{
  char *orig = s;

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      if (x == cb_any)
	strcpy (s, "ANY");
      else if (x == cb_true)
	strcpy (s, "TRUE");
      else if (x == cb_false)
	strcpy (s, "FALSE");
      else if (x == cb_null)
	strcpy (s, "NULL");
      else if (x == cb_zero)
	strcpy (s, "ZERO");
      else if (x == cb_space)
	strcpy (s, "SPACE");
      else if (x == cb_low)
	strcpy (s, "LOW-VALUE");
      else if (x == cb_high)
	strcpy (s, "HIGH-VALUE");
      else if (x == cb_quote)
	strcpy (s, "QUOTE");
      else
	strcpy (s, "#<unknown constant>");
      break;

    case CB_TAG_LITERAL:
      if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC)
	strcpy (s, CB_LITERAL (x)->data);
      else
	sprintf (s, "\"%s\"", CB_LITERAL (x)->data);
      break;

    case CB_TAG_FIELD:
      strcpy (s, CB_FIELD (x)->name);
      break;

    case CB_TAG_REFERENCE:
      {
	struct cb_reference *p = CB_REFERENCE (x);
	if (p->value)
	  s += cb_name_1 (s, p->value);
	else
	  s += sprintf (s, "#<reference %s>", p->word->name);
	if (p->subs)
	  {
	    struct cb_list *l;
	    s += sprintf (s, "(");
	    for (l = p->subs; l; l = l->next)
	      {
		s += cb_name_1 (s, l->item);
		s += sprintf (s, l->next ? ", " : ")");
	      }
	  }
	if (p->offset)
	  {
	    s += sprintf (s, "(");
	    s += cb_name_1 (s, p->offset);
	    s += sprintf (s, ":");
	    if (p->length)
	      s += cb_name_1 (s, p->length);
	    strcpy (s, ")");
	  }
      }
      break;

    case CB_TAG_LABEL:
      sprintf (s, "%s", CB_LABEL (x)->name);
      break;

    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	if (p->op == '@')
	  {
	    s += sprintf (s, "(");
	    s += cb_name_1 (s, p->x);
	    s += sprintf (s, ")");
	  }
	else if (p->op == '!')
	  {
	    s += sprintf (s, "!");
	    s += cb_name_1 (s, p->x);
	  }
	else
	  {
	    s += sprintf (s, "(");
	    s += cb_name_1 (s, p->x);
	    s += sprintf (s, " %c ", p->op);
	    s += cb_name_1 (s, p->y);
	    strcpy (s, ")");
	  }
	break;
      }

    case CB_TAG_FUNCALL:
      {
	int i;
	struct cb_funcall *p = CB_FUNCALL (x);
	s += sprintf (s, "%s", p->name);
	for (i = 0; i < p->argc; i++)
	  {
	    s += sprintf (s, (i == 0) ? "(" : ", ");
	    s += cb_name_1 (s, p->argv[i]);
	  }
	s += sprintf (s, ")");
	break;
      }

    default:
      sprintf (s, "#<unknown %d %p>", CB_TREE_TAG (x), x);
    }

  return strlen (orig);
}

char *
cb_name (cb_tree x)
{
  static char buff[BUFSIZ];
  cb_name_1 (buff, x);
  return buff;
}

enum cb_class
cb_tree_class (cb_tree x)
{
  static enum cb_class category_to_class_table[] = {
    CB_CLASS_UNKNOWN,		/* CB_CATEGORY_UNKNOWN */
    CB_CLASS_ALPHABETIC,	/* CB_CATEGORY_ALPHABETIC */
    CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC */
    CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
    CB_CLASS_BOOLEAN,		/* CB_CATEGORY_BOOLEAN */
    CB_CLASS_INDEX,		/* CB_CATEGORY_INDEX */
    CB_CLASS_NATIONAL,		/* CB_CATEGORY_NATIONAL */
    CB_CLASS_NATIONAL,		/* CB_CATEGORY_NATIONAL_EDITED */
    CB_CLASS_NUMERIC,		/* CB_CATEGORY_NUMERIC */
    CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_NUMERIC_EDITED */
    CB_CLASS_OBJECT,		/* CB_CATEGORY_OBJECT_REFERENCE */
    CB_CLASS_POINTER,		/* CB_CATEGORY_DATA_POINTER */
    CB_CLASS_POINTER,		/* CB_CATEGORY_PROGRAM_POINTER */
  };

  return category_to_class_table[CB_TREE_CATEGORY (x)];
}

enum cb_category
cb_tree_category (cb_tree x)
{
  if (x->category != CB_CATEGORY_UNKNOWN)
    return x->category;

  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	if (r->offset)
	  x->category = CB_CATEGORY_ALPHANUMERIC;
	else
	  x->category = cb_tree_category (r->value);
	break;
      }
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	if (f->children)
	  x->category = CB_CATEGORY_ALPHANUMERIC;
	else if (f->usage == CB_USAGE_POINTER)
	  x->category = CB_CATEGORY_DATA_POINTER;
	else
	  switch (f->level)
	    {
	    case 66:
	      if (f->rename_thru)
		x->category = CB_CATEGORY_ALPHANUMERIC;
	      else
		x->category = cb_tree_category (CB_TREE (f->redefines));
	      break;
	    case 88:
	      x->category = CB_CATEGORY_BOOLEAN;
	      break;
	    default:
	      x->category = f->pic->category;
	      break;
	    }
	break;
      }
    default:
      abort ();
    }

  return x->category;
}

int
cb_fits_int (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (x);
	if (l->expt >= 0 && l->size < 10)
	  return 1;
	return 0;
      }
    case CB_TAG_FIELD:
      {
	struct cb_field *f = CB_FIELD (x);
	switch (f->usage)
	  {
	  case CB_USAGE_INDEX:
	    return 1;
	  case CB_USAGE_BINARY:
	    if (f->pic->expt >= 0 && f->size <= sizeof (int))
	      return 1;
	    return 0;
	  case CB_USAGE_DISPLAY:
	    if (f->pic->expt >= 0 && f->size < 10)
	      return 1;
	    return 0;
	  default:
	    return 0;
	  }
      }
    case CB_TAG_REFERENCE:
      {
	return cb_fits_int (CB_REFERENCE (x)->value);
      }
    default:
      return 0;
    }
}


/*
 * Constants
 */

cb_tree cb_any;
cb_tree cb_true;
cb_tree cb_false;
cb_tree cb_null;
cb_tree cb_zero;
cb_tree cb_space;
cb_tree cb_low;
cb_tree cb_high;
cb_tree cb_quote;
cb_tree cb_return_code;
cb_tree cb_switch[8];
cb_tree cb_int0;
cb_tree cb_int1;
cb_tree cb_int2;
cb_tree cb_error_node;

struct cb_label *cb_standard_error_handler;

static cb_tree
make_constant (enum cb_category category, const char *val)
{
  struct cb_const *p =
    make_tree (CB_TAG_CONST, category, sizeof (struct cb_const));
  p->val = val;
  return CB_TREE (p);
}

static struct cb_label *
make_constant_label (const char *name)
{
  struct cb_label *l =
    CB_LABEL (cb_build_label (make_reference (name), NULL));
  l->cname = name;
  l->need_begin = 1;
  return l;
}

void
cb_init_constants (void)
{
  int i;
  cb_error_node  = make_constant (CB_CATEGORY_UNKNOWN, 0);
  cb_any         = make_constant (CB_CATEGORY_UNKNOWN, 0);
  cb_true        = make_constant (CB_CATEGORY_BOOLEAN, "1");
  cb_false       = make_constant (CB_CATEGORY_BOOLEAN, "0");
  cb_null        = make_constant (CB_CATEGORY_DATA_POINTER, "0");
  cb_return_code = make_constant (CB_CATEGORY_NUMERIC, "cob_return_code");
  cb_zero        = make_constant (CB_CATEGORY_NUMERIC, "&cob_zero");
  cb_space       = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_space");
  cb_low         = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_low");
  cb_high        = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_high");
  cb_quote       = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_quote");
  cb_int0        = cb_build_integer (0);
  cb_int1        = cb_build_integer (1);
  cb_int2        = cb_build_integer (2);
  for (i = 0; i < 8; i++)
    {
      char buff[16];
      sprintf (buff, "switch[%d]", i);
      cb_switch[i] = cb_build_index (make_reference (buff));
    }

  cb_standard_error_handler = make_constant_label ("standard_error_handler");
}


/*
 * System-name
 */

cb_tree
cb_build_system_name (enum cb_system_name_category category, int token)
{
  struct cb_system_name *p =
    make_tree (CB_TAG_SYSTEM_NAME, CB_CATEGORY_UNKNOWN, sizeof (struct cb_system_name));
  p->category = category;
  p->token = token;
  return CB_TREE (p);
}


/*
 * Integer
 */

cb_tree
cb_build_integer (int val)
{
  struct cb_integer *p =
    make_tree (CB_TAG_INTEGER, CB_CATEGORY_NUMERIC, sizeof (struct cb_integer));
  p->val = val;
  return CB_TREE (p);
}


/*
 * String
 */

cb_tree
cb_build_string (const unsigned char *str)
{
  struct cb_string *p =
    make_tree (CB_TAG_STRING, CB_CATEGORY_ALPHANUMERIC, sizeof (struct cb_string));
  p->str = str;
  return CB_TREE (p);
}


/*
 * Literal
 */

static struct cb_literal *
build_literal (enum cb_category category, size_t size, unsigned char *data)
{
  struct cb_literal *p =
    make_tree (CB_TAG_LITERAL, category, sizeof (struct cb_literal));
  p->size = size;
  p->data = malloc (size + 1);
  memcpy (p->data, data, size);
  p->data[size] = 0;
  return p;
}

cb_tree
cb_build_numeric_literal (int sign, unsigned char *data, int expt)
{
  struct cb_literal *p =
    build_literal (CB_CATEGORY_NUMERIC, strlen (data), data);
  p->sign = sign;
  p->expt = expt;
  return CB_TREE (p);
}

cb_tree
cb_build_alphanumeric_literal (size_t size, unsigned char *data)
{
  return CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, size, data));
}

int
cb_literal_to_int (struct cb_literal *l)
{
  int i;
  int val = 0;

  for (i = 0; i < l->size; i++)
    if (l->data[i] != '0')
      break;

  if (l->size - i >= 10)
    abort ();

  for (; i < l->size; i++)
    val = val * 10 + l->data[i] - '0';
  if (l->sign < 0)
    val = -val;
  return val;
}


/*
 * Decimal
 */

cb_tree
cb_build_decimal (int id)
{
  struct cb_decimal *p =
    make_tree (CB_TAG_DECIMAL, CB_CATEGORY_NUMERIC, sizeof (struct cb_decimal));
  p->id = id;
  return CB_TREE (p);
}


/*
 * Picture
 */

#define PIC_ALPHABETIC		0x01
#define PIC_NUMERIC		0x02
#define PIC_NATIONAL		0x04
#define PIC_EDITED		0x08
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHABETIC_EDITED	(PIC_ALPHABETIC | PIC_EDITED)
#define PIC_ALPHANUMERIC_EDITED	(PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED	(PIC_NUMERIC | PIC_EDITED)
#define PIC_NATIONAL_EDITED	(PIC_NATIONAL | PIC_EDITED)

struct cb_picture *
cb_parse_picture (const char *str)
{
  const char *p;
  char category = 0;
  int idx = 0;
  int size = 0;
  int digits = 0;
  int decimals = 0;
  int s_count = 0;
  int v_count = 0;
  int buff_size = 9;
  unsigned char *buff = malloc (buff_size);
  struct cb_picture *pic = malloc (sizeof (struct cb_picture));
  memset (pic, 0, sizeof (struct cb_picture));

  for (p = str; *p; p++)
    {
      int n = 1;
      unsigned char c = *p;

    repeat:
      /* count the number of repeated chars */
      while (p[1] == c)
	p++, n++;

      /* add parenthesized numbers */
      if (p[1] == '(')
	{
	  int i = 0;
	  for (p += 2; *p != ')'; p++)
	    if (!isdigit (*p))
	      goto error;
	    else
	      i = i * 10 + (*p - '0');
	  n += i - 1;
	  goto repeat;
	}

      /* check grammar and category */
      /* FIXME: need more error check */
      switch (c)
	{
	case 'A':
	  category |= PIC_ALPHABETIC;
	  break;

	case 'X':
	  category |= PIC_ALPHANUMERIC;
	  break;

	case '9':
	  category |= PIC_NUMERIC;
	  digits += n;
	  if (v_count)
	    decimals += n;
	  break;

	case 'N':
	  category |= PIC_NATIONAL;
	  break;

	case 'S':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  s_count += n;
	  if (s_count > 1 || idx != 0)
	    goto error;
	  continue;

	case ',':
	case '.':
	  category |= PIC_NUMERIC_EDITED;
	  if (c != current_program->decimal_point)
	    break;
	  /* fall through */
	case 'V':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  v_count += n;
	  if (v_count > 1)
	    goto error;
	  break;

	case 'P':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  {
	    int at_beginning =
	         (idx == 0)					 /* P... */
	      || (idx == 2 && buff[0] == 'V');			 /* VP... */
	    int at_end =
	         (p[1] == 0)					 /* ...P */
	      || (p[1] == 'V' && p[2] == 0);			 /* ...PV */
	    if (!at_beginning && !at_end)
	      goto error;
	    if (at_beginning)
	      v_count++;		/* implicit V */
	    digits += n;
	    if (v_count)
	      decimals += n;
	    else
	      decimals -= n;
	  }
	  break;

	case '0': case 'B': case '/':
	  category |= PIC_EDITED;
	  break;

	case '*': case 'Z':
	  category |= PIC_NUMERIC_EDITED;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  digits += n;
	  if (v_count)
	    decimals += n;
	  break;

	case '+': case '-':
	  category |= PIC_NUMERIC_EDITED;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  digits += n - 1;
	  s_count++;
	  /* FIXME: need more check */
	  break;

	case 'C':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'R' && p[2] == 0))
	    goto error;
	  p++;
	  s_count++;
	  break;

	case 'D':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'B' && p[2] == 0))
	    goto error;
	  p++;
	  s_count++;
	  break;

	default:
	  if (c == current_program->currency_symbol)
	    {
	      category |= PIC_NUMERIC_EDITED;
	      digits += n - 1;
	      /* FIXME: need more check */
	      break;
	    }

	  goto error;
	}

      /* calculate size */
      if (c != 'V' && c != 'P')
	size += n;
      if (c == 'C' || c == 'D' || c == 'N')
	size += n;

      /* allocate enough pic buffer */
      while (idx + n / 64 + 1 > buff_size)
	{
	  buff_size *= 2;
	  buff = realloc (buff, buff_size);
	}

      /* store in the buffer */
      while (n > 0)
	{
	  buff[idx++] = c;
	  buff[idx++] = (n < 256) ? n : 255;
	  n -= 255;
	}
    }
  buff[idx] = 0;

  /* set picture */
  pic->orig = strdup (str);
  pic->size = size;
  pic->digits = digits;
  pic->expt = - decimals;
  pic->have_sign = s_count;

  /* set picture category */
  switch (category)
    {
    case PIC_ALPHABETIC:
      pic->category = CB_CATEGORY_ALPHABETIC;
      break;
    case PIC_NUMERIC:
      pic->category = CB_CATEGORY_NUMERIC;
      if (digits > 18)
	cb_error (_("numeric entry cannot be larger than 18 digits"));
      break;
    case PIC_ALPHANUMERIC:
    case PIC_NATIONAL:
      pic->category = CB_CATEGORY_ALPHANUMERIC;
      break;
    case PIC_NUMERIC_EDITED:
      pic->str = buff;
      pic->category = CB_CATEGORY_NUMERIC_EDITED;
      break;
    case PIC_EDITED:
    case PIC_ALPHABETIC_EDITED:
    case PIC_ALPHANUMERIC_EDITED:
    case PIC_NATIONAL_EDITED:
      pic->str = buff;
      pic->category = CB_CATEGORY_ALPHANUMERIC_EDITED;
      break;
    default:
      goto error;
    }

  return pic;

 error:
  cb_error (_("invalid picture string"));
  return pic;
}


/*
 * Field
 */

cb_tree
make_field (cb_tree name)
{
  struct cb_field *p =
    make_tree (CB_TAG_FIELD, CB_CATEGORY_UNKNOWN, sizeof (struct cb_field));
  p->name = associate (name, CB_TREE (p));
  return CB_TREE (p);
}

cb_tree
cb_build_index (cb_tree name)
{
  cb_tree x = make_field (name);
  CB_FIELD (x)->usage = CB_USAGE_INDEX;
  validate_field (CB_FIELD (x));
  return x;
}

struct cb_field *
cb_field (cb_tree x)
{
  if (CB_REFERENCE_P (x))
    return CB_FIELD (CB_REFERENCE (x)->value);
  else
    return CB_FIELD (x);
}

int
field_size (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	return CB_LITERAL (x)->size;
      }
    case CB_TAG_FIELD:
      {
	return CB_FIELD (x)->size;
      }
    case CB_TAG_REFERENCE:
      {
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);
	if (r->length)
	  {
	    if (CB_LITERAL_P (r->length))
	      return cb_literal_to_int (CB_LITERAL (r->length));
	    else
	      return -1;
	  }
	else if (r->offset)
	  {
	    if (CB_LITERAL_P (r->offset))
	      return f->size - cb_literal_to_int (CB_LITERAL (r->offset)) + 1;
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

struct cb_field *
field_founder (struct cb_field *p)
{
  while (p->parent)
    p = p->parent;
  return p;
}

/* build */

struct cb_field *
build_field (int level, cb_tree name, struct cb_field *last_field,
	     enum cb_storage storage)
{
  struct cb_field *f;
  struct cb_reference *r = CB_REFERENCE (name);

  /* checks for redefinition */
  if (level == 01 || level == 77)
    {
      if (r->word->count > 0)
	{
	  redefinition_error (name);
	  return NULL;
	}
    }
  else
    {
      struct cb_list *l;
      for (l = r->word->items; l; l = l->next)
	if (!CB_FIELD_P (l->item)
	    || CB_FIELD (l->item)->level == 01
	    || CB_FIELD (l->item)->level == 77)
	  {
	    redefinition_error (name);
	    return NULL;
	  }
    }

  /* build the field */
  f = CB_FIELD (make_field (name));
  f->level = level;
  f->usage = CB_USAGE_DISPLAY;
  f->occurs_max = 1;
  f->storage = storage;

  if (last_field && last_field->level == 88)
    last_field = last_field->parent;

  if (f->level == 01 || f->level == 77)
    {
      if (last_field)
	field_founder (last_field)->sister = f;
    }
  else if (!last_field)
    {
      cb_error_x (name, _("level number must begin with 01 or 77"));
      return NULL;
    }
  else if (f->level == 66)
    {
      struct cb_field *p;
      f->parent = field_founder (last_field);
      for (p = f->parent->children; p->sister; p = p->sister);
      p->sister = f;
    }
  else if (f->level == 88)
    {
      if (last_field->level == 88)
	f->parent = last_field->parent;
      else
	f->parent = last_field;
    }
  else if (f->level > last_field->level)
    {
      /* lower level */
      last_field->children = f;
      f->parent = last_field;
    }
  else if (f->level == last_field->level)
    {
      /* same level */
      struct cb_field *p;
    sister:
      /* ensure that there is no field with the same name
	 in the same level */
      for (p = last_field->parent->children; p; p = p->sister)
	if (strcasecmp (f->name, p->name) == 0)
	  redefinition_error (name);
      last_field->sister = f;
      f->parent = last_field->parent;
    }
  else
    {
      /* upper level */
      struct cb_field *p;
      for (p = last_field->parent; p; p = p->parent)
	if (p->level == f->level)
	  {
	    last_field = p;
	    goto sister;
	  }
      cb_error_x (name, _("field hierarchy broken"));
      return NULL;
    }

  /* inherit parent's properties */
  if (f->parent)
    {
      f->usage = f->parent->usage;
      f->indexes = f->parent->indexes;
      f->flag_sign_leading = f->parent->flag_sign_leading;
      f->flag_sign_separate = f->parent->flag_sign_separate;
    }

  return f;
}

struct cb_field *
validate_redefines (struct cb_field *field, cb_tree redefines)
{
  struct cb_field *f, *p;
  struct cb_reference *r = CB_REFERENCE (redefines);
  cb_tree x = CB_TREE (field);

  /* ISO+IEC+1989-2002: 13.16.42.2-7 */
  if (r->next)
    {
      cb_error_x (x, _("`%s' cannot be qualified"), CB_NAME (redefines));
      return NULL;
    }

  /* resolve the name in the current group (if any) */
  if (field->parent)
    {
      cb_tree parent = CB_TREE (field->parent);
      r->next = CB_REFERENCE (copy_reference (redefines, parent));
    }
  if (resolve_data_name (redefines) == cb_error_node)
    return NULL;
  f = CB_FIELD (r->value);

  /* ISO+IEC+1989-2002: 13.16.42.2-2 */
  if (f->level != field->level)
    {
      cb_error_x (x, _("level number of REDEFINES entries must be identical"));
      return NULL;
    }
  if (f->level == 66 || f->level == 88)
    {
      cb_error_x (x, _("level number of REDEFINES entry cannot be 66 or 88"));
      return NULL;
    }

  /* ISO+IEC+1989-2002: 13.16.42.2-11 */
  for (p = f->sister; p && p->redefines; p = p->sister);
  if (p != field)
    {
      cb_error_x (x, _("REDEFINES must follow the original definition"));
      return NULL;
    }

  return f;
}

static void
group_error (cb_tree x, const char *clause)
{
  cb_error_x (x, _("group item `%s' cannot have %s clause"),
	      cb_name (x), clause);
}

static void
level_error (cb_tree x, const char *clause)
{
  cb_error_x (x, _("level %02d item `%s' cannot have %s"),
	      cb_field (x)->level, cb_name (x), clause);
}

static int
validate_field_1 (struct cb_field *f)
{
  cb_tree x = CB_TREE (f);
  char *name = cb_name (x);

  /* validate OCCURS */
  if (f->flag_occurs)
    if (f->level < 2 || f->level > 49)
      level_error (x, "OCCURS");

  if (f->children)
    {
      /* group */
      if (f->pic)
	group_error (x, "PICTURE");
      if (f->flag_justified)
	group_error (x, "JUSTIFIED RIGHT");
      if (f->flag_blank_zero)
	group_error (x, "BLANK WHEN ZERO");

      for (f = f->children; f; f = f->sister)
	validate_field_1 (f);
    }
  else if (f->level == 66)
    {
    }
  else if (f->level == 88)
    {
      /* conditional name */
      if (f->pic)
	cb_error_x (x, _("level 88 item `%s' may not have PICTURE"), name);
    }
  else
    {
      /* validate PICTURE */
      {
	int need_picture = 1;
	if (f->usage == CB_USAGE_INDEX
	    || f->usage == CB_USAGE_OBJECT
	    || f->usage == CB_USAGE_POINTER
	    || f->usage == CB_USAGE_PROGRAM)
	  need_picture = 0;
	if (f->pic == NULL && need_picture != 0)
	  {
	    cb_error_x (x, _("PICTURE clause required for `%s'"), name);
	    return -1; /* cannot continue */
	  }
	if (f->pic != NULL && need_picture == 0)
	  {
	    cb_error_x (x, _("`%s' cannot have PICTURE clause"), name);
	  }
      }

      /* validate USAGE */
      if (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_PACKED)
	if (f->pic->category != CB_CATEGORY_NUMERIC)
	  cb_warning_x (x, _("`%s' not numeric item"), name);

      /* validate SIGN */

      /* validate JUSTIFIED RIGHT */
      if (f->flag_justified)
	switch (f->pic->category)
	  {
	  case CB_CATEGORY_ALPHABETIC:
	  case CB_CATEGORY_ALPHANUMERIC:
	    break;
	  default:
	    cb_error_x (x, _("`%s' cannot have JUSTIFIED RIGHT"), name);
	    break;
	  }

      /* validate SYNCHRONIZED */

      /* validate BLANK ZERO */
      if (f->flag_blank_zero)
	switch (f->pic->category)
	  {
	  case CB_CATEGORY_NUMERIC:
	    /* reconstruct the picture string */
	    if (f->pic->expt < 0)
	      {
		f->pic->str = malloc (7);
		sprintf (f->pic->str, "9%cV%c9%c",
			 f->pic->digits + f->pic->expt, 1,
			 - f->pic->expt);
		f->pic->size++;
	      }
	    else
	      {
		f->pic->str = malloc (3);
		sprintf (f->pic->str, "9%c", f->pic->digits);
	      }
	    f->pic->category = CB_CATEGORY_NUMERIC_EDITED;
	    break;
	  case CB_CATEGORY_NUMERIC_EDITED:
	    break;
	  default:
	    cb_error_x (x, _("`%s' cannot have BLANK WHEN ZERO"), name);
	    break;
	  }

      /* validate VALUE */
      if (f->values)
	{
	  struct cb_field *p;

	  if (f->values->next || CB_PARAMETER_P (f->values->item))
	    cb_error_x (x, _("only level 88 item may have multiple values"));

	  /* ISO+IEC+1989-2002: 13.16.42.2-10 */
	  for (p = f; p; p = p->parent)
	    if (p->redefines)
	      cb_error_x (x, _("entries under REDEFINES cannot have VALUE clause"));
	}
    }

  return 0;
}

/* finalize */

static char *
to_cname (const char *s)
{
  char *copy = strdup (s);
  char *p;
  for (p = copy; *p; p++)
    *p = (*p == '-') ? '_' : toupper (*p);
  return copy;
}

static void
setup_parameters (struct cb_field *f)
{
  /* setup cname */
  char name[BUFSIZ] = "";
  if (f->parent)
    sprintf (name, "%s$", f->parent->cname);
  strcat (name, f->name);
  f->cname = to_cname (name);

  /* determine the class */
  if (f->children)
    {
      /* group field */
      int flag_local = f->flag_local;
      for (f = f->children; f; f = f->sister)
	{
	  f->flag_local = flag_local;
	  setup_parameters (f);
	}
    }
  else
    {
      /* regular field */
      if (f->usage == CB_USAGE_INDEX)
	f->pic = cb_parse_picture ("S9(9)");
    }
}

static int
compute_size (struct cb_field *f)
{
  if (f->level == 66)
    {
      /* rename */
      if (f->rename_thru)
	f->size =
	  f->rename_thru->offset + f->rename_thru->size - f->redefines->offset;
      else
	f->size = f->redefines->size;
      return f->size;
    }

  if (f->children)
    {
      /* groups */
      int size = 0;
      struct cb_field *c;
      for (c = f->children; c; c = c->sister)
	{
	  if (c->redefines)
	    {
	      c->offset = c->redefines->offset;
	      compute_size (c);
	    }
	  else
	    {
	      c->offset = f->offset + size;
	      size += compute_size (c) * c->occurs_max;
	    }
	}
      f->size = size;
    }
  else
    {
      /* elementary item */
      switch (f->usage)
	{
	case CB_USAGE_BINARY:
	  {
	    int size = f->pic->size;
	    switch (cb_binary_rep)
	      {
	      case CB_BINARY_REP_1_2_4_8:
		f->size = ((size <= 2) ? 1 :
			   (size <= 4) ? 2 :
			   (size <= 9) ? 4 : 8);
		break;
	      case CB_BINARY_REP_2_4_8:
		f->size = ((size <= 4) ? 2 :
			   (size <= 9) ? 4 : 8);
		break;
	      }
	    break;
	  }
	case CB_USAGE_DISPLAY:
	  {
	    f->size = f->pic->size;
	    if (f->pic->category == CB_CATEGORY_NUMERIC
		&& f->flag_sign_separate)
	      f->size++;
	    break;
	  }
	case CB_USAGE_PACKED:
	  {
	    f->size = f->pic->size / 2;
	    if (f->pic->size % 2 || f->pic->have_sign)
	      f->size++;
	    break;
	  }
	case CB_USAGE_INDEX:
	  {
	    f->size = sizeof (int);
	    break;
	  }
	case CB_USAGE_OBJECT:
	case CB_USAGE_POINTER:
	case CB_USAGE_PROGRAM:
	  {
	    f->size = sizeof (void *);
	    break;
	  }
	default:
	  abort ();
	}
    }

  /* ISO+IEC+1989-2002: 13.16.42.2-9 */
  if (f->redefines && f->size * f->occurs_max > f->redefines->size)
    if (f->redefines->level != 01 || f->redefines->flag_external)
      cb_error_x (CB_TREE (f), _("size of `%s' larger than size of `%s'"),
		 f->name, f->redefines->name);

  return f->size;
}

static void
finalize_field (struct cb_field *f)
{
  if (f->storage == CB_STORAGE_LOCAL
      || f->storage == CB_STORAGE_LINKAGE)
    f->flag_local = 1;

  if (f->storage == CB_STORAGE_LINKAGE)
    f->flag_base = 1;

  setup_parameters (f);

  /* compute size */
  compute_size (f);
  if (!f->redefines)
    f->memory_size = f->size;
  else if (f->redefines->memory_size < f->size)
    f->redefines->memory_size = f->size;
}

static int validate_move (cb_tree src, cb_tree dst, int value_flag);

static int
validate_field_value (struct cb_field *f)
{
  if (f->values)
    validate_move (f->values->item, CB_TREE (f), 1);

  if (f->children)
    for (f = f->children; f; f = f->sister)
      validate_field_value (f);

  return 0;
}

int
validate_field (struct cb_field *f)
{
  if (validate_field_1 (f) != 0)
    return -1;
  finalize_field (f);
  validate_field_value (f);
  return 0;
}


/*
 * File
 */

struct cb_file *
build_file (cb_tree name)
{
  struct cb_file *p =
    make_tree (CB_TAG_FILE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_file));
  p->name = associate (name, CB_TREE (p));
  p->cname = to_cname (p->name);

  p->organization = COB_ORG_SEQUENTIAL;
  p->access_mode = COB_ACCESS_SEQUENTIAL;
  p->handler = cb_standard_error_handler;
  return p;
}

static void
file_error (cb_tree name, const char *clause)
{
  cb_error_x (name, _("%s clause is required for file `%s'"),
	      clause, CB_NAME (name));
}

void
validate_file (struct cb_file *f, cb_tree name)
{
  /* check RECORD/RELATIVE KEY clause */
  switch (f->organization)
    {
    case COB_ORG_INDEXED:
      if (f->key == NULL)
	file_error (name, "RECORD KEY");
      break;
    case COB_ORG_RELATIVE:
      if (f->key == NULL && f->access_mode != COB_ACCESS_SEQUENTIAL)
	file_error (name, "RELATIVE KEY");
      break;
    }
}

void
finalize_file (struct cb_file *f, struct cb_field *records)
{
  char pic[BUFSIZ];
  struct cb_field *p;

  for (p = records; p; p = p->sister)
    {
      /* check the record size */
      if (f->record_min > 0)
	if (p->size < f->record_min)
	  cb_error (_("record size too small `%s'"), p->name);
      if (f->record_max > 0)
	if (p->size > f->record_max)
	  cb_error (_("record size too large `%s'"), p->name);
    }

  /* compute the record size */
  if (f->record_min == 0)
    f->record_min = records->size;
  for (p = records; p; p = p->sister)
    {
      if (p->size < f->record_min)
	f->record_min = p->size;
      if (p->size > f->record_max)
	f->record_max = p->size;
    }

  /* create record */
  sprintf (pic, "X(%d)", f->record_max);
  f->record = CB_FIELD (make_field (make_reference (f->name)));
  f->record->usage = CB_USAGE_DISPLAY;
  f->record->pic = cb_parse_picture (pic);
  f->record->sister = records;
  validate_field (f->record);

  for (p = records; p; p = p->sister)
    {
      p->file = f;
      p->redefines = f->record;
    }
}


/*
 * Reference
 */

cb_tree
make_reference (const char *name)
{
  struct cb_reference *p =
    make_tree (CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_reference));
  p->word = lookup_word (name);
  return CB_TREE (p);
}

cb_tree
copy_reference (cb_tree ref, cb_tree value)
{
  cb_tree x = make_reference (CB_FIELD (value)->name);
  struct cb_word *word = CB_REFERENCE (x)->word;
  memcpy (x, ref, sizeof (struct cb_reference));
  CB_REFERENCE (x)->word = word;
  set_value (x, value);
  return x;
}

void
set_value (cb_tree ref, cb_tree value)
{
  ref->category = CB_CATEGORY_UNKNOWN;
  CB_REFERENCE (ref)->value = value;
}

cb_tree
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_reference (name);
}

const char *
associate (cb_tree name, cb_tree val)
{
  struct cb_word *w = CB_REFERENCE (name)->word;
  w->items = list_add (w->items, val);
  w->count++;
  val->source_file = name->source_file;
  val->source_line = name->source_line;
  CB_REFERENCE (name)->value = val;
  return w->name;
}

/* resolve data name */

cb_tree
resolve_data_name (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f;
  cb_tree v = NULL;
  
  if (r->value)
    return x;

  /* resolve reference */
  if (r->next == NULL)
    {
      /* NAME */
      switch (r->word->count)
	{
	case 0:
	  undefined_error (x);
	  goto error;
	case 1:
	  v = r->word->items->item;
	  break;
	default:
	  ambiguous_error (x);
	  goto error;
	}
    }
  else
    {
      /* NAME IN <parent> */
      struct cb_list *l;
      struct cb_field *p, *pp;

      /* resolve the parent */
      cb_tree px = CB_TREE (r->next);
      if (resolve_data_name (px) == cb_error_node)
	goto error;

      /* find the definition in the parent */
      pp = CB_FIELD (r->next->value);
      for (l = r->word->items; l; l = l->next)
	if (CB_FIELD_P (l->item))
	  for (p = CB_FIELD (l->item)->parent; p; p = p->parent)
	    if (p == pp)
	      {
		if (v)
		  {
		    ambiguous_error (x);
		    goto error;
		  }
		v = l->item;
	      }
      if (v == NULL)
	{
	  if (pp->children == NULL)
	    cb_error_x (px, _("`%s' not a group"), pp->name);
	  else
	    undefined_error (x);
	  goto error;
	}
    }

  /* validate data name */
  if (!CB_FIELD_P (v))
    {
      cb_error_x (x, _("`%s' not data name"), r->word->name);
      abort ();
      goto error;
    }

  f = CB_FIELD (v);

  set_value (x, v);

  return x;

 error:
  r->value = cb_error_node;
  return cb_error_node;
}

int
validate_identifier (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f = CB_FIELD (r->value);
  const char *name = r->word->name;

  /* check the number of subscripts */
  if (list_length (r->subs) != f->indexes)
    {
      switch (f->indexes)
	{
	case 0:
	  cb_error_x (x, _("`%s' cannot be subscripted"), name);
	  break;
	case 1:
	  cb_error_x (x, _("`%s' requires 1 subscript"), name);
	  break;
	default:
	  cb_error_x (x, _("`%s' requires %d subscripts"), name, f->indexes);
	  break;
	}
      return -1;
    }

  /* check the range of constant subscripts */
  if (r->subs)
    {
      struct cb_field *p;
      struct cb_list *l = r->subs = list_reverse (r->subs);

      for (p = f; p; p = p->parent)
	if (p->flag_occurs)
	  {
	    cb_tree sub = l->item;
	    if (CB_LITERAL_P (sub))
	      {
		int n = cb_literal_to_int (CB_LITERAL (sub));
		if (n < p->occurs_min || n > p->occurs_max)
		  cb_error_x (x, _("subscript of `%s' out of bounds: %d"),
			     name, n);
	      }
	    l = l->next;
	  }

      r->subs = list_reverse (r->subs);
    }

  /* check the range of constant reference modification */
  if (r->offset && CB_LITERAL_P (r->offset))
    {
      int offset = cb_literal_to_int (CB_LITERAL (r->offset));
      if (offset < 1 || offset > f->size)
	cb_error_x (x, _("offset of `%s' out of bounds: %d"), name, offset);
      else if (r->length && CB_LITERAL_P (r->length))
	{
	  int length = cb_literal_to_int (CB_LITERAL (r->length));
	  if (length < 1 || length > f->size - offset + 1)
	    cb_error_x (x, _("length of `%s' out of bounds: %d"), name, length);
	}
    }

  return 0;
}

/* resolve label name */

static cb_tree
resolve_label_in (const char *name, struct cb_label *section)
{
  struct cb_list *l;
  for (l = section->children; l; l = l->next)
    if (strcasecmp (name, CB_LABEL (l->item)->name) == 0)
      return l->item;
  return cb_error_node;
}

cb_tree
resolve_label (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  cb_tree v;

  if (r->next == NULL)
    {
      /* LABEL */
      switch (r->word->count)
	{
	case 0:
	  undefined_error (x);
	  goto error;
	case 1:
	  v = r->word->items->item;
	  break;
	default:
	  v = resolve_label_in (r->word->name, CB_LABEL (r->offset));
	  if (v == cb_error_node)
	    if (CB_LABEL_P (r->word->items->item))
	      {
		ambiguous_error (x);
		goto error;
	      }
	  break;
	}
    }
  else
    {
      /* LABEL IN LABEL*/
      struct cb_reference *sr = r->next;
      cb_tree sx = CB_TREE (sr);

      switch (sr->word->count)
	{
	case 0:
	  undefined_error (sx);
	  goto error;
	case 1:
	  v = resolve_label_in (r->word->name, sr->word->items->item);
	  if (v == cb_error_node)
	    {
	      undefined_error (x);
	      goto error;
	    }
	  break;
	default:
	  cb_error_x (sx, _("`%s' not section name"), sr->word->name);
	  goto error;
	}
    }

  if (!CB_LABEL_P (v))
    {
      cb_error_x (x, _("`%s' not label name"), r->word->name);
      goto error;
    }

  CB_LABEL (v)->need_begin = 1;
  if (r->length)
    CB_LABEL (v)->need_return = 1;

  r->value = v;
  return v;

 error:
  r->value = cb_error_node;
  return cb_error_node;
}

/* resolve file name */

cb_tree
resolve_file_name (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (CB_FILE_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return r->value;
	}
      cb_error_x (x, _("`%s' not file name"), r->word->name);
      break;
    }

  r->value = cb_error_node;
  return cb_error_node;
}

/* resolve class name */

cb_tree
resolve_class_name (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (CB_PROPOSITION_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return r->value;
	}
      cb_error_x (x, _("`%s' not class name"), r->word->name);
      break;
    }

  r->value = cb_error_node;
  return cb_error_node;
}

/* resolve mnemonic-name */

cb_tree
resolve_mnemonic_name (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (CB_SYSTEM_NAME_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return x;
	}
      cb_error_x (x, _("`%s' not mnemonic name"), r->word->name);
      break;
    }

  r->value = cb_error_node;
  return cb_error_node;
}


/*
 * Expression
 */

cb_tree
cb_build_binary_op (cb_tree left, char op, cb_tree right)
{
  struct cb_binary_op *p;
  enum cb_category category;

  if (left == cb_error_node || right == cb_error_node)
    return cb_error_node;

  /* validate operators */
  switch (op)
    {
    case '+': case '-': case '*': case '/': case '^':
      /* numeric expression */
      if (CB_TREE_CLASS (left) != CB_CLASS_NUMERIC
	  || CB_TREE_CLASS (right) != CB_CLASS_NUMERIC)
	goto invalid;
      category = CB_CATEGORY_NUMERIC;
      break;

    case '=': case '~': case '<': case '>': case '[': case ']':
      /* comparison conditional */
      category = CB_CATEGORY_BOOLEAN;
      break;

    case '!': case '&': case '|':
      /* compound conditional */
      if (CB_TREE_CLASS (left) != CB_CLASS_BOOLEAN
	  || (right && CB_TREE_CLASS (right) != CB_CLASS_BOOLEAN))
	goto invalid;
      category = CB_CATEGORY_BOOLEAN;
      break;

    case '@':
      /* parentheses */
      category = CB_TREE_CATEGORY (left);
      break;

    default:
    invalid:
      cb_error ("invalid binary-op: %s", cb_name (CB_TREE (p)));
      abort ();
    }

  p = make_tree (CB_TAG_BINARY_OP, category, sizeof (struct cb_binary_op));
  p->op = op;
  p->x = left;
  p->y = right;
  return CB_TREE (p);
}

cb_tree
cb_build_connective_op (struct cb_list *l, char op)
{
  cb_tree e = l->item;
  for (l = l->next; l; l = l->next)
    e = cb_build_binary_op (e, op, l->item);
  return e;
}


/*
 * Function call
 */

cb_tree
cb_build_funcall (const char *name, int argc,
	      void *a1, void *a2, void *a3, void *a4)
{
  struct cb_funcall *p =
    make_tree (CB_TAG_FUNCALL, CB_CATEGORY_BOOLEAN, sizeof (struct cb_funcall));
  p->name = name;
  p->argc = argc;
  p->argv[0] = a1;
  p->argv[1] = a2;
  p->argv[2] = a3;
  p->argv[3] = a4;
  return CB_TREE (p);
}


/*
 * Cast to integer
 */

cb_tree
cb_build_cast_integer (cb_tree val)
{
  struct cb_cast_integer *p =
    make_tree (CB_TAG_CAST_INTEGER, CB_CATEGORY_NUMERIC, sizeof (struct cb_cast_integer));
  p->val = val;
  return CB_TREE (p);
}


/*
 * Label
 */

cb_tree
cb_build_label (cb_tree name, struct cb_label *section)
{
  char buff[BUFSIZ];
  struct cb_label *p =
    make_tree (CB_TAG_LABEL, CB_CATEGORY_UNKNOWN, sizeof (struct cb_label));
  p->name = associate (name, CB_TREE (p));
  p->section = section;
  if (section)
    sprintf (buff, "%s$%s", section->cname, p->name);
  else
    sprintf (buff, "%s", p->name);
  p->cname = to_cname (buff);
  return CB_TREE (p);
}


/*
 * IF
 */

cb_tree
cb_build_if (cb_tree test, cb_tree stmt1, cb_tree stmt2)
{
  struct cb_if *p =
    make_tree (CB_TAG_IF, CB_CATEGORY_UNKNOWN, sizeof (struct cb_if));
  p->test  = test;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  return CB_TREE (p);
}


/*
 * PERFORM
 */

cb_tree
cb_build_perform (int type)
{
  struct cb_perform *p =
    make_tree (CB_TAG_PERFORM, CB_CATEGORY_UNKNOWN, sizeof (struct cb_perform));
  p->type = type;
  return CB_TREE (p);
}

cb_tree
cb_build_perform_once (cb_tree body)
{
  cb_tree x = cb_build_perform (CB_PERFORM_ONCE);
  CB_PERFORM (x)->body = body;
  return x;
}

cb_tree
cb_build_perform_exit (struct cb_label *label)
{
  cb_tree x = cb_build_perform (CB_PERFORM_EXIT);
  CB_PERFORM (x)->data = CB_TREE (label);
  return x;
}

void
cb_add_perform_varying (struct cb_perform *perf, cb_tree name,
			cb_tree from, cb_tree step, cb_tree until)
{
  struct cb_perform_varying *p =
    malloc (sizeof (struct cb_perform_varying));
  p->name = name;
  p->from = from;
  p->step = step;
  p->until = until;
  p->next = NULL;
  if (perf->varying == NULL)
    perf->varying = p;
  else
    {
      struct cb_perform_varying *l = perf->varying;
      while (l->next)
	l = l->next;
      l->next = p;
    }
}


/*
 * Sequence
 */

cb_tree
make_sequence (struct cb_list *list)
{
  struct cb_sequence *p =
    make_tree (CB_TAG_SEQUENCE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_sequence));
  p->list = list;
  return CB_TREE (p);
}


/*
 * Statement
 */

struct cb_statement *
cb_build_statement (const char *name)
{
  struct cb_statement *p =
    make_tree (CB_TAG_STATEMENT, CB_CATEGORY_UNKNOWN, sizeof (struct cb_statement));
  p->name = name;
  CB_TREE (p)->source_file = cb_source_file;
  CB_TREE (p)->source_line = cb_source_line;
  return p;
}


/*
 * Proposition
 */

cb_tree
cb_build_proposition (cb_tree name, struct cb_list *list)
{
  char buff[BUFSIZ];
  struct cb_proposition *p =
    make_tree (CB_TAG_PROPOSITION, CB_CATEGORY_BOOLEAN, sizeof (struct cb_proposition));
  p->name = associate (name, CB_TREE (p));
  sprintf (buff, "is_%s", to_cname (p->name));
  p->cname = strdup (buff);
  p->list = list;
  return CB_TREE (p);
}


/*
 * Parameter
 */

cb_tree
cb_build_parameter (int type, cb_tree x, cb_tree y)
{
  struct cb_parameter *p =
    make_tree (CB_TAG_PARAMETER, CB_CATEGORY_UNKNOWN, sizeof (struct cb_parameter));
  p->type = type;
  p->x = x;
  p->y = y;
  return CB_TREE (p);
}


/*
 * Program
 */

struct cb_program *
cb_build_program (void)
{
  struct cb_program *p = malloc (sizeof (struct cb_program));
  memset (p, 0, sizeof (struct cb_program));
  p->decimal_point = '.';
  p->currency_symbol = '$';
  p->numeric_separator = ',';
  p->word_table = make_word_table ();
  return p;
}


/*
 * Numerical operation
 */

#define add_stmt(s,x) \
  CB_SEQUENCE (s)->list = list_add (CB_SEQUENCE (s)->list, x)

static cb_tree
decimal_alloc (void)
{
  cb_tree x = cb_build_decimal (current_program->decimal_index++);
  if (current_program->decimal_index > current_program->decimal_index_max)
    current_program->decimal_index_max = current_program->decimal_index;
  return x;
}

static void
decimal_free (void)
{
  current_program->decimal_index--;
}

static void
decimal_compute (cb_tree s, char op, cb_tree x, cb_tree y)
{
  const char *func;
  switch (op)
    {
    case '+': func = "cob_decimal_add"; break;
    case '-': func = "cob_decimal_sub"; break;
    case '*': func = "cob_decimal_mul"; break;
    case '/': func = "cob_decimal_div"; break;
    case '^': func = "cob_decimal_pow"; break;
    default: abort ();
    }
  add_stmt (s, cb_build_funcall_2 (func, x, y));
}

static void
decimal_expand (cb_tree s, cb_tree d, cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
      {
	cb_tree e;
	if (x == cb_zero)
	  e = cb_build_funcall_2 ("cob_decimal_set_int", d, cb_int0);
	else
	  abort ();
	add_stmt (s, e);
	break;
      }
    case CB_TAG_LITERAL:
      {
	/* set d, N */
	struct cb_literal *l = CB_LITERAL (x);
	if (l->size < 10 && l->expt == 0)
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_int",
				       d, cb_build_cast_integer (x)));
	else
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_REFERENCE:
      {
	/* set d, X */
	struct cb_field *f = cb_field (x);

	/* check numeric */
	if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE))
	  if (f->usage == CB_USAGE_DISPLAY)
	    add_stmt (s, cb_build_funcall_2 ("cob_check_numeric",
					 x, cb_build_string (f->name)));

	if (cb_fits_int (x))
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_int",
				       d, cb_build_cast_integer (x)));
	else
	  add_stmt (s, cb_build_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	if (p->op == '@')
	  {
	    decimal_expand (s, d, p->x);
	  }
	else
	  {
	    /* set d, X
	     * set t, Y
	     * OP d, t */
	    cb_tree t = decimal_alloc ();
	    decimal_expand (s, d, p->x);
	    decimal_expand (s, t, p->y);
	    decimal_compute (s, p->op, d, t);
	    decimal_free ();
	  }
	break;
      }
    default:
      abort ();
    }
}

static void
decimal_assign (cb_tree s, cb_tree x, cb_tree d, int round)
{
  const char *func =
    round ? "cob_decimal_get_field_r" : "cob_decimal_get_field";
  add_stmt (s, cb_build_funcall_2 (func, d, x));
}

static cb_tree
build_decimal_assign (struct cb_list *vars, char op, cb_tree val)
{
  struct cb_list *l;
  cb_tree s1 = make_sequence (NULL);
  cb_tree s2 = make_sequence (NULL);
  cb_tree d = decimal_alloc ();

  /* set d, VAL */
  decimal_expand (s2, d, val);

  if (op == 0)
    {
      for (l = vars; l; l = l->next)
	{
	  /* set VAR, d */
	  struct cb_parameter *p = l->item;
	  decimal_assign (s2, p->x, d, p->type);
	  add_stmt (s1, s2);
	  if (l->next)
	    s2 = make_sequence (NULL);
	}
    }
  else
    {
      cb_tree t = decimal_alloc ();
      for (l = vars; l; l = l->next)
	{
	  /* set t, VAR
	   * OP t, d
	   * set VAR, t
	   */
	  struct cb_parameter *p = l->item;
	  decimal_expand (s2, t, p->x);
	  decimal_compute (s2, op, t, d);
	  decimal_assign (s2, p->x, t, p->type);
	  add_stmt (s1, s2);
	  if (l->next)
	    s2 = make_sequence (NULL);
	}
      decimal_free ();
    }

  decimal_free ();
  return s1;
}

cb_tree
cb_build_assign (struct cb_list *vars, char op, cb_tree val)
{
  struct cb_list *l;

  for (l = vars; l; l = l->next)
    if (l->item == cb_error_node)
      return cb_error_node;

  if (val == cb_error_node)
    return cb_error_node;

  if (!CB_BINARY_OP_P (val))
    if (op == '+' || op == '-')
      {
	for (l = vars; l; l = l->next)
	  {
	    struct cb_parameter *p = CB_PARAMETER (l->item);
	    if (op == '+')
	      l->item = cb_build_add (p->x, val, p->type);
	    else
	      l->item = cb_build_sub (p->x, val, p->type);
	  }
	return make_sequence (vars);
      }

  return build_decimal_assign (vars, op, val);
}


/*
 * ADD/SUBTRACT/MOVE CORRESPONDING
 */

cb_tree
cb_build_add (cb_tree v, cb_tree n, int round)
{
  if (cb_field (v)->usage == CB_USAGE_INDEX)
    return cb_build_move (cb_build_binary_op (v, '+', n), v);

  if (round == 0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_add_int", v, cb_build_cast_integer (n));
  if (round)
    return cb_build_funcall_2 ("cob_add_r", v, n);
  else
    return cb_build_funcall_2 ("cob_add", v, n);
}

cb_tree
cb_build_sub (cb_tree v, cb_tree n, int round)
{
  if (cb_field (v)->usage == CB_USAGE_INDEX)
    return cb_build_move (cb_build_binary_op (v, '-', n), v);

  if (round == 0 && cb_fits_int (n))
    return cb_build_funcall_2 ("cob_sub_int", v, cb_build_cast_integer (n));
  if (round)
    return cb_build_funcall_2 ("cob_sub_r", v, n);
  else
    return cb_build_funcall_2 ("cob_sub", v, n);
}

static void
warning_destination (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  struct cb_field *f = CB_FIELD (r->value);
  cb_tree loc = CB_TREE (f);

  if (r->offset)
    return;

  if (f->pic)
    cb_warning_x (loc, _("`%s' defined here as PIC %s"), f->name, f->pic->orig);
  else
    cb_warning_x (loc, _("`%s' defined here as a group of length %d"),
	      f->name, f->size);
}

static int
move_error (cb_tree src, cb_tree dst, int value_flag, int flag, const char *msg)
{
  cb_tree loc = src->source_line ? src : dst;

  /* for VALUE clause */
  if (value_flag)
    {
      if (cb_standard == CB_STANDARD_COBOL2002)
	{
	  cb_error_x (loc, msg);
	  return -1;
	}
      else if (flag)
	{
	  cb_warning_x (loc, msg);
	  return 0;
	}
    }

  /* for MOVE statement */
  if (flag)
    {
      cb_warning_x (loc, msg);
      warning_destination (dst);
    }
  return 0;
}

static int
validate_move (cb_tree src, cb_tree dst, int value_flag)
{
  struct cb_field *f = cb_field (dst);
  cb_tree loc = src->source_line ? src : dst;

  switch (CB_TREE_TAG (src))
    {
    case CB_TAG_CONST:
      {
	if (src == cb_space)
	  {
	    if (f->pic)
	      if (f->pic->category == CB_CATEGORY_NUMERIC
		  || f->pic->category == CB_CATEGORY_NUMERIC_EDITED)
		goto invalid;
	  }
	else if (src == cb_zero)
	  {
	    if (f->pic)
	      if (f->pic->category == CB_CATEGORY_ALPHABETIC)
		goto invalid;
	  }
	break;
      }
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (src);

	/* TODO: ALL literal */

	if (CB_TREE_CLASS (src) == CB_CLASS_NUMERIC)
	  {
	    /* Numeric literal */
	    int i;
	    int most_significant = -999;
	    int least_significant = 999;

	    /* compute the most significant figure place */
	    for (i = 0; i < l->size; i++)
	      if (l->data[i] != '0')
		break;
	    if (i != l->size)
	      most_significant = l->size + l->expt - i - 1;

	    /* compute the least significant figure place */
	    for (i = 0; i < l->size; i++)
	      if (l->data[l->size - i - 1] != '0')
		break;
	    if (i != l->size)
	      least_significant = l->expt + i;

	    /* value check */
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHANUMERIC:
	      case CB_CATEGORY_ALPHANUMERIC_EDITED:
		{
		  if (value_flag)
		    goto expect_alphanumeric;

		  if (l->expt == 0)
		    goto expect_alphanumeric;
		  else
		    goto invalid;
		}
	      case CB_CATEGORY_NUMERIC:
		{
		  if (f->pic->expt > 0)
		    {
		      /* check for PIC 9(n)P(m) */
		      if (least_significant < f->pic->expt)
			goto value_mismatch;
		    }
		  else if (f->pic->expt < -f->pic->size)
		    {
		      /* check for PIC P(n)9(m) */
		      if (most_significant >= f->pic->expt + f->pic->size)
			goto value_mismatch;
		    }
		  break;
		}
	      case CB_CATEGORY_NUMERIC_EDITED:
		{
		  if (value_flag)
		    goto expect_alphanumeric;

		  /* TODO */
		  break;
		}
	      default:
		if (value_flag)
		  goto expect_alphanumeric;
		goto invalid;
	      }

	    /* sign check */
	    if (l->sign != 0 && !f->pic->have_sign)
	      {
		if (value_flag)
		  {
		    cb_error_x (loc, _("data item not signed"));
		    return -1;
		  }
		if (cb_warn_constant)
		  {
		    cb_warning_x (loc, _("ignoring negative sign"));
		  }
	      }

	    /* size check */
	    if (least_significant < f->pic->expt)
	      goto size_overflow;
	    if (most_significant >= (f->pic->digits
				     + (f->pic->expt < 0 ? f->pic->expt : 0)))
	      goto size_overflow;
	  }
	else
	  {
	    /* Alphanumeric literal */

	    /* value check */
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHABETIC:
		{
		  int i;
		  for (i = 0; i < l->size; i++)
		    if (!isalpha (l->data[i]) && !isspace (l->data[i]))
		      goto value_mismatch;
		  break;
		}
	      case CB_CATEGORY_NUMERIC:
		goto expect_numeric;
	      case CB_CATEGORY_NUMERIC_EDITED:
		if (!value_flag)
		  goto expect_numeric;

		/* TODO: validate the value */
		break;
	      default:
		break;
	      }

	    /* size check */
	    {
	      int size = field_size (dst);
	      if (size >= 0 && l->size > size)
		goto size_overflow;
	    }
	  }
	break;
      }
    case CB_TAG_FIELD:
    case CB_TAG_REFERENCE:
      {
	/* non-elementary move */
	if (cb_field (src)->children || cb_field (dst)->children)
	  break;

	/* elementary move */
	switch (CB_TREE_CATEGORY (src))
	  {
	  case CB_CATEGORY_ALPHANUMERIC:
	    break;
	  case CB_CATEGORY_ALPHABETIC:
	  case CB_CATEGORY_ALPHANUMERIC_EDITED:
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_NUMERIC:
	      case CB_CATEGORY_NUMERIC_EDITED:
		goto invalid;
	      default:
		break;
	      }
	    break;
	  case CB_CATEGORY_NUMERIC:
	  case CB_CATEGORY_NUMERIC_EDITED:
	    switch (CB_TREE_CATEGORY (dst))
	      {
	      case CB_CATEGORY_ALPHABETIC:
		goto invalid;
	      case CB_CATEGORY_ALPHANUMERIC:
	      case CB_CATEGORY_ALPHANUMERIC_EDITED:
		if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC
		    && cb_field (src)->pic->expt < 0)
		  goto invalid;
	      default:
		break;
	      }
	    break;
	  default:
	    abort ();
	  }
	break;
      }
    case CB_TAG_BINARY_OP:
      break;
    default:
      abort ();
    }
  return 0;

 invalid:
  if (value_flag)
    cb_error_x (loc, _("invalid VALUE clause"));
  else
    cb_error_x (loc, _("invalid MOVE statement"));
  return -1;

 expect_numeric:
  return move_error (src, dst, value_flag, cb_warn_strict_typing,
		     _("numeric value is expected"));

 expect_alphanumeric:
  return move_error (src, dst, value_flag, cb_warn_strict_typing,
		     _("alphanumeric value is expected"));

 value_mismatch:
  return move_error (src, dst, value_flag, cb_warn_constant,
		     _("value does not fit the picture string"));

 size_overflow:
  return move_error (src, dst, value_flag, cb_warn_constant,
		     _("value size exceeds data size"));
}

cb_tree
cb_build_move (cb_tree src, cb_tree dst)
{
  validate_move (src, dst, 0);
  return cb_build_funcall_2 ("@move", src, dst);
}

static struct cb_list *
build_corr_1 (cb_tree (*func)(), cb_tree x1, cb_tree x2,
	      int opt, struct cb_list *l)
{
  struct cb_field *f1, *f2;
  for (f1 = cb_field (x1)->children; f1; f1 = f1->sister)
    if (!f1->redefines && !f1->flag_occurs)
      for (f2 = cb_field (x2)->children; f2; f2 = f2->sister)
	if (!f2->redefines && !f2->flag_occurs)
	  if (strcmp (f1->name, f2->name) == 0)
	    {
	      cb_tree t1 = copy_reference (x1, CB_TREE (f1));
	      cb_tree t2 = copy_reference (x2, CB_TREE (f2));
	      if (f1->children && f2->children)
		l = build_corr_1 (func, t1, t2, opt, l);
	      else
		{
		  if (opt < 0)
		    l = cons (func (t1, t2), l);
		  else
		    l = cons (func (t1, t2, opt), l);
		}
	    }
  return l;
}

cb_tree
cb_build_corr (cb_tree (*func)(), cb_tree x1, cb_tree x2, int opt)
{
  return make_sequence (build_corr_1 (func, x1, x2, opt, NULL));
}


/*
 * DIVIDE
 */

cb_tree
cb_build_divide (cb_tree dividend, cb_tree divisor,
		 cb_tree quotient, cb_tree remainder)
{
  struct cb_list *l = NULL;
  struct cb_parameter *pq = CB_PARAMETER (quotient);
  struct cb_parameter *pr = CB_PARAMETER (remainder);
  l = list_add (l, cb_build_funcall_4 ("cob_div_quotient",
				   dividend, divisor, pq->x,
				   pq->type ? cb_int1 : cb_int0));
  l = list_add (l, cb_build_funcall_1 ("cob_div_remainder", pr->x));
  return make_sequence (l);
}


/*
 * Condition
 */

static cb_tree
build_cond_88 (cb_tree x)
{
  struct cb_field *f = cb_field (x);
  struct cb_list *l;
  cb_tree c1 = NULL;

  /* refer to parent's data storage */
  x = copy_reference (x, CB_TREE (f->parent));

  /* build condition */
  for (l = f->values; l; l = l->next)
    {
      cb_tree c2;
      if (CB_PARAMETER_P (l->item))
	{
	  /* VALUE THRU VALUE */
	  struct cb_parameter *p = CB_PARAMETER (l->item);
	  c2 = cb_build_binary_op (cb_build_binary_op (p->x, '[', x),
				  '&',
				  cb_build_binary_op (x, '[', p->y));
	}
      else
	{
	  /* VALUE */
	  c2 = cb_build_binary_op (x, '=', l->item);
	}
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = cb_build_binary_op (c1, '|', c2);
    }
  return c1;
}

cb_tree
cb_build_cond (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_CONST:
    case CB_TAG_FUNCALL:
      return x;
    case CB_TAG_REFERENCE:
      {
	/* level 88 condition */
	if (cb_field (x)->level == 88)
	  {
	    /* We need to build a 88 condition at every occurrence
	       instead of once at the beginning because a 88 item
	       may be subscripted (i.e., it is not a constant tree). */
	    return cb_build_cond (build_cond_88 (x));
	  }

	abort ();
      }
    case CB_TAG_BINARY_OP:
      {
	struct cb_binary_op *p = CB_BINARY_OP (x);
	switch (p->op)
	  {
	  case '@':
	    return cb_build_cond (p->x);
	  case '!':
	    p->x = cb_build_cond (p->x);
	    break;
	  case '&': case '|':
	    p->x = cb_build_cond (p->x);
	    p->y = cb_build_cond (p->y);
	    break;
	  default:
	    if (CB_INDEX_P (p->x) || CB_INDEX_P (p->y))
	      return x;
	    else if (CB_BINARY_OP_P (p->x) || CB_BINARY_OP_P (p->y))
	      {
		/* decimal comparison */
		cb_tree s = make_sequence (NULL);
		cb_tree d1 = decimal_alloc ();
		cb_tree d2 = decimal_alloc ();
		decimal_expand (s, d1, p->x);
		decimal_expand (s, d2, p->y);
		add_stmt (s, cb_build_funcall_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		p->x = s;
	      }
	    else if (CB_LITERAL_P (p->y))
	      {
		struct cb_literal *l = CB_LITERAL (p->y);
		int size = field_size (p->x);

		if (CB_TREE_CLASS (p->x) == CB_CLASS_NUMERIC
		    && CB_TREE_CLASS (p->y) == CB_CLASS_NUMERIC)
		  {
		    if (cb_fits_int (p->y))
		      p->x = cb_build_funcall_2 ("cob_cmp_int",
						 p->x, cb_build_cast_integer (p->y));
		    else
		      p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
		  }
		else if (size > 0 && size >= l->size && !l->all)
		  p->x = cb_build_funcall_2 ("@memcmp", p->x, p->y);
		else
		  p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    else
	      {
		/* field comparison */
		p->x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    break;
	  }
	return x;
      }
    default:
      abort ();
    }
}


/*
 * EVALUATE
 */

static cb_tree
evaluate_test (cb_tree s, cb_tree o)
{
  struct cb_parameter *p;

  /* ANY is always true */
  if (o == cb_any)
    return cb_true;

  /* object TRUE or FALSE */
  if (o == cb_true)
    return s;
  if (o == cb_false)
    return cb_build_negation (s);

  p = CB_PARAMETER (o);

  /* subject TRUE or FALSE */
  if (s == cb_true)
    return p->type ? cb_build_negation (p->x) : p->x;
  if (s == cb_false)
    return p->type ? p->x : cb_build_negation (p->x);

  /* x THRU y */
  if (p->y)
    {
      cb_tree x = cb_build_binary_op (cb_build_binary_op (p->x, '[', s),
				     '&',
				     cb_build_binary_op (s, '[', p->y));
      return p->type ? cb_build_negation (x) : x;
    }

  /* regular comparison */
  if (p->type)
    return cb_build_binary_op (s, '~', p->x);
  else
    return cb_build_binary_op (s, '=', p->x);
}

static cb_tree
evaluate_internal (struct cb_list *subject_list, struct cb_list *case_list)
{
  cb_tree stmt;
  cb_tree c1 = NULL;
  struct cb_list *subjs, *whens, *objs;

  if (case_list == NULL)
    return NULL;

  whens = case_list->item;
  stmt = whens->item;
  whens = whens->next;

  /* for each WHEN sequence */
  for (; whens; whens = whens->next)
    {
      cb_tree c2 = NULL;
      /* single WHEN test */
      for (subjs = subject_list, objs = whens->item;
	   subjs && objs;
	   subjs = subjs->next, objs = objs->next)
	{
	  cb_tree c3 = evaluate_test (subjs->item, objs->item);
	  if (c2 == NULL)
	    c2 = c3;
	  else
	    c2 = cb_build_binary_op (c2, '&', c3);
	}
      if (subjs || objs)
	cb_error (_("wrong number of WHEN parameters"));
      /* connect multiple WHEN's */
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = cb_build_binary_op (c1, '|', c2);
    }

  if (c1 == NULL)
    return stmt;
  else
    return cb_build_if (cb_build_cond (c1), stmt,
			evaluate_internal (subject_list, case_list->next));
}

cb_tree
cb_build_evaluate (struct cb_list *subject_list, struct cb_list *case_list)
{
  return evaluate_internal (subject_list, case_list);
}


/*
 * SEARCH ALL
 */

static void
search_set_keys (struct cb_field *f, cb_tree x)
{
  struct cb_binary_op *p;

  if (CB_REFERENCE_P (x))
    x = build_cond_88 (x);
  
  p = CB_BINARY_OP (x);
  switch (p->op)
    {
    case '&':
      search_set_keys (f, p->x);
      search_set_keys (f, p->y);
      break;
    case '=':
      {
	int i;
	for (i = 0; i < f->nkeys; i++)
	  if (cb_field (p->x) == cb_field (f->keys[i].key))
	    {
	      f->keys[i].ref = p->x;
	      f->keys[i].val = p->y;
	      break;
	    }
	if (i == f->nkeys)
	  cb_error_x (x, _("undeclared key `%s'"), cb_field (p->x)->name);
	break;
      }
    default:
      cb_error_x (x, _("invalid SEARCH ALL condition"));
      break;
    }
}

cb_tree
cb_build_search_all (cb_tree table, cb_tree cond)
{
  int i;
  struct cb_field *f = cb_field (table);
  cb_tree c1 = NULL;

  /* set keys */
  for (i = 0; i < f->nkeys; i++)
    f->keys[i].ref = 0;
  search_set_keys (f, cond);

  /* build condition */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].ref)
      {
	cb_tree c2;
	if (f->keys[i].dir == COB_ASCENDING)
	  c2 = cb_build_binary_op (f->keys[i].ref, '=', f->keys[i].val);
	else
	  c2 = cb_build_binary_op (f->keys[i].val, '=', f->keys[i].ref);
	if (c1 == NULL)
	  c1 = c2;
	else
	  c1 = cb_build_binary_op (c1, '&', c2);
      }

  return cb_build_cond (c1);
}
