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
#include "tree.h"

static struct cb_word *lookup_word (const char *name);

static char *
to_cname (const char *s)
{
  char *copy = strdup (s);
  char *p;
  for (p = copy; *p; p++)
    *p = (*p == '-') ? '_' : toupper (*p);
  return copy;
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
	    cb_tree l = p->subs = list_reverse (p->subs);
	    s += sprintf (s, "(");
	    for (; l; l = CB_CHAIN (l))
	      {
		s += cb_name_1 (s, CB_VALUE (l));
		s += sprintf (s, CB_CHAIN (l) ? ", " : ")");
	      }
	    p->subs = list_reverse (p->subs);
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
  static char buff[CB_MAX_CNAME];
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
cb_tree_type (cb_tree x)
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
	case CB_USAGE_BINARY_SWAP:
	case CB_USAGE_BINARY_NATIVE:
	case CB_USAGE_INDEX:
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

int
cb_fits_int (cb_tree x)
{
  switch (CB_TREE_TAG (x))
    {
    case CB_TAG_LITERAL:
      {
	struct cb_literal *l = CB_LITERAL (x);
	if (l->scale <= 0 && l->size < 10)
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
	  case CB_USAGE_BINARY_SWAP:
	  case CB_USAGE_BINARY_NATIVE:
	    if (f->pic->scale <= 0 && f->size <= sizeof (int))
	      return 1;
	    return 0;
	  case CB_USAGE_DISPLAY:
	    if (f->pic->scale <= 0 && f->size < 10)
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
cb_tree cb_int0;
cb_tree cb_int1;
cb_tree cb_int2;
cb_tree cb_i[8];
cb_tree cb_error_node;
cb_tree cb_return_code;

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
  cb_zero        = make_constant (CB_CATEGORY_NUMERIC, "&cob_zero");
  cb_space       = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_space");
  cb_low         = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_low");
  cb_high        = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_high");
  cb_quote       = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_quote");
  cb_int0        = cb_int (0);
  cb_int1        = cb_int (1);
  cb_int2        = cb_int (2);
  for (i = 1; i < 8; i++)
    {
      char *s = malloc (3);
      sprintf (s, "i%d", i);
      cb_i[i] = make_constant (CB_CATEGORY_NUMERIC, s);
    }
  cb_standard_error_handler = make_constant_label ("standard_error_handler");
}


/*
 * Integer
 */

#define INT_NODE_TABLE_SIZE	13

static struct int_node {
  int n;
  cb_tree node;
  struct int_node *next;
} *int_node_table[INT_NODE_TABLE_SIZE];

cb_tree
cb_int (int n)
{
  struct cb_integer *x;
  struct int_node *p;

  for (p = int_node_table[n % INT_NODE_TABLE_SIZE]; p; p = p->next)
    if (p->n == n)
      return p->node;

  x = make_tree (CB_TAG_INTEGER, CB_CATEGORY_NUMERIC, sizeof (struct cb_integer));
  x->val = n;

  p = malloc (sizeof (struct int_node));
  p->n = n;
  p->node = CB_TREE (x);
  p->next = int_node_table[n % INT_NODE_TABLE_SIZE];
  int_node_table[n % INT_NODE_TABLE_SIZE] = p;
  return p->node;
}


/*
 * String
 */

cb_tree
cb_build_string (const unsigned char *data, size_t size)
{
  struct cb_string *p =
    make_tree (CB_TAG_STRING, CB_CATEGORY_ALPHANUMERIC, sizeof (struct cb_string));
  p->size = size;
  p->data = data;
  return CB_TREE (p);
}


/*
 * Alphabet-name
 */

cb_tree
cb_build_alphabet_name (cb_tree name, enum cb_alphabet_name_type type)
{
  struct cb_alphabet_name *p =
    make_tree (CB_TAG_ALPHABET_NAME, CB_CATEGORY_UNKNOWN, sizeof (struct cb_alphabet_name));
  p->name = cb_define (name, CB_TREE (p));
  p->cname = to_cname (p->name);
  p->type = type;
  return CB_TREE (p);
}


/*
 * Class-name
 */

cb_tree
cb_build_class_name (cb_tree name, cb_tree list)
{
  char buff[CB_MAX_CNAME];
  struct cb_class_name *p =
    make_tree (CB_TAG_CLASS_NAME, CB_CATEGORY_BOOLEAN, sizeof (struct cb_class_name));
  p->name = cb_define (name, CB_TREE (p));
  sprintf (buff, "is_%s", to_cname (p->name));
  p->cname = strdup (buff);
  p->list = list;
  return CB_TREE (p);
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
 * Literal
 */

static struct cb_literal *
build_literal (enum cb_category category, const unsigned char *data, size_t size)
{
  struct cb_literal *p =
    make_tree (CB_TAG_LITERAL, category, sizeof (struct cb_literal));
  p->data = malloc (size + 1);
  p->size = size;
  memcpy (p->data, data, size);
  p->data[size] = 0;
  return p;
}

cb_tree
cb_build_numeric_literal (int sign, const unsigned char *data, int scale)
{
  struct cb_literal *p =
    build_literal (CB_CATEGORY_NUMERIC, data, strlen (data));
  p->sign = sign;
  p->scale = scale;
  return CB_TREE (p);
}

cb_tree
cb_build_alphanumeric_literal (const unsigned char *data, size_t size)
{
  return CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, data, size));
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

cb_tree
cb_build_picture (const char *str)
{
  struct cb_picture *pic =
    make_tree (CB_TAG_PICTURE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_picture));
  const char *p;
  char category = 0;
  int idx = 0;
  int size = 0;
  int digits = 0;
  int scale = 0;
  int s_count = 0;
  int v_count = 0;
  int buff_size = 9;
  unsigned char *buff = malloc (buff_size);

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
	    scale += n;
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
	      scale += n;
	    else
	      scale -= n;
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
	    scale += n;
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
  pic->scale = scale;
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
  goto end;

 error:
  cb_error (_("invalid picture string"));

 end:
  if (!pic->str)
    free (buff);
  return CB_TREE (pic);
}


/*
 * Field
 */

static cb_tree
make_field (cb_tree name)
{
  struct cb_field *p =
    make_tree (CB_TAG_FIELD, CB_CATEGORY_UNKNOWN, sizeof (struct cb_field));
  p->name = cb_define (name, CB_TREE (p));
  return CB_TREE (p);
}

cb_tree
cb_build_implicit_field (cb_tree name, int len)
{
  char pic[256];
  cb_tree x = make_field (name);
  sprintf (pic, "X(%d)", len);
  CB_FIELD (x)->pic = CB_PICTURE (cb_build_picture (pic));
  CB_FIELD (x)->usage = CB_USAGE_DISPLAY;
  cb_validate_field (CB_FIELD (x));
  return x;
}

cb_tree
cb_build_index (cb_tree name)
{
  cb_tree x = make_field (name);
  CB_FIELD (x)->usage = CB_USAGE_INDEX;
  cb_validate_field (CB_FIELD (x));
  return x;
}

cb_tree
cb_build_constant (cb_tree name, cb_tree value)
{
  cb_tree x = make_field (name);
  x->category = cb_tree_category (value);
  CB_FIELD (x)->storage = CB_STORAGE_CONSTANT;
  CB_FIELD (x)->values = list (value);
  return x;
}

static int
get_level (cb_tree x)
{
  const char *p;
  int level = 0;

  if (x == cb_error_node)
    return -1;

  /* get level */
  for (p = CB_NAME (x); *p; p++)
    {
      if (!isdigit (*p))
	goto level_error;
      level = level * 10 + (*p - '0');
    }

  /* check level */
  if (!((01 <= level && level <= 49)
	|| (level == 66 || level == 77 || level == 88)))
    {
    level_error:
      cb_error_x (x, _("invalid level number `%s'"), CB_NAME (x));
      return -1;
    }

  return level;
}

cb_tree
cb_build_field (cb_tree level, cb_tree name, struct cb_field *last_field,
		enum cb_storage storage)
{
  struct cb_reference *r;
  struct cb_field *f;

  if (get_level (level) == -1 || name == cb_error_node)
    return cb_error_node;

  /* build the field */
  r = CB_REFERENCE (name);
  f = CB_FIELD (make_field (name));
  f->level = get_level (level);
  f->usage = CB_USAGE_DISPLAY;
  f->occurs_max = 1;
  f->storage = storage;

  /* checks for redefinition */
  if (cb_warn_redefinition)
    {
      if (f->level == 01 || f->level == 77)
	{
	  if (r->word->count >= 2)
	    redefinition_warning (name);
	}
      else
	{
	  cb_tree l;
	  for (l = r->word->items; l; l = CB_CHAIN (l))
	    {
	      cb_tree x = CB_VALUE (l);
	      if (!CB_FIELD_P (x)
		  || CB_FIELD (x)->level == 01
		  || CB_FIELD (x)->level == 77)
		{
		  redefinition_warning (name);
		  break;
		}
	    }
	}
    }

  if (last_field && last_field->level == 88)
    last_field = last_field->parent;

  if (f->level == 01 || f->level == 77)
    {
      if (last_field)
	cb_field_founder (last_field)->sister = f;
    }
  else if (!last_field)
    {
      cb_error_x (name, _("level number must begin with 01 or 77"));
      return cb_error_node;
    }
  else if (f->level == 66)
    {
      struct cb_field *p;
      f->parent = cb_field_founder (last_field);
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
    sister:
      if (cb_warn_redefinition)
	{
	  /* ensure that there is no field with the same name
	     in the same level */
	  struct cb_field *p;
	  for (p = last_field->parent->children; p; p = p->sister)
	    if (strcasecmp (f->name, p->name) == 0)
	      {
		redefinition_warning (name);
		break;
	      }
	}
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
      return cb_error_node;
    }

  /* inherit parent's properties */
  if (f->parent)
    {
      f->usage = f->parent->usage;
      f->indexes = f->parent->indexes;
      f->flag_sign_leading = f->parent->flag_sign_leading;
      f->flag_sign_separate = f->parent->flag_sign_separate;
    }

  return CB_TREE (f);
}

struct cb_field *
cb_resolve_redefines (struct cb_field *field, cb_tree redefines)
{
  struct cb_field *f;
  struct cb_reference *r = CB_REFERENCE (redefines);
  cb_tree x = CB_TREE (field);

  /* check qualification */
  if (r->chain)
    {
      cb_error_x (x, _("`%s' cannot be qualified here"), CB_NAME (redefines));
      return NULL;
    }

  /* check subscripts */
  if (r->subs)
    {
      cb_error_x (x, _("`%s' cannot be subscripted here"), CB_NAME (redefines));
      return NULL;
    }

  /* resolve the name in the current group (if any) */
  if (field->parent)
    r->chain = cb_build_field_reference (field->parent, redefines);
  if (cb_ref (redefines) == cb_error_node)
    return NULL;
  f = CB_FIELD (r->value);

  /* check level number */
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

  return f;
}

static int
validate_field_1 (struct cb_field *f)
{
  cb_tree x = CB_TREE (f);
  char *name = cb_name (x);
  struct cb_field *p;

  if (f->level == 66)
    {
      if (!f->redefines)
	{
	  level_require_error (x, "RENAMES");
	  return -1;
	}

      if (f->flag_occurs)
	level_except_error (x, "RENAMES");
      return 0;
    }

  /* validate OCCURS */
  if (f->flag_occurs)
    if (f->level < 2 || f->level > 49)
      level_redundant_error (x, "OCCURS");

  /* validate OCCURS DEPENDING */
  if (f->occurs_depending)
    {
      /* the data item that contains a OCCURS DEPENDING clause shall not
	 be subordinate to a data item that has the OCCURS clause */
      for (p = f->parent; p; p = p->parent)
	if (p->flag_occurs)
	  {
	    cb_error_x (CB_TREE (p),
			_("`%s' cannot have the OCCURS clause due to `%s'"),
			p->name, name);
	    break;
	  }

      /* the data item that contains a OCCURS DEPENDING clause must be
	 the last data item in the group */
      for (p = f; p->parent; p = p->parent)
	for (; p->sister; p = p->sister)
	  if (!p->sister->redefines)
	    {
	      cb_error_x (x, _("`%s' cannot have OCCURS DEPENDING"), name);
	      break;
	    }
    }

  /* validate REDEFINES */
  if (f->redefines)
    {
      /* check OCCURS */
      if (f->redefines->flag_occurs)
	cb_warning_x (x, _("the original definition `%s' should not have OCCURS"),
		      f->redefines->name);

      /* check definition */
      for (p = f->redefines->sister; p && p != f; p = p->sister)
	if (!p->redefines)
	  {
	    cb_error_x (x, _("REDEFINES must follow the original definition"));
	    break;
	  }

      /* check variable occurrence */
      if (f->occurs_depending || cb_field_varying (f))
	cb_error_x (x, _("`%s' cannot be variable length"), f->name);
      if (cb_field_varying (f->redefines))
	cb_error_x (x, _("the original definition `%s' cannot be variable length"),
		    f->redefines->name);
    }

  if (f->children)
    {
      /* group item */

      if (f->pic)
	group_error (x, "PICTURE");
      if (f->flag_justified)
	group_error (x, "JUSTIFIED RIGHT");
      if (f->flag_blank_zero)
	group_error (x, "BLANK WHEN ZERO");

      for (f = f->children; f; f = f->sister)
	if (validate_field_1 (f) != 0)
	  return -1;
    }
  else
    {
      /* elementary item */

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
      if (f->usage == CB_USAGE_BINARY_SWAP
	  || f->usage == CB_USAGE_BINARY_NATIVE
	  || f->usage == CB_USAGE_PACKED)
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
	    if (f->pic->scale > 0)
	      {
		f->pic->str = malloc (7);
		sprintf (f->pic->str, "9%cV%c9%c",
			 f->pic->digits - f->pic->scale, 1,
			 f->pic->scale);
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
	  if (CB_PAIR_P (CB_VALUE (f->values)) || CB_CHAIN (f->values))
	    cb_error_x (x, _("only level 88 item may have multiple values"));

	  /* ISO+IEC+1989-2002: 13.16.42.2-10 */
	  for (p = f; p; p = p->parent)
	    if (p->redefines)
	      cb_error_x (x, _("entries under REDEFINES cannot have VALUE clause"));
	}
    }

  return 0;
}

static void
setup_parameters (struct cb_field *f)
{
  static int id = 0;

  /* setup cname */
  char name[CB_MAX_CNAME];
  sprintf (name, "%s_%d", f->name, id++);
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
	f->pic = CB_PICTURE (cb_build_picture ("S9(9)"));
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
	case CB_USAGE_BINARY_SWAP:
	case CB_USAGE_BINARY_NATIVE:
	  {
	    int size = f->pic->size;
	    switch (cb_binary_size)
	      {
	      case CB_BINARY_SIZE_2_4_8:
		f->size = ((size <= 4) ? 2 :
			   (size <= 9) ? 4 : 8);
		break;
	      case CB_BINARY_SIZE_1_2_4_8:
		f->size = ((size <= 2) ? 1 :
			   (size <= 4) ? 2 :
			   (size <= 9) ? 4 : 8);
		break;
	      case CB_BINARY_SIZE_1__8:
		if (f->pic->have_sign)
		  f->size = ((size <= 2)  ? 1 : (size <= 4)  ? 2 :
			     (size <= 6)  ? 3 : (size <= 9)  ? 4 :
			     (size <= 11) ? 5 : (size <= 14) ? 6 :
			     (size <= 16) ? 7 : 8);
		else
		  f->size = ((size <= 2)  ? 1 : (size <= 4)  ? 2 :
			     (size <= 7)  ? 3 : (size <= 9)  ? 4 :
			     (size <= 12) ? 5 : (size <= 14) ? 6 :
			     (size <= 16) ? 7 : 8);
		break;
	      }

	    /* modify digits */
	    if (!cb_binary_truncate)
	      {
		static int digits[] = {1, 3, 5, 7, 10, 12, 15, 17, 19};
		f->pic->digits = digits[f->size];
	      }
	    break;
	  }
	case CB_USAGE_DISPLAY:
	  {
	    f->size = f->pic->size;
	    if (f->pic->category == CB_CATEGORY_NUMERIC
		&& f->pic->have_sign
		&& f->flag_sign_separate)
	      f->size++;
	    break;
	  }
	case CB_USAGE_PACKED:
	  {
	    f->size = f->pic->size / 2 + 1;
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
  if (f->redefines
      && (f->size * f->occurs_max
	  > f->redefines->size * f->redefines->occurs_max)
      && (f->redefines->level != 01 || f->redefines->flag_external))
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

static int
validate_field_value (struct cb_field *f)
{
  if (f->values)
    validate_move (CB_VALUE (f->values), CB_TREE (f), 1);

  if (f->children)
    for (f = f->children; f; f = f->sister)
      validate_field_value (f);

  return 0;
}

void
cb_validate_field (struct cb_field *f)
{
  if (validate_field_1 (f) != 0)
    return;
  finalize_field (f);
  validate_field_value (f);
}

void
cb_validate_88_item (struct cb_field *f)
{
  cb_tree x = CB_TREE (f);

  if (!f->values)
    level_require_error (x, "VALUE");

  if (f->pic || f->flag_occurs)
    level_except_error (x, "VALUE");
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
cb_field_size (cb_tree x)
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
cb_field_founder (struct cb_field *f)
{
  while (f->parent)
    f = f->parent;
  return f;
}

struct cb_field *
cb_field_varying (struct cb_field *f)
{
  struct cb_field *p;
  for (f = f->children; f; f = f->sister)
    if (f->occurs_depending)
      return f;
    else if ((p = cb_field_varying (f)) != NULL)
      return p;
  return NULL;
}

/* Return 1 if P is subordinate to F */

int
cb_field_subordinate (struct cb_field *p, struct cb_field *f)
{
  for (p = p->parent; p; p = p->parent)
    if (p == f)
      return 1;
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
  p->name = cb_define (name, CB_TREE (p));
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
  char buff[CB_MAX_CNAME];
  struct cb_field *p;

  /* check the record size if it is limited */
  for (p = records; p; p = p->sister)
    {
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
      struct cb_field *v = cb_field_varying (p);
      if (v && v->offset + v->size * v->occurs_min < f->record_min)
	f->record_min = v->offset + v->size * v->occurs_min;
      if (p->size < f->record_min)
	f->record_min = p->size;
      if (p->size > f->record_max)
	f->record_max = p->size;
    }

  /* create record */
  sprintf (buff, "%s$record", f->name);
  f->record = CB_FIELD (make_field (make_reference (buff)));
  f->record->usage = CB_USAGE_DISPLAY;
  sprintf (buff, "X(%d)", f->record_max);
  f->record->pic = CB_PICTURE (cb_build_picture (buff));
  f->record->sister = records;
  cb_validate_field (f->record);

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
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_reference (name);
}

cb_tree
make_reference (const char *name)
{
  struct cb_reference *p =
    make_tree (CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_reference));
  p->word = lookup_word (name);
  return CB_TREE (p);
}

cb_tree
cb_build_field_reference (struct cb_field *f, cb_tree ref)
{
  cb_tree x = make_reference (f->name);
  struct cb_word *word = CB_REFERENCE (x)->word;
  if (ref)
    memcpy (x, ref, sizeof (struct cb_reference));
  x->category = CB_CATEGORY_UNKNOWN;
  CB_REFERENCE (x)->word = word;
  CB_REFERENCE (x)->value = CB_TREE (f);
  return x;
}

const char *
cb_define (cb_tree name, cb_tree val)
{
  struct cb_word *w = CB_REFERENCE (name)->word;
  w->items = list_add (w->items, val);
  w->count++;
  val->source_file = name->source_file;
  val->source_line = name->source_line;
  CB_REFERENCE (name)->value = val;
  return w->name;
}

void
cb_define_system_name (const char *name)
{
  cb_tree x = make_reference (name);
  if (CB_REFERENCE (x)->word->count == 0)
    cb_define (x, lookup_system_name (name));
}

static cb_tree
resolve_label (const char *name, struct cb_label *section)
{
  cb_tree l;
  for (l = section->children; l; l = CB_CHAIN (l))
    if (strcasecmp (name, CB_LABEL (CB_VALUE (l))->name) == 0)
      return CB_VALUE (l);
  return cb_error_node;
}

cb_tree
cb_ref (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  cb_tree pv;

  if (r->value)
    return r->value;

  if (r->chain == NULL)
    {
      switch (r->word->count)
	{
	case 0:
	  undefined_error (x);
	  goto error;
	case 1:
	  r->value = CB_VALUE (r->word->items);
	  return r->value;
	default:
	  if (r->offset && CB_LABEL_P (r->offset))
	    {
	      cb_tree v = resolve_label (r->word->name, CB_LABEL (r->offset));
	      if (v != cb_error_node)
		{
		  r->value = v;
		  return r->value;
		}
	    }
	  ambiguous_error (x);
	  goto error;
	}
    }

  pv = cb_ref (r->chain);
  if (pv == cb_error_node)
    goto error;

  switch (CB_TREE_TAG (pv))
    {
    case CB_TAG_FILE:
      pv = CB_TREE (CB_FILE (pv)->record->sister);
      /* fall through */
    case CB_TAG_FIELD:
      {
	cb_tree l;
	struct cb_field *p, *pp;
	cb_tree v = NULL;

	/* find the definition in the parent */
	pp = CB_FIELD (pv);
	for (l = r->word->items; l; l = CB_CHAIN (l))
	  if (CB_FIELD_P (CB_VALUE (l)))
	    for (p = CB_FIELD (CB_VALUE (l))->parent; p; p = p->parent)
	      if (p == pp)
		{
		  if (v)
		    {
		      ambiguous_error (x);
		      goto error;
		    }
		  v = CB_VALUE (l);
		}
	if (v == NULL)
	  {
	    if (pp->children == NULL)
	      cb_error_x (r->chain, _("`%s' not a group"), pp->name);
	    else
	      undefined_error (x);
	    goto error;
	  }
	r->value = v;
	return r->value;
      }
    case CB_TAG_LABEL:
      {
	cb_tree v = resolve_label (r->word->name, CB_LABEL (pv));
	if (v == cb_error_node)
	  {
	    undefined_error (x);
	    goto error;
	  }
	r->value = v;
	return r->value;
      }
    default:
      abort ();
    }

 error:
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
      abort ();
    }

  p = make_tree (CB_TAG_BINARY_OP, category, sizeof (struct cb_binary_op));
  p->op = op;
  p->x = left;
  p->y = right;
  return CB_TREE (p);
}

cb_tree
cb_build_connective_op (cb_tree list, char op)
{
  cb_tree e = CB_VALUE (list);
  for (list = CB_CHAIN (list); list; list = CB_CHAIN (list))
    e = cb_build_binary_op (e, op, CB_VALUE (list));
  return e;
}


/*
 * Function call
 */

cb_tree
cb_build_funcall (const char *name, int argc,
		  cb_tree a1, cb_tree a2, cb_tree a3, cb_tree a4)
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
 * Type cast
 */

cb_tree
cb_build_cast (enum cb_cast_type type, cb_tree val)
{
  enum cb_category category =
    (type == CB_CAST_INTEGER) ? CB_CATEGORY_NUMERIC : CB_CATEGORY_UNKNOWN;
  struct cb_cast *p =
    make_tree (CB_TAG_CAST, category, sizeof (struct cb_cast));
  p->type = type;
  p->val = val;
  return CB_TREE (p);
}


/*
 * Label
 */

cb_tree
cb_build_label (cb_tree name, struct cb_label *section)
{
  static int id = 0;
  char buff[CB_MAX_CNAME];
  struct cb_label *p =
    make_tree (CB_TAG_LABEL, CB_CATEGORY_UNKNOWN, sizeof (struct cb_label));
  p->id = id++;
  p->name = cb_define (name, CB_TREE (p));
  p->section = section;
  if (section)
    sprintf (buff, "%s$%s", section->cname, p->name);
  else
    sprintf (buff, "%s", p->name);
  p->cname = to_cname (buff);
  return CB_TREE (p);
}


/*
 * Assign
 */

cb_tree
cb_build_assign (cb_tree var, cb_tree val)
{
  struct cb_assign *p =
    make_tree (CB_TAG_ASSIGN, CB_CATEGORY_UNKNOWN, sizeof (struct cb_assign));
  p->var = var;
  p->val = val;
  return CB_TREE (p);
}


/*
 * INITIALIZE
 */

cb_tree
cb_build_initialize (cb_tree var, cb_tree val, cb_tree rep, cb_tree def)
{
  struct cb_initialize *p =
    make_tree (CB_TAG_INITIALIZE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_initialize));
  p->var = var;
  p->val = val;
  p->rep = rep;
  p->def = def;
  return CB_TREE (p);
}


/*
 * SEARCH
 */

cb_tree
cb_build_search (int flag_all, cb_tree table, cb_tree var,
		 cb_tree end_stmt, cb_tree whens)
{
  struct cb_search *p =
    make_tree (CB_TAG_SEARCH, CB_CATEGORY_UNKNOWN, sizeof (struct cb_search));
  p->flag_all = flag_all;
  p->table    = table;
  p->var      = var;
  p->end_stmt = end_stmt;
  p->whens    = whens;
  return CB_TREE (p);
}


/*
 * CALL
 */

cb_tree
cb_build_call (cb_tree name, cb_tree args, cb_tree stmt1, cb_tree stmt2)
{
  struct cb_call *p =
    make_tree (CB_TAG_CALL, CB_CATEGORY_UNKNOWN, sizeof (struct cb_call));
  p->name  = name;
  p->args  = args;
  p->stmt1 = stmt1;
  p->stmt2 = stmt2;
  return CB_TREE (p);
}


/*
 * GO TO
 */

cb_tree
cb_build_goto (cb_tree target, cb_tree depending)
{
  struct cb_goto *p =
    make_tree (CB_TAG_GOTO, CB_CATEGORY_UNKNOWN, sizeof (struct cb_goto));
  p->target = target;
  p->depending = depending;
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
 * List
 */

cb_tree
cb_build_list (cb_tree purpose, cb_tree value, cb_tree rest)
{
  struct cb_list *p =
    make_tree (CB_TAG_LIST, CB_CATEGORY_UNKNOWN, sizeof (struct cb_list));
  p->purpose = purpose;
  p->value = value;
  p->chain = rest;
  return CB_TREE (p);
}

cb_tree
list_add (cb_tree l, cb_tree x)
{
  return list_append (l, list (x));
}

cb_tree
list_append (cb_tree l1, cb_tree l2)
{
  if (l1 == NULL)
    {
      return l2;
    }
  else
    {
      cb_tree l = l1;
      while (CB_CHAIN (l))
	l = CB_CHAIN (l);
      CB_CHAIN (l) = l2;
      return l1;
    }
}

cb_tree
list_reverse (cb_tree l)
{
  cb_tree next, last = NULL;
  for (; l; l = next)
    {
      next = CB_CHAIN (l);
      CB_CHAIN (l) = last;
      last = l;
    }
  return last;
}

int
list_length (cb_tree l)
{
  int n = 0;
  for (; l; l = CB_CHAIN (l))
    n++;
  return n;
}


/*
 * Program
 */

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return val % CB_WORD_HASH_SIZE;
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

struct cb_program *
cb_build_program (void)
{
  struct cb_program *p = malloc (sizeof (struct cb_program));
  memset (p, 0, sizeof (struct cb_program));
  p->decimal_point = '.';
  p->currency_symbol = '$';
  p->numeric_separator = ',';
  return p;
}
