/*
 * Copyright (C) 2001-2004 Keisuke Nishida
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
cb_tree cb_int3;
cb_tree cb_i[8];
cb_tree cb_error_node;
cb_tree cb_return_code;
cb_tree cb_call_params;

cb_tree cb_intr_whencomp;
cb_tree cb_intr_pi;
cb_tree cb_intr_e;

cb_tree cb_standard_error_handler;

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
  cb_tree x = cob_malloc (size);
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
	strcpy (s, (char *)CB_LITERAL (x)->data);
      else
	sprintf (s, "\"%s\"", CB_LITERAL (x)->data);
      break;

    case CB_TAG_FIELD:
      strcpy (s, CB_FIELD (x)->name);
      break;

    case CB_TAG_REFERENCE:
      {
	struct cb_reference *p = CB_REFERENCE (x);
	s += sprintf (s, "%s", p->word->name);
	if (p->subs)
	  {
	    cb_tree l = p->subs = cb_list_reverse (p->subs);
	    s += sprintf (s, "(");
	    for (; l; l = CB_CHAIN (l))
	      {
		s += cb_name_1 (s, CB_VALUE (l));
		s += sprintf (s, CB_CHAIN (l) ? ", " : ")");
	      }
	    p->subs = cb_list_reverse (p->subs);
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
	if (p->chain)
	  {
	    s += sprintf (s, " in ");
	    s += cb_name_1 (s, p->chain);
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
    case CB_TAG_CAST:
      {
	struct cb_cast *p = CB_CAST (x);
	switch (p->type)
	  {
	  case CB_CAST_ADDRESS:
	    x->category = CB_CATEGORY_DATA_POINTER;
	    break;
	  default:
	    ABORT ();
	  }
	break;
      }
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
	else if (f->usage == CB_USAGE_POINTER && f->level != 88)
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
	fprintf(stderr, "Unknown tree tag %d Category %d\n", CB_TREE_TAG(x), x->category);
	fflush(stderr);
      ABORT ();
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
    case CB_CATEGORY_ALPHANUMERIC:
      return COB_TYPE_ALPHANUMERIC;
    case CB_CATEGORY_ALPHANUMERIC_EDITED:
      return COB_TYPE_ALPHANUMERIC_EDITED;
    case CB_CATEGORY_NUMERIC:
      switch (f->usage)
	{
	case CB_USAGE_DISPLAY:
	  return COB_TYPE_NUMERIC_DISPLAY;
	case CB_USAGE_BINARY:
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
	case CB_USAGE_INDEX:
	case CB_USAGE_LENGTH:
	  return COB_TYPE_NUMERIC_BINARY;
	case CB_USAGE_FLOAT:
	  return COB_TYPE_NUMERIC_FLOAT;
	case CB_USAGE_DOUBLE:
	  return COB_TYPE_NUMERIC_DOUBLE;
	case CB_USAGE_PACKED:
	  return COB_TYPE_NUMERIC_PACKED;
	default:
	  ABORT ();
	}
    case CB_CATEGORY_NUMERIC_EDITED:
      return COB_TYPE_NUMERIC_EDITED;
    case CB_CATEGORY_OBJECT_REFERENCE:
    case CB_CATEGORY_DATA_POINTER:
    case CB_CATEGORY_PROGRAM_POINTER:
      return COB_TYPE_NUMERIC_BINARY;
    default:
      ABORT ();
    }
/* NOT REACHED */
    return 0;
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
	  case CB_USAGE_LENGTH:
	    return 1;
	  case CB_USAGE_BINARY:
	  case CB_USAGE_COMP_5:
	  case CB_USAGE_COMP_X:
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

int
cb_get_int (cb_tree x)
{
  int i;
  int val = 0;
  struct cb_literal *l = CB_LITERAL (x);

  for (i = 0; i < l->size; i++)
    if (l->data[i] != '0')
      break;

  if (l->size - i >= 10)
    ABORT ();

  for (; i < l->size; i++)
    val = val * 10 + l->data[i] - '0';
  if (l->sign < 0)
    val = -val;
  return val;
}

static cb_tree
make_constant (enum cb_category category, const char *val)
{
  struct cb_const *p =
    make_tree (CB_TAG_CONST, category, sizeof (struct cb_const));
  p->val = val;
  return CB_TREE (p);
}

static cb_tree
make_constant_label (const char *name)
{
  struct cb_label *p = CB_LABEL (cb_build_label (cb_build_reference (name), NULL));
  p->need_begin = 1;
  return CB_TREE (p);
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
  cb_int3        = cb_int (3);
  for (i = 1; i < 8; i++)
    {
      char *s = cob_malloc (3);
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

  p = cob_malloc (sizeof (struct int_node));
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
  p->data = cob_malloc ((size_t)(size + 1));
  p->size = size;
  memcpy (p->data, data, (size_t)size);
  p->data[size] = 0;
  return p;
}

cb_tree
cb_build_numeric_literal (int sign, const unsigned char *data, int scale)
{
  struct cb_literal *p =
    build_literal (CB_CATEGORY_NUMERIC, data, strlen ((char *)data));
  p->sign = (char)sign;
  p->scale = (char)scale;
  return CB_TREE (p);
}

cb_tree
cb_build_alphanumeric_literal (const unsigned char *data, size_t size)
{
  return CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, data, size));
}

cb_tree
cb_concat_literals (cb_tree x1, cb_tree x2)
{
  struct cb_literal *l1 = CB_LITERAL (x1);
  struct cb_literal *l2 = CB_LITERAL (x2);
  unsigned char buff[l1->size + l2->size];
  memcpy (buff, l1->data, l1->size);
  memcpy (buff + l1->size, l2->data, l2->size);
  return cb_build_alphanumeric_literal (buff, l1->size + l2->size);
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
  int buff_size = 12;
  unsigned char *buff = cob_malloc (buff_size);

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
      if (idx + n / 64 + 1 > buff_size) {
	while (idx + n / 64 + 1 > buff_size) {
		buff_size *= 2;
	}
	buff = realloc (buff, buff_size);
	if ( !buff ) {
		fprintf (stderr, "Memory realloc failed - Aborting\n");
		fflush (stderr);
		(void)longjmp (cob_jmpbuf, 1);
	}
      }

      /* store in the buffer */
      while (n > 0)
	{
	  buff[idx++] = c;
	  buff[idx++] = (unsigned char)((n < 256) ? n : 255);
	  n -= 255;
	}
    }
  buff[idx] = 0;

  /* set picture */
  pic->orig = strdup (str);
  pic->size = size;
  pic->digits = (char)digits;
  pic->scale = (char)scale;
  pic->have_sign = (char)s_count;

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
      pic->str = (char *)buff;
      pic->category = CB_CATEGORY_NUMERIC_EDITED;
      break;
    case PIC_EDITED:
    case PIC_ALPHABETIC_EDITED:
    case PIC_ALPHANUMERIC_EDITED:
    case PIC_NATIONAL_EDITED:
      pic->str = (char *)buff;
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

cb_tree
cb_build_field (cb_tree name)
{
  struct cb_field *p =
    make_tree (CB_TAG_FIELD, CB_CATEGORY_UNKNOWN, sizeof (struct cb_field));
  p->id = cb_id++;
  p->name = cb_define (name, CB_TREE (p));
  p->ename = NULL;
  p->usage = CB_USAGE_DISPLAY;
  p->storage = CB_STORAGE_WORKING;
  p->occurs_max = 1;
  return CB_TREE (p);
}

cb_tree
cb_build_implicit_field (cb_tree name, int len)
{
  char pic[256];
  cb_tree x = cb_build_field (name);
  sprintf (pic, "X(%d)", len);
  CB_FIELD (x)->pic = CB_PICTURE (cb_build_picture (pic));
  cb_validate_field (CB_FIELD (x));
  return x;
}

cb_tree
cb_build_constant (cb_tree name, cb_tree value)
{
  cb_tree x = cb_build_field (name);
  x->category = cb_tree_category (value);
  CB_FIELD (x)->storage = CB_STORAGE_CONSTANT;
  CB_FIELD (x)->values = cb_list (value);
  return x;
}

struct cb_field *
cb_field (cb_tree x)
{
  if (CB_REFERENCE_P (x))
    return CB_FIELD (cb_ref (x));
  else
    return CB_FIELD (x);
}

struct cb_field *
cb_field_add (struct cb_field *f, struct cb_field *p)
{
  struct cb_field *t;

  if (f == NULL)
    return p;

  for (t = f; t->sister; t = t->sister);
  t->sister = p;
  return f;
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
	      return cb_get_int (r->length);
	    else
	      return -1;
	  }
	else if (r->offset)
	  {
	    if (CB_LITERAL_P (r->offset))
	      return f->size - cb_get_int (r->offset) + 1;
	    else
	      return -1;
	  }
	else
	  {
	    return f->size;
	  }
      }
    default:
      ABORT ();
    }
/* NOT REACHED */
    return 0;
}

struct cb_field *
cb_field_founder (struct cb_field *f)
{
  while (f->parent)
    f = f->parent;
  return f;
}

struct cb_field *
cb_field_variable_size (struct cb_field *f)
{
  struct cb_field *p;
  for (f = f->children; f; f = f->sister)
    if (f->occurs_depending)
      return f;
    else if ((p = cb_field_variable_size (f)) != NULL)
      return p;
  return NULL;
}

struct cb_field *
cb_field_variable_address (struct cb_field *f)
{
  struct cb_field *p;
  for (p = f->parent; p; f = f->parent, p = f->parent)
    for (p = p->children; p != f; p = p->sister)
      if (p->occurs_depending || cb_field_variable_size (p))
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
  p->handler = CB_LABEL (cb_standard_error_handler);
  return p;
}

static void
file_error (cb_tree name, const char *clause)
{
  cb_error_x (name, _("%s clause is required for file '%s'"),
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
	  cb_error (_("record size too small '%s'"), p->name);
      if (f->record_max > 0)
	if (p->size > f->record_max)
	  cb_error (_("record size too large '%s'"), p->name);
    }

  /* compute the record size */
  if (f->record_min == 0)
    f->record_min = records->size;
  for (p = records; p; p = p->sister)
    {
      struct cb_field *v = cb_field_variable_size (p);
      if (v && v->offset + v->size * v->occurs_min < f->record_min)
	f->record_min = v->offset + v->size * v->occurs_min;
      if (p->size < f->record_min)
	f->record_min = p->size;
      if (p->size > f->record_max)
	f->record_max = p->size;
    }

  /* create record */
  sprintf (buff, "%s$record", f->name);
  f->record = CB_FIELD (cb_build_implicit_field (cb_build_reference (buff),
						 f->record_max));
  if ( f->linage ) {
	cb_tree x;

	sprintf (buff, "LC$%s", f->name);
	x = cb_build_field (cb_build_reference (buff));
	CB_FIELD (x)->pic = CB_PICTURE (cb_build_picture ("9(9)"));
	CB_FIELD (x)->usage = CB_USAGE_COMP_5;
	CB_FIELD (x)->values = cb_list (cb_zero);
	CB_FIELD (x)->count++;
	cb_validate_field (CB_FIELD (x));
	f->linage_ctr = x;
	current_program->working_storage =
		cb_field_add (current_program->working_storage, CB_FIELD (x));
  }
  f->record->sister = records;
  f->record->count++;

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
cb_build_filler (void)
{
  static int id = 1;
  char name[32];

  sprintf (name, "WORK$%d", id++);
  return cb_build_reference (name);
}

cb_tree
cb_build_reference (const char *name)
{
  struct cb_reference *p =
    make_tree (CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_reference));
  p->word = lookup_word (name);
  return CB_TREE (p);
}

cb_tree
cb_build_field_reference (struct cb_field *f, cb_tree ref)
{
  cb_tree x = cb_build_reference (f->name);
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
  w->items = cb_list_add (w->items, val);
  w->count++;
  val->source_file = name->source_file;
  val->source_line = name->source_line;
  CB_REFERENCE (name)->value = val;
  return w->name;
}

void
cb_define_system_name (const char *name)
{
  cb_tree x = cb_build_reference (name);
  if (CB_REFERENCE (x)->word->count == 0)
    cb_define (x, lookup_system_name (name));
}

cb_tree
cb_ref (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  int ambiguous = 0;
  cb_tree candidate = NULL;
  cb_tree items;

  /* if this reference has already been resolved (and the value
     has been cached), then just return the value */
  if (r->value)
    return r->value;

  /* resolve the value */
  for (items = r->word->items; items; items = CB_CHAIN (items))
    {
      /* find a candidate value by resolving qualification */
      cb_tree v = CB_VALUE (items);
      cb_tree c = r->chain;
      switch (CB_TREE_TAG (v))
	{
	case CB_TAG_FIELD:
	  {
	    /* in case the value is a field, it might be qualified
	       by its parent names and a file name */
	    struct cb_field *p = CB_FIELD (v)->parent;

	    /* resolve by parents */
	    for (; p; p = p->parent)
	      if (c && strcasecmp (CB_NAME (c), p->name) == 0)
		c = CB_REFERENCE (c)->chain;

	    /* resolve by file */
	    if (c && CB_REFERENCE (c)->chain == 0)
	      if (CB_REFERENCE (c)->word->count == 1
		  && CB_FILE_P (cb_ref (c))
		  && (CB_FILE (cb_ref (c)) ==
		      cb_field_founder (CB_FIELD (v))->file))
		c = CB_REFERENCE (c)->chain;

	    break;
	  }
	case CB_TAG_LABEL:
	  {
	    /* in case the value is a label, it might be qualified
	       by its section name */
	    struct cb_label *s = CB_LABEL (v)->section;

	    /* unqualified paragraph name referenced within the section
	       is resolved without ambiguity check */
	    if (c == NULL && r->offset && s == CB_LABEL (r->offset))
	      {
		candidate = v;
		goto end;
	      }

	    /* resolve by section name */
	    if (c && strcasecmp (CB_NAME (c), s->name) == 0)
	      c = CB_REFERENCE (c)->chain;

	    break;
	  }
	default:
	  /* other values cannot be qualified */
	  break;
	}

      /* a well qualified value is a good candidate */
      if (c == NULL)
	{
	  if (candidate == NULL)
	    {
	      /* keep the first candidate */
	      candidate = v;
	    }
	  else
	    {
	      /* there are several candidates and possibly ambiguous */
	      ambiguous = 1;
	      /* continue search because the reference might not ambiguous
		and exit loop by "goto end" later on */
	    }
	}
    }

  /* there is no candidate */
  if (candidate == NULL)
    {
      undefined_error (x);
      goto error;
    }

  /* the reference is ambiguous */
  if (ambiguous)
    {
      ambiguous_error (x);
      goto error;
    }

 end:
  if (CB_FIELD_P (candidate))
    {
      CB_FIELD (candidate)->count++;
      if (CB_FIELD (candidate)->flag_invalid)
	goto error;
    }

  r->value = candidate;
  return r->value;

 error:
  r->value = cb_error_node;
  return cb_error_node;
}


/*
 * Expression
 */

cb_tree
cb_build_binary_op (cb_tree x, char op, cb_tree y)
{
  struct cb_binary_op *p;
  enum cb_category category;

  switch (op)
    {
    case '+': case '-': case '*': case '/': case '^':
      /* arithmetic operators */
      if (CB_TREE_CLASS (x) == CB_CLASS_POINTER
	  || CB_TREE_CLASS (y) == CB_CLASS_POINTER)
	{
	  category = CB_CATEGORY_DATA_POINTER;
	  break;
	}
      x = cb_check_numeric_value (x);
      y = cb_check_numeric_value (y);
      if (x == cb_error_node || y == cb_error_node)
	return cb_error_node;
      category = CB_CATEGORY_NUMERIC;
      break;

    case '=': case '~': case '<': case '>': case '[': case ']':
      /* relational operators */
      category = CB_CATEGORY_BOOLEAN;
      break;

    case '!': case '&': case '|':
      /* logical operators */
      if (CB_TREE_CLASS (x) != CB_CLASS_BOOLEAN
	  || (y && CB_TREE_CLASS (y) != CB_CLASS_BOOLEAN))
	ABORT ();
      category = CB_CATEGORY_BOOLEAN;
      break;

    case '@':
      /* parentheses */
      category = CB_TREE_CATEGORY (x);
      break;

    default:
      ABORT ();
    }

  p = make_tree (CB_TAG_BINARY_OP, category, sizeof (struct cb_binary_op));
  p->op = op;
  p->x = x;
  p->y = y;
  return CB_TREE (p);
}

cb_tree
cb_build_binary_list (cb_tree l, char op)
{
  cb_tree e = CB_VALUE (l);
  for (l = CB_CHAIN (l); l; l = CB_CHAIN (l))
    e = cb_build_binary_op (e, op, CB_VALUE (l));
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
  p->varcnt = 0;
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
  struct cb_label *p =
    make_tree (CB_TAG_LABEL, CB_CATEGORY_UNKNOWN, sizeof (struct cb_label));
  p->id = cb_id++;
  p->name = cb_define (name, CB_TREE (p));
  p->section = section;
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
cb_build_initialize (cb_tree var, cb_tree val, cb_tree rep, cb_tree def, int flag)
{
  struct cb_initialize *p =
    make_tree (CB_TAG_INITIALIZE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_initialize));
  p->var = var;
  p->val = val;
  p->rep = rep;
  p->def = def;
  p->flag_statement = flag;
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
cb_build_perform_varying (cb_tree name, cb_tree from, cb_tree by, cb_tree until)
{
  struct cb_perform_varying *p =
    make_tree (CB_TAG_PERFORM_VARYING, CB_CATEGORY_UNKNOWN, sizeof (struct cb_perform_varying));
  p->name = name;
  p->from = from;
  p->step = name ? cb_build_add (name, by, cb_int0) : NULL;
  p->until = until;
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
cb_list_add (cb_tree l, cb_tree x)
{
  return cb_list_append (l, cb_list (x));
}

cb_tree
cb_list_append (cb_tree l1, cb_tree l2)
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
cb_list_reverse (cb_tree l)
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
cb_list_length (cb_tree l)
{
  int n = 0;
  for (; l; l = CB_CHAIN (l))
    n++;
  return n;
}

void
cb_list_map (cb_tree (*func) (cb_tree x), cb_tree l)
{
  for (; l; l = CB_CHAIN (l))
    CB_VALUE (l) = func (CB_VALUE (l));
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
  p = cob_malloc (sizeof (struct cb_word));
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
  struct cb_program *p = cob_malloc (sizeof (struct cb_program));
  memset (p, 0, sizeof (struct cb_program));
  p->decimal_point = '.';
  p->currency_symbol = '$';
  p->numeric_separator = ',';
  return p;
}

/*
 * FUNCTION
 */

static cb_tree
make_intrinsic (cb_tree name, struct cb_intrinsic_table *cbp, cb_tree args, cb_tree field)
{
	struct cb_intrinsic	*x;

/* Leave in, we may need this
	cb_tree			l;
	for ( l = args; l; l = CB_CHAIN(l) ) {
		switch (CB_TREE_TAG (CB_VALUE(l)) ) {
		case CB_TAG_CONST:
		case CB_TAG_INTEGER:
		case CB_TAG_LITERAL:
		case CB_TAG_DECIMAL:
		case CB_TAG_FIELD:
		case CB_TAG_REFERENCE:
		case CB_TAG_INTRINSIC:
			break;
		default:
			cb_error (_("FUNCTION %s has invalid/not supported arguments - Tag %d"), cbp->name, CB_TREE_TAG(l));
			return cb_error_node;

		}
	}
*/
	x = make_tree (CB_TAG_INTRINSIC, cbp->category, sizeof(struct cb_intrinsic));
	x->name = name;
	x->args = args;
	x->intr_tab = cbp;
	x->intr_field = field;
	return CB_TREE(x);
}

cb_tree
cb_build_intrinsic (cb_tree name, cb_tree args)
{
	struct cb_intrinsic_table	*cbp;
	int				numargs;

	numargs = cb_list_length (args);

	cbp = lookup_intrinsic(CB_NAME(name));
	if (cbp) {
		if ( (cbp->args != -1 && numargs != cbp->args) ||
		     (cbp->args == -1 && cbp->intr_enum != CB_INTR_RANDOM && numargs < 1) ) {
			cb_error_x (name,_("FUNCTION %s has wrong number of arguments"),
				cbp->name);
			return cb_error_node;
		}
		/* cb_tree	x; */
		switch(cbp->intr_enum) {
		case CB_INTR_LENGTH:
			if ( CB_INTRINSIC_P (CB_VALUE(args)) ) {
				return make_intrinsic (name, cbp, args, NULL);
			} else {
				return cb_build_length (CB_VALUE(args));
			}

		case CB_INTR_WHEN_COMPILED:
			return cb_intr_whencomp;
		case CB_INTR_PI:
			return cb_intr_pi;
		case CB_INTR_E:
			return cb_intr_e;

		case CB_INTR_LOWER_CASE:
		case CB_INTR_UPPER_CASE:
		case CB_INTR_REVERSE:
			if ( CB_INTRINSIC_P (CB_VALUE(args)) ) {
				return make_intrinsic (name, cbp, args, cb_int0);
			} else {
				return make_intrinsic (name, cbp, args, cb_build_length(CB_VALUE(args)));
			}

		case CB_INTR_NUMVAL:
		case CB_INTR_NUMVAL_C:
			return make_intrinsic (name, cbp, args, NULL);
		case CB_INTR_CURRENT_DATE:
		case CB_INTR_CHAR:
		case CB_INTR_DATE_OF_INTEGER:
		case CB_INTR_DAY_OF_INTEGER:
		case CB_INTR_INTEGER_OF_DATE:
		case CB_INTR_INTEGER_OF_DAY:
		case CB_INTR_TEST_DATE_YYYYMMDD:
		case CB_INTR_TEST_DAY_YYYYDDD:
		case CB_INTR_FACTORIAL:
		case CB_INTR_ABS:
		case CB_INTR_ACOS:
		case CB_INTR_ASIN:
		case CB_INTR_ATAN:
		case CB_INTR_COS:
		case CB_INTR_EXP:
		case CB_INTR_EXP10:
		case CB_INTR_LOG:
		case CB_INTR_LOG10:
		case CB_INTR_SIN:
		case CB_INTR_SQRT:
		case CB_INTR_TAN:
		case CB_INTR_ORD:
		case CB_INTR_INTEGER:
		case CB_INTR_INTEGER_PART:
		case CB_INTR_ANNUITY:
		case CB_INTR_MOD:
		case CB_INTR_REM:
			return make_intrinsic (name, cbp, args, NULL);

		case CB_INTR_SUM:
		case CB_INTR_MIN:
		case CB_INTR_MAX:
		case CB_INTR_MIDRANGE:
		case CB_INTR_MEDIAN:
		case CB_INTR_MEAN:
		case CB_INTR_RANGE:
		case CB_INTR_RANDOM:
		case CB_INTR_VARIANCE:
		case CB_INTR_STANDARD_DEVIATION:
		case CB_INTR_PRESENT_VALUE:
		case CB_INTR_ORD_MIN:
		case CB_INTR_ORD_MAX:
		case CB_INTR_YEAR_TO_YYYY:
		case CB_INTR_DATE_TO_YYYYMMDD:
		case CB_INTR_DAY_TO_YYYYDDD:
			return make_intrinsic (name, cbp, args, cb_int1);

		default:
			break;
		}
	}
	cb_error_x (name,_("FUNCTION %s not implemented"), CB_NAME(name));
	return cb_error_node;
}


