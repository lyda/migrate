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

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return val % HASH_SIZE;
}

static struct cobc_word *
lookup_word (const char *name)
{
  struct cobc_word *p;
  int val = hash (name);

  /* find the existing word */
  if (current_program)
    for (p = current_program->word_table[val]; p; p = p->next)
      if (strcasecmp (p->name, name) == 0)
	return p;

  /* create new word */
  p = malloc (sizeof (struct cobc_word));
  memset (p, 0, sizeof (struct cobc_word));
  p->name = strdup (name);

  /* insert it into the table */
  if (current_program)
    {
      p->next = current_program->word_table[val];
      current_program->word_table[val] = p;
    }

  return p;
}

static struct cobc_word **
make_word_table (void)
{
  size_t size = sizeof (struct cobc_word *) * HASH_SIZE;
  struct cobc_word **p = malloc (size);
  memset (p, 0, size);
  return p;
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
      sprintf (s, "%s", COBC_LABEL (x)->name);
      break;

    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	if (p->op == '@')
	  {
	    s += sprintf (s, "(");
	    s += tree_name_1 (s, p->x);
	    s += sprintf (s, ")");
	  }
	else if (p->op == '!')
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

int
tree_category (cobc_tree x)
{
  switch (COBC_TREE_TYPE (x))
    {
    case COB_TYPE_GROUP:
      return COB_TYPE_ALPHANUMERIC;
    case COB_TYPE_NUMERIC_BINARY:
    case COB_TYPE_NUMERIC_PACKED:
      return COB_TYPE_NUMERIC;
    default:
      return COBC_TREE_TYPE (x);
    }
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
cobc_tree cobc_error_node;

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
    COBC_LABEL (make_label (make_reference (name), NULL));
  free (l->cname);
  l->cname = name;
  l->need_begin = 1;
  return l;
}

void
init_constants (void)
{
  int i;
  cobc_error_node  = make_constant (COB_TYPE_UNKNOWN, 0);
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
make_string (const unsigned char *str)
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
make_numeric_literal (int sign, unsigned char *digits, int expt)
{
  struct cobc_literal *p = make_literal (COB_TYPE_NUMERIC, digits);
  p->sign = sign;
  p->expt = expt;
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

struct cobc_picture *
parse_picture (const char *str)
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
  struct cobc_picture *pic = malloc (sizeof (struct cobc_picture));
  memset (pic, 0, sizeof (struct cobc_picture));

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
	  /* FIXME: need more check */
	  break;

	case 'C':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'R' && p[2] == 0))
	    goto error;
	  p++;
	  break;

	case 'D':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'B' && p[2] == 0))
	    goto error;
	  p++;
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
  pic->size = size;
  pic->digits = digits;
  pic->expt = - decimals;
  pic->have_sign = s_count;

  /* set picture category */
  switch (category)
    {
    case PIC_ALPHABETIC:
      pic->category = COB_TYPE_ALPHABETIC;
      break;
    case PIC_NUMERIC:
      pic->category = COB_TYPE_NUMERIC;
      if (digits > 18)
	yyerror (_("numeric entry cannot be larger than 18 digits"));
      break;
    case PIC_ALPHANUMERIC:
    case PIC_NATIONAL:
      pic->category = COB_TYPE_ALPHANUMERIC;
      break;
    case PIC_NUMERIC_EDITED:
      pic->str = buff;
      pic->category = COB_TYPE_NUMERIC_EDITED;
      break;
    case PIC_EDITED:
    case PIC_ALPHABETIC_EDITED:
    case PIC_ALPHANUMERIC_EDITED:
    case PIC_NATIONAL_EDITED:
      pic->str = buff;
      pic->category = COB_TYPE_ALPHANUMERIC_EDITED;
      break;
    default:
      goto error;
    }

  return pic;

 error:
  yyerror (_("invalid picture string"));
  return pic;
}


/*
 * Field
 */

cobc_tree
make_field (cobc_tree name)
{
  struct cobc_field *p =
    make_tree (cobc_tag_field, COB_TYPE_ALPHANUMERIC, sizeof (struct cobc_field));
  p->name = associate (name, COBC_TREE (p));
  return COBC_TREE (p);
}

cobc_tree
make_field_3 (cobc_tree name, const char *pic, int usage)
{
  cobc_tree x = make_field (name);
  COBC_FIELD (x)->pic = parse_picture (pic);
  COBC_FIELD (x)->usage = usage;
  finalize_field (COBC_FIELD (x));
  return x;
}

cobc_tree
make_field_x (const char *name, const char *pic, int usage)
{
  return make_field_3 (make_reference (name), pic, usage);
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
    case cobc_tag_field:
      {
	return COBC_FIELD (x)->size;
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
	      return f->size - literal_to_int (COBC_LITERAL (r->offset)) + 1;
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
    if (p->flag_used)
      return 1;
  return 0;
}

int
field_used_any_child (struct cobc_field *p)
{
  if (p->flag_used)
    return 1;
  for (p = p->children; p; p = p->sister)
    if (field_used_any_child (p))
      return 1;
  return 0;
}

void
field_set_used (struct cobc_field *p)
{
  p->flag_used = 1;
  for (; p; p = p->parent)
    if (p->redefines)
      {
	p->redefines->flag_used = 1;
	break;
      }
}

/* build */

struct cobc_field *
build_field (cobc_tree level, cobc_tree name, struct cobc_field *last_field)
{
  struct cobc_field *f;
  struct cobc_reference *r = COBC_REFERENCE (name);
  int lv = 0;
  {
    const char *p = COBC_REFERENCE (level)->word->name;
    for (; *p; p++)
      lv = lv * 10 + (*p - '0');
  }

  /* checks for redefinition */
  if (lv == 01 || lv == 77)
    {
      if (r->word->count > 0)
	{
	  redefinition_error (name);
	  return NULL;
	}
    }
  else
    {
      struct cobc_list *l;
      for (l = r->word->items; l; l = l->next)
	if (!COBC_FIELD_P (l->item)
	    || COBC_FIELD (l->item)->level == 01
	    || COBC_FIELD (l->item)->level == 77)
	  {
	    redefinition_error (name);
	    return NULL;
	  }
    }

  /* build the field */
  f = COBC_FIELD (make_field (name));
  f->level = lv;
  f->usage = COBC_USAGE_DISPLAY;
  f->occurs = 1;

  if (last_field && last_field->level == 88)
    last_field = last_field->parent;

  if (f->level == 01 || f->level == 77)
    {
      if (last_field)
	field_founder (last_field)->sister = f;
    }
  else if (!last_field)
    {
      yyerror_x (level, _("level number must begin with 01 or 77"));
      return NULL;
    }
  else if (f->level == 66)
    {
      struct cobc_field *p;
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
      COBC_TREE_CLASS (f) = COB_TYPE_BOOLEAN;
    }
  else if (f->level > 49)
    {
      yyerror_x (level, _("invalid level number `%s'"),
		   COBC_LITERAL (level)->data);
      return NULL;
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
      struct cobc_field *p;
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
      struct cobc_field *p;
      for (p = last_field->parent; p; p = p->parent)
	if (p->level == f->level)
	  {
	    last_field = p;
	    goto sister;
	  }
      yyerror_x (level, _("field hierarchy broken"));
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

struct cobc_field *
validate_redefines (struct cobc_field *field, cobc_tree redefines)
{
  struct cobc_field *f, *p;
  struct cobc_reference *r = COBC_REFERENCE (redefines);
  cobc_tree x = COBC_TREE (field);

  /* ISO+IEC+1989-2002: 13.16.42.2-7 */
  if (r->next)
    {
      yyerror_x (x, _("`%s' cannot be qualified"), COBC_NAME (redefines));
      return NULL;
    }

  /* resolve the name in the current group (if any) */
  if (field->parent)
    {
      cobc_tree parent = COBC_TREE (field->parent);
      r->next = COBC_REFERENCE (copy_reference (redefines, parent));
    }
  if (resolve_data_name (redefines) == cobc_error_node)
    return NULL;
  f = COBC_FIELD (r->value);

  /* ISO+IEC+1989-2002: 13.16.42.2-2 */
  if (f->level != field->level)
    {
      yyerror_x (x, _("level number of REDEFINES entries must be identical"));
      return NULL;
    }
  if (f->level == 66 || f->level == 88)
    {
      yyerror_x (x, _("level number of REDEFINES entry cannot be 66 or 88"));
      return NULL;
    }

  /* ISO+IEC+1989-2002: 13.16.42.2-11 */
  for (p = f->sister; p && p->redefines; p = p->sister);
  if (p != field)
    {
      yyerror_x (x, _("REDEFINES must follow the original definition"));
      return NULL;
    }

  return f;
}

static int
validate_field_1 (struct cobc_field *f)
{
  cobc_tree x = COBC_TREE (f);
  char *name = tree_name (x);

  if (f->children)
    {
      /* group */
      if (f->pic)
	yyerror_x (x, _("group name `%s' may not have PICTURE"), name);

      if (f->flag_justified)
	yyerror_x (x, _("group name `%s' may not have JUSTIFIED RIGHT"), name);

      for (f = f->children; f; f = f->sister)
	validate_field_1 (f);
    }
  else if (f->level == 66)
    {
    }
  else if (f->level == 88)
    {
      /* conditional variable */
      if (f->pic)
	yyerror_x (x, _("level 88 field `%s' may not have PICTURE"), name);
    }
  else
    {
      /* validate PICTURE */
      if (!f->pic)
	if (f->usage != COBC_USAGE_INDEX)
	  {
	    yyerror_x (x, _("PICTURE clause required for `%s'"), name);
	    return -1; /* cannot continue */
	  }

      /* validate USAGE */
      switch (f->usage)
	{
	case COBC_USAGE_DISPLAY:
	  break;
	case COBC_USAGE_BINARY:
	case COBC_USAGE_PACKED:
	  if (f->pic->category != COB_TYPE_NUMERIC)
	    yywarn (_("field must be numeric"));
	  break;
	case COBC_USAGE_INDEX:
	  break;
	default:
	  abort ();
	}

      /* validate SIGN */

      /* validate OCCURS */
      if (f->flag_occurs)
	if (f->level < 2 || f->level > 49)
	  yyerror_x (x, _("level %02d field `%s' cannot have OCCURS"),
		     f->level, name);

      /* validate JUSTIFIED RIGHT */
      if (f->flag_justified)
	switch (f->pic->category)
	  {
	  case COB_TYPE_ALPHABETIC:
	  case COB_TYPE_ALPHANUMERIC:
	  case COB_TYPE_NATIONAL:
	    break;
	  default:
	    yyerror_x (x, _("`%s' cannot have JUSTIFIED RIGHT"), name);
	    break;
	  }

      /* validate SYNCHRONIZED */
      if (f->flag_synchronized)
	if (f->usage != COBC_USAGE_BINARY)
	  {
	    // yywarn ("SYNCHRONIZED here has no effect");
	    f->flag_synchronized = 0;
	  }

      /* validate BLANK ZERO */

      /* validate VALUE */
      if (f->values)
	{
	  struct cobc_field *p;

	  if (f->values->next || COBC_PARAMETER_P (f->values->item))
	    yyerror_x (x, _("only level 88 item may have multiple values"));

	  /* ISO+IEC+1989-2002: 13.16.42.2-10 */
	  for (p = f; p; p = p->parent)
	    if (p->redefines)
	      yyerror_x (x, _("entries under REDEFINES cannot have VALUE clause"));
	}
    }

  return 0;
}

static int validate_move (cobc_tree src, cobc_tree dst, int value_flag);

static int
validate_field_value (struct cobc_field *f)
{
  if (f->values)
    validate_move (f->values->item, COBC_TREE (f), 1);

  if (f->children)
    for (f = f->children; f; f = f->sister)
      validate_field_value (f);

  return 0;
}

int
validate_field (struct cobc_field *f)
{
  if (validate_field_1 (f) != 0)
    return -1;
  finalize_field (f);
  validate_field_value (f);
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
      COBC_TREE_TYPE (p) = COB_TYPE_GROUP;

      for (p = p->children; p; p = p->sister)
	setup_parameters (p);
    }
  else if (p->level == 66)
    {
      COBC_TREE_CLASS (p) = COBC_TREE_CLASS (p->redefines);
      COBC_TREE_TYPE (p) = COBC_TREE_TYPE (p->redefines);
      if (p->rename_thru)
	COBC_TREE_TYPE (p) = COB_TYPE_GROUP;
    }
  else
    {
      /* regular field */
      if (p->usage == COBC_USAGE_INDEX)
	p->pic = parse_picture ("S9(9)");

      /* set class */
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

      /* set type */
      switch (p->usage)
	{
	case COBC_USAGE_BINARY:
	case COBC_USAGE_INDEX:
	  COBC_TREE_TYPE (p) = COB_TYPE_NUMERIC_BINARY;
	  break;
	case COBC_USAGE_PACKED:
	  COBC_TREE_TYPE (p) = COB_TYPE_NUMERIC_PACKED;
	  break;
	default:
	  COBC_TREE_TYPE (p) = p->pic->category;
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

  if (p->children)
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
	      if (c->flag_synchronized)
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
    }
  else
    {
      /* terminals */
      switch (p->usage)
	{
	case COBC_USAGE_BINARY:
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
	    break;
	  }
	case COBC_USAGE_DISPLAY:
	  {
	    p->size = p->pic->size;
	    if (p->pic->category == COB_TYPE_NUMERIC && p->flag_sign_separate)
	      p->size++;
	    break;
	  }
	case COBC_USAGE_INDEX:
	  {
	    p->size = sizeof (int);
	    break;
	  }
	case COBC_USAGE_PACKED:
	  {
	    p->size = p->pic->size / 2;
	    if (p->pic->size % 2 || p->pic->have_sign)
	      p->size++;
	    break;
	  }
	default:
	  abort ();
	}
    }

  /* ISO+IEC+1989-2002: 13.16.42.2-9 */
  if (p->redefines && p->size * p->occurs > p->redefines->size)
    if (p->redefines->level != 01 || p->redefines->flag_external)
      yyerror_x (COBC_TREE (p), _("size of `%s' larger than size of `%s'"),
		 p->name, p->redefines->name);

  return p->size;
}

void
finalize_field (struct cobc_field *p)
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

void
finalize_file (struct cobc_file *f, struct cobc_field *records)
{
  char pic[BUFSIZ];
  struct cobc_field *p;

  for (p = records; p; p = p->sister)
    {
      /* check the record size */
      if (f->record_min > 0)
	if (p->size < f->record_min)
	  yyerror (_("record size too small `%s'"), p->name);
      if (f->record_max > 0)
	if (p->size > f->record_max)
	  yyerror (_("record size too large `%s'"), p->name);
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
  f->record = COBC_FIELD (make_field_x (f->name, pic, COBC_USAGE_DISPLAY));
  field_set_used (f->record);
  f->record->sister = records;

  for (p = records; p; p = p->sister)
    {
      p->file = f;
      p->redefines = f->record;
      field_set_used (p);
    }
}


/*
 * Reference
 */

cobc_tree
make_reference (const char *name)
{
  struct cobc_reference *p =
    make_tree (cobc_tag_reference, COB_TYPE_UNKNOWN, sizeof (struct cobc_reference));
  p->word = lookup_word (name);
  return COBC_TREE (p);
}

cobc_tree
copy_reference (cobc_tree ref, cobc_tree value)
{
  cobc_tree x = make_reference (COBC_FIELD (value)->name);
  struct cobc_word *word = COBC_REFERENCE (x)->word;
  memcpy (x, ref, sizeof (struct cobc_reference));
  COBC_REFERENCE (x)->word = word;
  set_value (x, value);
  return x;
}

void
set_value (cobc_tree ref, cobc_tree value)
{
  COBC_REFERENCE (ref)->value = value;
  if (COBC_REFERENCE (ref)->offset)
    {
      COBC_TREE_CLASS (ref) = COB_TYPE_ALPHANUMERIC;
      COBC_TREE_TYPE (ref) = COB_TYPE_ALPHANUMERIC;
    }
  else
    {
      COBC_TREE_CLASS (ref) = COBC_TREE_CLASS (value);
      COBC_TREE_TYPE (ref) = COBC_TREE_TYPE (value);
    }
}

cobc_tree
make_filler (void)
{
  static int id = 1;
  char name[256];
  sprintf (name, "$%d", id++);
  return make_reference (name);
}

const char *
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

/* resolve data name */

cobc_tree
resolve_data_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_field *f;
  cobc_tree v = NULL;
  
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
      struct cobc_list *l;
      struct cobc_field *p, *pp;

      /* resolve the parent */
      cobc_tree px = COBC_TREE (r->next);
      if (resolve_data_name (px) == cobc_error_node)
	goto error;

      /* find the definition in the parent */
      pp = COBC_FIELD (r->next->value);
      for (l = r->word->items; l; l = l->next)
	if (COBC_FIELD_P (l->item))
	  for (p = COBC_FIELD (l->item)->parent; p; p = p->parent)
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
	    yyerror_x (px, _("`%s' not a group"), pp->name);
	  else
	    undefined_error (x);
	  goto error;
	}
    }

  /* validate data name */
  if (!COBC_FIELD_P (v))
    {
      yyerror_x (x, _("`%s' not data name"), r->word->name);
      abort ();
      goto error;
    }

  f = COBC_FIELD (v);

  set_value (x, v);

  if (f->level == 88)
    field_set_used (f->parent);
  else
    field_set_used (f);

  return x;

 error:
  r->value = cobc_error_node;
  return cobc_error_node;
}

int
validate_data_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_field *f = COBC_FIELD (r->value);
  const char *name = r->word->name;

  /* check the number of subscripts */
  if (list_length (r->subs) != f->indexes)
    {
      switch (f->indexes)
	{
	case 0:
	  yyerror_x (x, _("`%s' cannot be subscripted"), name);
	  break;
	case 1:
	  yyerror_x (x, _("`%s' requires 1 subscript"), name);
	  break;
	default:
	  yyerror_x (x, _("`%s' requires %d subscripts"), name, f->indexes);
	  break;
	}
      return -1;
    }

  /* check the range of constant subscripts */
  if (r->subs)
    {
      struct cobc_field *p;
      struct cobc_list *l = r->subs = list_reverse (r->subs);

      for (p = f; p; p = p->parent)
	if (p->flag_occurs)
	  {
	    cobc_tree sub = l->item;
	    if (COBC_LITERAL_P (sub))
	      {
		int n = literal_to_int (COBC_LITERAL (sub));
		if (n < p->occurs_min || n > p->occurs)
		  yyerror_x (x, _("subscript of `%s' out of bounds: %d"),
			     name, n);
	      }
	    l = l->next;
	  }

      r->subs = list_reverse (r->subs);
    }

  /* check the range of constant reference modification */
  if (r->offset && COBC_LITERAL_P (r->offset))
    {
      int offset = literal_to_int (COBC_LITERAL (r->offset));
      if (offset < 1 || offset > f->size)
	yyerror_x (x, _("offset of `%s' out of bounds: %d"), name, offset);
      else if (r->length && COBC_LITERAL_P (r->length))
	{
	  int length = literal_to_int (COBC_LITERAL (r->length));
	  if (length < 1 || length > f->size - offset + 1)
	    yyerror_x (x, _("length of `%s' out of bounds: %d"), name, length);
	}
    }

  return 0;
}

/* resolve label name */

static cobc_tree
resolve_label_in (const char *name, struct cobc_label *section)
{
  struct cobc_list *l;
  for (l = section->children; l; l = l->next)
    if (strcasecmp (name, COBC_LABEL (l->item)->name) == 0)
      return l->item;
  return cobc_error_node;
}

cobc_tree
resolve_label (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  cobc_tree v;

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
	  v = resolve_label_in (r->word->name, COBC_LABEL (r->offset));
	  if (v == cobc_error_node)
	    if (COBC_LABEL_P (r->word->items->item))
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
      struct cobc_reference *sr = r->next;
      cobc_tree sx = COBC_TREE (sr);

      switch (sr->word->count)
	{
	case 0:
	  undefined_error (sx);
	  goto error;
	case 1:
	  v = resolve_label_in (r->word->name, sr->word->items->item);
	  if (v == cobc_error_node)
	    {
	      undefined_error (x);
	      goto error;
	    }
	  break;
	default:
	  yyerror_x (sx, _("`%s' not section name"), sr->word->name);
	  goto error;
	}
    }

  if (!COBC_LABEL_P (v))
    {
      yyerror_x (x, _("`%s' not label name"), r->word->name);
      goto error;
    }

  COBC_LABEL (v)->need_begin = 1;
  if (r->length)
    COBC_LABEL (v)->need_return = 1;

  r->value = v;
  return v;

 error:
  r->value = cobc_error_node;
  return cobc_error_node;
}

/* resolve file name */

cobc_tree
resolve_file_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (COBC_FILE_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return r->value;
	}
      yyerror_x (x, _("`%s' not file name"), r->word->name);
      break;
    }

  r->value = cobc_error_node;
  return cobc_error_node;
}

/* resolve class name */

cobc_tree
resolve_class_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (COBC_CLASS_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return r->value;
	}
      yyerror_x (x, _("`%s' not class name"), r->word->name);
      break;
    }

  r->value = cobc_error_node;
  return cobc_error_node;
}

/* resolve builtin name */

cobc_tree
resolve_mnemonic_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);

  switch (r->word->count)
    {
    case 0:
      undefined_error (x);
      break;
    default:
      if (COBC_BUILTIN_P (r->word->items->item))
	{
	  r->value = r->word->items->item;
	  return x;
	}
      yyerror_x (x, _("`%s' not builtin name"), r->word->name);
      break;
    }

  r->value = cobc_error_node;
  return cobc_error_node;
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

    case '@':
      /* parentheses */
      COBC_TREE_CLASS (p) = COBC_TREE_CLASS (left);
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


/*
 * Program
 */

struct cobc_program *
build_program (void)
{
  struct cobc_program *p = malloc (sizeof (struct cobc_program));
  memset (p, 0, sizeof (struct cobc_program));
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
  COBC_SEQUENCE (s)->list = list_add (COBC_SEQUENCE (s)->list, x)

static cobc_tree
decimal_alloc (void)
{
  cobc_tree x = make_decimal (current_program->decimal_index++);
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
decimal_compute (cobc_tree s, char op, cobc_tree x, cobc_tree y)
{
  char *func;
  switch (op)
    {
    case '+': func = "cob_decimal_add"; break;
    case '-': func = "cob_decimal_sub"; break;
    case '*': func = "cob_decimal_mul"; break;
    case '/': func = "cob_decimal_div"; break;
    case '^': func = "cob_decimal_pow"; break;
    default: abort ();
    }
  add_stmt (s, make_funcall_2 (func, x, y));
}

static void
decimal_expand (cobc_tree s, cobc_tree d, cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      {
	cobc_tree e;
	if (x == cobc_zero)
	  e = make_funcall_2 ("cob_decimal_set_int", d, cobc_int0);
	else
	  abort ();
	add_stmt (s, e);
	break;
      }
    case cobc_tag_literal:
      {
	/* set d, N */
	struct cobc_literal *l = COBC_LITERAL (x);
	if (l->size < 10 && l->expt == 0)
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_int",
				       d, make_cast_int32 (x)));
	else
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case cobc_tag_reference:
      {
	/* set d, X */
	struct cobc_field *f = field (x);
	if (f->usage == COBC_USAGE_DISPLAY)
	  add_stmt (s, make_funcall_2 ("cob_check_numeric",
				       x, make_string (f->name)));
	if (f->usage == COBC_USAGE_INDEX)
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_int",
				       d, make_cast_int32 (x)));
	else
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	if (p->op == '@')
	  {
	    decimal_expand (s, d, p->x);
	  }
	else
	  {
	    /* set d, X
	     * set t, Y
	     * OP d, t */
	    cobc_tree t = decimal_alloc ();
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
decimal_assign (cobc_tree s, cobc_tree x, cobc_tree d, int round)
{
  const char *func =
    round ? "cob_decimal_get_field_r" : "cob_decimal_get_field";
  add_stmt (s, make_funcall_2 (func, d, x));
}

static cobc_tree
build_decimal_assign (struct cobc_list *vars, char op, cobc_tree val)
{
  struct cobc_list *l;
  cobc_tree s1 = make_sequence (NULL);
  cobc_tree s2 = make_sequence (NULL);
  cobc_tree d = decimal_alloc ();

  /* set d, VAL */
  decimal_expand (s2, d, val);

  if (op == 0)
    {
      for (l = vars; l; l = l->next)
	{
	  /* set VAR, d */
	  struct cobc_parameter *p = l->item;
	  decimal_assign (s2, p->x, d, p->type);
	  add_stmt (s1, s2);
	  if (l->next)
	    s2 = make_sequence (NULL);
	}
    }
  else
    {
      cobc_tree t = decimal_alloc ();
      for (l = vars; l; l = l->next)
	{
	  /* set t, VAR
	   * OP t, d
	   * set VAR, t
	   */
	  struct cobc_parameter *p = l->item;
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

cobc_tree
build_assign (struct cobc_list *vars, char op, cobc_tree val)
{
  if (!COBC_BINARY_OP_P (val))
    if (op == '+' || op == '-')
      {
	struct cobc_list *l;
	for (l = vars; l; l = l->next)
	  {
	    struct cobc_parameter *p = l->item;
	    if (op == '+')
	      l->item = build_add (p->x, val, p->type);
	    else
	      l->item = build_sub (p->x, val, p->type);
	  }
	return make_sequence (vars);
      }

  return build_decimal_assign (vars, op, val);
}


/*
 * ADD/SUBTRACT/MOVE CORRESPONDING
 */

cobc_tree
build_add (cobc_tree v, cobc_tree n, int round)
{
  struct cobc_field *f = field (v);

  if (f->usage == COBC_USAGE_INDEX)
    return build_move (make_binary_op (v, '+', n), v);

  switch (COBC_TREE_TAG (n))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (n);
	if (l->size < 10 && l->expt == 0 && round == 0)
	  return make_funcall_2 ("cob_add_int", v, make_cast_int32 (n));
	/* fall through */
      }
    default:
      {
	if (round)
	  return make_funcall_2 ("cob_add_r", v, n);
	else
	  return make_funcall_2 ("cob_add", v, n);
      }
    }
}

cobc_tree
build_sub (cobc_tree v, cobc_tree n, int round)
{
  struct cobc_field *f = field (v);

  if (f->usage == COBC_USAGE_INDEX)
    return build_move (make_binary_op (v, '-', n), v);

  switch (COBC_TREE_TAG (n))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (n);
	if (l->size < 10 && l->expt == 0 && round == 0)
	  return make_funcall_2 ("cob_sub_int", v, make_cast_int32 (n));
	/* fall through */
      }
    default:
      {
	if (round)
	  return make_funcall_2 ("cob_sub_r", v, n);
	else
	  return make_funcall_2 ("cob_sub", v, n);
      }
    }
}

static void
warning_destination (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_field *f = COBC_FIELD (r->value);
  cobc_tree loc = COBC_TREE (f);

  if (r->offset)
    return;

  if (f->pic)
    {
      char str[256];

      /* reconstruct the PICTURE string */
      if (f->pic->str)
	{
	  char *s = str;
	  char *p = f->pic->str;
	  while (*p)
	    {
	      int c = *p++;
	      int n = *p++;
	      if (n == 1)
		*s++ = c;
	      else
		s += sprintf (s, "%c(%d)", c, n);
	    }
	  *s = 0;
	}
      else
	{
	  int c = ((f->pic->category == COB_TYPE_ALPHABETIC) ? 'A' :
		   (f->pic->category == COB_TYPE_ALPHANUMERIC) ? 'X' :
		   (f->pic->category == COB_TYPE_NUMERIC) ? '9' : '?');
	  if (f->pic->size == 1)
	    sprintf (str, "%c", c);
	  else
	    sprintf (str, "%c(%d)", c, f->pic->size);
	}

      yywarn_x (loc, _("`%s' defined here as PIC %s"), f->name, str);
    }
  else
    {
      yywarn_x (loc, _("`%s' defined here as a group of length %d"),
		f->name, f->size);
    }
}

static int
validate_move (cobc_tree src, cobc_tree dst, int value_flag)
{
  struct cobc_field *f = field (dst);
  cobc_tree loc = src->source_line ? src : dst;

  switch (COBC_TREE_TAG (src))
    {
    case cobc_tag_const:
      {
	if (src == cobc_space)
	  {
	    if (f->pic)
	      if (f->pic->category == COB_TYPE_NUMERIC
		  || f->pic->category == COB_TYPE_NUMERIC_EDITED)
		goto invalid;
	  }
	else if (src == cobc_zero)
	  {
	    if (f->pic)
	      if (f->pic->category == COB_TYPE_ALPHABETIC)
		goto invalid;
	  }
	break;
      }
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (src);

	/* TODO: ALL literal */

	if (COBC_TREE_CLASS (src) == COB_TYPE_NUMERIC)
	  {
	    /* Numeric literal */

	    /* value check */
	    switch (tree_category (dst))
	      {
	      case COB_TYPE_ALPHANUMERIC:
	      case COB_TYPE_ALPHANUMERIC_EDITED:
		{
		  if (l->expt == 0)
		    goto type_mismatch;
		  else
		    goto invalid;
		}
	      case COB_TYPE_NUMERIC:
		{
		  if (f->pic->expt > 0)
		    {
		      /* check for PIC 9..P.. */
		      int i = 0;
		      if (l->expt != 0)
			goto value_mismatch;
		      if (l->size > f->pic->expt)
			i = l->size - f->pic->expt;
		      for (; i < l->size; i++)
			if (l->data[i] != '0')
			  goto value_mismatch;
		    }
		  else if (f->pic->expt == - f->pic->digits
			   && f->pic->size < f->pic->digits)
		    {
		      /* check for PIC P..9.. */
		      int i;
		      int limit = - f->pic->digits - f->pic->size;
		      if (l->size != - l->expt)
			goto value_mismatch;
		      if (limit > l->size)
			limit = l->size;
		      for (i = 0; i < limit; i++)
			if (l->data[i] != '0')
			  goto value_mismatch;
		    }
		  break;
		}
	      case COB_TYPE_NUMERIC_EDITED:
		{
		  /* TODO */
		  break;
		}
	      default:
		goto invalid;
	      }

	    /* sign check */
	    if (cobc_warn_constant)
	      if (l->sign < 0 && !f->pic->have_sign)
		yywarn_x (src, _("ignoring negative sign"));

	    /* size check */
	    if (l->expt < 0 && l->expt < f->pic->expt)
	      goto size_overflow;
	    if (l->size + l->expt > f->pic->digits + f->pic->expt)
	      goto size_overflow;
	  }
	else
	  {
	    /* Alphanumeric literal */

	    /* value check */
	    switch (tree_category (dst))
	      {
	      case COB_TYPE_ALPHABETIC:
		{
		  int i;
		  for (i = 0; i < l->size; i++)
		    if (!isalpha(l->data[i]))
		      goto value_mismatch;
		  break;
		}
	      case COB_TYPE_NUMERIC:
	      case COB_TYPE_NUMERIC_EDITED:
		goto type_mismatch;
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
    case cobc_tag_field:
    case cobc_tag_reference:
      {
	/* non-elementary move (ISO+IEC+1989-2002 14.8.24.3-2) */
	if (COBC_TREE_TYPE (src) == COB_TYPE_GROUP
	    || COBC_TREE_TYPE (dst) == COB_TYPE_GROUP)
	  break;

	/* elementary move */
	switch (tree_category (src))
	  {
	  case COB_TYPE_ALPHANUMERIC:
	    break;
	  case COB_TYPE_ALPHABETIC:
	  case COB_TYPE_ALPHANUMERIC_EDITED:
	    switch (tree_category (dst))
	      {
	      case COB_TYPE_NUMERIC:
	      case COB_TYPE_NUMERIC_EDITED:
		goto invalid;
	      }
	    break;
	  case COB_TYPE_NUMERIC:
	  case COB_TYPE_NUMERIC_EDITED:
	    switch (tree_category (dst))
	      {
	      case COB_TYPE_ALPHABETIC:
		goto invalid;
	      case COB_TYPE_ALPHANUMERIC:
	      case COB_TYPE_ALPHANUMERIC_EDITED:
		if (tree_category (src) == COB_TYPE_NUMERIC
		    && field (src)->pic->expt < 0)
		  goto invalid;
		break;
	      }
	    break;
	  }
	break;
      }
    case cobc_tag_binary_op:
      break;
    default:
      abort ();
    }
  return 0;

 invalid:
  if (value_flag)
    yyerror_x (loc, _("invalid VALUE clause"));
  else
    yyerror_x (loc, _("invalid MOVE statement"));
  return -1;

 type_mismatch:
  if (cobc_warn_strict_typing)
    {
      yywarn_x (loc, _("type mismatch"));
      if (!value_flag)
	warning_destination (dst);
    }
  return 0;

 value_mismatch:
  if (cobc_warn_constant)
    {
      yywarn_x (loc, _("constant value mismatch"));
      if (!value_flag)
	warning_destination (dst);
    }
  return 0;

 size_overflow:
  if (cobc_warn_constant)
    {
      yywarn_x (loc, _("constant size overflow"));
      if (!value_flag)
	warning_destination (dst);
    }
  return 0;
}

cobc_tree
build_move (cobc_tree src, cobc_tree dst)
{
  validate_move (src, dst, 0);
  return make_funcall_2 ("@move", src, dst);
}

static struct cobc_list *
build_corresponding_1 (cobc_tree (*func)(), cobc_tree x1, cobc_tree x2,
		       int opt, struct cobc_list *l)
{
  struct cobc_field *f1, *f2;
  for (f1 = field (x1)->children; f1; f1 = f1->sister)
    if (!f1->redefines && !f1->flag_occurs)
      for (f2 = field (x2)->children; f2; f2 = f2->sister)
	if (!f2->redefines && !f2->flag_occurs)
	  if (strcmp (f1->name, f2->name) == 0)
	    {
	      cobc_tree t1 = copy_reference (x1, COBC_TREE (f1));
	      cobc_tree t2 = copy_reference (x2, COBC_TREE (f2));
	      if (f1->children && f2->children)
		l = build_corresponding_1 (func, t1, t2, opt, l);
	      else
		{
		  field (t1)->flag_used = 1;
		  field (t2)->flag_used = 1;
		  if (opt < 0)
		    l = cons (func (t1, t2), l);
		  else
		    l = cons (func (t1, t2, opt), l);
		}
	    }
  return l;
}

cobc_tree
build_corresponding (cobc_tree (*func)(), cobc_tree x1, cobc_tree x2, int opt)
{
  return make_sequence (build_corresponding_1 (func, x1, x2, opt, NULL));
}


/*
 * DIVIDE
 */

cobc_tree
build_divide (cobc_tree dividend, cobc_tree divisor,
	      cobc_tree quotient, cobc_tree remainder)
{
  struct cobc_list *l = NULL;
  struct cobc_parameter *pq = COBC_PARAMETER (quotient);
  struct cobc_parameter *pr = COBC_PARAMETER (remainder);
  l = list_add (l, make_funcall_4 ("cob_div_quotient",
				   dividend, divisor, pq->x,
				   pq->type ? cobc_int1 : cobc_int0));
  l = list_add (l, make_funcall_1 ("cob_div_remainder", pr->x));
  return make_sequence (l);
}


/*
 * Condition
 */

static cobc_tree
build_cond_88 (cobc_tree x)
{
  struct cobc_field *f = field (x);
  struct cobc_list *l;
  cobc_tree c1 = NULL;

  /* refer to parent's data storage */
  x = copy_reference (x, COBC_TREE (f->parent));

  /* build condition */
  for (l = f->values; l; l = l->next)
    {
      cobc_tree c2;
      if (COBC_PARAMETER_P (l->item))
	{
	  /* VALUE THRU VALUE */
	  struct cobc_parameter *p = COBC_PARAMETER (l->item);
	  c2 = make_binary_op (make_binary_op (p->x, '[', x),
			       '&',
			       make_binary_op (x, '[', p->y));
	}
      else
	{
	  /* VALUE */
	  c2 = make_binary_op (x, '=', l->item);
	}
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = make_binary_op (c1, '|', c2);
    }
  return c1;
}

cobc_tree
build_cond (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
    case cobc_tag_funcall:
      return x;
    case cobc_tag_reference:
      {
	/* level 88 condition */
	if (field (x)->level == 88)
	  {
	    /* We need to build a 88 condition at every occurrence
	       instead of once at the beginning, because a 88 item
	       may be subscripted (i.e., not a constant tree). */
	    return build_cond (build_cond_88 (x));
	  }

	abort ();
      }
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	switch (p->op)
	  {
	  case '@':
	    return build_cond (p->x);
	  case '!':
	    p->x = build_cond (p->x);
	    break;
	  case '&': case '|':
	    p->x = build_cond (p->x);
	    p->y = build_cond (p->y);
	    break;
	  default:
	    if ((COBC_REFERENCE_P (p->x)
		 && field (p->x)->usage == COBC_USAGE_INDEX)
		|| (COBC_REFERENCE_P (p->y)
		    && field (p->y)->usage == COBC_USAGE_INDEX))
	      {
		return x;
	      }
	    else if (COBC_BINARY_OP_P (p->x) || COBC_BINARY_OP_P (p->y))
	      {
		/* decimal comparison */
		cobc_tree s = make_sequence (NULL);
		cobc_tree d1 = decimal_alloc ();
		cobc_tree d2 = decimal_alloc ();
		decimal_expand (s, d1, p->x);
		decimal_expand (s, d2, p->y);
		add_stmt (s, make_funcall_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		p->x = s;
	      }
	    else if (COBC_LITERAL_P (p->y))
	      {
		struct cobc_literal *l = COBC_LITERAL (p->y);
		int size = field_size (p->x);

		if (COBC_TREE_CLASS (p->x) == COB_TYPE_NUMERIC
		    && COBC_TREE_CLASS (p->y) == COB_TYPE_NUMERIC)
		  {
		    if (l->size < 10 && l->expt == 0)
		      p->x = make_funcall_2 ("cob_cmp_int",
					     p->x, make_cast_int32 (p->y));
		    else
		      p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
		  }
		else if (size > 0 && size >= l->size && !l->all)
		  p->x = make_funcall_2 ("@memcmp", p->x, p->y);
		else
		  p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    else
	      {
		/* field comparison */
		p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
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

static cobc_tree
build_evaluate_test (cobc_tree s, cobc_tree o)
{
  struct cobc_parameter *p;

  /* ANY is always true */
  if (o == cobc_any)
    return cobc_true;

  /* object TRUE or FALSE */
  if (o == cobc_true)
    return s;
  if (o == cobc_false)
    return make_negative (s);

  p = COBC_PARAMETER (o);

  /* subject TRUE or FALSE */
  if (s == cobc_true)
    return p->type ? make_negative (p->x) : p->x;
  if (s == cobc_false)
    return p->type ? p->x : make_negative (p->x);

  /* x THRU y */
  if (p->y)
    {
      cobc_tree x = make_binary_op (make_binary_op (p->x, '[', s),
				    '&',
				    make_binary_op (s, '[', p->y));
      return p->type ? make_negative (x) : x;
    }

  /* regular comparison */
  if (p->type)
    return make_binary_op (s, '~', p->x);
  else
    return make_binary_op (s, '=', p->x);
}

static cobc_tree
build_evaluate_internal (struct cobc_list *subject_list, struct cobc_list *case_list)
{
  cobc_tree stmt;
  cobc_tree c1 = NULL;
  struct cobc_list *subjs, *whens, *objs;

  if (case_list == NULL)
    return NULL;

  whens = case_list->item;
  stmt = whens->item;
  whens = whens->next;

  /* for each WHEN sequence */
  for (; whens; whens = whens->next)
    {
      cobc_tree c2 = NULL;
      /* single WHEN test */
      for (subjs = subject_list, objs = whens->item;
	   subjs && objs;
	   subjs = subjs->next, objs = objs->next)
	{
	  cobc_tree c3 = build_evaluate_test (subjs->item, objs->item);
	  if (c2 == NULL)
	    c2 = c3;
	  else
	    c2 = make_binary_op (c2, '&', c3);
	}
      if (subjs || objs)
	yyerror (_("wrong number of WHEN parameters"));
      /* connect multiple WHEN's */
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = make_binary_op (c1, '|', c2);
    }

  if (c1 == NULL)
    return stmt;
  else
    return make_if (build_cond (c1), stmt,
		    build_evaluate_internal (subject_list, case_list->next));
}

cobc_tree
build_evaluate (struct cobc_list *subject_list, struct cobc_list *case_list)
{
  return build_evaluate_internal (subject_list, case_list);
}


/*
 * SEARCH ALL
 */

static void
search_set_keys (struct cobc_field *f, cobc_tree x)
{
  struct cobc_binary_op *p;

  if (COBC_REFERENCE_P (x))
    x = build_cond_88 (x);
  
  p = COBC_BINARY_OP (x);
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
	  if (field (p->x) == field (f->keys[i].key))
	    {
	      f->keys[i].ref = p->x;
	      f->keys[i].val = p->y;
	      break;
	    }
	if (i == f->nkeys)
	  yyerror_x (x, _("undeclared key `%s'"), field (p->x)->name);
	break;
      }
    default:
      yyerror_x (x, _("invalid SEARCH ALL condition"));
      break;
    }
}

cobc_tree
build_search_all (cobc_tree table, cobc_tree cond)
{
  int i;
  struct cobc_field *f = field (table);
  cobc_tree c1 = NULL;

  /* set keys */
  for (i = 0; i < f->nkeys; i++)
    f->keys[i].ref = 0;
  search_set_keys (f, cond);

  /* build condition */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].ref)
      {
	cobc_tree c2;
	if (f->keys[i].dir == COB_ASCENDING)
	  c2 = make_binary_op (f->keys[i].ref, '=', f->keys[i].val);
	else
	  c2 = make_binary_op (f->keys[i].val, '=', f->keys[i].ref);
	if (c1 == NULL)
	  c1 = c2;
	else
	  c1 = make_binary_op (c1, '&', c2);
      }

  return build_cond (c1);
}
