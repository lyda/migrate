
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"


/*
 * Tree
 */

cob_tree cob_any;
cob_tree cob_true;
cob_tree cob_false;

void
init_tree (void)
{
  cob_any = malloc (sizeof (struct cob_tree_list));
  cob_true = malloc (sizeof (struct cob_tree_list));
  cob_false = malloc (sizeof (struct cob_tree_list));
  COB_TREE_TAG (cob_any) = cob_tag_any;
  COB_TREE_TAG (cob_true) = cob_tag_true;
  COB_TREE_TAG (cob_false) = cob_tag_false;
}


/*
 * Tree list
 */

cob_tree_list
cons (void *x, cob_tree_list l)
{
  struct cob_tree_list *p = malloc (sizeof (struct cob_tree_list));
  p->item = x;
  p->next = l;
  return p;
}

cob_tree_list
make_list (void *x)
{
  return cons (x, NULL);
}

cob_tree_list
list_add (cob_tree_list l, void *x)
{
  return list_append (l, make_list (x));
}

cob_tree_list
list_append (cob_tree_list l1, cob_tree_list l2)
{
  if (l1 == NULL)
    return l2;
  else
    {
      cob_tree_list p;
      for (p = l1; p->next != NULL; p = p->next);
      p->next = l2;
      return l1;
    }
}

int
list_length (cob_tree_list l)
{
  int n = 0;
  for (; l; l = l->next)
    n++;
  return n;
}


/*
 * Literals
 */

cob_tree
make_literal (char *name)
{
  struct lit *p = malloc (sizeof (struct lit));
  COB_TREE_TAG (p) = cob_tag_literal;
  COB_FIELD_NEXT (p) = NULL;
  COB_FIELD_NAME (p) = name;
  COB_FIELD_TYPE (p) = 0;
  p->all = 0;
  p->nick = NULL;
  p->len = strlen (COB_FIELD_NAME (p));
  return COB_TREE (p);
}

static char
sign_to_char (int digit)
{
  if (!digit)
    return '}';
  if (digit == 0x80)
    return '{';
  if (digit > 0)
    return 'A' + (char) (digit - 1);
  digit = -digit;
  return 'J' + (char) (digit - 1);
}

cob_tree
invert_literal_sign (cob_tree x)
{
  char *s = COB_FIELD_NAME (x);
  s += strlen (s) - 1;
  *s = sign_to_char (-(*s - 0x30));
  return x;
}


/*
 * Symbols
 */

cob_tree 
make_symbol (char *name)
{
  struct sym *p = malloc (sizeof (struct sym));
  COB_TREE_TAG (p) = cob_tag_symbol;
  COB_FIELD_NEXT (p) = NULL;
  COB_FIELD_NAME (p) = name;
  COB_FIELD_TYPE (p) = 0;
  p->times = 0;
  p->slack = 0;
  p->level = 0;
  p->defined = 0;
  p->decimals = 0;
  p->redefines = NULL;
  p->value = NULL;
  p->sort_data = NULL;
  p->linkage_flg = 0;
  p->scr = NULL;
  p->clone = NULL;
  p->parent = NULL;
  p->son = NULL;
  p->brother = NULL;
  p->occurs = NULL;
  p->flags.is_pointer = 0;
  p->flags.just_r = 0;
  p->flags.separate_sign = 0;
  p->flags.leading_sign = 0;
  p->flags.blank = 0;
  p->flags.sync = 0;
  return COB_TREE (p);
}

cob_tree 
make_filler (void)
{
  char name[15];
  cob_tree sy;
  static int filler_num = 1;

  sprintf (name, "FIL$%05d", filler_num++);
  sy = install (name, SYTB_VAR, 0);
  SYMBOL (sy)->defined = 1;
  return sy;
}


/*
 * Subscript references
 */

cob_tree
make_subref (cob_tree sy, cob_tree_list subs)
{
  struct subref *p = malloc (sizeof (struct subref));
  COB_TREE_TAG (p) = cob_tag_subref;
  p->sym     = sy;
  p->subs    = subs;
  return COB_TREE (p);
}


/*
 * Substring
 */

cob_tree
make_substring (cob_tree var, cob_tree offset, cob_tree len)
{
  struct substring *p = malloc (sizeof (struct substring));
  COB_TREE_TAG (p) = cob_tag_substring;
  p->sym = var;
  p->off = offset;
  p->len = len;
  p->slot = substring_slots++;
  return COB_TREE (p);
}


/*
 * Expression
 */

cob_tree
make_expr (cob_tree left, char op, cob_tree right)
{
  struct expr *p = malloc (sizeof (struct expr));
  COB_TREE_TAG (p) = cob_tag_expr;
  p->op = op;
  p->left = left;
  p->right = right;
  return COB_TREE (p);
}


/*
 * Condition
 */

cob_tree
make_cond (cob_tree x, enum cond_type type, cob_tree y)
{
  struct cond *p = malloc (sizeof (struct cond));
  COB_TREE_TAG (p) = cob_tag_cond;
  p->type  = type;
  p->left  = x;
  p->right = y;
  return COB_TREE (p);
}

cob_tree
make_unary_cond (cob_tree x, enum cond_type type)
{
  return make_cond (x, type, 0);
}


/*
 * Range
 */

cob_tree
make_range (cob_tree lower, cob_tree upper)
{
  struct cob_range *p = malloc (sizeof (struct cob_range));
  COB_TREE_TAG (p) = cob_tag_range;
  p->lower = lower;
  p->upper = upper;
  return COB_TREE (p);
}



struct call_parameter *
make_parameter (cob_tree var, int mode)
{
  struct call_parameter *p = malloc (sizeof (struct call_parameter));
  p->var = var;
  p->mode = mode;
  p->location = 0;
  p->sec_no = 0;
  p->next = NULL;
  return p;
}

struct inspect_item *
make_inspect_item (int type, cob_tree sy1, cob_tree sy2, cob_tree_list list)
{
  struct inspect_item *p = malloc (sizeof (struct inspect_item));
  p->type = type;
  p->sy1  = sy1;
  p->sy2  = sy2;
  p->list = list;
  return p;
}


/*
 * Type test
 */

int
is_variable (cob_tree x)
{
  if (SYMBOL_P (x))
    switch (COB_FIELD_TYPE (x))
      {
      case '8':		/* 88 field */
      case '9':		/* numeric */
      case 'A':		/* alpha */
      case 'B':		/* binary */
      case 'C':		/* packed decimal */
      case 'D':		/* screen data */
      case 'E':		/* edited */
      case 'G':		/* group */
      case 'U':		/* float or double */
      case 'X':		/* alphanum */
	return 1;
      }

  return 0;
}

int
is_subscripted (cob_tree x)
{
  for (; x; x = x->parent)
    if (x->times > 1)
      return 1;
  return 0;
}

int
count_subscripted (cob_tree x)
{
  int n = 0;
  for (; x; x = x->parent)
    if (x->times > 1)
      n++;
  return n;
}

int
is_numeric (cob_tree x)
{
  if (SUBSTRING_P (x))
    x = SUBSTRING_VAR (x);

  if (SUBREF_P (x))
    x = SUBREF_SYM (x);

  if (COB_FIELD_P (x))
    {
      char ty = COB_FIELD_TYPE (x);
      if (ty == '9' || ty == 'B' || ty == 'C' || ty == 'U')
	return 1;
    }

  return 0;
}

int
is_editable (cob_tree x)
{
  if (SUBSTRING_P (x))
    x = SUBSTRING_VAR (x);

  if (SUBREF_P (x))
    x = SUBREF_SYM (x);

  if (COB_FIELD_P (x))
    {
      char ty = COB_FIELD_TYPE (x);
      if (ty == '9' || ty == 'B' || ty == 'C' || ty == 'U' || ty == 'E')
	return 1;
    }

  return 0;
}

int
is_valid_expr (cob_tree x)
{
  if (EXPR_P (x))
    if (is_valid_expr (EXPR_LEFT (x)) && is_valid_expr (EXPR_RIGHT (x)))
      return 1;

  if (is_numeric (x))
    return 1;

  return 0;
}


void
print_tree (cob_tree x, FILE *fp)
{
  switch (COB_TREE_TAG (x))
    {
    case cob_tag_literal:
      if (x == spe_lit_ZE)
	fputs ("ZERO", fp);
      else
	fprintf (fp, "\"%s\"", COB_FIELD_NAME (x));
      break;

    case cob_tag_symbol:
      fputs (COB_FIELD_NAME (x), fp);
      break;

    case cob_tag_subref:
      {
	cob_tree_list ls;
	print_tree (SUBREF_SYM (x), fp);
	fputs ("(", fp);
	for (ls = SUBREF_SUBS (x); ls; ls = ls->next)
	  {
	    print_tree (ls->item, fp);
	    if (ls->next)
	      fputs (", ", fp);
	    else
	      fputs (")", fp);
	  }
      }
      break;

    case cob_tag_substring:
      {
	print_tree (SUBSTRING_VAR (x), fp);
	fputs ("(", fp);
	print_tree (SUBSTRING_OFFSET (x), fp);
	fputs (":", fp);
	if (SUBSTRING_LENGTH (x))
	  print_tree (SUBSTRING_LENGTH (x), fp);
	fputs (")", fp);
      }
      break;

    case cob_tag_expr:
      {
	fprintf (fp, "(%c ", EXPR_OP (x));
	print_tree (EXPR_LEFT (x), fp);
	fputs (" ", fp);
	print_tree (EXPR_RIGHT (x), fp);
	fputs (")", fp);
      }
      break;

    case cob_tag_cond:
      {
	cob_tree l = COND_LEFT (x);
	cob_tree r = COND_RIGHT (x);

	switch (COND_TYPE (x))
	  {
	  case COND_EQ:
	    fputs ("(= ", fp);
	    print_tree (l, fp);
	    fputs (" ", fp);
	    print_tree (r, fp);
	    fputs (")", fp);
	    break;

	  case COND_AND:
	    fputs ("(and ", fp);
	    print_tree (l, fp);
	    fputs (" ", fp);
	    print_tree (r, fp);
	    fputs (")", fp);
	    break;

	  default:
	    fprintf (fp, "(cond %d ", COND_TYPE (x));
	    print_tree (l, fp);
	    if (r)
	      {
		fputs (" ", fp);
		print_tree (r, fp);
	      }
	    fputs (")", fp);
	  }
	break;
      }

    default:
      fprintf (fp, "#<unknown %d %p>", COB_TREE_TAG (x), x);
    }
}
