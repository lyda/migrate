
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"


/*
 * Tree
 */


/*
 * Tree list
 */

cob_tree_list
make_list (cob_tree x)
{
  struct cob_tree_list *p = malloc (sizeof (struct cob_tree_list));
  p->tree = x;
  p->next = NULL;
  return p;
}

cob_tree_list
list_append (cob_tree_list l, cob_tree x)
{
  cob_tree_list p;
  for (p = l; p->next != NULL; p = p->next);
  p->next = make_list (x);
  return l;
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
  /* FIXME: error check here!! */
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
  p->type    = type;
  p->x       = x;
  p->y       = y;
  return COB_TREE (p);
}

cob_tree
make_unary_cond (cob_tree x, enum cond_type type)
{
  return make_cond (x, type, 0);
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


void
print_tree (cob_tree x, FILE *fp)
{
  if (x == spe_lit_ZE)
    fputs ("ZERO", fp);
  else if (LITERAL_P (x))
    fprintf (fp, "\"%s\"", COB_FIELD_NAME (x));
  else if (SYMBOL_P (x))
    fputs (COB_FIELD_NAME (x), fp);
  else if (SUBREF_P (x))
    {
      cob_tree_list ls;
      print_tree (SUBREF_SYM (x), fp);
      fputs ("(", fp);
      for (ls = SUBREF_SUBS (x); ls; ls = ls->next)
	{
	  print_tree (ls->tree, fp);
	  if (ls->next)
	    fputs (", ", fp);
	  else
	    fputs (")", fp);
	}
    }
  else if (COND_P (x))
    {
      cob_tree l = COND_X (x);
      cob_tree r = COND_Y (x);
      enum cond_type type = COND_TYPE (x);

      switch (type)
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
	  fprintf (fp, "(cond %d ", type);
	  print_tree (l, fp);
	  if (r)
	    {
	      fputs (" ", fp);
	      print_tree (r, fp);
	    }
	  fputs (")", fp);
	}
    }
  else
    fprintf (fp, "tree(%p)", x);
}
