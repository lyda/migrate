
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
  p->litflag = 1;
  p->name = name;
  p->type = 0;
  p->all = 0;
  p->nick = NULL;
  p->len = strlen (p->name);
  p->next = NULL;
  return COB_TREE (p);
}


/*
 * Symbols
 */

cob_tree 
make_symbol (char *name)
{
  struct sym *p = malloc (sizeof (struct sym));
  p->litflag = 0;
  p->name = name;
  p->next = NULL;
  p->times = 0;
  p->type = 0;
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
  p->litflag = 2;
  p->sym     = sy;
  p->subs    = subs;
  /* FIXME: error check here!! */
  return COB_TREE (p);
}


/*
 * refmod
 */

cob_tree
create_refmoded_var (cob_tree sy, cob_tree syoff, cob_tree sylen)
{
  struct refmod *ref;
  ref = malloc (sizeof (struct refmod));
  ref->litflag = 4;
  ref->sym = sy;
  ref->off = syoff;
  ref->len = sylen;
  ref->slot = refmod_slots++;
  return COB_TREE (ref);
}


/*
 * Expression
 */

cob_tree
make_expr (cob_tree left, char op, cob_tree right)
{
  struct expr *p = malloc (sizeof (struct expr));
  p->litflag = 5;
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
  p->litflag = 8;
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


void
print_tree (cob_tree x)
{
  if (x == spe_lit_ZE)
    printf ("ZERO");
  else if (SYMBOL_P (x) || LITERAL_P (x))
    printf (FIELD_NAME (x));
  else if (SUBREF_P (x))
    {
      cob_tree_list ls;
      print_tree (SUBREF_SYM (x));
      printf ("(");
      for (ls = SUBREF_SUBS (x); ls; ls = ls->next)
	{
	  print_tree (ls->tree);
	  if (ls->next)
	    printf (", ");
	  else
	    printf (")");
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
	  printf ("(= ");
	  print_tree (l);
	  printf (" ");
	  print_tree (r);
	  printf (")");
	  break;

	case COND_AND:
	  printf ("(and ");
	  print_tree (l);
	  printf (" ");
	  print_tree (r);
	  printf (")");
	  break;

	default:
	  printf ("(cond %d ", type);
	  print_tree (l);
	  if (r)
	    {
	      printf (" ");
	      print_tree (r);
	    }
	  printf (")");
	}
    }
  else
    printf ("litflag(%d)", x->litflag);
}
