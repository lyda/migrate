
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"


/*
 * Tree list
 */

cob_tree_list
list_append (cob_tree_list l, cob_tree x)
{
  cob_tree_list e = malloc (sizeof (struct cob_tree_list));
  e->tree = x;
  e->next = NULL;

  if (l == NULL)
    return e;
  else
    {
      cob_tree_list p;
      for (p = l; p->next != NULL; p = p->next);
      p->next = e;
      return l;
    }
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
  return (cob_tree) p;
}


/*
 * Symbols
 */

cob_tree 
make_symbol (char *name)
{
  cob_tree sy = malloc (sizeof (struct sym));
  sy->litflag = 0;
  sy->name = name;
  sy->next = NULL;
  sy->times = 0;
  sy->type = 0;
  sy->slack = 0;
  sy->level = 0;
  sy->defined = 0;
  sy->decimals = 0;
  sy->redefines = NULL;
  sy->value = NULL;
  sy->sort_data = NULL;
  sy->linkage_flg = 0;
  sy->scr = NULL;
  sy->clone = NULL;
  sy->parent = NULL;
  sy->son = NULL;
  sy->brother = NULL;
  sy->occurs = NULL;
  sy->flags.is_pointer = 0;
  sy->flags.just_r = 0;
  sy->flags.separate_sign = 0;
  sy->flags.leading_sign = 0;
  sy->flags.blank = 0;
  sy->flags.sync = 0;
  return sy;
}

cob_tree 
make_filler (void)
{
  static int filler_num = 1;

  char s[15];
  cob_tree sy;
  sprintf (s, "FIL$%05d", filler_num++);
  sy = install (s, SYTB_VAR, 0);
  sy->defined = 1;
  return sy;
}


/*
 * Expressions
 */

cob_tree
make_expr (cob_tree left, char op, cob_tree right)
{
  struct expr *e = malloc (sizeof (struct expr));
  e->litflag = 5;
  e->op = op;
  e->left = left;
  e->right = right;
  return (cob_tree) e;
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
  return (cob_tree) p;
}
