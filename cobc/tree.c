
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"

/*
 * Subscript references
 */

static int
check_subscripts (struct sym *subs)
{
  struct subref *ref;
  struct sym *sy;
  sy = SUBREF_SYM (subs);
  for (ref = (struct subref *) subs; ref; ref = ref->next)
    {
      if (ref->litflag == ',')
	{
	  while (sy && !sy->occurs_flg)
	    sy = sy->parent;
	  if (!sy)
	    {
	      yyerror ("check_subscripts: no parent found");
	      return 0;		/* excess subscripts, error */
	    }
	  sy = sy->parent;
	}
    }
  while (sy && !sy->occurs_flg)	/* any other subscripts needed ? */
    sy = sy->parent;
  return (sy == NULL) ? 1 : 0;
}

struct subref *
make_subref (struct sym *sy, struct subref *next)
{
  struct subref *ref = malloc (sizeof (struct subref));
  ref->litflag = 2;
  ref->sym     = sy;
  ref->next    = next;
  check_subscripts ((struct sym *) ref);
  return ref;
}

struct subref *
create_subscript (struct sym *sy)
{
  struct subref *ref;
  ref = malloc (sizeof (struct subref));
  ref->litflag = ',';		/* the end of subscript is here */
  ref->sym = sy;		/* this is the actual variable */
  ref->next = NULL;
  return ref;
}

struct subref *
add_subscript_item (struct subref *subs, char op, struct sym *item)
{
  struct subref *p = subs;
  while (p->next)
    p = p->next;
  p->litflag = op;
  p->next = create_subscript (item);
  return subs;
}

struct subref *
add_subscript (struct subref *ref, struct subref *subs)
{
  struct subref *p = subs;
  while (p->next)
    p = p->next;
  p->next = ref;
  return subs;
}


/*
 * Pair
 */

struct pair *
cons (void *car, struct pair *cdr)
{
  struct pair *p = malloc (sizeof (struct pair));
  p->car = car;
  p->cdr = cdr;
  return p;
}
