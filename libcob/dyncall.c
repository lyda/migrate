/* Dynamic CALL library
 *
 * Copyright (C) 2001 Keisuke Nishida
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#include "_libcob.h"
#include "defaults.h"
#include <ltdl.h>


/*
 * Symbol table hash
 */

#define HASHSIZE	131

static struct sym
{
  char *name;
  lt_ptr func;
  struct sym *next;
} *symtab[HASHSIZE];

static int
hash (const char *s)
{
  int i = 0;
  while (*s)
    i += *s++;
  return i % HASHSIZE;
}

static struct sym *
lookup (const char *name)
{
  struct sym *sym;
  for (sym = symtab[hash (name)]; sym; sym = sym->next)
    if (strcmp (name, sym->name) == 0)
      return sym;
  return NULL;
}

static void
insert (const char *name, lt_ptr func)
{
  if (!lookup (name))
    {
      int idx = hash (name);
      struct sym *sym = malloc (sizeof (struct sym));
      sym->name = strdup (name);
      sym->func = func;
      sym->next = symtab[idx];
      symtab[idx] = sym;
    }
}


/*
 * Library functions
 */

static char subrname[256];

void *
cob_resolve (const char *name)
{
  char *path;
  struct sym *sym;
  lt_dlhandle handle;

  strcpy (subrname, name);

  /* check if we've seen it before */
  sym = lookup (subrname);
  if (sym)
    return sym->func;

  path = getenv ("COB_LIBRARY_PATH");
  if (!path)
    path = COB_LIBRARY_PATH;

  if (lt_dlinit () != 0 || lt_dlsetsearchpath (path) != 0)
    {
      fprintf (stderr, "cob_resolve: %s\n", lt_dlerror ());
      return NULL;
    }

  handle = lt_dlopenext (subrname);
  if (handle)
    {
      lt_ptr func = lt_dlsym (handle, subrname);
      if (func)
	{
	  insert (subrname, func);
	  return func;
	}
    }
  return NULL;
}

void *
cob_resolve_subr (struct fld_desc *f, char *s)
{
  char name[256];

  /* get subroutine name */
  strncpy (name, s, f->len);
  name[f->len] = '\0';

  /* remove unnecessary spaces */
  s = strchr (name, ' ');
  if (s)
    *s = '\0';

  return cob_resolve (name);
}

void
resolve_subr_error ()
{
  fprintf (stderr, "libcob: dynamic library call \"%s\" not found\n",
	   subrname);
}
