/*
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>

#include "_libcob.h"
#include "defaults.h"


/*
 * Call table
 */

#define HASH_SIZE	131

static struct call_hash
{
  const char *name;
  void *func;
  void *handle;
  struct call_hash *next;
} *call_table[HASH_SIZE];

static int
hash (const char *s)
{
  int val = 0;
  while (*s)
    val += *s++;
  return val % HASH_SIZE;
}

static void
insert (const char *name, void *handle, void *func)
{
  int val = hash (name);
  struct call_hash *p = malloc (sizeof (struct call_hash));
  p->name = strdup (name);
  p->func = func;
  p->handle = handle;
  p->next = call_table[val];
  call_table[val] = p;
}

static void *
lookup (const char *name)
{
  struct call_hash *p;
  for (p = call_table[hash (name)]; p; p = p->next)
    if (strcmp (name, p->name) == 0)
      return p->func;
  return NULL;
}

static void
drop (const char *name)
{
  struct call_hash **pp;
  for (pp = &call_table[hash (name)]; *pp; pp = &(*pp)->next)
    if (strcmp (name, (*pp)->name) == 0)
      {
	struct call_hash *p = *pp;
	dlclose (p->handle);
	*pp = p->next;
	free (p);
	return;
      }
}


/*
 * C interface
 */

static char resolve_error_buff[FILENAME_MAX];
static char *resolve_error = NULL;

static char *path_str = NULL;

void
cob_set_library_path (const char *path)
{
  if (path_str)
    free (path_str);
  path_str = strdup (path);
}

void *
cob_resolve (const char *name)
{
  int i;
  void *func, *handle;
  static int size = 0;
  static char **path = NULL;

  /* Search from cache */
  func = lookup (name);
  if (func)
    return func;

  /* Build search path at the first time */
  if (!path)
    {
      char *p;
      if (!path_str)
	path_str = getenv ("COB_LIBRARY_PATH");
      if (!path_str)
	path_str = COB_LIBRARY_PATH;
      path_str = strdup (path_str);

      /* count the number of ':'s */
      size = 1;
      for (p = strchr (path_str, ':'); p; p = strchr (p + 1, ':'))
	size++;

      path = malloc (sizeof (char *) * size);
      path[0] = strtok (path_str, ":");
      for (i = 1; i < size; i++)
	path[i] = strtok (NULL, ":");
    }

  /* Search module */
  for (i = 0; i < size; i++)
    {
      struct stat st;
      char filename[FILENAME_MAX];
      sprintf (filename, "%s/%s.so", path[i], name);
      if (stat (filename, &st) == 0)
	{
	  if ((handle = dlopen (filename, RTLD_LAZY)) != NULL
	      && (func = dlsym (handle, name)) != NULL)
	    {
	      insert (name, handle, func);
	      resolve_error = NULL;
	      return func;
	    }
	  strcpy (resolve_error_buff, dlerror ());
	  resolve_error = resolve_error_buff;
	  return NULL;
	}
    }
  sprintf (resolve_error_buff, "cannot find module: %s", name);
  resolve_error = resolve_error_buff;
  return NULL;
}

const char *
cob_resolve_error (void)
{
  const char *p = resolve_error;
  resolve_error = NULL;
  return p;
}


/*
 * COBOL interface
 */

static char *
subrname (struct fld_desc *f, char *s)
{
  int i;
  static char buff[FILENAME_MAX];
  for (i = 0; i < f->len; i++)
    if (s[i] == ' ')
      break;
    else
      buff[i] = s[i];
  buff[i] = '\0';
  return buff;
}

void *
cob_dyncall_resolve (struct fld_desc *f, char *s)
{
  return cob_resolve (subrname (f, s));
}

void
cob_dyncall_error (void)
{
  fprintf (stderr, "%s\n", cob_resolve_error ());
}

void
cob_cancel (struct fld_desc *f, char *s)
{
  return drop (subrname (f, s));
}
