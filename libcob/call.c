/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ltdl.h>

#include "libcob.h"
#include "defaults.h"

static char resolve_error_buff[FILENAME_MAX];
static char *resolve_error = NULL;
static int resolve_size = 0;
static char **resolve_path = NULL;

void
cob_init_call (void)
{
  char *p = getenv ("COB_LIBRARY_PATH");
  if (!p)
    p = COB_LIBRARY_PATH;

  lt_dlinit ();
  cob_set_library_path (p);
}


/*
 * Call table
 */

#define HASH_SIZE	131

static struct call_hash
{
  const char *name;
  const char *path;
  lt_ptr func;
  lt_dlhandle handle;
  time_t mtime;
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
insert (const char *name, const char *path, lt_dlhandle handle, lt_ptr func, time_t mtime)
{
  int val = hash (name);
  struct call_hash *p = malloc (sizeof (struct call_hash));
  p->name = strdup (name);
  p->path = strdup (path);
  p->func = func;
  p->handle = handle;
  p->mtime = mtime;
  p->next = call_table[val];
  call_table[val] = p;
}

static void
drop (const char *name)
{
  struct call_hash **pp;
  for (pp = &call_table[hash (name)]; *pp; pp = &(*pp)->next)
    if (strcmp (name, (*pp)->name) == 0)
      {
	struct call_hash *p = *pp;
	lt_dlclose (p->handle);
	*pp = p->next;
	free (p);
	return;
      }
}

static void *
lookup (const char *name)
{
  struct stat st;
  struct call_hash *p;
  for (p = call_table[hash (name)]; p; p = p->next)
    if (strcmp (name, p->name) == 0)
      {
	if (stat (p->path, &st) == 0 && p->mtime == st.st_mtime)
	  return p->func;
	drop (name);
	return NULL;
      }
  return NULL;
}


/*
 * C interface
 */

void
cob_set_library_path (const char *path)
{
  int i;
  char *p;

  /* clear the previous path */
  if (resolve_path)
    {
      free (resolve_path[0]);
      free (resolve_path);
    }

  /* count the number of ':'s */
  resolve_size = 1;
  for (p = strchr (path, ':'); p; p = strchr (p + 1, ':'))
    resolve_size++;

  /* build path array */
  p = strdup (path);
  resolve_path = malloc (sizeof (char *) * resolve_size);
  resolve_path[0] = strtok (p, ":");
  for (i = 1; i < resolve_size; i++)
    resolve_path[i] = strtok (NULL, ":");
}

void *
cob_resolve (const char *name)
{
  int i;
  lt_ptr func;
  lt_dlhandle handle;

  /* search from the cache */
  func = lookup (name);
  if (func)
    return func;

  /* search module */
  for (i = 0; i < resolve_size; i++)
    {
      struct stat st;
      char filename[FILENAME_MAX];

      sprintf (filename, "%s/%s.so", resolve_path[i], name);
      if (stat (filename, &st) == 0)
	{
	  if ((handle = lt_dlopen (filename)) != NULL
	      && (func = lt_dlsym (handle, name)) != NULL)
	    {
	      insert (name, filename, handle, func, st.st_mtime);
	      resolve_error = NULL;
	      return func;
	    }
	  strcpy (resolve_error_buff, lt_dlerror ());
	  resolve_error = resolve_error_buff;
	  return NULL;
	}
    }
  sprintf (resolve_error_buff, "cannot find module `%s'", name);
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

void *
cob_call_resolve (struct cob_field f)
{
  char buff[FILENAME_MAX];
  void (*func) () = cob_resolve (cob_field_to_string (f, buff));
  if (func)
    cob_status = COB_STATUS_SUCCESS;
  else
    cob_status = COB_STATUS_OVERFLOW;
  return func;
}

void
cob_call_error (void)
{
  cob_runtime_error ("%s", cob_resolve_error ());
}

void
cob_cancel (struct cob_field f)
{
  char buff[FILENAME_MAX];
  return drop (cob_field_to_string (f, buff));
}
