/*
 * Copyright (C) 2001-2006 Keisuke Nishida
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
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	USE_LIBDL

#include <dlfcn.h>
#define lt_dlopen(x)	dlopen(x, RTLD_LAZY | RTLD_GLOBAL)
#define lt_dlsym(x, y)	dlsym(x, y)
#define lt_dlclose(x)	dlclose(x)
#define lt_dlerror()	dlerror()
#define lt_ptr_t	void *
#define lt_dlhandle	void *

#elif	defined(_WIN32)

#include <windows.h>
/* Prototype */
static char *	lt_dlerror (void);

static HMODULE
lt_dlopen (const char * x)
{
	if(x == NULL)
		return GetModuleHandle(NULL);
	return LoadLibrary(x);
}
#define lt_dlsym(x, y)	GetProcAddress(x, y)
#define lt_dlclose(x)	FreeLibrary(x)
static char errbuf[64];
static char *
lt_dlerror()
{
	sprintf(errbuf, "LoadLibrary/GetProcAddress error %d", (int)GetLastError());
	return errbuf;
}
#define lt_ptr_t	void *
#define	lt_dlinit()
#define lt_dlhandle	HMODULE

#else

#define LT_NON_POSIX_NAMESPACE 1
#include <ltdl.h>

#endif

#include "call.h"
#include "common.h"
#include "lib/gettext.h"

#ifdef	_MSC_VER
#define PATHSEPC ';'
#define PATHSEPS ";"
#else
#define PATHSEPC ':'
#define PATHSEPS ":"
#endif

static int		dynamic_reloading = 0;

static int		resolve_size = 0;
static char		**resolve_path = NULL;
static char		*resolve_error = NULL;
static char		*resolve_error_buff = NULL;
static lt_dlhandle	mainhandle = NULL;

/*
 * Call table
 */

#define HASH_SIZE	131

struct call_hash {
	const char		*name;
	const char		*path;
	lt_ptr_t		func;
	lt_dlhandle		handle;
	time_t			mtime;
	struct call_hash	*next;
};

static struct call_hash **call_table;

static void
cob_set_library_path (const char *path)
{
	int		i;
	char		*p;

	/* clear the previous path */
	if (resolve_path) {
		free (resolve_path[0]);
		free (resolve_path);
	}

	/* count the number of ':'s */
	resolve_size = 1;
	for (p = strchr (path, PATHSEPC); p; p = strchr (p + 1, PATHSEPC)) {
		resolve_size++;
	}

	/* build path array */
	p = strdup (path);
	resolve_path = cob_malloc (sizeof (char *) * resolve_size);
	resolve_path[0] = strtok (p, PATHSEPS);
	for (i = 1; i < resolve_size; i++) {
		resolve_path[i] = strtok (NULL, PATHSEPS);
	}
}

void
cob_init_call (void)
{
	char		*s;
	char		*p;
	int		i;
	struct stat	st;
	char		filename[COB_MEDIUM_BUFF];

#ifndef	USE_LIBDL
	lt_dlinit ();
#endif

	/* big enough for anything from libdl/libltdl */
	resolve_error_buff = cob_malloc (256);

	call_table = (struct call_hash **)cob_malloc (sizeof (struct call_hash *) * HASH_SIZE);

	s = getenv ("COB_LIBRARY_PATH");
	if (s == NULL) {
		s = COB_LIBRARY_PATH;
	}
	cob_set_library_path (s);

	mainhandle = lt_dlopen (NULL);

	s = getenv ("COB_DYNAMIC_RELOADING");
	if (s != NULL && strcmp (s, "yes") == 0) {
		dynamic_reloading = 1;
	}

	s = getenv ("COB_PRE_LOAD");
	if (s != NULL) {
		p = strdup (s);
		s = strtok (p, ":");
		for ( ; s; s = strtok (NULL, ":")) {
			for (i = 0; i < resolve_size; i++) {
				sprintf (filename, "%s/%s.%s", resolve_path[i], s, COB_MODULE_EXT);
				if (stat (filename, &st) == 0) {
					if (lt_dlopen (filename) != NULL) {
						break;
					}
				}
			}
		}
	}
}

static inline int
hash (const char *s)
{
	int		val = 0;

	while (*s) {
		val += *s++;
	}
	return val % HASH_SIZE;
}

static void
insert (const char *name, const char *path, lt_dlhandle handle, lt_ptr_t func, time_t mtime)
{
	int			val = hash (name);
	struct call_hash	*p = cob_malloc (sizeof (struct call_hash));

	p->name = strdup (name);
	p->path = path ? strdup (path) : NULL;
	p->func = func;
	p->handle = handle;
	p->mtime = mtime;
	p->next = call_table[val];
	call_table[val] = p;
}

static void
drop (const char *name)
{
	struct call_hash	**pp;

	for (pp = &call_table[hash (name)]; *pp; pp = &(*pp)->next) {
		if (strcmp (name, (*pp)->name) == 0) {
			struct call_hash *p = *pp;

			lt_dlclose (p->handle);
			*pp = p->next;
			free ((char *)p->name);
			if (p->path) {
				free ((char *)p->path);
			}
			free (p);
			return;
		}
	}
}

static void *
lookup (const char *name)
{
	struct stat		st;
	struct call_hash	*p;

	for (p = call_table[hash (name)]; p; p = p->next) {
		if (strcmp (name, p->name) == 0) {
			if (dynamic_reloading == 0 || !p->path) {
				return p->func;
			}
			if (stat (p->path, &st) == 0 && p->mtime == st.st_mtime) {
				return p->func;
			}
			drop (name);
			break;
		}
	}
	return NULL;
}

/*
 * C interface
 */

void *
cob_resolve_1 (const char *name)
{
	void	*p;

	p = cob_resolve (name);
	if (!p) {
		cob_call_error ();
	}
	return p;
}

void *
cob_call_resolve_1 (cob_field * f)
{
	void	*p;

	p = cob_call_resolve (f);
	if (!p) {
		cob_call_error ();
	}
	return p;
}

void *
cob_resolve (const char *name)
{
	int		i;
	char		*p;
	const char	*s;
	lt_ptr_t	func;
	lt_dlhandle	handle;
	struct stat	st;
	char		buff[COB_SMALL_BUFF];
	char		filename[COB_MEDIUM_BUFF];

/* Checked in generated code
	if (!cob_initialized) {
		fputs (_("cob_init() must be called before cob_resolve()"), stderr);
		cob_stop_run (1);
	}
*/

	/* search the cache */
	func = lookup (name);
	if (func) {
		return func;
	}

	/* encode program name */
	p = buff;
	s = name;
	if (isdigit (*s)) {
		p += sprintf (p, "$%02X", *s++);
	}
	for (; *s; s++) {
		if (isalnum (*s) || *s == '_') {
			*p++ = *s;
		} else {
			p += sprintf (p, "$%02X", *s);
		}
	}
	*p = 0;

	/* search the main program */
	if (mainhandle != NULL && (func = lt_dlsym (mainhandle, buff)) != NULL) {
		insert (name, NULL, mainhandle, func, 0);
		resolve_error = NULL;
		return func;
	}

	/* search external modules */
	for (i = 0; i < resolve_size; i++) {

		sprintf (filename, "%s/%s.%s", resolve_path[i], name, COB_MODULE_EXT);
		if (stat (filename, &st) == 0) {
			if ((handle = lt_dlopen (filename)) != NULL
			    && (func = lt_dlsym (handle, buff)) != NULL) {
				insert (name, filename, handle, func, st.st_mtime);
				resolve_error = NULL;
				return func;
			}
			strcpy (resolve_error_buff, lt_dlerror ());
			resolve_error = resolve_error_buff;
			return NULL;
		}
	}
#if	defined(USE_LIBDL) && defined (RTLD_DEFAULT)
	if ((func = lt_dlsym (RTLD_DEFAULT, buff)) != NULL) {
		insert (name, NULL, NULL, func, 0);
		resolve_error = NULL;
		return func;
	}
#endif
	sprintf (resolve_error_buff, _("cannot find module '%s'"), name);
	resolve_error = resolve_error_buff;
	return NULL;
}

const char *
cob_resolve_error (void)
{
	const char	*p = resolve_error;

	resolve_error = NULL;
	return p;
}

/*
 * COBOL interface
 */

static void *
cob_get_buff (size_t buffsize)
{
	static int	lastsize = 0;
	static void	*buffer = NULL;

	if (!buffer) {
		if (buffsize <= COB_SMALL_BUFF) {
			buffer = cob_malloc (COB_SMALL_BUFF);
			lastsize = COB_SMALL_BUFF;
		} else {
			buffer = cob_malloc (buffsize);
			lastsize = buffsize;
		}
	} else {
		if (buffsize > lastsize) {
			free (buffer);
			buffer = cob_malloc (buffsize);
			lastsize = buffsize;
		}
	}
	return buffer;
}

void *
cob_call_resolve (cob_field * f)
{
	char	*buff;

	buff = cob_get_buff (f->size + 1);
	return cob_resolve (cob_field_to_string (f, buff));
}

void
cob_call_error (void)
{
	const char	*s;

	s = cob_resolve_error ();
	if ( s ) {
		cob_runtime_error ("%s", s);
	} else {
		cob_runtime_error ("%s", "Unknown error");
	}
	cob_stop_run (1);
}

void
cob_cancel (cob_field * f)
{
	char	*buff;

	buff = cob_get_buff (f->size + 1);
	drop (cob_field_to_string (f, buff));
}
