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
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
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
lt_dlopen (const char *x)
{
	if (x == NULL) {
		return GetModuleHandle (NULL);
	}
	return LoadLibrary(x);
}
#define lt_dlsym(x, y)	GetProcAddress(x, y)
#define lt_dlclose(x)	FreeLibrary(x)
static char errbuf[64];
static char *
lt_dlerror ()
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
#include "fileio.h"
#include "lib/gettext.h"

#ifdef	_MSC_VER
#define PATHSEPC ';'
#define PATHSEPS ";"
#else
#define PATHSEPC ':'
#define PATHSEPS ":"
#endif

static int		resolve_size = 0;
static char		**resolve_path = NULL;
static char		*resolve_error = NULL;
static char		*resolve_error_buff = NULL;
static lt_dlhandle	mainhandle = NULL;

#ifdef	_WIN32
struct struct_handle {
	struct struct_handle	*next;
	struct struct_handle	*prev;
	lt_dlhandle		preload_handle;
};

static struct struct_handle	*pre_handle = NULL;
#endif

/*
 * Call table
 */

#define HASH_SIZE	131

struct call_hash {
	struct call_hash	*next;
	struct call_hash	*prev;
	const char		*name;
	lt_ptr_t		func;
	lt_dlhandle		handle;
};

#ifdef	COB_ALT_HASH
static struct call_hash *call_table = NULL;
#else
static struct call_hash **call_table;
#endif

struct system_table {
	const char		*syst_name;
	const lt_ptr_t		syst_call;
};

static const struct system_table	system_tab[] = {
#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(x, y, z)	{ x, z },
#include "system.def"
	{ NULL, NULL }
};

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
	p = cob_strdup (path);
	resolve_path = cob_malloc (sizeof (char *) * resolve_size);
	resolve_path[0] = strtok (p, PATHSEPS);
	for (i = 1; i < resolve_size; i++) {
		resolve_path[i] = strtok (NULL, PATHSEPS);
	}
}

#ifndef	COB_ALT_HASH
static inline size_t
hash (const unsigned char *s)
{
	size_t		val = 0;

	while (*s) {
		val += *s++;
	}
	return val % HASH_SIZE;
}
#endif

static void
insert (const char *name, lt_dlhandle handle, lt_ptr_t func)
{
#ifndef	COB_ALT_HASH
	int			val = hash ((unsigned char *)name);
#endif
	struct call_hash	*p = cob_malloc (sizeof (struct call_hash));

	p->name = cob_strdup (name);
	p->func = func;
	p->handle = handle;
#ifdef	COB_ALT_HASH
	p->next = call_table;
	if (call_table) {
		call_table->prev = p;
	}
	call_table = p;
#else
	p->next = call_table[val];
	call_table[val] = p;
#endif
}

static void
drop (const char *name)
{
#ifdef	COB_ALT_HASH
	struct call_hash	*p;

	for (p = call_table; p; p = p->next) {
		if (strcmp (name, p->name) == 0) {
			lt_dlclose (p->handle);
			if (p->prev) {
				p->prev->next = p->next;
			} else {
				call_table = p->next;
			}
			free ((char *)p->name);
			free (p);
			return;
		}
	}
#else
	struct call_hash	**pp;

	for (pp = &call_table[hash ((unsigned char *)name)]; *pp; pp = &(*pp)->next) {
		if (strcmp (name, (*pp)->name) == 0) {
			struct call_hash *p = *pp;

			lt_dlclose (p->handle);
			*pp = p->next;
			free ((char *)p->name);
			free (p);
			return;
		}
	}
#endif
}

static void *
lookup (const char *name)
{
	struct call_hash	*p;

#ifdef	COB_ALT_HASH
	for (p = call_table; p; p = p->next) {
#else
	for (p = call_table[hash ((unsigned char *)name)]; p; p = p->next) {
#endif
		if (strcmp (name, p->name) == 0) {
			return p->func;
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
	if (unlikely(!p)) {
		cob_call_error ();
	}
	return p;
}

void *
cob_call_resolve_1 (cob_field *f)
{
	void	*p;

	p = cob_call_resolve (f);
	if (unlikely(!p)) {
		cob_call_error ();
	}
	return p;
}

void *
cob_resolve (const char *name)
{
	int			i;
	unsigned char		*p;
	const unsigned char	*s;
	lt_ptr_t		func;
	lt_dlhandle		handle;
	struct stat		st;
#ifdef	_WIN32
	struct struct_handle	*chkhandle;
#endif
	unsigned char		buff[COB_SMALL_BUFF];
	char			filename[COB_MEDIUM_BUFF];

/* Checked in generated code
	if (!cob_initialized) {
		fputs ("cob_init() must be called before cob_resolve()", stderr);
		cob_stop_run (1);
	}
*/

	/* search the cache */
	cob_exception_code = 0;
	func = lookup (name);
	if (func) {
		return func;
	}

	/* encode program name */
	p = buff;
	s = (unsigned char *)name;
	if (unlikely(isdigit (*s))) {
		p += sprintf ((char *)p, "_%02X", *s++);
	}
	for (; *s; s++) {
		if (likely(isalnum (*s) || *s == '_')) {
			*p++ = *s;
		} else {
			p += sprintf ((char *)p, "_%02X", *s);
		}
	}
	*p = 0;

	/* search the main program */
	if (mainhandle != NULL && (func = lt_dlsym (mainhandle, (char *)buff)) != NULL) {
		insert (name, mainhandle, func);
		resolve_error = NULL;
		return func;
	}

	/* Search preloaded modules */
#ifdef	_WIN32
	for (chkhandle = pre_handle; chkhandle; chkhandle = chkhandle->next) {
		if ((func = lt_dlsym (chkhandle->preload_handle, (char *)buff)) != NULL) {
			insert (name, chkhandle->preload_handle, func);
			resolve_error = NULL;
			return func;
		}
	}
#endif
#if	defined(USE_LIBDL) && defined (RTLD_DEFAULT)
	if ((func = lt_dlsym (RTLD_DEFAULT, buff)) != NULL) {
		insert (name, NULL, func);
		resolve_error = NULL;
		return func;
	}
#endif

	/* search external modules */
	for (i = 0; i < resolve_size; i++) {
		if (resolve_path[i] == NULL) {
			sprintf (filename, "%s.%s", name, COB_MODULE_EXT);
		} else {
			sprintf (filename, "%s/%s.%s", resolve_path[i], name, COB_MODULE_EXT);
		}
		if (stat (filename, &st) == 0) {
			if ((handle = lt_dlopen (filename)) != NULL
			    && (func = lt_dlsym (handle, (char *)buff)) != NULL) {
				insert (name, handle, func);
				resolve_error = NULL;
				return func;
			}
			strcpy (resolve_error_buff, lt_dlerror ());
			resolve_error = resolve_error_buff;
			COB_SET_EXCEPTION (COB_EC_PROGRAM_NOT_FOUND);
			return NULL;
		}
	}
	sprintf (resolve_error_buff, "Cannot find module '%s'", name);
	resolve_error = resolve_error_buff;
	COB_SET_EXCEPTION (COB_EC_PROGRAM_NOT_FOUND);
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
	static size_t	lastsize = 0;
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
cob_call_resolve (cob_field *f)
{
	char	*buff;

	buff = cob_get_buff (f->size + 1);
	cob_field_to_string (f, buff);
	return cob_resolve (buff);
}

void
cob_call_error (void)
{
	const char	*s;

	s = cob_resolve_error ();
	if (!s) {
		s = "Unknown error";
	}
	cob_runtime_error ("%s", s);
	cob_stop_run (1);
}

void
cob_cancel (cob_field *f)
{
	char	*buff;

	buff = cob_get_buff (f->size + 1);
	cob_field_to_string (f, buff);
	drop (buff);
}

void
cob_init_call (void)
{
	char			*s;
	char			*p;
	int			i;
	struct stat		st;
	struct system_table	*psyst;
#ifdef	_WIN32
	lt_dlhandle		libhandle;
#endif
	char			filename[COB_MEDIUM_BUFF];

#ifndef	USE_LIBDL
	lt_dlinit ();
#endif

	/* big enough for anything from libdl/libltdl */
	resolve_error_buff = cob_malloc (256);

#ifndef	COB_ALT_HASH
	call_table = (struct call_hash **)cob_malloc (sizeof (struct call_hash *) * HASH_SIZE);
#endif

	s = getenv ("COB_LIBRARY_PATH");
	if (s == NULL) {
		s = COB_LIBRARY_PATH;
	}
	cob_set_library_path (s);

	mainhandle = lt_dlopen (NULL);

	s = getenv ("COB_PRE_LOAD");
	if (s != NULL) {
		p = cob_strdup (s);
		s = strtok (p, ":");
		for (; s; s = strtok (NULL, ":")) {
			for (i = 0; i < resolve_size; i++) {
				sprintf (filename, "%s/%s.%s", resolve_path[i], s, COB_MODULE_EXT);
				if (stat (filename, &st) == 0) {
#ifdef	_WIN32
					if ((libhandle = lt_dlopen (filename)) != NULL) {
						struct struct_handle *newhandle;

						newhandle = cob_malloc (sizeof (struct struct_handle));
						newhandle->preload_handle = libhandle;
						newhandle->next = pre_handle;
						if (pre_handle) {
							pre_handle->prev = newhandle;
						}
						pre_handle = newhandle;
#else
					if (lt_dlopen (filename) != NULL) {
#endif
						break;
					}
				}
			}
		}
	}
	for (psyst = (struct system_table *)&system_tab[0]; psyst->syst_name; psyst++) {
		insert (psyst->syst_name, NULL, (lt_ptr_t)psyst->syst_call);
	}
}
