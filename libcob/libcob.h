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

#ifndef _LIBCOB_H_
#define _LIBCOB_H_

#define COB_STATUS_SUCCESS	0
#define COB_STATUS_OVERFLOW	1

extern int cob_status;

extern void cob_init (int argc, char **argv);
extern int cob_exit (void);
extern void cob_set_library_path (const char *path);
extern void *cob_resolve (const char *name);
extern const char *cob_resolve_error (void);

#endif /* _LIBCOB_H_ */
