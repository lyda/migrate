/*
 * Copyright (C) 2002-2006 Keisuke Nishida
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

#ifndef COB_CALL_H
#define COB_CALL_H

#include <libcob/common.h>

extern void cob_init_call (void);
extern void *cob_resolve (const char *name);
extern void *cob_resolve_1 (const char *name);
extern const char *cob_resolve_error (void);

extern void *cob_call_resolve (cob_field *f);
extern void *cob_call_resolve_1 (cob_field *f);
#ifdef __GNUC__
extern void cob_call_error (void) __attribute__ ((noreturn));
#else
extern void cob_call_error (void);
#endif
extern void cob_cancel (cob_field *f);

#endif /* COB_CALL_H_ */
