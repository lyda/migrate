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

#ifndef COB_TERMIO_H
#define COB_TERMIO_H

#include <libcob/common.h>

extern void cob_new_display (int outorerr, int newline, int varcnt, ...);
extern void cob_accept (cob_field *f);

#if 0
extern void cob_init_termio (void);
#endif

#endif /* COB_TERMIO_H_ */
