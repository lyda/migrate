/*
 * Copyright (C) 2002-2005 Keisuke Nishida
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

#ifndef COB_MOVE_H
#define COB_MOVE_H

#include <libcob/common.h>

extern void		cob_move (cob_field *src, cob_field *dst);
extern void		cob_memcpy (cob_field *dst, unsigned char *src, int size);
extern void		cob_set_int (cob_field *f, int n);
extern int		cob_get_int (cob_field *f);
extern int		cob_binary_get_int (cob_field *f);
extern long long	cob_binary_get_int64 (cob_field *f);
extern void		cob_binary_set_int (cob_field *f, int n);
extern void		cob_binary_set_int64 (cob_field *f, long long n);

#endif /* COB_MOVE_H_ */
