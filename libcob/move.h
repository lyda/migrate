/*
 * Copyright (C) 2002 Keisuke Nishida
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

extern void cob_move (struct cob_field src, struct cob_field dst);
extern void cob_move_alphanum_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_alphanum (struct cob_field f1, struct cob_field f2);
extern void cob_move_alphanum_to_alphanum (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_packed (struct cob_field f1, struct cob_field f2);
extern void cob_move_packed_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_binary (struct cob_field f1, struct cob_field f2);
extern void cob_move_binary_to_display (struct cob_field f1, struct cob_field f2);
extern void cob_move_display_to_edited (struct cob_field f1, struct cob_field f2);
extern void cob_move_alphanum_to_edited (struct cob_field f1, struct cob_field f2);
extern void cob_mem_move (struct cob_field dst, unsigned char *src, int len);
extern int cob_to_int (struct cob_field f);
extern void cob_set_int (struct cob_field f, int n);

#endif /* COB_MOVE_H_ */
