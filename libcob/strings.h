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

#ifndef COB_STRINGS_H
#define COB_STRINGS_H

#include <libcob/common.h>

#define COB_INSPECT_END			0
#define COB_INSPECT_INIT	     	1
#define COB_INSPECT_BEFORE      	2
#define COB_INSPECT_AFTER		3
#define COB_INSPECT_TALLYING     	4
#define COB_INSPECT_REPLACING     	5
#define COB_INSPECT_CONVERTING     	6
#define COB_INSPECT_CHARACTERS		7
#define COB_INSPECT_ALL			8
#define COB_INSPECT_LEADING		9
#define COB_INSPECT_FIRST	      	10

#define COB_STRING_END			0
#define COB_STRING_CONCATENATE		1
#define COB_STRING_DELIMITED_NAME	2
#define COB_STRING_DELIMITED_SIZE	3
#define COB_STRING_WITH_POINTER		4

#define COB_UNSTRING_END		0
#define COB_UNSTRING_INTO		1
#define COB_UNSTRING_DELIMITER		2
#define COB_UNSTRING_COUNT		3
#define COB_UNSTRING_DELIMITED_BY	4
#define COB_UNSTRING_DELIMITED_ALL	5
#define COB_UNSTRING_WITH_POINTER	6
#define COB_UNSTRING_TALLYING		7

extern void cob_inspect (struct cob_field var, ...);
extern void cob_string (struct cob_field dst, ...);
extern void cob_unstring (struct cob_field src, ...);

#endif /* COB_STRINGS_H_ */
