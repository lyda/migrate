/*
 * Copyright (C) 2001  Keisuke Nishida
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

#ifndef _SCANNER_H_
#define _SCANNER_H_

#include "tree.h"

#define START_ID 	1
#define START_COMMENT	2

#define CDIV_INITIAL 	1
#define CDIV_ENVIR 	2
#define CDIV_DATA 	3

struct cob_picture {
  unsigned char *str;		/* picture string */
  char type;			/* field type */
  int size;			/* byte size */
  int decimals;			/* the number of decimal digits */
};

extern cob_tree curr_file;
extern int curr_division;
extern int start_condition;
extern int need_subscripts;
extern int need_separator;
extern int in_procedure;
extern int last_lineno;
extern char last_text[];

#endif /* _SCANNER_H_ */
