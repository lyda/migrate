/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#ifndef _COBC_H_
#define _COBC_H_

#include <stdio.h>

#define COBC_PACKAGE	"OpenCOBOL"
#define COBC_VERSION	VERSION
#define COBC_COPYRIGHT	"Copyright (C) 2001-2002 Keisuke Nishida\n"

#define LINK_STATIC	0
#define LINK_DYNAMIC	1

extern int cobc_module_flag;
extern int cobc_optimize_flag;
extern int cobc_link_style;

extern FILE *yyin;
extern FILE *cobc_out;

#endif /* _COBC_H_ */
