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

#include "config.h"

#include <stdio.h>

#define COBC_PACKAGE	"cobc (" PACKAGE_NAME ")"
#define COBC_VERSION	PACKAGE_VERSION
#define COBC_COPYRIGHT	"Copyright (C) 2001-2003 Keisuke Nishida\n"

extern struct cobc_flags {
  int main;		/* a main function */
  int failsafe;		/* run-time error checking */
  int static_call;	/* static CALL statements */
  int source_location;	/* cob_source_file/line */
  int line_directive;	/* line directives */
} cobc_flags;

extern FILE *yyin;
extern FILE *cobc_out;

#endif /* _COBC_H_ */
