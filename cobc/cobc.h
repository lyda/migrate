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

#define COBC_PACKAGE	"cobc (OpenCOBOL)"
#define COBC_VERSION	VERSION
#define COBC_COPYRIGHT	"Copyright (C) 2001-2002 Keisuke Nishida\n"

extern struct cobc_flags {
  int main;		/* 1 generates a main function */
  int failsafe;		/* 1 generates run-time error checking */
  int static_call;	/* 1 generates static CALL statements */
  int source_location;	/* 1 generates cob_source_file/line */
  int line_directive;	/* 1 generates line directives */
} cobc_flags;

extern char *cobc_index_func;
extern char *cobc_index_depending_func;

extern FILE *yyin;
extern FILE *cobc_out;

#endif /* _COBC_H_ */
