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

#ifndef _COBC_H_
#define _COBC_H_

#include <stdio.h>

#define COB_PACKAGE	"OpenCOBOL"
#define COB_VERSION	VERSION
#define COB_COPYRIGHT					\
  "Copyright (C) 2001 Keisuke Nishida\n"		\
  "Copyright (C) 1999-2001 Rildo Pragana et. al.\n"

extern int cob_stabs_flag;
extern int cob_debug_flag;
extern int cob_module_flag;
extern int cob_verbose_flag;

#define LINK_STATIC	0
#define LINK_DYNAMIC	1

extern int cob_link_style;

extern int cob_trace_scanner;
extern int cob_trace_parser;
extern int cob_trace_codegen;

extern int cob_orig_lineno;
extern char *cob_orig_filename;
extern char *cob_source_filename;
extern char *cob_include_filename;

extern char HTG_COPYDIR[];

extern FILE *yyin;
extern FILE *o_src;

#endif /* _COBC_H_ */
