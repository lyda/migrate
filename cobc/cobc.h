/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

#include "tree.h"
#include "lib/gettext.h"

#define COBC_PACKAGE	"cobc (" PACKAGE_NAME ")"
#define COBC_VERSION	PACKAGE_VERSION
#define COBC_COPYRIGHT	"Copyright (C) 2001-2003 Keisuke Nishida\n"

#define COBC_FORMAT_FREE		1
#define COBC_FORMAT_FIXED		2

#define COBC_DEFAULT_TAB_WIDTH		8
#define COBC_DEFAULT_TEXT_COLUMN	72

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK 1	/* These are in libmingwex.a */
#endif


/* have a main function */
extern int cobc_flag_main;

/* CALL statements are static */
extern int cobc_flag_call_static;

/* enable debugging lines */
extern int cobc_flag_debugging_line;

/* output line directives */
extern int cobc_flag_line_directive;

#undef COBC_WARNING
#define COBC_WARNING(sig,var,name,doc) extern int var;
#include "warning.def"


struct cobc_name_list {
  const char *name;
  struct cobc_name_list *next;
};

struct cobc_replacement {
  const char *old_text;
  const char *new_text;
  struct cobc_replacement *next;
};

extern int errorcount;
extern int warningcount;

extern char *cobc_source_file;
extern int cobc_source_line;
extern int cobc_source_format;
extern int cobc_tab_width;
extern int cobc_text_column;

extern FILE *cobc_depend_file;
extern char *cobc_depend_target;
extern struct cobc_name_list *cobc_depend_list;
extern struct cobc_name_list *cobc_include_list;

extern struct cobc_program *current_program;
extern struct cobc_label *current_section, *current_paragraph;

extern void yywarn (const char *fmt, ...);
extern void yyerror (const char *fmt, ...);
extern void yywarn_x (cobc_tree x, const char *fmt, ...);
extern void yyerror_x (cobc_tree x, const char *fmt, ...);

extern void redefinition_error (cobc_tree x);
extern void undefined_error (cobc_tree x);
extern void ambiguous_error (cobc_tree x);

/* preprocessor (in pplex.l, ppparse.y) */
extern FILE *ppin;
extern FILE *ppout;
extern int pplex (void);
extern int ppparse (void);
extern int ppopen (char *name, const char *lib, struct cobc_replacement *replacement);

/* parser (in scanner.l, parser.y) */
extern FILE *yyin;
extern FILE *cobc_out;
extern int yylex (void);
extern int yyparse (void);

#endif /* _COBC_H_ */
