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

#define CB_FORMAT_FREE		1
#define CB_FORMAT_FIXED		2

#define CB_DEFAULT_TAB_WIDTH	8
#define CB_DEFAULT_TEXT_COLUMN	72

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK 1	/* These are in libmingwex.a */
#endif


/* COBOL standard */
extern enum cb_standard {
  CB_STANDARD_GNU,
  CB_STANDARD_COBOL85,
  CB_STANDARD_COBOL2002,
  CB_STANDARD_MVS,
} cb_standard;

extern const char *cb_standard_name;

/* Binary representation */
extern enum cb_binary_rep {
  CB_BINARY_REP_1_2_4_8,	/* 1,2,4,8 bytes */
  CB_BINARY_REP_2_4_8,		/* 2,4,8 bytes */
} cb_binary_rep;

extern struct cb_exception {
  int code;			/* exception code */
  const char *name;		/* exception name */
  int enable;			/* if turn on */
} cb_exception_table[];

#define CB_EXCEPTION_CODE(id)	cb_exception_table[id].code
#define CB_EXCEPTION_NAME(id)	cb_exception_table[id].name
#define CB_EXCEPTION_ENABLE(id)	cb_exception_table[id].enable

/* CALL statements are static */
extern int cb_flag_call_static;

/* enable debugging lines */
extern int cb_flag_debugging_line;

/* output line directives */
extern int cb_flag_line_directive;

/* exit after error check */
extern int cb_flag_parse_only;

#undef CB_FLAG
#define CB_FLAG(var,name,doc) extern int var;
#include "flag.def"

#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc) extern int var;
#include "warning.def"


struct cb_name_list {
  const char *name;
  struct cb_name_list *next;
};

struct cb_replacement {
  const char *old_text;
  const char *new_text;
  struct cb_replacement *next;
};

extern int errorcount;
extern int warningcount;

extern char *cb_source_file;
extern int cb_source_line;
extern int cb_source_format;
extern int cb_tab_width;
extern int cb_text_column;

extern FILE *cb_depend_file;
extern char *cb_depend_target;
extern struct cb_name_list *cb_depend_list;
extern struct cb_name_list *cb_include_list;

extern struct cb_program *current_program;
extern struct cb_label *current_section, *current_paragraph;

extern void cb_warning (const char *fmt, ...);
extern void cb_error (const char *fmt, ...);
extern void cb_warning_x (cb_tree x, const char *fmt, ...);
extern void cb_error_x (cb_tree x, const char *fmt, ...);
extern void cb_archaic (const char *feature);
extern void cb_obsolete_85 (const char *feature);
extern void cb_obsolete_2002 (const char *feature);

extern void redefinition_error (cb_tree x);
extern void undefined_error (cb_tree x);
extern void ambiguous_error (cb_tree x);

/* preprocessor (in pplex.l, ppparse.y) */
extern FILE *ppin;
extern FILE *ppout;
extern int pplex (void);
extern int ppparse (void);
extern int ppopen (char *name, const char *lib, struct cb_replacement *replacement);
extern struct cb_replacement *add_replacement (struct cb_replacement *replacement, const char *old_text, const char *new_text);

/* parser (in scanner.l, parser.y) */
extern FILE *yyin;
extern FILE *yyout;
extern int yylex (void);
extern int yyparse (void);

/* code generation (in codegen.c) */
extern FILE *storage_file;
extern char *storage_file_name;
extern void codegen (struct cb_program *prog);

#endif /* _COBC_H_ */
