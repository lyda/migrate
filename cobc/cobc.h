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

#ifndef CB_COBC_H
#define CB_COBC_H

#include "config.h"

#include <stdio.h>

#include "lib/gettext.h"

#define CB_FORMAT_FREE		1
#define CB_FORMAT_FIXED		2

#define CB_DEFAULT_TAB_WIDTH	8
#define CB_DEFAULT_TEXT_COLUMN	72

#define CB_MAX_LITERAL_SIZE	1024
#define CB_MAX_CNAME		8096


/* Compile level */
extern enum cb_compile_level {
  CB_LEVEL_PREPROCESS,
  CB_LEVEL_PARSE,
  CB_LEVEL_TRANSLATE,
  CB_LEVEL_COMPILE,
  CB_LEVEL_ASSEMBLE,
  CB_LEVEL_MODULE,
  CB_LEVEL_EXECUTABLE
} cb_compile_level;

/* Compile target */
extern enum cb_compile_target {
  CB_TARGET_NATIVE,
  CB_TARGET_CLASS,
} cb_compile_target;

extern struct cb_exception {
  int code;			/* exception code */
  const char *name;		/* exception name */
  int enable;			/* if turn on */
} cb_exception_table[];

#define CB_EXCEPTION_CODE(id)	cb_exception_table[id].code
#define CB_EXCEPTION_NAME(id)	cb_exception_table[id].name
#define CB_EXCEPTION_ENABLE(id)	cb_exception_table[id].enable

#undef CB_FLAG
#define CB_FLAG(var,name,doc) extern int var;
#include "flag.def"

#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc) extern int var;
#include "warning.def"


/* spec */

extern char cob_specs_dir[];

enum cb_support {
  CB_OK,
  CB_ARCHAIC,
  CB_OBSOLETE,
  CB_UNCONFORMABLE,
};

extern struct cb_spec {
  const char *name;
  enum {
    CB_BINARY_REP_1_2_4_8,	/* 1,2,4,8 bytes */
    CB_BINARY_REP_2_4_8,	/* 2,4,8 bytes */
  } binary_rep;
  int flag_value_error;
  int flag_redefines_occurs;
  enum cb_support memory_size;
  enum cb_support multiple_file_tape;
  enum cb_support label_records;
  enum cb_support value_of;
  enum cb_support data_records;
  enum cb_support alter;
  enum cb_support goto_without_name;
  enum cb_support stop_literal;
  enum cb_support author_paragraph;
  enum cb_support debugging_mode;
  enum cb_support padding_character;
  enum cb_support next_sentence;
} cb_spec;

extern int cb_load_std (const char *name);
extern int cb_load_spec (const char *fname);


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

extern FILE *cb_storage_file;
extern char *cb_storage_file_name;

extern FILE *cb_depend_file;
extern char *cb_depend_target;
extern struct cb_name_list *cb_depend_list;
extern struct cb_name_list *cb_include_list;
extern struct cb_name_list *cb_extension_list;

extern struct cb_program *current_program;
extern struct cb_label *current_section, *current_paragraph;

extern struct cb_name_list *cb_name_list_add (struct cb_name_list *list, const char *name);

/* preprocessor (in pplex.l, ppparse.y) */
extern FILE *ppin;
extern FILE *ppout;
extern int pplex (void);
extern int ppparse (void);
extern int ppopen (char *name, struct cb_replacement *replacement);
extern int ppcopy (char *name, const char *lib, struct cb_replacement *replacement);
extern struct cb_replacement *add_replacement (struct cb_replacement *replacement, const char *old_text, const char *new_text);

/* parser (in scanner.l, parser.y) */
extern FILE *yyin;
extern FILE *yyout;
extern int yylex (void);
extern int yyparse (void);

/* error.c */
extern void cb_warning (const char *fmt, ...);
extern void cb_error (const char *fmt, ...);
extern int cb_verify (enum cb_support tag, const char *feature);

#endif /* CB_COBC_H */
