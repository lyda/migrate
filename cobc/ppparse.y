/*							-*- c -*-
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

%expect 0

%defines
%name-prefix="pp"

%{
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "cobc.h"

#define yyerror cb_error

extern struct cb_replacement *add_replacement (struct cb_replacement *replacement, const char *old_text, const char *new_text);
%}

%union {
  char *s;
  struct cb_replacement *r;
}

%token DIRECTIVE SOURCE FORMAT IS FIXED FREE
%token COPY REPLACE REPLACING OFF IN BY
%token <s> NAME TEXT
%type <s> text copy_in
%type <r> copy_replacing replacing_list

%%

statement_list: | statement_list statement ;
statement: directive | copy_statement | replace_statement ;

directive:
  DIRECTIVE source_format
;

source_format:
  SOURCE _format _is format
;
format:
  FIXED				{ cb_source_format = CB_FORMAT_FIXED; }
| FREE				{ cb_source_format = CB_FORMAT_FREE; }
;
_format: | FORMAT ;
_is: | IS ;

copy_statement:
  COPY NAME copy_in copy_replacing '.'
  {
    fputc ('\n', ppout);
    ppopen ($2, $3, $4);
  }
;
copy_in:
  /* nothing */			{ $$ = NULL; }
| IN text			{ $$ = $2; }
;
copy_replacing:
  /* nothing */			{ $$ = NULL; }
| REPLACING replacing_list	{ $$ = $2; }
;
replace_statement:
  REPLACE replacing_list '.'
| REPLACE OFF '.'
;
replacing_list:
  text BY text			{ $$ = add_replacement (NULL, $1, $3); }
| replacing_list text BY text	{ $$ = add_replacement ($1, $2, $4); }
;
text: NAME | TEXT ;
