/*							-*- c -*-
 *  Copyright (C) 2001 Keisuke Nishida
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this software; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
*/ 

%{
#include <stdio.h>
#include <stdlib.h>

#include "scanner.h"

static void yyerror (char *msg);
%}

%union {
  int i;
  const char *s;
  struct replacement *r;
}

%token TOK_COPY, TOK_REPLACE, TOK_REPLACING, TOK_OFF, TOK_BY, DOT, COMMA
%token <s> NAME, TEXT
%type <s> text
%type <r> replacing_list

%%

statements:
    /* nothing */
  | statements statement
  ;

statement: 
    copy_statement
  | replace_statement
  ;

/* COPY */

copy_statement:
    TOK_COPY NAME DOT
    {
      include_copybook ($2, NULL);
    }
  | 
    TOK_COPY NAME TOK_REPLACING replacing_list DOT
    {
      include_copybook ($2, $4);
    }
  ;

/* REPLACE */

replace_statement:
    TOK_REPLACE replacing_list DOT
  | TOK_REPLACE TOK_OFF DOT
  ;

replacing_list:
    text TOK_BY text { $$ = add_replacement (NULL, $1, $3); }
  | replacing_list opt_comma
    text TOK_BY text { $$ = add_replacement ($1, $3, $5); }

text:
    NAME	{ $$ = $1; }
  | TEXT	{ $$ = $1; }

opt_comma: | COMMA ;

%%

static void
yyerror (char *msg)
{
  const char *name = yyfilename[0] ? yyfilename : "<stdin>";
  fprintf (stderr, "%s:%d: %s before `%s'\n", name, yylineno, msg, yytext);
}
