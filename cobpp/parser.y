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

%{
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "cobpp.h"
#include "scanner.h"
%}

%union {
  const char *s;
  struct replacement *r;
}

%token COPY, REPLACE, REPLACING, OFF, IN, BY
%token <s> NAME, TEXT
%type <s> text,copy_in
%type <r> copy_replacing,replacing_list

%%

statement_list: | statement_list statement ;
statement: copy_statement | replace_statement ;

copy_statement:
  COPY NAME copy_in
  copy_replacing '.'		{ include_copybook ($2, $3, $4); }
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
| REPLACE OFF '.'		{ remove_replacements (); }
;
replacing_list:
  text BY text			{ $$ = add_replacement (NULL, $1, $3); }
| replacing_list text BY text	{ $$ = add_replacement ($1, $2, $4); }
;
text: NAME | TEXT ;

%%

void
yyerror (const char *fmt, ...)
{
  const char *filename = yyfilename ? yyfilename : "<stdin>";
  va_list ap;
  va_start (ap, fmt);
  fprintf (stderr, "%s:%d: ", filename, yylineno);
  vfprintf (stderr, fmt, ap);
  fputs ("\n", stderr);
  va_end (ap);
  cobpp_exit_status = 1;
}
