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
  const char *s;
  struct replacement *r;
}

%token COPY, REPLACE, REPLACING, OFF, BY
%token <s> NAME, TEXT
%type <s> text
%type <r> replacing_list

%%

statement_list: | statement_list statement ;
statement: copy_statement | replace_statement ;

copy_statement:
  COPY NAME '.'                          { include_copybook ($2, NULL); }
| COPY NAME REPLACING replacing_list '.' { include_copybook ($2, $4); }
;
replace_statement:
  REPLACE replacing_list '.'
| REPLACE OFF '.'	{ remove_replacements (); }
;
replacing_list:
  text BY text		{ $$ = add_replacement (NULL, $1, $3); }
| replacing_list opt_comma
  text BY text		{ $$ = add_replacement ($1, $3, $5); }
;
text: NAME | TEXT ;
opt_comma: | ',' ;

%%

static void
yyerror (char *msg)
{
  fprintf (stderr, "%s:%d: %s before `%s'\n",
	   yyfilename ? yyfilename : "<stdin>", yylineno, msg, yytext);
}
