/* 
 *  Copyright (C) 2001, 2000, 1999 David Essex
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
#include <string.h> 
#include <stdarg.h>
#include <ctype.h>

#include "cobpp.h"

//#define YYDEBUG 1

extern unsigned long lineCount;
extern unsigned long columnCount;
extern int copy_state_sw;
extern int copy_start_sw;
extern int replace_state_sw;
extern int replace_start_sw;
//extern char *include_filename;
extern char *include_cpybook;
extern char *include_cpylib;

extern int  yylex(void);
extern char *yytext;
extern void add_copy_replacement(char *, char *);

%}
%union {
    char *str;  /* string */
    int ival;   /* int */
}

%token <str> TOK_PSEUDO_TEXT,TOK_IDENTIFIER,TOK_LITERAL,TOK_WORD
%token <ival> ERROR,YYERROR_VERBOSE

%token TOK_COPY,TOK_PERIOD,TOK_OFF,TOK_BY,TOK_OF,TOK_IN
%token TOK_REPLACING,TOK_REPLACE
%token TOK_MISC,TOK_COMMA

%type <str> pseudo_text,identifier,literal,word,name,library_name_opt


%%

root: statements
    | /* nill */
    ;

statements: statement
    | statements statement
    ;

statement: TOK_COPY 
      {
       if (globalEnvPtr->debugFlag != 0 ) {
         fprintf(stderr, "yytrace: TOK_COPY\n");
       }
      } 
      copy_statement TOK_PERIOD
      { 
       copy_state_sw--;
       copy_start_sw++;
      }
    | TOK_REPLACE replace_statements TOK_PERIOD
      { 
       replace_state_sw--;  
       replace_start_sw++;
      }
    | TOK_REPLACE TOK_OFF TOK_PERIOD
      { 
       replace_state_sw--;  
       replace_start_sw--;
      }
    | error
      {
        return 11;
      }
    | ERROR
      {
        return 2;
      }
    | YYERROR_VERBOSE
      {
        return 3;
      }
    ;

copy_statement: name library_name_opt 
    { 
     /*include_filename = $1; */ 
     include_cpybook  = $1;
     include_cpylib   = $2;
     if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: copy_statement: name=%s, lib=%s;\n",
                include_cpybook, 
                include_cpylib
                );
     }
    }
    TOK_REPLACING replacing_statement
    | name library_name_opt
    { 
     /*include_filename = $1;*/
     include_cpybook  = $1;
     include_cpylib   = $2;
     if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: copy_statement: name=%s, lib=%s;\n",
                include_cpybook, 
                include_cpylib
                );
     }
    }
    ;

replacing_statement: replacing_statements
    | replacing_statements  replacing_statement
    ;

replacing_statements: pseudo_text TOK_BY pseudo_text comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | pseudo_text TOK_BY identifier comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
   | pseudo_text TOK_BY literal comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
   | pseudo_text TOK_BY word comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | identifier TOK_BY identifier comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | identifier TOK_BY pseudo_text comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | identifier TOK_BY literal comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | identifier TOK_BY word comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | literal TOK_BY literal comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | literal TOK_BY pseudo_text comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | literal TOK_BY identifier comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | literal TOK_BY word comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | word TOK_BY word comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | word TOK_BY pseudo_text comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | word TOK_BY identifier comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    | word TOK_BY literal comma_delimiter_opt
      {
       add_copy_replacement($1,$3); 
       if (globalEnvPtr->debugFlag != 0 ) {
        fprintf(stderr, "yytrace: replacing_statements: \'%s\' BY \'%s\':\n", $1, $3);
       }
      }
    ;

library_name_opt: of_in name { $$=$2; }
    | { $$=NULL;  }
    ;

replace_statements: pseudo_text TOK_BY pseudo_text { }
    | pseudo_text TOK_BY pseudo_text replace_statements { }
    ;

name: literal { $$=$1; }
    | identifier { $$=$1; }
    ;

pseudo_text: TOK_PSEUDO_TEXT { $$=$1; }
    ;

identifier: TOK_IDENTIFIER { $$=$1; }
    ;

literal: TOK_LITERAL { $$=$1; }
    ;

word: TOK_WORD { $$=$1; }
    ;

of_in: TOK_OF
    | TOK_IN
    ;

comma_delimiter_opt: TOK_COMMA
    | /* no comma */
    ;

%%


void yyerror(char *s)
{
  fprintf(stderr, "yyerror (%lu, %lu): %s :%s:\n", lineCount, columnCount, s, yytext);

  /* return 0; */
}
