/*							-*- c -*-
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

%option 8bit
%option caseless
%option noyywrap
%option never-interactive

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobc.h"
#include "codegen.h"
#include "parser.h"
#include "reserved.h"
#include "_libcob.h"

extern FILE *o_lst;
extern char picture[];
extern struct sym *curr_field;
extern int picix,piccnt,sign,v_flag;
extern int curr_division;
extern int start_condition;
extern int need_subscripts;
extern int in_procedure;

static int read_literal (char mark);
static int scan_x (char *text);
static int scan_word (char *text);
static int scan_picture (char *text);
%}

%x ID_STATE COMMENT_STATE PICTURE_STATE

%%

%{
  switch (start_condition)
    {
    case START_ID:
      BEGIN ID_STATE;
      break;
    case START_COMMENT:
      BEGIN COMMENT_STATE;
      break;
    case START_PICTURE:
      BEGIN PICTURE_STATE;
      break;
    }
  start_condition = 0;
%}

<*>[ \t]* { /* ignore */ }

<*>\n {
  cob_orig_lineno++;
  if (cob_stabs_flag && in_procedure)
    stabs_line ();
}

^"#".* {
  /* line directive */
  char *endp;
  cob_orig_lineno = strtol (yytext + 2, &endp, 10) - 1;
  if (cob_orig_filename)
    free (cob_orig_filename);
  cob_orig_filename = strdup (strchr (endp, '"') + 1);
  strrchr (cob_orig_filename, '"')[0] = '\0';
}

[\'\"] {
  /* string literal */
  return read_literal (yytext[0]);
}

X\'[^\'\n]\' |
X\"[^\"\n]\" {
  /* X string literal */
  return scan_x (yytext);
}

[0-9]*[.,]?[0-9]+ {
  /* numeric literal */
  yylval.lval = install_literal (yytext);
  return NLITERAL;
}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  /* symbol or label */
  return scan_word (yytext);
}

"(" {
  if (need_subscripts) {
    need_subscripts=0;
    return LPAR;
  }
  else
    return '(';
}

"**"		{ return '^'; }
"<="		{ return TOK_LE; }
">="		{ return TOK_GE; }
.		{ return yytext[0]; }

<ID_STATE>{
  [A-Z0-9_-]+ {
    BEGIN INITIAL;
    yylval.str = strdup (yytext);
    return IDSTRING;
  }
  .		{ /* ignore */ }
}

<COMMENT_STATE>{
  .*		{ BEGIN INITIAL; }
}

<PICTURE_STATE>{
  "IS"		{ /* ignore */ }
  [^ \t\n]+	{
    BEGIN INITIAL;
    return scan_picture (yytext);
  }
}

<<EOF>> {
  yyterminate();
}

%%

static int
read_literal (char mark)
{
  int c;
  char buff[BUFSIZ];
  char *p = buff;

  while ((c = input ()) != EOF)
    {
      *p++ = c;
      if (c == mark && (c = input ()) != mark)
	{
	  p--;
	  unput (c);
	  break;
	}
      if (p - buff >= BUFSIZ - 1)
	yyerror ("too large literal");
    }
  *p = 0;
  yylval.lval = install_literal (buff);
  return CLITERAL;
}

static int
scan_x (char *text)
{
  int high = 1;
  char *src = text;
  char *dst = alloca (strlen (text));
  while (*src)
    {
      int c = toupper (*src);
      if ('0' <= c && c <= '9')
	c = c - '0';
      else if ('A' <= c && c <= 'F')
	c = c - 'A' + 10;
      else
	goto error;

      if (high)
	*dst = c << 4;
      else
	*dst++ += c;

      src++;
      high = 1 - high;
    }
  *dst = '\0';

  if (high)
    {
      yylval.lval = install_literal (dst);
      return CLITERAL;
    }
  else
    {
    error:
      yyerror ("invalid X literal: %s", text);
      yylval.lval = install_literal (dst);
      return CLITERAL;
    }
}

static int
scan_word (char *text)
{
  int token = lookup_reserved_word (text);
  if (token != 0)
    return token;

  switch (curr_division)
    {
    case CDIV_ENVIR:
      yylval.sval = install (yytext, SYTB_VAR, 0);
      return SYMBOL;

    case CDIV_DATA:
      yylval.sval = install (yytext, SYTB_VAR, 1);
      return SYMBOL;

    case CDIV_FD:
      if ((yylval.sval = lookup_symbol (yytext)) == NULL)
	yyerror ("undefined %s at FD/SD", yytext);
      return SYMBOL;

    default:
      /* if symbol was defined at the data division */
      if ((yylval.sval = lookup_symbol (yytext)) != NULL)
	{
	  if (yylval.sval->type == '8')
	    return VARCOND;
	  else if (is_subscripted (yylval.sval))
	    return SUBSCVAR;
	  else
	    return VARIABLE;
	}
      /* otherwise, it should be a label */
      else
	{
	  yylval.sval = install_label (yytext);
	  return (LABELSTR);
	}
    }
}

static int
scan_picture (char *text)
{
  char *p;
  picix=piccnt=v_flag=curr_field->decimals=0;
  picture[picix]=0;

  for (p = text; *p; p++)
    {
      int c = *p;
      int n = 1;

      if (c == '.' && p[1] == '\0')
	{
	  unput (c);
	  break;
	}
      if (p[1] == '(')
	{
	  n = 0;
	  for (p += 2; *p != ')'; p++)
	    {
	      if (!('0' <= *p && *p <= '9'))
		goto error;
	      n = n * 10 + *p - '0';
	    }
	}
      if (!save_pic_char (c, n))
	goto error;
    }
  picture[picix + 2] = 0;
  return PICTURE_TOK;

 error:
  yyerror ("invalid picture string");
  return PICTURE_TOK;
}
