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

static int read_literal (char mark);
static int read_literal_x (char mark);
static int scan_word (char *text);
static int scan_reserved_word (char *text);
static int scan_picture (char *text);
%}

%x ID_STATE COMMENT_STATE PICTURE_STATE

%%

%{
  extern int start_condition;
  extern int curr_division;
  extern int need_subscripts;

  switch (start_condition) {
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
  start_condition=0; /* avoid new state switch */
%}

<*>[ \t\r]*	{ /* ignore */ }
<*>\n		{ cob_orig_lineno++; }

^"#".* {
  /* line directive */
  char *endp;
  cob_orig_lineno = strtol (yytext + 2, &endp, 10) - 1;
  if (cob_orig_filename)
    free (cob_orig_filename);
  cob_orig_filename = strdup (strchr (endp, '"') + 1);
  strrchr (cob_orig_filename, '"')[0] = '\0';
}

[0-9]*[.,]?[0-9]+ {
  /* numeric literal */
  yylval.lval = install_literal (yytext);
  return NLITERAL;
}

X?[\'\"] {
  /* string literal */
  if (yytext[0] == 'X')
    return read_literal_x (yytext[1]);
  else
    return read_literal (yytext[0]);
}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  /* symbol or label */
  int tok = scan_word (yytext);
  if (tok)
    return tok;
  switch (curr_division)
    {
    case CDIV_ENVIR:
      yylval.sval = install (yytext, SYTB_VAR, 0);
      return (STRING);
    case CDIV_DATA:
      yylval.sval = install (yytext, SYTB_VAR, 1);
      return (STRING);
    case CDIV_FD:
      if ((yylval.sval = lookup_symbol (yytext)) == NULL)
	yyerror ("undefined %s at FD/SD", yytext);
      return (STRING);
    default:
      /* if symbol was defined at the data division */
      if ((yylval.sval = lookup_symbol (yytext)) != NULL)
	{
	  if (yylval.sval->type == '8')
	    return (VARCOND);
	  else if (is_subscripted (yylval.sval))
	    return (SUBSCVAR);
	  else
	    return (VARIABLE);
	}
      /* otherwise, it should be a label */
      else
	{
	  yylval.sval = install_label (yytext);
	  return (LABELSTR);
	}
    }
}

[<>=][>=]? {
  int tok;
  if ((tok = scan_reserved_word (yytext)) != 0)
    return tok;
  yyerror("unknown operator: `%s'", yytext);
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
  [^ \t\r\n]+	{
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
read_literal_x (char mark)
{
  unsigned char hnum[512];
  unsigned char *s = hnum;
  char toktext[1024];
  char *tp = toktext;
  while ((*tp = toupper (input ())) != mark)
    {
      if (tp >= toktext + 1024)
	{
	  yyerror ("scanner error: token too large");
	  while (input () != mark);
	  *tp = 0;
	  yylval.lval = install_literal (toktext);
	  return (CLITERAL);
	}
      *s = (*tp > '9') ? (*tp - 'A' + 0x0a) : (*tp - '0');
      if ((*tp = toupper (input ())) == mark)
	{
	  break;
	}
      *s++ = *s * 0x10 + ((*tp > '9') ? (*tp - 'A' + 0x0a) : (*tp - '0'));
    }
  {
    char buff[1024];
    strncpy (buff, hnum, s - hnum);
    buff[s - hnum] = '\0';
    yylval.lval = install_literal (buff);
  }
  return CLITERAL;
}

static int
is_integer_token (const char *tok)
{
  while (*tok)
    if (!isdigit (*tok++))
      return 0;
  return 1;
}

static int
scan_word (char *text)
{
  int token;

  if (is_integer_token (text))
    {
      yylval.lval = install_literal (text);
      return (NLITERAL);
    }
  else if ((token = scan_reserved_word (text)) != 0)
    {
      return token;
    }
  else
    return 0;
}

static int
scan_reserved_word (char *s)
{
  struct reserved_word *p = lookup_reserved_word (s);
  if (!p)
    return 0;
  yylval.ival = p->minor;
  return p->token;
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
  return PICTURE;

 error:
  yyerror ("syntax error, invalid picture");
  return PICTURE;
}
