/* COBOL scanner 					-*- c -*-
 *
 * Copyright (C) 2001  Keisuke Nishida
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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
%option stack
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

#define YY_NO_TOP_STATE

extern char picture[];
extern struct sym *curr_field;
extern int picix,piccnt,sign,v_flag;

extern FILE *o_lst;

#define YY_INPUT(buf,result,max_size)  result = yyinput (buf, max_size);
static int yyinput (char *buf, int maxsize);

static int read_literal (char mark);
static int read_literal_x (char mark);
static int scan_picture (char *text);
static int scan_word (char *text);
static int scan_reserved_word (char *text);
%}

white [\r\t ]
any [!-~]

word [A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)?

%x ENVIR_ST DATA_ST FD_ST REDEF_ST EXCEPTION_ST
%x ID_STATE COMMENT_STATE PICTURE_STATE

%%

%{
  extern int curr_division;
  extern int need_subscripts;

  switch (curr_division) {
  case CDIV_ENVIR:
    BEGIN ENVIR_ST;
    break;
  case CDIV_DATA:
    BEGIN DATA_ST;
    break;
  case CDIV_PROC:
    BEGIN INITIAL;
    break;
  case CDIV_FD:
    BEGIN FD_ST;
    break;
  case CDIV_REDEF:
    BEGIN REDEF_ST;
    break;
  case CDIV_EXCEPTION:
    BEGIN EXCEPTION_ST;
    break;
  case CINITIAL:
    BEGIN INITIAL;
    break;
  case START_ID:
    yy_push_state (ID_STATE);
    break;
  case START_COMMENT:
    yy_push_state (COMMENT_STATE);
    break;
  case START_PICTURE:
    yy_push_state (PICTURE_STATE);
    break;
  }
  curr_division=0; /* avoid new state switch */
%}

<*>^"#".* {
  /* Parse line directive */
  char *endp;
  cob_orig_lineno = strtol (yytext + 2, &endp, 10) - 1;
  if (cob_orig_filename)
    free (cob_orig_filename);
  cob_orig_filename = strdup (strchr (endp, '"') + 1);
  strrchr (cob_orig_filename, '"')[0] = '\0';
}

<ENVIR_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  yylval.sval = install (yytext, SYTB_VAR, 0);
  return (STRING);
}

<DATA_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  /* install w/clone if neccessary */
  yylval.sval = install (yytext, SYTB_VAR, 1);
  return (STRING);
}

<INITIAL,EXCEPTION_ST>{word} {
  struct sym *symbol;
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  /* if symbol was defined at the data division */
  if ((symbol = lookup_symbol (yytext)) != NULL)
    {
      yylval.sval = symbol;
      if (symbol->type == '8')
	return (VARCOND);
      else if (is_subscripted (symbol))
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

<FD_ST>{
  {word} {
    int tok = scan_word (yytext);
    if (tok)
      return tok;

    if ((yylval.sval = lookup_symbol (yytext)) == NULL)
      yyerror ("undefined %s at FD/SD", yytext);
    return (STRING);
  }
  . { }
}

<REDEF_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  /* don't clone redefined var */
  yylval.sval = install (yytext, SYTB_VAR, 0);
  return (STRING);
}

<INITIAL,ENVIR_ST,DATA_ST,REDEF_ST,EXCEPTION_ST>{

  "."/[ \t\n]	{ return '.'; }

  {white}+ { }
  "\n"   { }

  \'		{ return read_literal ('\''); }
  \"		{ return read_literal ('\"'); }
  X\'		{ return read_literal_x ('\''); }
  X\"		{ return read_literal_x ('\"'); }

  [0-9]*[.,]?[0-9]+ {
    yylval.lval = install_literal (yytext);
    return(NLITERAL);
  }

  [<>=][>=]? {
    int token;
    if ((token = scan_reserved_word (yytext)) != 0)
      return token;
    yyerror("unknown operator: `%s'", yytext);
  }

  "(" {
    if (need_subscripts) {
      need_subscripts=0;
      return (LPAR);
    }
    else
      return ('(');
  }
  "**"		{ return '^'; }
  {any}		{ return yytext[0]; }
  . { }
}

<ID_STATE>{
  .		{ /* ignore */ }
  {word}	{
    yy_pop_state ();
    yylval.str = strdup (yytext);
    return IDSTRING;
  }
}

<COMMENT_STATE>{
  .*		{ /* ignore */ }
  "\n"		{ yy_pop_state (); }
}

<PICTURE_STATE>{
  "IS" |
  [ \t\r\n]+	{ /* ignore */ }
  [^ \t\r\n]+	{
    yy_pop_state ();
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

static int
is_integer_token (const char *tok)
{
  while (*tok)
    if (!isdigit (*tok++))
      return 0;
  return 1;
}

static int
is_valid_identifier (const char *tok)
{
  while (*tok)
    if (isalpha (*tok++))
      return 1;
  return 0;
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
  else if (!is_valid_identifier (text))
    {
      yylval.sval = install_label (text);
      return (LABELSTR);
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
yyinput (char *buf, int maxsize)
{
  if (fgets (buf, maxsize, yyin) == NULL)
    return YY_NULL;

  cob_orig_lineno++;
  return strlen (buf);
}

void
dummy ()
{
  /* This avoids warnings */
  yy_flex_realloc (0, 0);
}
