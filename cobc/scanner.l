/*							-*- c -*-
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

%option 8bit
%option caseless
%option noyywrap
%option never-interactive

%{
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libcob.h>

#include "cobc.h"
#include "parser.h"
#include "reserved.h"

#define SET_LOCATION(x)				\
  (x)->source_file = cb_source_file;		\
  (x)->source_line = cb_source_line

static int last_token_is_dot = 0;
static int integer_is_label = 0;

static int read_literal (char mark);
static int scan_x (char *text);
static int scan_h (char *text);
static int scan_numeric (char *text);
static int scan_picture (char *text);

%}

%x PICTURE_STATE FUNCTION_STATE

%%
%{
  /* We treat integer literals immediately after '.' as labels;
     that is, they must be level numbers or section names. */
  integer_is_label = 0;
  if (last_token_is_dot)
    {
      integer_is_label = 1;
      last_token_is_dot = 0;
    }
%}


<*>[ \t,;]+	{ /* ignore */ }

<*>\n		{ cb_source_line++; }

^"#".* {
  /* line directive */
  char *endp;
  cb_source_line = strtol (yytext + 2, &endp, 10) - 1;
  cb_source_file = strdup (strchr (endp, '"') + 1);
  strrchr (cb_source_file, '"')[0] = '\0';
}

"PIC" |
"PICTURE"	{ BEGIN PICTURE_STATE; }
"FUNCTION"	{ BEGIN FUNCTION_STATE; }

[\'\"] {
  /* string literal */
  return read_literal (yytext[0]);
}

X\'[^\'\n]*\' |
X\"[^\"\n]*\" {
  /* X string literal */
  return scan_x (yytext + 2);
}

[0-9]+ {
  if (integer_is_label)
    {
      /* integer label */
      yylval.tree = make_reference (yytext);
      SET_LOCATION (yylval.tree);
      return WORD;
    }
  else
    {
      /* numeric literal */
      return scan_numeric (yytext);
    }
}

[+-]?[0-9]*[.,]?[0-9]+ {
  /* numeric literal */
  return scan_numeric (yytext);
}

H\'[^\'\n]*\' |
H\"[^\"\n]*\" {
  /* H numric literal */
  return scan_h (yytext + 2);
}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  int token;
  struct cb_word *word;

  /* reserved word */
  token = lookup_reserved_word (yytext);
  if (token != 0)
    return token;

  /* user word */
  yylval.tree = make_reference (yytext);
  SET_LOCATION (yylval.tree);

  /* special name handling */
  word = CB_REFERENCE (yylval.tree)->word;
  if (word->count > 0)
    {
      cb_tree x = word->items->item;
      if (CB_BUILTIN_P (x))
	return MNEMONIC_NAME;
      if (CB_PROPOSITION_P (x))
	return CLASS_NAME;
    }

  return WORD;
}

"<="		{ return LE; }
">="		{ return GE; }
"**"		{ return '^'; }
"."		{ last_token_is_dot = 1; return '.'; }
.		{ return yytext[0]; }

<PICTURE_STATE>{
  "IS"		{ /* ignore */ }
  [^ \t\n;]+	{
    BEGIN INITIAL;
    return scan_picture (yytext);
  }
}

<FUNCTION_STATE>{
  [a-z0-9-]+	{
    BEGIN INITIAL;
    yylval.sval = strdup (yytext);
    return FUNCTION_NAME;
  }
  .		{ return yytext[0]; }
}

<<EOF>> {
  yyterminate ();
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
	cb_error (_("too large literal"));
    }
  *p = 0;
  yylval.tree = make_nonnumeric_literal (strlen (buff), buff);
  SET_LOCATION (yylval.tree);
  return LITERAL;
}

static int
scan_x (char *text)
{
  int high = 1;
  char *src = text;
  char buff[strlen (text)];
  char *dst = buff;
  while (isalnum (*src))
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
      yylval.tree = make_nonnumeric_literal (dst - buff, buff);
      SET_LOCATION (yylval.tree);
      return LITERAL;
    }

 error:
  cb_error (_("invalid X literal: %s"), text);
  yylval.tree = cb_error_node;
  return LITERAL;
}

static int
scan_h (char *text)
{
  char *p;
  char buff[19];
  long long val = 0;

  for (p = text; *p != '\'' && *p != '\"'; p++)
    {
      int c = toupper (*p);
      if ('0' <= c && c <= '9')
	c = c - '0';
      else if ('A' <= c && c <= 'F')
	c = c - 'A' + 10;
      else
	goto error;

      val = (val << 4) + c;
    }

  sprintf (buff, "%lld", val);
  yylval.tree = make_numeric_literal (0, buff, 0);
  SET_LOCATION (yylval.tree);
  return LITERAL;

 error:
  cb_error (_("invalid H literal: %s"), text);
  yylval.tree = cb_error_node;
  return LITERAL;
}

static int
scan_numeric (char *text)
{
  int sign, decimals = 0;
  unsigned char *s;
  /* get sign */
  sign = (*text == '+') ? 1 : (*text == '-') ? -1 : 0;
  if (sign)
    text++;

  /* get decimal point */
  s = strchr (text, current_program->decimal_point);
  if (s) {
    decimals = strlen (s) - 1;
    memmove (s, s + 1, decimals + 1);
  }
  if (strchr (text, (current_program->decimal_point == ',') ? '.' : ','))
    cb_error (_("invalid numeric literal"));

  yylval.tree = make_numeric_literal (sign, text, - decimals);
  SET_LOCATION (yylval.tree);
  return LITERAL;
}

static int
scan_picture (char *text)
{
  char *p;

  /* normalize the input */
  for (p = text; *p; p++)
    {
      /* unput trailing '.' or ',' */
      if (p[1] == 0 && (*p == '.' || *p == ','))
	{
	  unput (*p);
	  *p = 0;
	  break;
	}
      /* upcase */
      *p = toupper (*p);
    }

  yylval.pict = parse_picture (text);
  return PICTURE;
}
