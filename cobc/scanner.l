/*							-*- c -*-
 * Copyright (C) 2001-2004 Keisuke Nishida
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
#include "tree.h"
#include "parser.h"

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
static void count_lines (char *text);

%}

%s DECIMAL_IS_PERIOD DECIMAL_IS_COMMA
%x PICTURE_STATE FUNCTION_STATE

%%
%{
  if (current_program)
    {
      if (current_program->decimal_point == '.')
	BEGIN DECIMAL_IS_PERIOD;
      else
	BEGIN DECIMAL_IS_COMMA;
    }

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
      yylval = cb_build_reference (yytext);
      SET_LOCATION (yylval);
      return WORD;
    }
  else
    {
      /* numeric literal */
      return scan_numeric (yytext);
    }
}

<DECIMAL_IS_PERIOD>[+-]?[0-9.]*[0-9]+ {
  /* numeric literal */
  return scan_numeric (yytext);
}

<DECIMAL_IS_COMMA>[+-]?[0-9,]*[0-9]+ {
  /* numeric literal */
  return scan_numeric (yytext);
}

H\'[^\'\n]*\' |
H\"[^\"\n]*\" {
  /* H numric literal */
  return scan_h (yytext + 2);
}

"NOT"+[ \t\n]+"ON"+[ \t\n]+"SIZE"+[ \t\n]+"ERROR" |
"NOT"+[ \t\n]+"SIZE"+[ \t\n]+"ERROR" {	
	count_lines(yytext);
	return NOT_SIZE_ERROR;
	}

"ON"+[ \t\n]+"SIZE"+[ \t\n]+"ERROR" |
"SIZE"+[ \t\n]+"ERROR" {	
	count_lines(yytext);
	return SIZE_ERROR;
	}

"NOT"+[ \t\n]+"ON"+[ \t\n]+"EXCEPTION" |
"NOT"+[ \t\n]+"EXCEPTION" {	
	count_lines(yytext);
	return NOT_EXCEPTION;
	}

"ON"+[ \t\n]+"EXCEPTION" |
"EXCEPTION" {	
	count_lines(yytext);
	return EXCEPTION;
	}

"NOT"+[ \t\n]+"ON"+[ \t\n]+"OVERFLOW" |
"NOT"+[ \t\n]+"OVERFLOW" {	
	count_lines(yytext);
	return NOT_OVERFLOW;
	}

"ON"+[ \t\n]+"OVERFLOW" |
"OVERFLOW" {	
	count_lines(yytext);
	return OVERFLOW;
	}

"NOT"+[ \t\n]+"AT"+[ \t\n]+"END-OF-PAGE" |
"NOT"+[ \t\n]+"AT"+[ \t\n]+"EOP" |
"NOT"+[ \t\n]+"END-OF-PAGE" |
"NOT"+[ \t\n]+"EOP" {	
	count_lines(yytext);
	return NOT_EOP;
	}

"AT"+[ \t\n]+"END-OF-PAGE" |
"AT"+[ \t\n]+"EOP" |
"END-OF-PAGE" |
"EOP" {	
	count_lines(yytext);
	return EOP;
	}

"NOT"+[ \t\n]+"INVALID"+[ \t\n]+"KEY" {
	count_lines(yytext);
	return NOT_INVALID_KEY;
	}

"NOT"+[ \t\n]+"INVALID" {
	count_lines(yytext);
	return NOT_INVALID_KEY;
	}

"INVALID"+[ \t\n]+"KEY" {
	count_lines(yytext);
	return INVALID_KEY;
	}

"INVALID" {
	count_lines(yytext);
	return INVALID_KEY;
	}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  int token;
  struct cb_word *word;

  /* reserved word */
  token = lookup_reserved_word (yytext);
  if (token != 0)
    {
      yylval = 0;
      return token;
    }

  /* user word */
  yylval = cb_build_reference (yytext);
  SET_LOCATION (yylval);

  /* special name handling */
  word = CB_REFERENCE (yylval)->word;
  if (word->count > 0)
    {
      cb_tree x = CB_VALUE (word->items);
      if (CB_SYSTEM_NAME_P (x))
	return MNEMONIC_NAME;
    }

  return WORD;
}

"<="		{ yylval = 0; return LE; }
">="		{ yylval = 0; return GE; }
"<>"		{ yylval = 0; return NE; }
"**"		{ yylval = 0; return '^'; }
"."		{ last_token_is_dot = 1; yylval = 0; return '.'; }
.		{ yylval = 0; return yytext[0]; }

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
    yylval = cb_build_reference (yytext);
    SET_LOCATION (yylval);
    return FUNCTION_NAME;
  }
  .		{ yylval = 0; return yytext[0]; }
}

<<EOF>> {
	last_token_is_dot = 0;
	integer_is_label = 0;
	yyterminate ();
}

%%

static int
read_literal (char mark)
{
  static size_t size = 64;
  static char *buff = NULL;

  int c;
  int i = 0;

  if (!buff)
    buff = malloc (size);

  while ((c = input ()) != EOF)
    {
      buff[i++] = c;
      if (c == mark && (c = input ()) != mark)
	{
	  i--;
	  unput (c);
	  break;
	}
      if (i >= size)
	{
	  size *= 2;
	  buff = realloc (buff, size);
	}
    }
  buff[i] = 0;
  yylval = cb_build_alphanumeric_literal (buff, i);
  SET_LOCATION (yylval);
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

  if (high)
    {
      yylval = cb_build_alphanumeric_literal (buff, dst - buff);
      SET_LOCATION (yylval);
      return LITERAL;
    }

 error:
  cb_error (_("invalid X literal: %s"), text);
  yylval = cb_error_node;
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

#ifdef __MINGW32__
  sprintf (buff, "%I64d", val);
#else
  sprintf (buff, "%lld", val);
#endif
  yylval = cb_build_numeric_literal (0, buff, 0);
  SET_LOCATION (yylval);
  return LITERAL;

 error:
  cb_error (_("invalid H literal: %s"), text);
  yylval = cb_error_node;
  return LITERAL;
}

static int
scan_numeric (char *text)
{
  int sign, scale = 0;
  unsigned char *s;
  /* get sign */
  sign = (*text == '+') ? 1 : (*text == '-') ? -1 : 0;
  if (sign)
    text++;

  /* get decimal point */
  s = strchr (text, current_program->decimal_point);
  if (s) {
    scale = strlen (s) - 1;
    memmove (s, s + 1, scale + 1);
  }
  if (strchr (text, (current_program->decimal_point == ',') ? '.' : ','))
    cb_error (_("invalid numeric literal"));

  yylval = cb_build_numeric_literal (sign, text, scale);
  SET_LOCATION (yylval);
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

  yylval = cb_build_picture (text);
  return PICTURE;
}

static void
count_lines (char *text)
{
	char *p;

	for ( p = text; *p; p++ ) {
		if ( *p == '\n' ) {
			cb_source_line++;
		}
	}
}
