/*							-*- c -*-
 * Copyright (C) 2001-2006 Keisuke Nishida
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
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
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
  (x)->source_file = (unsigned char *)cb_source_file;		\
  (x)->source_line = cb_source_line

struct cb_level_78 {
	struct cb_level_78	*next;
	struct cb_field		*fld78;
};

static struct cb_level_78	*lev78ptr = NULL;
static int last_token_is_dot = 0;
static int integer_is_label = 0;
static int inside_bracket = 0;

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


<*>[ \t,;]+	{
	if (yytext[0] == ',' && yytext[1] == ' ' && inside_bracket) {
		return COMMA_DELIM;
	}
}

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

"("	{ inside_bracket++; return '('; }

")"	{ inside_bracket--; return ')'; }

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
  /* H numeric literal */
  return scan_h (yytext + 2);
}

"SCREEN"[ \t\n]+"CONTROL" {
	count_lines (yytext);
	return SCREEN_CONTROL;
}

"EVENT"[ \t\n]+"STATUS" {
	count_lines (yytext);
	return EVENT_STATUS;
}

"BLANK"[ \t\n]+"SCREEN" {
	count_lines (yytext);
	return BLANK_SCREEN;
}

"BLANK"[ \t\n]+"LINE" {
	count_lines (yytext);
	return BLANK_LINE;
}

"NO"[ \t\n]+"ADVANCING" {
	count_lines (yytext);
	return NO_ADVANCING;
}

"NOT"[ \t\n]+"ON"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] |
"NOT"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] {	
	count_lines (yytext);
	return NOT_SIZE_ERROR;
	}

"ON"[ \t\n]+"SIZE"[ \t\n]+"ERROR"[ \t\n] |
"SIZE"[ \t\n]+"ERROR"[ \t\n] {	
	count_lines (yytext);
	return SIZE_ERROR;
	}

"NOT"[ \t\n]+"ON"[ \t\n]+"EXCEPTION"[ \t\n] |
"NOT"[ \t\n]+"EXCEPTION"[ \t\n] {	
	count_lines (yytext);
	return NOT_EXCEPTION;
	}

"ON"[ \t\n]+"EXCEPTION"[ \t\n] |
"EXCEPTION"[ \t\n] {	
	count_lines (yytext);
	return EXCEPTION;
	}

"NOT"[ \t\n]+"ON"[ \t\n]+"OVERFLOW"[ \t\n] |
"NOT"[ \t\n]+"OVERFLOW"[ \t\n] {	
	count_lines (yytext);
	return NOT_OVERFLOW;
	}

"ON"[ \t\n]+"OVERFLOW"[ \t\n] |
"OVERFLOW"[ \t\n] {	
	count_lines (yytext);
	return OVERFLOW;
	}

"NOT"[ \t\n]+"AT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"NOT"[ \t\n]+"AT"[ \t\n]+"EOP"[ \t\n] |
"NOT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"NOT"[ \t\n]+"EOP"[ \t\n] {	
	count_lines (yytext);
	return NOT_EOP;
	}

"AT"[ \t\n]+"END-OF-PAGE"[ \t\n] |
"AT"[ \t\n]+"EOP"[ \t\n] |
"END-OF-PAGE"[ \t\n] |
"EOP"[ \t\n] {	
	count_lines (yytext);
	return EOP;
	}

"NOT"[ \t\n]+"INVALID"[ \t\n]+"KEY"[ \t\n] {
	count_lines (yytext);
	return NOT_INVALID_KEY;
	}

"NOT"[ \t\n]+"INVALID"[ \t\n] {
	count_lines (yytext);
	return NOT_INVALID_KEY;
	}

"INVALID"[ \t\n]+"KEY"[ \t\n] {
	count_lines (yytext);
	return INVALID_KEY;
	}

"INVALID"[ \t\n] {
	count_lines (yytext);
	return INVALID_KEY;
	}

"ID"[ ]+"DIVISION"[ ]*"." |
"IDENTIFICATION"[ ]+"DIVISION"[ ]*"." { /* Ignore */ }

"SWITCH"[ ]+"1" {
  yylval = cb_build_reference ("SWITCH-1");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"2" {
  yylval = cb_build_reference ("SWITCH-2");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"3" {
  yylval = cb_build_reference ("SWITCH-3");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"4" {
  yylval = cb_build_reference ("SWITCH-4");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"5" {
  yylval = cb_build_reference ("SWITCH-5");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"6" {
  yylval = cb_build_reference ("SWITCH-6");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"7" {
  yylval = cb_build_reference ("SWITCH-7");
  SET_LOCATION (yylval);
  return WORD;
}

"SWITCH"[ ]+"8" {
  yylval = cb_build_reference ("SWITCH-8");
  SET_LOCATION (yylval);
  return WORD;
}

[A-Z0-9]([_A-Z0-9-]*[A-Z0-9]+)? {
  int			token;
  struct cb_word	*word;
  struct cb_level_78	*p78;

  /* reserved word */
  token = lookup_reserved_word (yytext);
  if (token != 0)
    {
      yylval = 0;
      return token;
    }

  for (p78 = lev78ptr; p78; p78 = p78->next) {
	if (strcasecmp (yytext, p78->fld78->name) == 0) {
		yylval = CB_VALUE (p78->fld78->values);
		return LITERAL;
	}
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
	inside_bracket = 0;
	lev78ptr = NULL;
	yyterminate ();
}

%%

static int
read_literal (char mark)
{
	static size_t	size = 192;
	static char	*buff = NULL;
	int		c;
	size_t		i = 0;

	if (!buff) {
		buff = cobc_malloc (size);
	}

	while ((c = input ()) != EOF) {
		buff[i++] = c;
		if (c == mark && (c = input ()) != mark) {
			i--;
			unput (c);
			break;
		}
		if (i >= size) {
			size *= 2;
			buff = realloc (buff, size);
			if (!buff) {
				fprintf (stderr, "Memory realloc failed - Aborting\n");
				fflush (stderr);
				(void)longjmp (cob_jmpbuf, 1);
			}
		}
	}
	buff[i] = 0;
	yylval = cb_build_alphanumeric_literal ((unsigned char *)buff, i);
	SET_LOCATION (yylval);
	return LITERAL;
}

static int
scan_x (char *text)
{
	static int		lastlen = 0;
	static unsigned char	*buff = NULL;

	unsigned char		*src = (unsigned char *)text;
	unsigned char		*dst;
	int			currlen;
	int			high = 1;

	currlen = strlen (text);
	if (!buff) {
		if (currlen <= COB_SMALL_BUFF) {
			buff = cobc_malloc (COB_SMALL_BUFF);
			lastlen = COB_SMALL_BUFF;
		} else {
			buff = cobc_malloc (currlen);
			lastlen = currlen;
		}
	} else {
		if (currlen > lastlen) {
			free (buff);
			buff = cobc_malloc (currlen);
			lastlen = currlen;
		} else {
			memset (buff, 0, lastlen);
		}
	}
	dst = buff;
	while (isalnum (*src)) {
		int c = toupper (*src);

		if ('0' <= c && c <= '9') {
			c = c - '0';
		} else if ('A' <= c && c <= 'F') {
			c = c - 'A' + 10;
		} else {
			goto error;
		}

		if (high) {
			*dst = c << 4;
		} else {
			*dst++ += c;
		}

		src++;
		high = 1 - high;
	}

	if (high) {
		yylval = cb_build_alphanumeric_literal ((unsigned char *)buff, dst - buff);
		SET_LOCATION (yylval);
		return LITERAL;
	}

error:
	cb_error (_("Invalid X literal: %s"), text);
	yylval = cb_error_node;
	return LITERAL;
}

static int
scan_h (char *text)
{
	unsigned char	*p;
	long long	val = 0;
	char		buff[48];

	for (p = (unsigned char *)text; *p != '\'' && *p != '\"'; p++) {
		int c = toupper (*p);

		if ('0' <= c && c <= '9') {
			c = c - '0';
		} else if ('A' <= c && c <= 'F') {
			c = c - 'A' + 10;
		} else {
			goto error;
		}

		val = (val << 4) + c;
	}

#ifdef _WIN32
	sprintf (buff, "%I64d", val);
#else
	sprintf (buff, "%lld", val);
#endif
	yylval = cb_build_numeric_literal (0, (unsigned char *)buff, 0);
	SET_LOCATION (yylval);
	return LITERAL;

error:
	cb_error (_("Invalid H literal: %s"), text);
	yylval = cb_error_node;
	return LITERAL;
}

static int
scan_numeric (char *text)
{
	int		sign, scale = 0;
	unsigned char	*s;

	/* get sign */
	sign = (*text == '+') ? 1 : (*text == '-') ? -1 : 0;
	if (sign) {
		text++;
	}

	/* get decimal point */
	s = (unsigned char *)strchr (text, current_program->decimal_point);
	if (s) {
		scale = strlen ((char *)s) - 1;
		memmove (s, s + 1, scale + 1);
	}
/*
	if (strchr (text, (current_program->decimal_point == ',') ? '.' : ',')) {
		cb_error (_("Invalid numeric literal"));
	}
*/
	if (strchr (text, '.')) {
		cb_error (_("Invalid numeric literal"));
	}
	if (strchr (text, ',')) {
		cb_error (_("Invalid numeric literal"));
	}

	yylval = cb_build_numeric_literal (sign, (unsigned char *)text, scale);
	SET_LOCATION (yylval);
	return LITERAL;
}

static int
can_replace (const char *src1, const char *src2, const size_t size,
	     const size_t iteration)
{
	const unsigned char	*p;

	if (strncasecmp (src1, src2, size)) {
		return 0;
	}
	p = (unsigned char *)src1 + size;
	if (isalnum (*p) || *p == '-' || *p == '_') {
		return 0;
	}
	if (iteration) {
		p = (unsigned char *)src1 - 1;
		if (isalnum (*p) || *p == '-' || *p == '_') {
			return 0;
		}
	}
	return 1;
}

static int
scan_picture (char *text)
{
	unsigned char		*p;
	struct cb_level_78	*p78;
	size_t			n;
	size_t			i;
	size_t			size;
	size_t			sizep;
	char			buff[COB_SMALL_BUFF];
	char			buff2[COB_SMALL_BUFF];

	/* normalize the input */
	for (p = (unsigned char *)text; *p; p++) {
		/* unput trailing '.' or ',' */
		if (p[1] == 0 && (*p == '.' || *p == ',')) {
			unput (*p);
			*p = 0;
			break;
		}
		/* upcase */
		*p = toupper (*p);
	}

	if (lev78ptr) {
		memset (buff, 0, sizeof (buff));
		memset (buff2, 0, sizeof (buff2));
		strcpy (buff, text);
		for (p78 = lev78ptr; p78; p78 = p78->next) {
			p = (unsigned char *)CB_LITERAL(CB_VALUE(p78->fld78->values))->data;
			size = strlen (p78->fld78->name);
			sizep = strlen ((char *)p);
			i = 0;
			for (n = 0; n < strlen (buff); n++) {
				if (can_replace (&buff[n], p78->fld78->name, size, n)) {
					memcpy (&buff2[i], p, sizep);
					n += size - 1;
					i += sizep;
				} else {
					buff2[i++] = buff[n];
				}
			}
			buff2[i] = 0;
			strcpy (buff, buff2);
		}
		yylval = cb_build_picture (buff2);
	} else {
		yylval = cb_build_picture (text);
	}
	return PICTURE;
}

static void
count_lines (char *text)
{
	char *p;

	for (p = text; *p; p++) {
		if (*p == '\n') {
			cb_source_line++;
		}
	}
}

void
cb_add_78 (struct cb_field *f)
{
	struct cb_level_78	*p78;

	p78 = cobc_malloc (sizeof(struct cb_level_78));
	p78->fld78 = f;
	p78->next = lev78ptr;
	lev78ptr = p78;
}
