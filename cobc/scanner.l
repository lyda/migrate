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
#include "scanner.h"
#include "codegen.h"
#include "reserved.h"
#include "lib/gettext.h"

int cobc_in_procedure = 0;

char *cobc_source_file = NULL;
int cobc_source_line = 0;

int cobc_last_line;
char cobc_last_text[YY_BUF_SIZE];
static int this_lineno = 0;
static char this_text[YY_BUF_SIZE] = "";
#define SAVE_YYTEXT				\
{						\
  cobc_last_line = this_lineno;			\
  this_lineno = cobc_source_line;		\
  strcpy (cobc_last_text, this_text);		\
  strcpy (this_text, yytext);			\
}

static int read_literal (char mark);
static int scan_x (char *text);
static int scan_numeric (char *text);
static int scan_picture (char *text);

%}

%x PICTURE_STATE FUNCTION_STATE

%%

<*>[ \t,;]+ { /* ignore */ }

<*>\n { yylloc.first_line = ++cobc_source_line; }

^"#".* {
  /* line directive */
  char *endp;
  cobc_source_line = strtol (yytext + 2, &endp, 10) - 1;
  cobc_source_file = strdup (strchr (endp, '"') + 1);
  strrchr (cobc_source_file, '"')[0] = '\0';
  yylloc.text = cobc_source_file;
}

"PIC" |
"PICTURE"	{ BEGIN PICTURE_STATE; }
"FUNCTION"	{ BEGIN FUNCTION_STATE; }

[\'\"] {
  /* string literal */
  SAVE_YYTEXT;
  return read_literal (yytext[0]);
}

X\'[^\'\n]*\' |
X\"[^\"\n]*\" {
  /* X string literal */
  SAVE_YYTEXT;
  return scan_x (yytext + 2);
}

[0-9]+ {
  /* integer literal */
  SAVE_YYTEXT;
  yylval.tree = make_numeric_literal (0, yytext, 0);
  return INTEGER_LITERAL;
}

[+-]?[0-9]*[.,]?[0-9]+ {
  /* numeric literal */
  SAVE_YYTEXT;
  return scan_numeric (yytext);
}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  int token;
  SAVE_YYTEXT;

  /* reserved word */
  token = lookup_reserved_word (yytext);
  if (token != 0)
    return token;

  /* user word */
  yylval.word = lookup_user_word (yytext);
  if (cobc_in_procedure)
    {
      if (!yylval.word->item || COBC_LABEL_P (yylval.word->item))
	{
	  return LABEL_WORD;
	}
      if (COBC_BUILTIN_P (yylval.word->item))
	{
	  yylval.tree = yylval.word->item;
	  return MNEMONIC_NAME;
	}
      if (COBC_CLASS_P (yylval.word->item))
	{
	  yylval.tree = yylval.word->item;
	  return CLASS_NAME;
	}
    }
  return WORD;
}

"<="		{ return LE; }
">="		{ return GE; }
"**"		{ return '^'; }
.		{ return yytext[0]; }

<PICTURE_STATE>{
  "IS"		{ /* ignore */ }
  [^ \t\n;]+	{
    BEGIN INITIAL;
    SAVE_YYTEXT;
    return scan_picture (yytext);
  }
}

<FUNCTION_STATE>{
  [a-z0-9-]+	{
    BEGIN INITIAL;
    SAVE_YYTEXT;
    yylval.str = strdup (yytext);
    return FUNCTION_NAME;
  }
  .		{ return yytext[0]; }
}

<<EOF>> {
  SAVE_YYTEXT;
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
	yyerror (_("too large literal"));
    }
  *p = 0;
  yylval.tree = make_nonnumeric_literal (buff);
  return NONNUMERIC_LITERAL;
}

static int
scan_x (char *text)
{
  int high = 1;
  char *src = text;
  char dst[strlen (text)];
  char *p = dst;
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
	*p = c << 4;
      else
	*p++ += c;

      src++;
      high = 1 - high;
    }
  *p = '\0';

  if (high)
    {
      yylval.tree = make_nonnumeric_literal (dst);
      return NONNUMERIC_LITERAL;
    }
  else
    {
    error:
      yyerror (_("invalid X literal: %s"), text);
      yylval.tree = make_nonnumeric_literal (dst);
      return NONNUMERIC_LITERAL;
    }
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
  s = strchr (text, cob_decimal_point);
  if (s) {
    decimals = strlen (s) - 1;
    memmove (s, s + 1, decimals + 1);
  }
  if (strchr (text, (cob_decimal_point == ',') ? '.' : ','))
    yyerror (_("invalid numeric literal"));

  yylval.tree = make_numeric_literal (sign, text, decimals);
  return NUMERIC_LITERAL;
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

  yylval.pict = yylex_picture (text);
  return PICTURE_TOK;
}


/*
 * Picture scanner
 */

#define PIC_ALPHABETIC		0x01
#define PIC_NUMERIC		0x02
#define PIC_NATIONAL		0x04
#define PIC_EDITED		0x08
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHANUMERIC_EDITED	(PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED	(PIC_NUMERIC | PIC_EDITED)
#define PIC_NATIONAL_EDITED	(PIC_NATIONAL | PIC_EDITED)

struct cobc_picture *
yylex_picture (char *str)
{
  char *p;
  char category = 0;
  int idx = 0;
  int size = 0;
  int digits = 0;
  int decimals = 0;
  int s_count = 0;
  int v_count = 0;
  int buff_size = 9;
  unsigned char *buff = malloc (buff_size);
  struct cobc_picture *pic = make_picture ();

  for (p = str; *p; p++)
    {
      int n = 1;
      unsigned char c = *p;

    repeat:
      /* count the number of repeated chars */
      while (p[1] == c)
	p++, n++;

      /* add parenthesized numbers */
      if (p[1] == '(')
	{
	  int i = 0;
	  for (p += 2; *p != ')'; p++)
	    if (!isdigit (*p))
	      goto error;
	    else
	      i = i * 10 + (*p - '0');
	  n += i - 1;
	  goto repeat;
	}

      /* check grammar and category */
      /* FIXME: need more error check */
      switch (c)
	{
	case 'A':
	  category |= PIC_ALPHABETIC;
	  break;

	case 'X':
	  category |= PIC_ALPHANUMERIC;
	  break;

	case '9':
	  category |= PIC_NUMERIC;
	  digits += n;
	  if (v_count)
	    decimals += n;
	  break;

	case 'N':
	  category |= PIC_NATIONAL;
	  break;

	case 'S':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  s_count += n;
	  if (s_count > 1 || idx != 0)
	    goto error;
	  continue;

	case ',':
	case '.':
	  category |= PIC_NUMERIC_EDITED;
	  if (c != cob_decimal_point)
	    break;
	  /* fall through */
	case 'V':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  v_count += n;
	  if (v_count > 1)
	    goto error;
	  break;

	case 'P':
	  category |= PIC_NUMERIC;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  {
	    int at_beginning =
	         (idx == 0)					 /* P... */
	      || (idx == 2 && buff[0] == 'V');			 /* VP... */
	    int at_end =
	         (p[1] == 0)					 /* ...P */
	      || (p[1] == 'V' && p[2] == 0);			 /* ...PV */
	    if (!at_beginning && !at_end)
	      goto error;
	    if (at_beginning)
	      v_count++;		/* implicit V */
	    digits += n;
	    if (v_count)
	      decimals += n;
	    else
	      decimals -= n;
	  }
	  break;

	case '0': case 'B': case '/':
	  category |= PIC_EDITED;
	  break;

	case '*': case 'Z':
	  category |= PIC_NUMERIC_EDITED;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  digits += n;
	  if (v_count)
	    decimals += n;
	  break;

	case '+': case '-':
	  category |= PIC_NUMERIC_EDITED;
	  if (category & PIC_ALPHABETIC)
	    goto error;
	  digits += n - 1;
	  /* FIXME: need more check */
	  break;

	case 'C':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'R' && p[2] == 0))
	    goto error;
	  p++;
	  break;

	case 'D':
	  category |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'B' && p[2] == 0))
	    goto error;
	  p++;
	  break;

	default:
	  if (c == cob_currency_symbol)
	    {
	      category |= PIC_NUMERIC_EDITED;
	      digits += n - 1;
	      /* FIXME: need more check */
	      break;
	    }

	  goto error;
	}

      /* calculate size */
      if (c != 'V' && c != 'P')
	size += n;
      if (c == 'C' || c == 'D' || c == 'N')
	size += n;

      /* allocate enough pic buffer */
      while (idx + n / 64 + 1 > buff_size)
	{
	  buff_size *= 2;
	  buff = realloc (buff, buff_size);
	}

      /* store in the buffer */
      while (n > 0)
	{
	  buff[idx++] = c;
	  buff[idx++] = (n < 256) ? n : 255;
	  n -= 255;
	}
    }
  buff[idx] = 0;

  /* set picture */
  pic->str = buff;
  pic->size = size;
  pic->digits = digits;
  pic->decimals = decimals;
  pic->have_sign = s_count;

  /* set picture category */
  switch (category)
    {
    case PIC_ALPHABETIC:		pic->category = 'A'; break;
    case PIC_NUMERIC:			pic->category = '9'; break;
    case PIC_ALPHANUMERIC:
    case PIC_NATIONAL:			pic->category = 'X'; break;
    case PIC_NUMERIC_EDITED:		pic->category = '0'; break;
    case PIC_EDITED:
    case PIC_ALPHABETIC | PIC_EDITED:
    case PIC_ALPHANUMERIC_EDITED:
    case PIC_NATIONAL_EDITED:		pic->category = 'E'; break;
    default:				goto error;
    }

  return pic;

 error:
  yyerror (_("invalid picture string"));
  return pic;
}
