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
#include <ctype.h>

#include "cobc.h"
#include "codegen.h"
#include "parser.h"
#include "scanner.h"
#include "reserved.h"

cob_tree curr_file;
int curr_division = 0;
int start_condition = 0;
int need_subscripts = 0;
int in_procedure = 0;

/* save the last yytext */
int last_lineno;
char last_text[YY_BUF_SIZE];
static int this_lineno = 0;
static char this_text[YY_BUF_SIZE] = "";
#define SAVE_YYTEXT				\
{						\
  last_lineno = this_lineno;			\
  this_lineno = cob_orig_lineno;		\
  strcpy (last_text, this_text);		\
  strcpy (this_text, yytext);			\
}

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

<*>[ \t,;]+ { /* ignore */ }

<*>\n {
  if (cob_stabs_flag && in_procedure)
    stabs_line ();
  cob_orig_lineno++;
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
  yylval.tree = install_literal (yytext);
  return INTEGER_LITERAL;
}

[+-]?[0-9]*[.,]?[0-9]+ {
  /* numeric literal */
  char sign = yytext[0];
  SAVE_YYTEXT;
  if (sign == '+' || sign == '-')
    yytext++;
  yylval.tree = install_literal (yytext);
  if (sign == '-')
    invert_literal_sign (yylval.tree);
  if (strchr (yytext, decimal_comma ? '.' : ','))
    yyerror ("wrong decimal point character in numeric literal");
  return NUMERIC_LITERAL;
}

[A-Z0-9]([A-Z0-9-]*[A-Z0-9]+)? {
  /* symbol or label */
  SAVE_YYTEXT;
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

"<="		{ return LE; }
">="		{ return GE; }
"**"		{ return '^'; }
.		{ return yytext[0]; }

<ID_STATE>{
  [A-Z0-9_-]+ {
    BEGIN INITIAL;
    SAVE_YYTEXT;
    yylval.str = strdup (yytext);
    return ID_TOK;
  }
  .		{ /* ignore */ }
}

<COMMENT_STATE>{
  .*		{ BEGIN INITIAL; }
}

<PICTURE_STATE>{
  "IS"		{ /* ignore */ }
  [^ \t\n;]+	{
    BEGIN INITIAL;
    SAVE_YYTEXT;
    return scan_picture (yytext);
  }
}

<<EOF>> {
  SAVE_YYTEXT;
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
  yylval.tree = install_literal (buff);
  return NONNUMERIC_LITERAL;
}

static int
scan_x (char *text)
{
  int high = 1;
  char *src = text;
  char *dst = alloca (strlen (text));
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
      yylval.tree = install_literal (dst);
      return NONNUMERIC_LITERAL;
    }
  else
    {
    error:
      yyerror ("invalid X literal: %s", text);
      yylval.tree = install_literal (dst);
      return NONNUMERIC_LITERAL;
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
    case CDIV_DATA:
      yylval.tree = install (yytext, SYTB_VAR, 1);
      return SYMBOL_TOK;

    default:
      /* if symbol was defined at the data division */
      if ((yylval.tree = lookup_symbol (yytext)) != NULL)
	{
	  if (COB_FIELD_TYPE (yylval.tree) == '8')
	    return VARCOND;
	  else if (COB_FIELD_TYPE (yylval.tree) == '!')
	    return SPECIAL_TOK;
	  else if (COB_FIELD_TYPE (yylval.tree) == '@')
	    return CLASS_TOK;
	  else if (is_subscripted (yylval.tree))
	    return SUBSCVAR;
	  else
	    return VARIABLE;
	}
      /* otherwise, it should be a label */
      else
	{
	  yylval.tree = install_label (yytext);
	  return SYMBOL_TOK;
	}
    }
}

#define PIC_ALPHABETIC		0x01
#define PIC_NUMERIC		0x02
#define PIC_NATIONAL		0x04
#define PIC_EDITED		0x08
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHANUMERIC_EDITED	(PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED	(PIC_NUMERIC | PIC_EDITED)
#define PIC_NATIONAL_EDITED	(PIC_NATIONAL | PIC_EDITED)

static int
scan_picture (char *text)
{
  char *p;
  unsigned char type = 0;
  int idx = 0;
  int size = 0;
  int decimals = 0;
  int s_count = 0;
  int v_count = 0;
  int buff_size = 9;
  unsigned char *buff = malloc (buff_size);

  /* allocate new picture */
  struct cob_picture *pic = malloc (sizeof (struct cob_picture));
  pic->str = "";
  pic->type = 0;
  pic->size = 0;
  pic->decimals = 0;

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

  for (p = text; *p; p++)
    {
      int n = 1;
      char c = *p;

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

      /* check grammar and type */
      /* FIXME: need more error check */
      switch (c)
	{
	case 'A':
	  type |= PIC_ALPHABETIC;
	  break;

	case 'X':
	  type |= PIC_ALPHANUMERIC;
	  break;

	case '9':
	  type |= PIC_NUMERIC;
	  if (v_count)
	    decimals += n;
	  break;

	case 'N':
	  type |= PIC_NATIONAL;
	  break;

	case 'S':
	  type |= PIC_NUMERIC;
	  if (type & PIC_ALPHABETIC)
	    goto error;
	  s_count += n;
	  if (s_count > 1 || idx != 0)
	    goto error;
	  break;

	case ',':
	case '.':
	  type |= PIC_NUMERIC_EDITED;
	  if ((decimal_comma && c == '.') || (!decimal_comma && c == ','))
	    break;
	  /* fall through */
	case 'V':
	  type |= PIC_NUMERIC;
	  if (type & PIC_ALPHABETIC)
	    goto error;
	  v_count += n;
	  if (v_count > 1)
	    goto error;
	  break;

	case 'P':
	  type |= PIC_NUMERIC;
	  if (type & PIC_ALPHABETIC)
	    goto error;
	  {
	    int at_beginning =
	         (idx == 0)					 /* P... */
	      || (idx == 2 && buff[0] == 'S')			 /* SP... */
	      || (idx == 2 && buff[0] == 'V')			 /* VP... */
	      || (idx == 4 && buff[0] == 'V' && buff[2] == 'V'); /* SVP... */
	    int at_end =
	         (p[1] == 0)					 /* ...P */
	      || (p[1] == 'V' && p[2] == 0);			 /* ...PV */
	    if (!at_beginning && !at_end)
	      goto error;
	    if (at_beginning)
	      v_count++;		/* implicit V */
	    if (v_count)
	      decimals += n;
	    else
	      decimals -= n;
	  }
	  break;

	case '0': case 'B': case '/':
	  type |= PIC_EDITED;
	  break;

	case '*': case 'Z':
	  type |= PIC_NUMERIC_EDITED;
	  if (type & PIC_ALPHABETIC)
	    goto error;
	  if (v_count)
	    decimals += n;
	  break;

	case '+': case '-':
	  type |= PIC_NUMERIC_EDITED;
	  if (type & PIC_ALPHABETIC)
	    goto error;
	  /* FIXME: need more check */
	  break;

	case 'C':
	  type |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'R' && p[2] == 0))
	    goto error;
	  break;

	case 'R':
	  break;

	case 'D':
	  type |= PIC_NUMERIC_EDITED;
	  if (!(p[1] == 'B' && p[2] == 0))
	    goto error;
	  break;

	default:
	  if (c == currency_symbol)
	    {
	      type |= PIC_NUMERIC_EDITED;
	      /* FIXME: need more check */
	      break;
	    }

	  goto error;
	}

      /* calculate size */
      if (c != 'S' && c != 'V' && c != 'P')
	size += n;
      if (c == 'N')
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
	  buff[idx++] = n % 256;
	  n /= 256;
	}
    }
  buff[idx] = 0;

  /* set picture */
  pic->str = buff;
  pic->size = size;
  pic->decimals = decimals;

  /* set picture type */
  switch (type)
    {
    case PIC_ALPHABETIC:		pic->type = 'A'; break;
    case PIC_NUMERIC:			pic->type = '9'; break;
    case PIC_ALPHANUMERIC:		pic->type = 'X'; break;
    case PIC_NATIONAL:			pic->type = 'X'; break;
    case PIC_EDITED:
    case PIC_NUMERIC_EDITED:		pic->type = 'E'; break;
    case PIC_ALPHABETIC | PIC_EDITED:
    case PIC_ALPHANUMERIC_EDITED:	pic->type = 'E'; break;
    case PIC_NATIONAL_EDITED:		pic->type = 'E'; break;
    default:				goto error;
    }

  yylval.pic = pic;
  return PICTURE_TOK;

 error:
  yyerror ("invalid picture string");
  yylval.pic = pic;
  return PICTURE_TOK;
}
