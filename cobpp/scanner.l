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

%option 8bit
%option caseless
%option noyywrap
%option never-interactive

%{
#include <stdio.h>
#include <string.h>

#include "cobpp.h"
#include "parser.h"
#include "scanner.h"

#define YY_NO_UNPUT

int yylineno;
const char *yyfilename;
static struct replacement *yyreplacing = NULL;

static int display_line = 0;
static const char *copybook_name = NULL;
static struct replacement *copybook_replacing = NULL;

static struct copybook {
  int lineno;
  const char *filename;
  struct replacement *replacing;
  YY_BUFFER_STATE buffer;
  struct copybook *next;
} *copybook_stack = NULL;

#define YY_INPUT(buf,result,max_size) result = yyinput (buf, max_size);
static int yyinput (char *buf, int max_size);

static char *substring (const char *s, int size);
static void set_buffer (int lineno, const char *filename,
			struct replacement *replacing, YY_BUFFER_STATE buffer);
%}

%x COPY_STATE

%%

<*>^[*/D].* {
  /* Comment */
  if (yytext[0] == 'D' && cob_debug_flag)
    /* Activate debugging code */
    yyless(1);
}

<*>\n-[ \t]*[\"\']? {
  /* Line connection */
  yylineno++;
  display_line = 1;
}

<*>\n {
  ECHO;
  yylineno++;

  if (copybook_name)
    {
      open_buffer (copybook_name, copybook_replacing);
      copybook_name = NULL;
    }
  else if (display_line)
    {
      fprintf (yyout, "# %d \"%s\"\n", yylineno, yyfilename);
      display_line = 0;
    }
}

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

. |
[ \t]* |
[0-9A-Z_-]+ |
\'[^\'\n]*\' |
\"[^\"\n]*\" { ECHO; }

<COPY_STATE>","		{ return ','; }
<COPY_STATE>"."		{ BEGIN INITIAL; return '.'; }
<COPY_STATE>"BY"	{ return BY; }
<COPY_STATE>"OFF"	{ return OFF; }
<COPY_STATE>"REPLACING"	{ return REPLACING; }
<COPY_STATE>[0-9A-Z_-]+ { yylval.s = strdup (yytext); return NAME; }
<COPY_STATE>.		{ }

<COPY_STATE>\'[^\'\n]*\' |
<COPY_STATE>\"[^\"\n]*\" {
  yylval.s = substring (yytext, 1);
  return NAME;
}

<COPY_STATE>"=="[^=\n]*"==" {
  yylval.s = substring (yytext, 2);
  return TEXT;
}

<<EOF>> {
  yy_delete_buffer (YY_CURRENT_BUFFER);

  /* Terminate at the end of all input */
  if (copybook_stack->next == NULL)
    yyterminate ();

  /* Switch to the last buffer */
  set_buffer (copybook_stack->lineno,
	      copybook_stack->filename,
	      copybook_stack->replacing,
	      copybook_stack->buffer);
  copybook_stack = copybook_stack->next;
}

%%
/*
 * Public functions
 */

static void
set_buffer (int lineno, const char *filename,
	    struct replacement *replacing, YY_BUFFER_STATE buffer)
{
  yylineno = lineno;
  yyfilename = filename;
  yyreplacing = replacing;
  yy_switch_to_buffer (buffer);
  fprintf (yyout, "# %d \"%s\"\n", yylineno, yyfilename);
}

void
open_buffer (const char *name, struct replacement *replacing)
{
  /* Preserve the current buffer */
  struct copybook *p = malloc (sizeof (struct copybook));
  p->lineno    = yylineno;
  p->filename  = yyfilename;
  p->replacing = yyreplacing;
  p->buffer    = YY_CURRENT_BUFFER;
  p->next      = copybook_stack;
  copybook_stack = p;

  /* Switch to new buffer */
  if (name == NULL)
    {
      name = "";
      yyin = stdin;
    }
  else
    {
      yyin = fopen (name, "r");
      if (!yyin)
	{
	  perror (name);
	  exit (1);
	}
    }
  set_buffer (1, name, replacing, yy_create_buffer (yyin, YY_BUF_SIZE));
}

void
include_copybook (const char *name, struct replacement *replacing)
{
  /* delay copying until the end of line */
  copybook_name = strdup (name);
  copybook_replacing = replacing;
}

struct replacement *
add_replacement (struct replacement *replacing,
		 const char *old_text, const char *new_text)
{
  struct replacement *p = malloc (sizeof (struct replacement));
  p->old_text = old_text;
  p->new_text = new_text;
  p->next = replacing;
  return p;
}


/*
 * Local functions
 */

static char *
substring (const char *s, int size)
{
  int len = strlen (yytext) - size * 2;
  char *buff = malloc (len + 1);
  strncpy (buff, yytext + size, len);
  buff[len] = '\0';
  return buff;
}

static int
yyinput (char *buf, int max_size)
{
  int len;
  struct replacement *ls;

  /* Read a line */
  if (fgets (buf, max_size, yyin) == NULL)
    return YY_NULL;

  /* Check the length of the line */
  len = strlen (buf);
  if (len >= max_size / cob_tab_width)
    {
      fprintf (stderr, "%s:%d: source line too long\n", yyfilename, yylineno);
      exit (1);
    }

  /* Regularize the end of line (i.e. \n) */
  switch (buf[len - 1])
    {
    case '\n':
      if (buf[len - 2] != '\r')
	break;
      buf[--len] = '\0';
      /* fall throw */
    case '\r':
      buf[len - 1] = '\n';
      break;
    default:
      buf[len++] = '\n';
      buf[len] = '\0';
      break;
    }

  /* If fixed format... */
  if (cob_format == COB_FORMAT_FIXED)
    {
      /* Tab expansion */
      int i, column = 0;
      char *p = buf;
      static char buf_copy[YY_READ_BUF_SIZE];
      memcpy (buf_copy, buf, len + 1);
      for (i = 0; i <= len; i++)
	if (buf_copy[i] != '\t')
	  {
	    *p++ = buf_copy[i]; column++;
	  }
	else
	  {
	    *p++ = ' '; column++;
	    while (column % cob_tab_width != 0)
	      {
		*p++ = ' '; column++;
	      }
	  }

      /* Remove the letters after column 72  */
      len = strlen (buf);
      if (len > 73)
	strcpy (buf + 72, "\n");

      /* Remove the first 6 letters */
      if (len > 6)
	strcpy (buf, buf + 6);
      else
	strcpy (buf, "\n");
    }

  /* Do replacement */
  for (ls = yyreplacing; ls; ls = ls->next)
    {
      char *p = buf;
      while ((p = strstr (p, ls->old_text)) != NULL)
	{
	  int old_len = strlen (ls->old_text);
	  int new_len = strlen (ls->new_text);
	  if (old_len == new_len)
	    {
	      strncpy (p, ls->new_text, new_len);
	    }
	  else if (old_len > new_len)
	    {
	      strncpy (p, ls->new_text, new_len);
	      strcpy (p + new_len, p + old_len);
	    }
	  else
	    {
	      /* FIXME: need to check buffer overflow */
	      char *s = strdup (p + old_len);
	      strncpy (p, ls->new_text, new_len);
	      strcpy (p + new_len, s);
	      free (s);
	    }
	  p += new_len;
	}
    }

  return strlen (buf);
}
