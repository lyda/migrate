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

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "cobpp.h"
#include "parser.h"
#include "scanner.h"
#include "lib/gettext.h"

int yylineno;
const char *yyfilename;
static struct replacement *yyreplacing = NULL;

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
static void set_buffer (int lineno, const char *filename, struct replacement *replacing, YY_BUFFER_STATE buffer);

%}

%x COPY_STATE

%%

<*>"*>".*		{ /* comment */ }

<*>\n			{ ECHO; yylineno++; }

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"INCLUDE"		{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

"AUTHOR." |
"DATE-WRITTEN." |
"DATE-COMPILED." |
"INSTALLATION." |
"SECURITY." {
  if (cobpp_source_format == COBPP_FORMAT_FREE)
    yyerror ("obsolete");
  else
    {
      int c;

      /* skip comments until the end of line */
      while ((c = input ()) != EOF)
	if (c == '\n')
	  break;
      fputs ("\n", yyout);

      /* skip comments on the following lines */
      while (1)
	{
	  int i;
	  for (i = 0; i < 7; i++)
	    fputc (input (), yyout);
	  if ((c = input ()) != ' ')
	    {
	      unput (c);
	      break;
	    }
	  while ((c = input ()) != '\n')
	    if (c == '\n')
	      break;
	  fputs ("\n", yyout);
	}
    }
}

. |
[0-9A-Z_-]+ |
\'[^\'\n]*\' |
\"[^\"\n]*\"		{ ECHO; }

<COPY_STATE>{
  "."			{ BEGIN INITIAL; return '.'; }
  "BY"			{ return BY; }
  "IN"			{ return IN; }
  "OF"			{ return IN; }
  "OFF"			{ return OFF; }
  "REPLACING"		{ return REPLACING; }
  [0-9A-Z_-]+		{ yylval.s = strdup (yytext); return NAME; }
  .			{ }

  \'[^\'\n]*\' |
  \"[^\"\n]*\" {
    yylval.s = substring (yytext, 1);
    return NAME;
  }

  "=="[^=\n]*"==" {
    yylval.s = substring (yytext, 2);
    return TEXT;
  }
}

<<EOF>> {
  yy_delete_buffer (YY_CURRENT_BUFFER);

  /* Terminate at the end of all input */
  if (copybook_stack->next == NULL)
    yyterminate ();

  /* Close the current file */
  if (yyin != stdin)
    fclose (yyin);

  /* Switch to the last buffer */
  set_buffer (copybook_stack->lineno,
	      copybook_stack->filename,
	      copybook_stack->replacing,
	      copybook_stack->buffer);
  copybook_stack = copybook_stack->next;
}

%%

static char *
substring (const char *s, int size)
{
  int len = strlen (yytext) - size * 2;
  char *buff = malloc (len + 1);
  strncpy (buff, yytext + size, len);
  buff[len] = '\0';
  return buff;
}

static void
set_buffer (int lineno, const char *filename,
	    struct replacement *replacing, YY_BUFFER_STATE buffer)
{
  yylineno = lineno;
  yyfilename = filename;
  yyreplacing = replacing;
  yy_switch_to_buffer (buffer);
  fprintf (yyout, "# %d \"%s\"\n", yylineno, yyfilename ? yyfilename : "");
}

void
open_buffer (const char *name, struct replacement *replacing)
{
  char buff[FILENAME_MAX];

  /* Prepare input file */
  if (name == NULL)
    yyin = stdin;
  else
    {
      FILE *fp;
      struct stat st;

      /* Find the copy file */
      if (name[0] != '/' && stat (name, &st) == -1)
	{
	  struct cobpp_path *p;
	  for (p = cobpp_include_path; p; p = p->next)
	    {
	      sprintf (buff, "%s/%s", p->dir, name);
	      if (stat (buff, &st) == 0)
		{
		  name = buff;
		  break;
		}
	    }
	  if (!p)
	    {
	      yyerror ("%s: no such file", name);
	      return;
	    }
	}

      /* Add to dependency list */
      if (cobpp_depend_file)
	{
	  struct cobpp_path *p = malloc (sizeof (struct cobpp_path));
	  p->dir = strdup (name);
	  p->next = NULL;
	  if (!cobpp_depend_list)
	    cobpp_depend_list = p;
	  else
	    {
	      struct cobpp_path *l;
	      for (l = cobpp_depend_list; l->next; l = l->next);
	      l->next = p;
	    }
	}

      /* Open the copy file */
      fp = fopen (name, "r");
      if (!fp)
	{
	  yyerror ("%s: %s", name, strerror (errno));
	  return;
	}
      yyin = fp;
    }

  /* Preserve the current buffer */
  {
    struct copybook *p = malloc (sizeof (struct copybook));
    p->lineno    = yylineno;
    p->filename  = yyfilename;
    p->replacing = yyreplacing;
    p->buffer    = YY_CURRENT_BUFFER;
    p->next      = copybook_stack;
    copybook_stack = p;
  }

  /* Switch to new buffer */
  set_buffer (1, name, replacing, yy_create_buffer (yyin, YY_BUF_SIZE));
}

void
include_copybook (const char *name, const char *lib,
		  struct replacement *replacing)
{
  if (lib)
    {
      char *s = malloc (strlen (name) + strlen (lib) + 2);
      sprintf (s, "%s/%s", lib, name);
      name = s;
    }
  fputc ('\n', yyout);
  open_buffer (name, replacing);
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

void
remove_replacements (void)
{
  yyreplacing = NULL;
}


/*
 * Read line
 */

static int
read_line (char *buff, int size)
{
  int c;
  int n = 0;

  /* FIXME: check buffer overflow */

  /* read characters until the end of line */
  while ((c = getc (yyin)) != EOF)
    switch (c)
      {
      case '\t':
	/* tab expansion */
	if (cobpp_source_format == COBPP_FORMAT_FREE)
	  buff[n++] = c;
	else
	  while ((n + 1) % cobpp_tab_width != 0)
	    buff[n++] = c;
	break;

      case '\r':
	/* end-of-line */
	c = getc (yyin);
	if (c != '\n')
	  ungetc (c, yyin);
	/* fall through */
      case '\n':
	buff[n++] = '\n';
	goto end;

      default:
	buff[n++] = c;
	break;
      }
 end:
  if (n > 0 && buff[n - 1] != '\n')
    {
      yywarn (_("file unterminated by a newline"));
      buff[n++] = '\n';
    }
  buff[n] = '\0';
  if (n == 0)
    return 0;

  /* nothing to be done for free format */
  if (cobpp_source_format == COBPP_FORMAT_FREE)
    return n;

  /* clear the first 6 letters */
  memset (buff, ' ', 6);
  if (n < 6)
    strcpy (buff + 6, " \n");

  /* check the column 7 */
  switch (buff[6])
    {
    case 'D':
      if (cobpp_debug_flag)
	{
	  /* activate debugging line */
	  buff[6] = ' ';
	  break;
	}
      /* fall through */
    case '*':
    case '/':
      /* remove comments */
      strcpy (buff + 6, " \n");
      n = 7;
      break;
    }

  /* remove letters after column 72 */
  if (cobpp_source_format == COBPP_FORMAT_FIXED && n > 73)
    {
      static int count = 0;
      if (cobpp_source_format_inferred || cobpp_warn_trailing_line)
	if (count < 4)
	  {
	    yywarn (_("source line over column 72"));
	    if (cobpp_source_format_inferred && count == 0)
	      yywarn (_("specify source format explicitly"));
	    if (++count == 4)
	      yywarn (_("more warnings inhibited"));
	  }
      strcpy (buff + 72, "\n");
    }

  return strlen (buff);
}

static int
yyinput (char *buff, int max_size)
{
  struct replacement *ls;

  /* Read a line */
  if (read_line (buff, max_size) == 0)
    return YY_NULL;

  /* Line concatenation */
  if (cobpp_source_format != COBPP_FORMAT_FREE)
    {
      char *p;
      int lines = 0;
      while (1)
	{
	  size_t size;
	  char *bp, *np;
	  char lookahead[7];

	  /* look ahead the following line */
	  size = fread (lookahead, 1, 7, yyin);
	  fseek (yyin, - size, SEEK_CUR);
	  if (size != 7 || lookahead[6] != '-')
	    break;

	  /* concatenate lines */
	  bp = buff + strlen (buff) - 1;
	  read_line (bp, max_size - (bp - buff));
	  for (np = bp + 7; *np == ' '; np++);
	  if (*np == '"')
	    np++;
	  else
	    for (; bp[-1] == ' '; bp--);
	  strcpy (bp, np);
	  lines++;
	}

      /* append LF's */
      p = buff + strlen (buff);
      while (lines-- > 0)
	*p++ = '\n';
      *p = '\0';
    }

  /* Do replacement */
  for (ls = yyreplacing; ls; ls = ls->next)
    {
      char *p = buff;
      int len = strlen (buff);
      while ((p = strstr (p, ls->old_text)) != NULL)
	{
	  int old_len = strlen (ls->old_text);
	  int new_len = strlen (ls->new_text);
	  if (old_len == new_len)
	    {
	      memcpy (p, ls->new_text, new_len);
	    }
	  else if (len - old_len + new_len >= max_size)
	    {
	      yyerror (_("source line too long"));
	      exit (1);
	    }
	  else
	    {
	      memmove (p + new_len, p + old_len, len - old_len + 1);
	      memcpy (p, ls->new_text, new_len);
	      len = len - old_len + new_len;
	    }
	  p += new_len;
	}
    }

  return strlen (buff);
}
