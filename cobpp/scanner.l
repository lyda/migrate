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

static int display_line = 0;

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

<*>\n {
  /* end of line */
  ECHO;
  yylineno++;

  if (display_line)
    {
      fprintf (yyout, "# %d \"%s\"\n", yylineno, yyfilename ? yyfilename : "");
      display_line = 0;
    }
}

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"INCLUDE"		{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

"AUTHOR." |
"DATE_WRITTEN." |
"DATE_COMPILED." |
"INSTALLATION." |
"SECURITY." {
  if (cob_file_format == COB_FORMAT_FREE)
    yyerror ("obsolete");
  else
    {
      while (input () != '\n');
      fputs ("\n", yyout);
      while (1)
	{
	  int i, c;
	  for (i = 0; i < 7; i++)
	    input ();
	  if ((c = input ()) == ' ')
	    {
	      while (input () != '\n');
	      fputs ("       \n", yyout);
	    }
	  else
	    {
	      fputs ("       ", yyout);
	      unput (c);
	      break;
	    }
	}
    }
}

. |
[ \t]* |
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
	  struct cob_path *p;
	  for (p = cob_include_path; p; p = p->next)
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
      if (cob_depend_file)
	{
	  struct cob_path *p = malloc (sizeof (struct cob_path));
	  p->dir = strdup (name);
	  p->next = NULL;
	  if (!cob_depend_list)
	    cob_depend_list = p;
	  else
	    {
	      struct cob_path *l;
	      for (l = cob_depend_list; l->next; l = l->next);
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
  int len;

  /* Read a line */
  if (fgets (buff, size, yyin) == NULL)
    return 0;

  /* Regularize the end-of-line to LF */
  len = strlen (buff);
  switch (buff[len - 1])
    {
    case '\n':
      if (buff[len - 2] != '\r')
	break;
      buff[--len] = '\0';
      /* fall throw */
    case '\r':
      buff[len - 1] = '\n';
      break;
    default:
      buff[len++] = '\n';
      buff[len] = '\0';
      break;
    }

  /* Nothing to be done for free format */
  if (cob_file_format == COB_FORMAT_FREE)
    return len;

  /* Tab expansion */
  if (strchr (buff, '\t') != NULL)
    {
      int i, column = 0;
      char *p = buff;
      char copy[len + 1];
      memcpy (copy, buff, len + 1);
      for (i = 0; i <= len; i++)
	if (copy[i] != '\t')
	  {
	    *p++ = copy[i];
	    column++;
	  }
	else
	  do {
	    *p++ = ' ';
	    column++;
	  } while (column % cob_tab_width != 0);
    }

  /* clear the first 6 letters */
  memset (buff, ' ', 6);
  if (len < 6)
    strcpy (buff + 6, " \n");

  /* Remove the letters after column 72  */
  len = strlen (buff);
  if (cob_file_format != COB_FORMAT_SEMI_FIXED && len > 73)
    strcpy (buff + 72, "\n");

  switch (buff[6])
    {
    case 'D':
      /* Debug line */
      if (cob_debug_flag)
	{
	  buff[6] = ' ';
	  break;
	}
      /* fall through */
    case '*':
    case '/':
      /* Remove comments */
      strcpy (buff + 6, " \n");
      break;
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
  if (cob_file_format == COB_FORMAT_FIXED
      || cob_file_format == COB_FORMAT_SEMI_FIXED)
    {
      while (1)
	{
	  int i;
	  size_t size;
	  char lookahead[7];

	  /* look ahead the following line */
	  size = fread (lookahead, 1, 7, yyin);
	  fseek (yyin, - size, SEEK_CUR);
	  if (size != 7 || lookahead[6] != '-')
	    break;

	  /* concatenate lines */
	  size = strlen (buff) - 1;
	  read_line (buff + size, max_size - size);
	  for (i = size + 7; buff[i] == ' '; i++);
	  if (buff[i] == '"')
	    i++;
	  else
	    for (; buff[size - 1] == ' '; size--);
	  strcpy (buff + size, buff + i);

	  yylineno++;
	  display_line = 1;
	}
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
