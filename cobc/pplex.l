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
%option prefix="pp"

%{

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "cobc.h"
#include "ppparse.h"

static int within_comment = 0;

static struct cb_replace_list *current_replace_list = NULL;

static struct copy_info {
  int line;
  char *file;
  int replacing;
  YY_BUFFER_STATE buffer;
  struct copy_info *next;
} *copy_stack = NULL;

#define YY_INPUT(buf,result,max_size) result = ppinput (buf, max_size);
static int ppinput (char *buf, int max_size);
static void ppecho (const char *text);

static void switch_to_buffer (int lineno, char *filename, YY_BUFFER_STATE buffer);

%}

WORD		[0-9A-Z-]+
NUMRIC_LITERAL	[+-]?[0-9,.]*[0-9]
ALNUM_LITERAL	\"[^\"\n]*\"|\'[^\'\n]*\'

%x PROCESS_STATE DIRECTIVE_STATE COPY_STATE PSEUDO_STATE

%%

<*>^"*".* |
<*>^"/".* |
<*>"*>".*		{ ppecho (" "); }

"PROCESS"		{ BEGIN PROCESS_STATE; }

<PROCESS_STATE>{
  \n			{ BEGIN INITIAL; unput ('\n'); }
  .*			{ cb_warning (_("PROCESS statement is ignored")); }
}

">>"			{ BEGIN DIRECTIVE_STATE; return DIRECTIVE; }

<DIRECTIVE_STATE>{
  \n			{ BEGIN INITIAL; unput ('\n'); }
  "SOURCE"		{ return SOURCE; }
  "FORMAT"		{ return FORMAT; }
  "IS"			{ return IS; }
  "FIXED"		{ return FIXED; }
  "FREE"		{ return FREE; }
  .			{ }
}

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"INCLUDE"		{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

<COPY_STATE>{
  [,;]?\n		{ ECHO; cb_source_line++; }
  [,;]?[ ]+		{ /* ignore */ }
  "."			{ BEGIN INITIAL; return '.'; }
  "=="			{ BEGIN PSEUDO_STATE; return EQEQ; }
  "("			{ return '('; }
  ")"			{ return ')'; }
  "BY"			{ return BY; }
  "IN"			{ return IN; }
  "OF"			{ return OF; }
  "OFF"			{ return OFF; }
  "SUPPRESS"		{ return SUPPRESS; }
  "PRINTING"		{ return PRINTING; }
  "REPLACING"		{ return REPLACING; }
  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{ pplval.s = strdup (yytext); return TOKEN; }
}

<PSEUDO_STATE>{
  [,;]?\n		{ ECHO; cb_source_line++; }
  [,;]?[ ]+		{ pplval.s = strdup (" "); return TOKEN; }
  "=="			{ BEGIN COPY_STATE; return EQEQ; }
  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{ pplval.s = strdup (yytext); return TOKEN; }
}

"AUTHOR" |
"DATE-WRITTEN" |
"DATE-COMPILED" |
"INSTALLATION" |
"SECURITY" {
  /* these words are treated as comments */
  if (cb_verify (cb_author_paragraph, yytext))
    {
      /* skip comments until the end of line */
      int c;
      while ((c = input ()) != EOF)
	if (c == '\n')
	  break;
      unput (c);

      within_comment = 1;
    }
}

"EJECT" |
"SKIP1" |
"SKIP2" |
"SKIP3" {
  /* these words are comments in IBM COBOL */
  if (cb_verify (cb_eject_statement, yytext))
    {
      /* do nothing for now */
    }
  else
    {
      ECHO;
    }
}

[,;]?\n			{ ppecho ("\n"); cb_source_line++; }
[,;]?[ ]+		{ ppecho (" "); }

{WORD} |
{NUMRIC_LITERAL} |
{ALNUM_LITERAL} |
.			{ ppecho (yytext); }

<<EOF>> {
  struct copy_info *p = copy_stack;

  yy_delete_buffer (YY_CURRENT_BUFFER);

  /* Terminate at the end of all input */
  if (p->next == NULL)
    yyterminate ();

  /* Close the current file */
  fclose (ppin);

  /* Switch to the last buffer */
  if (p->replacing)
    pp_set_replace_list (NULL);
  switch_to_buffer (p->line, p->file, p->buffer);

  copy_stack = p->next;
  free (p);
}

%%

void
pp_set_replace_list (struct cb_replace_list *list)
{
  current_replace_list = list;
}

static void
switch_to_buffer (int line, char *file, YY_BUFFER_STATE buffer)
{
  cb_source_line = line;
  cb_source_file = file;
  yy_switch_to_buffer (buffer);
  fprintf (yyout, "# %d \"%s\"\n", line, file);
}

int
ppopen (char *name, struct cb_replace_list *replace_list)
{
  /* Open the copy file */
  ppin = fopen (name, "rb");
  if (!ppin)
    {
      if (cb_source_file)
	cb_error ("%s: %s", name, strerror (errno));
      else
	perror (name);
      return -1;
    }

  /* Add to dependency list */
  if (cb_depend_file)
    cb_depend_list = cb_text_list_add (cb_depend_list, name);

  /* Preserve the current buffer */
  {
    struct copy_info *p = malloc (sizeof (struct copy_info));
    p->line      = cb_source_line;
    p->file      = cb_source_file;
    p->replacing = replace_list ? 1 : 0;
    p->buffer    = YY_CURRENT_BUFFER;
    p->next      = copy_stack;
    copy_stack = p;
  }

  /* Switch to new buffer */
  if (replace_list)
    pp_set_replace_list (replace_list);
  switch_to_buffer (1, name, yy_create_buffer (ppin, YY_BUF_SIZE));
  return 0;
}

int
ppcopy (char *name, const char *lib, struct cb_replace_list *replace_list)
{
  struct stat st;
  char buff1[FILENAME_MAX];
  char buff2[FILENAME_MAX];

  if (lib)
    {
      sprintf (buff1, "%s/%s", lib, name);
      name = buff1;
    }

  /* Find the file */
  if (name[0] != '/' && stat (name, &st) == -1)
    {
      struct cb_text_list *il, *el;
      for (el = cb_extension_list; el; el = el->next)
	{
	  sprintf (buff2, "%s%s", name, el->text);
	  if (stat (buff2, &st) == 0)
	    {
	      name = buff2;
	      goto found;
	    }
	}
      for (il = cb_include_list; il; il = il->next)
	for (el = cb_extension_list; el; el = el->next)
	  {
	    sprintf (buff2, "%s/%s%s", il->text, name, el->text);
	    if (stat (buff2, &st) == 0)
	      {
		name = buff2;
		goto found;
	      }
	  }
      cb_error ("%s: %s", name, strerror (errno));
      return -1;
    }

 found:
  return ppopen (name, replace_list);
}


/*
 * Read line
 */

static int
ppinput_1 (char *buff, int size)
{
  int c;
  int n = 0;

  /* FIXME: check buffer overflow */

  /* read characters until the end of line */
  while ((c = getc (ppin)) != EOF)
    switch (c)
      {
      case '\t':
	/* tab expansion */
	if (cb_source_format == CB_FORMAT_FREE)
	  buff[n++] = c;
	else
	  {
	    buff[n++] = ' ';
	    while (n % cb_tab_width != 0)
	      buff[n++] = ' ';
	  }
	break;

      case '\r':
	/* end-of-line */
	c = getc (ppin);
	if (c != '\n')
	  ungetc (c, ppin);
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
      cb_warning (_("file unterminated by a newline"));
      buff[n++] = '\n';
    }
  buff[n] = '\0';
  if (n == 0)
    return 0;

  /* compiler directives are reserved */
  if (memcmp (buff, ">>", 2) == 0)
    return n;

  /* nothing to do for free format */
  if (cb_source_format == CB_FORMAT_FREE)
    return n;

  /* clear the first 6 letters */
  memset (buff, ' ', 6);
  if (n < 8)
    strcpy (buff + 6, " \n");

  /* check the indicator (column 7) */
  switch (buff[6])
    {
    case ' ':
    case '-':
      break;
    case 'd':
    case 'D':
      if (cb_flag_debugging_line)
	{
	  /* activate debugging line */
	  buff[6] = ' ';
	  break;
	}
      /* fall through */
    case '*':
    case '/':
      /* remove comments */
      memset (buff + 6, ' ', n - 7);
      break;
    default:
      {
	static int last_line = -1;
	if (last_line < cb_source_line - 1)
	  cb_warning (_("invalid indicator `%c' at column 7"), buff[6]);
	last_line = cb_source_line;
      }
      break;
    }

  /* skip comments following AUTHORS, etc. */
  if (within_comment)
    {
      if (buff[7] == ' ')
	memset (buff + 6, ' ', n - 7);
      else
	within_comment = 0;
    }

  /* check the text after cb_text_column */
  if (n > cb_text_column + 1)
    {
      static int last_line = -1;
      if (cb_warn_column_overflow && last_line < cb_source_line - 1)
	{
	  int i;
	  for (i = cb_text_column; i < n; i++)
	    if (buff[i] != ' ' && buff[i] != '\n')
	      {
		/* show warning if it is not whitespaces */
		cb_warning (_("source text after column %d"), cb_text_column);
		break;
	      }
	}
      /* remove it */
      strcpy (buff + cb_text_column, "\n");
      last_line = cb_source_line;
    }

  return strlen (buff);
}

static int
ppinput (char *buff, int max_size)
{
  /* Read a line */
  if (ppinput_1 (buff, max_size) == 0)
    return YY_NULL;

  /* Line concatenation */
  if (cb_source_format != CB_FORMAT_FREE)
    {
      char *p;
      int lines = 0;
      while (1)
	{
	  int i;
	  int mark = 0;
	  int consecutive = 0;
	  size_t size;
	  char *bp, *np;
	  char lookahead[7];

	  /* look ahead the next line */
	  size = fread (lookahead, 1, 7, ppin);
	  fseek (ppin, -size, SEEK_CUR);
	  for (i = 0; i < size; i++)
	    if (lookahead[i] == '\t')
	      goto finish;
	  if (size != 7 || lookahead[6] != '-')
	    goto finish;

	  /* check if string literal is to be continued */
	  size = strlen (buff);
	  for (i = 0; i < size; i++)
	    if (buff[i] == '\'' || buff[i] == '\"')
	      {
		if (mark == 0)
		  {
		    /* literal start */
		    mark = buff[i];
		  }
		else if (mark == buff[i])
		  {
		    if (i == cb_text_column - 1)
		      /* consecutive quotation */
		      consecutive = 1;
		    else
		      /* litaral end */
		      mark = 0;
		  }
	      }

	  /* read the next line */
	  cb_source_line++;
	  bp = buff + strlen (buff) - 1;
	  ppinput_1 (bp, max_size - (bp - buff));
	  for (np = bp + 7; *np == ' '; np++);

	  /* validate concatenation */
	  if (consecutive)
	    {
	      if (np[0] == mark && np[1] == mark)
		np++;
	      else
		{
		invalid:
		  cb_error (_("invalid line concatenation"));
		  return YY_NULL;
		}
	    }
	  else if (mark)
	    {
	      /* literal concatenation */
	      if (*np == mark)
		np++;
	      else
		goto invalid;
	    }
	  else if (*np == '\'' || *np == '\"')
	    {
	      /* separate literals */
	      *bp++;
	    }
	  else
	    {
	      /* word concatenation */
	      for (; bp[-1] == ' '; bp--);
	    }

	  strcpy (bp, np);
	  lines++;
	}
    finish:
      cb_source_line -= lines;

      /* append LF's */
      p = buff + strlen (buff);
      while (lines-- > 0)
	*p++ = '\n';
      *p = '\0';
    }

  return strlen (buff);
}

static struct cb_text_list *text_queue = NULL;

static void
ppecho (const char *text)
{
  if (text_queue == NULL && (text[0] == ' ' || text[0] == '\n'))
    {
      fputs (text, ppout);
    }
  else if (!current_replace_list)
    {
      for (; text_queue; text_queue = text_queue->next)
	fputs (text_queue->text, ppout);
      fputs (text, ppout);
    }
  else
    {
      /* Do replacement */
      struct cb_replace_list *r;
      struct cb_text_list *l, *queue;

      text_queue = cb_text_list_add (text_queue, text);

      for (r = current_replace_list; r; r = r->next)
	{
	  queue = text_queue;
	  for (l = r->old_text; l; l = l->next)
	    {
	      while (l && (l->text[0] == ' ' || l->text[0] == '\n'))
		l = l->next;
	      if (l == NULL)
		break;
	      while (queue
		     && (queue->text[0] == ' ' || queue->text[0] == '\n'))
		queue = queue->next;
	      if (queue == NULL)
		return; /* partial match */
	      if (strcasecmp (l->text, queue->text) != 0)
		break;
	      queue = queue->next;
	    }
	  if (l == NULL)
	    {
	      /* match */
	      for (l = r->new_text; l; l = l->next)
		fputs (l->text, ppout);
	      text_queue = queue ? queue->next : NULL;
	      return;
	    }
	}

      /* no match */
      for (; text_queue; text_queue = text_queue->next)
	fputs (text_queue->text, ppout);
    }
}
