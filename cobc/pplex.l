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

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "cobc.h"
#include "ppparse.h"

static struct cobc_replacement *current_replacement = NULL;

static struct copy_info {
  int line;
  char *file;
  struct cobc_replacement *replacement;
  YY_BUFFER_STATE buffer;
  struct copy_info *next;
} *copy_stack = NULL;

#define YY_INPUT(buf,result,max_size) result = ppinput (buf, max_size);
static int ppinput (char *buf, int max_size);

static char *substring (const char *s, int offset);
static void switch_to_buffer (int lineno, char *filename, struct cobc_replacement *replacing, YY_BUFFER_STATE buffer);

%}

%x DIRECTIVE_STATE COPY_STATE

%%

">>"			{ BEGIN DIRECTIVE_STATE; return DIRECTIVE; }

"COPY"			{ BEGIN COPY_STATE; return COPY; }
"INCLUDE"		{ BEGIN COPY_STATE; return COPY; }
"REPLACE"		{ BEGIN COPY_STATE; return REPLACE; }

"AUTHOR." |
"DATE-WRITTEN." |
"DATE-COMPILED." |
"INSTALLATION." |
"SECURITY." {
  if (cobc_source_format == COBC_FORMAT_FREE)
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

<DIRECTIVE_STATE>{
  \n			{ BEGIN INITIAL; unput ('\n'); }
  "SOURCE"		{ return SOURCE; }
  "FORMAT"		{ return FORMAT; }
  "IS"			{ return IS; }
  "FIXED"		{ return FIXED; }
  "FREE"		{ return FREE; }
  .			{ }
}

<COPY_STATE>{
  "."			{ BEGIN INITIAL; return '.'; }
  "BY"			{ return BY; }
  "IN"			{ return IN; }
  "OF"			{ return IN; }
  "OFF"			{ return OFF; }
  "REPLACING"		{ return REPLACING; }
  [0-9A-Z_-]+		{ pplval.s = strdup (yytext); return NAME; }
  .			{ }

  \'[^\'\n]*\' |
  \"[^\"\n]*\" {
    pplval.s = substring (yytext, 1);
    return NAME;
  }

  "=="[^=\n]*"==" {
    pplval.s = substring (yytext, 2);
    return TEXT;
  }
}

<*>"*>".*		{ /* comment */ }

<*>\n			{ ECHO; cobc_source_line++; }

. |
[0-9A-Z_-]+ |
\'[^\'\n]*\' |
\"[^\"\n]*\"		{ ECHO; }

<<EOF>> {
  yy_delete_buffer (YY_CURRENT_BUFFER);

  /* Terminate at the end of all input */
  if (copy_stack->next == NULL)
    yyterminate ();

  /* Close the current file */
  fclose (ppin);

  /* Switch to the last buffer */
  switch_to_buffer (copy_stack->line, copy_stack->file,
		    copy_stack->replacement, copy_stack->buffer);

  {
    struct copy_info *p = copy_stack;
    copy_stack = p->next;
    free (p);
  }
}

%%

static char *
substring (const char *s, int offset)
{
  int size = strlen (yytext) - offset * 2;
  char *buff = malloc (size + 1);
  strncpy (buff, yytext + offset, size);
  buff[size] = '\0';
  return buff;
}

static void
switch_to_buffer (int line, char *file,
		  struct cobc_replacement *replacement, YY_BUFFER_STATE buffer)
{
  cobc_source_line = line;
  cobc_source_file = file;
  current_replacement = replacement;
  yy_switch_to_buffer (buffer);
  fprintf (yyout, "# %d \"%s\"\n", line, file);
}

int
ppopen (char *name, const char *lib, struct cobc_replacement *replacement)
{
  struct stat st;
  char buff[FILENAME_MAX];

  if (lib)
    {
      char *s = alloca (strlen (name) + strlen (lib) + 2);
      sprintf (s, "%s/%s", lib, name);
      name = s;
    }

  /* Find the file */
  if (name[0] != '/' && stat (name, &st) == -1)
    {
      struct cobc_name_list *l;
      for (l = cobc_include_list; l; l = l->next)
	{
	  sprintf (buff, "%s/%s", l->name, name);
	  if (stat (buff, &st) == 0)
	    {
	      name = buff;
	      break;
	    }
	}
      if (l == NULL)
	{
	  perror (name);
	  return -1;
	}
    }

  /* Open the copy file */
  ppin = fopen (name, "rb");
  if (!ppin)
    {
      perror (name);
      return -1;
    }

  /* Add to dependency list */
  if (cobc_depend_file)
    {
      struct cobc_name_list *p = malloc (sizeof (struct cobc_name_list));
      p->name = strdup (name);
      p->next = NULL;
      if (!cobc_depend_list)
	cobc_depend_list = p;
      else
	{
	  struct cobc_name_list *l;
	  for (l = cobc_depend_list; l->next; l = l->next);
	  l->next = p;
	}
    }

  /* Preserve the current buffer */
  {
    struct copy_info *p = malloc (sizeof (struct copy_info));
    p->line        = cobc_source_line;
    p->file        = cobc_source_file;
    p->replacement = current_replacement;
    p->buffer      = YY_CURRENT_BUFFER;
    p->next        = copy_stack;
    copy_stack = p;
  }

  /* Switch to new buffer */
  switch_to_buffer (1, name, replacement,
		    yy_create_buffer (ppin, YY_BUF_SIZE));
  return 0;
}

struct cobc_replacement *
add_replacement (struct cobc_replacement *replacement,
		 const char *old_text, const char *new_text)
{
  struct cobc_replacement *p = malloc (sizeof (struct cobc_replacement));
  p->old_text = old_text;
  p->new_text = new_text;
  p->next = replacement;
  return p;
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
	if (cobc_source_format == COBC_FORMAT_FREE)
	  buff[n++] = c;
	else
	  while ((n + 1) % cobc_tab_width != 0)
	    buff[n++] = ' ';
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
      yywarn (_("file unterminated by a newline"));
      buff[n++] = '\n';
    }
  buff[n] = '\0';
  if (n == 0)
    return 0;

  /* compiler directives are reserved */
  if (memcmp (buff, ">>", 2) == 0)
    return n;

  /* nothing to do for free format */
  if (cobc_source_format == COBC_FORMAT_FREE)
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
      if (cobc_flag_debugging_line)
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
    default:
      yyerror (_("invalid indicator: `%c'"), buff[6]);
      exit (1);
    }

  /* check the text after cobc_text_column */
  if (n > cobc_text_column + 1)
    {
      static int count = 0;
      if (cobc_warn_column_overflow && count < 4)
	{
	  int i;
	  for (i = cobc_text_column; i < n; i++)
	    if (buff[i] != ' ' && buff[i] != '\n')
	      {
		/* show warning if it is not whitespaces */
		yywarn (_("source text after column %d"), cobc_text_column);
		if (++count == 4)
		  yywarn (_("more warnings inhibited"));
		break;
	      }
	}
      /* remove it */
      strcpy (buff + cobc_text_column, "\n");
    }

  return strlen (buff);
}

static int
ppinput (char *buff, int max_size)
{
  struct cobc_replacement *l;

  /* Read a line */
  if (ppinput_1 (buff, max_size) == 0)
    return YY_NULL;

  /* Line concatenation */
  if (cobc_source_format != COBC_FORMAT_FREE)
    {
      char *p;
      int lines = 0;
      while (1)
	{
	  size_t size;
	  char *bp, *np;
	  char lookahead[7];

	  /* look ahead the following line */
	  size = fread (lookahead, 1, 7, ppin);
	  fseek (ppin, - size, SEEK_CUR);
	  if (size != 7 || lookahead[6] != '-')
	    break;

	  /* concatenate lines */
	  bp = buff + strlen (buff) - 1;
	  ppinput_1 (bp, max_size - (bp - buff));
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
  for (l = current_replacement; l; l = l->next)
    {
      char *p = buff;
      int len = strlen (buff);
      while ((p = strstr (p, l->old_text)) != NULL)
	{
	  int old_len = strlen (l->old_text);
	  int new_len = strlen (l->new_text);
	  if (old_len == new_len)
	    {
	      memcpy (p, l->new_text, new_len);
	    }
	  else if (len - old_len + new_len >= max_size)
	    {
	      yyerror (_("source line too long"));
	      exit (1);
	    }
	  else
	    {
	      memmove (p + new_len, p + old_len, len - old_len + 1);
	      memcpy (p, l->new_text, new_len);
	      len = len - old_len + new_len;
	    }
	  p += new_len;
	}
    }

  return strlen (buff);
}
