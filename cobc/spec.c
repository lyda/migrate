/*
 * Copyright (C) 2003 Keisuke Nishida
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

#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"

enum cb_spec_type {
  CB_SPEC_ANY,
  CB_SPEC_BOOLEAN,	/* `yes', `no' */
  CB_SPEC_STRING,	/* "..." */
  CB_SPEC_ERROR,	/* `warning', `error' */
  CB_SPEC_SUPPORT,	/* `ok', `archaic', `obsolete', `unconformable' */
};

struct cb_spec_entry {
  enum cb_spec_type type;
  const char *tag;
  void *var;
  char *val;
} spec_table[] = {
  {CB_SPEC_STRING,  "include", 0},
  {CB_SPEC_STRING,  "name", &cb_spec.name},
  {CB_SPEC_BOOLEAN, "binary-bigendian", &cb_flag_binary_bigendian},
  {CB_SPEC_ANY,     "binary-size", 0},
  {CB_SPEC_ERROR,   "invalid-value", &cb_spec.flag_value_error},
  {CB_SPEC_SUPPORT, "author-paragraph", &cb_spec.author_paragraph},
  {CB_SPEC_SUPPORT, "memory-size-clause", &cb_spec.memory_size},
  {CB_SPEC_SUPPORT, "multiple-file-tape-clause", &cb_spec.multiple_file_tape},
  {CB_SPEC_SUPPORT, "label-records-clause", &cb_spec.label_records},
  {CB_SPEC_SUPPORT, "value-of-clause", &cb_spec.value_of},
  {CB_SPEC_SUPPORT, "data-records-clause", &cb_spec.data_records},
  {CB_SPEC_SUPPORT, "alter-statement", &cb_spec.alter},
  {CB_SPEC_SUPPORT, "goto-statement-without-name", &cb_spec.goto_without_name},
  {CB_SPEC_SUPPORT, "stop-literal-statement", &cb_spec.stop_literal},
  {CB_SPEC_SUPPORT, "debugging-line", &cb_spec.debugging_mode},
  {CB_SPEC_SUPPORT, "padding-character-clause", &cb_spec.padding_character},
  {CB_SPEC_SUPPORT, "next-sentence-phrase", &cb_spec.next_sentence},
  {0, 0, 0}
};

static char *
read_string (const char *text)
{
  char *p;
  char *s = strdup (text);
  if (*s == '\"')
    s++;
  for (p = s; *p; p++)
    if (*p == '\"')
      *p = '\0';
  return s;
}

int
cb_load_std (const char *name)
{
  char fname[FILENAME_MAX];
  sprintf (fname, "%s/%s.spec", cob_specs_dir, name);
  return cb_load_spec (fname);
}

int
cb_load_spec (const char *fname)
{
  int i, ret, line;
  char buff[BUFSIZ];
  char *s, *e;
  FILE *fp;

  /* initialize the spec table */
  for (i = 0; spec_table[i].tag; i++)
    spec_table[i].val = NULL;

  /* open the spec file */
  fp = fopen (fname, "r");
  if (fp == NULL)
    return -1;

  /* read the spec file */
  line = 0;
  while (fgets (buff, BUFSIZ, fp))
    {
      line++;

      /* skip comments */
      if (buff[0] == '#')
	continue;

      /* skip blank lines */
      for (s = buff; *s; s++)
	if (isgraph (*s))
	  break;
      if (!*s)
	continue;

      /* get the tag */
      s = strpbrk (buff, " \t:=");
      if (!s)
	{
	  fprintf (stderr, "%s:%d: invalid line\n", fname, line);
	  continue;
	}
      *s = 0;

      /* find the entry */
      for (i = 0; spec_table[i].tag; i++)
	if (strcmp (buff, spec_table[i].tag) == 0)
	  break;
      if (!spec_table[i].tag)
	{
	  fprintf (stderr, "%s:%d: unknown tag `%s'\n", fname, line, buff);
	  continue;
	}

      /* get the value */
      for (s++; *s && strchr (" \t:=", *s); s++);
      for (e = s + strlen (s) - 1; e >= s && strchr (" \t\r\n", *e); e--);
      e[1] = 0;
      spec_table[i].val = s;

      /* set the value */
      switch (spec_table[i].type)
	{
	case CB_SPEC_ANY:
	  {
	    if (strcmp (spec_table[i].tag, "binary-size") == 0)
	      {
		if (strcmp (spec_table[i].val, "1-2-4-8") == 0)
		  cb_spec.binary_rep = CB_BINARY_REP_1_2_4_8;
		else if (strcmp (spec_table[i].val, "2-4-8") == 0)
		  cb_spec.binary_rep = CB_BINARY_REP_2_4_8;
		else
		  goto invalid_value;
	      }
	    break;
	  }
	case CB_SPEC_BOOLEAN:
	  {
	    if (strcmp (spec_table[i].val, "yes") == 0)
	      *((int *) spec_table[i].var) = 1;
	    else if (strcmp (spec_table[i].val, "no") == 0)
	      *((int *) spec_table[i].var) = 0;
	    else
	      goto invalid_value;
	    break;
	  }
	case CB_SPEC_STRING:
	  {
	    char *val = read_string (spec_table[i].val);

	    /* include another spec (if appropriate) */
	    if (strcmp (spec_table[i].tag, "include") == 0)
	      {
		char fname[FILENAME_MAX];
		sprintf (fname, "%s/%s", cob_specs_dir, val);
		if (cb_load_spec (fname) != 0)
		  {
		    perror (fname);
		    return -1;
		  }
	      }
	    else
	      {
		*((char **) spec_table[i].var) = val;
	      }
	    break;
	  }
	case CB_SPEC_ERROR:
	  {
	    if (strcmp (spec_table[i].val, "warning") == 0)
	      *((int *) spec_table[i].var) = 0;
	    else if (strcmp (spec_table[i].val, "error") == 0)
	      *((int *) spec_table[i].var) = 1;
	    else
	      goto invalid_value;
	    break;
	  }
	case CB_SPEC_SUPPORT:
	  {
	    if (strcmp (spec_table[i].val, "ok") == 0)
	      *((enum cb_support *) spec_table[i].var) = CB_OK;
	    else if (strcmp (spec_table[i].val, "archaic") == 0)
	      *((enum cb_support *) spec_table[i].var) = CB_ARCHAIC;
	    else if (strcmp (spec_table[i].val, "obsolete") == 0)
	      *((enum cb_support *) spec_table[i].var) = CB_OBSOLETE;
	    else if (strcmp (spec_table[i].val, "unconformable") == 0)
	      *((enum cb_support *) spec_table[i].var) = CB_UNCONFORMABLE;
	    else
	      goto invalid_value;
	    break;
	  }
	invalid_value:
	  fprintf (stderr, "%s:%d: invalid value for `%s'\n",
		   fname, line, spec_table[i].tag);
	  break;
	}
    }

  /* checks for no definition */
  ret = 0;
  for (i = 1; spec_table[i].tag; i++)
    if (spec_table[i].val == NULL)
      {
	fprintf (stderr, "%s: no definition of `%s'\n",
		 fname, spec_table[i].tag);
	ret = -1;
      }
  if (ret != 0)
    return ret;

  fclose (fp);
  return 0;
}
