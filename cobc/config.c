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
#include "libcob.h"

#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT
#define CB_CONFIG_ANY(type,var,name)	type var;
#define CB_CONFIG_INT(var,name)		int var;
#define CB_CONFIG_STRING(var,name)	const char *var;
#define CB_CONFIG_BOOLEAN(var,name)	int var;
#define CB_CONFIG_SUPPORT(var,name)	enum cb_support var;
#include "config.def"

enum cb_config_type {
  ANY,
  INT,		/* integer */
  STRING,	/* "..." */
  BOOLEAN,	/* 'yes', 'no' */
  SUPPORT,	/* 'ok', 'archaic', 'obsolete',
		   'skip', 'ignore', 'unconformable' */
};

struct {
  enum cb_config_type type;
  const char *name;
  void *var;
  char *val;
} config_table[] = {
  {STRING, "include", 0, 0},
#undef CB_CONFIG_ANY
#undef CB_CONFIG_INT
#undef CB_CONFIG_STRING
#undef CB_CONFIG_BOOLEAN
#undef CB_CONFIG_SUPPORT
#define CB_CONFIG_ANY(type,var,name)	{ANY, name, &var, 0},
#define CB_CONFIG_INT(var,name)		{INT, name, &var, 0},
#define CB_CONFIG_STRING(var,name)	{STRING, name, &var, 0},
#define CB_CONFIG_BOOLEAN(var,name)	{BOOLEAN, name, &var, 0},
#define CB_CONFIG_SUPPORT(var,name)	{SUPPORT, name, &var, 0},
#include "config.def"
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
  sprintf (fname, "%s/%s.conf", cob_config_dir, name);
  return cb_load_conf (fname, 1);
}

int
cb_load_conf (const char *fname, int check_nodef)
{
  int i, ret, line;
  char buff[BUFSIZ];
  char *s, *e;
  const char *name, *val;
  void *var;
  FILE *fp;

  /* initialize the config table */
  if (check_nodef)
    for (i = 0; config_table[i].name; i++)
      config_table[i].val = NULL;

  /* open the config file */
  fp = fopen (fname, "r");
  if (fp == NULL)
    {
      perror (fname);
      return -1;
    }

  /* read the config file */
  ret = 0;
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
	  ret = -1;
	  continue;
	}
      *s = 0;

      /* find the entry */
      for (i = 0; config_table[i].name; i++)
	if (strcmp (buff, config_table[i].name) == 0)
	  break;
      if (!config_table[i].name)
	{
	  fprintf (stderr, "%s:%d: unknown tag '%s'\n", fname, line, buff);
	  ret = -1;
	  continue;
	}

      /* get the value */
      for (s++; *s && strchr (" \t:=", *s); s++);
      for (e = s + strlen (s) - 1; e >= s && strchr (" \t\r\n", *e); e--);
      e[1] = 0;
      config_table[i].val = s;

      /* set the value */
      name = config_table[i].name;
      var = config_table[i].var;
      val = config_table[i].val;
      switch (config_table[i].type)
	{
	case ANY:
	  {
	    if (strcmp (name, "source-format") == 0)
	      {
		if (strcmp (val, "auto") == 0)
		  goto unsupported_value;
		else if (strcmp (val, "free") == 0)
		  cb_source_format = CB_FORMAT_FREE;
		else if (strcmp (val, "fixed") == 0)
		  cb_source_format = CB_FORMAT_FIXED;
		else
		  goto invalid_value;
	      }
	    else if (strcmp (name, "assign-clause") == 0)
	      {
		if (strcmp (val, "cobol2002") == 0)
		  goto unsupported_value;
		else if (strcmp (val, "mf") == 0)
		  cb_assign_clause = CB_ASSIGN_MF;
		else if (strcmp (val, "ibm") == 0)
		  cb_assign_clause = CB_ASSIGN_IBM;
		else
		  goto invalid_value;
	      }
	    else if (strcmp (name, "display-sign") == 0)
	      {
		if (strcmp (val, "ascii") == 0)
		  cb_display_sign = COB_DISPLAY_SIGN_ASCII;
		else if (strcmp (val, "ebcdic") == 0)
		  goto unsupported_value;
		else if (strcmp (val, "ascii10") == 0)
		  cb_display_sign = COB_DISPLAY_SIGN_ASCII10;
		else if (strcmp (val, "ascii20") == 0)
		  goto unsupported_value;
		else
		  goto invalid_value;
	      }
	    else if (strcmp (name, "binary-size") == 0)
	      {
		if (strcmp (val, "2-4-8") == 0)
		  cb_binary_size = CB_BINARY_SIZE_2_4_8;
		else if (strcmp (val, "1-2-4-8") == 0)
		  cb_binary_size = CB_BINARY_SIZE_1_2_4_8;
		else if (strcmp (val, "1--8") == 0)
		  cb_binary_size = CB_BINARY_SIZE_1__8;
		else
		  goto invalid_value;
	      }
	    else if (strcmp (name, "binary-byteorder") == 0)
	      {
		if (strcmp (val, "native") == 0)
		  cb_binary_byteorder = CB_BYTEORDER_NATIVE;
		else if (strcmp (val, "big-endian") == 0)
		  cb_binary_byteorder = CB_BYTEORDER_BIG_ENDIAN;
		else
		  goto invalid_value;
	      }
	    break;
	  }
	case INT:
	  {
	    int j;
	    for (j = 0; val[j]; j++)
	      if (!isdigit (val[j]))
		goto invalid_value;
	    *((int *) var) = atoi (val);
	    break;
	  }
	case STRING:
	  {
	    val = read_string (val);

	    if (strcmp (name, "include") == 0)
	      {
		/* include another conf file */
		char fname[FILENAME_MAX];
		sprintf (fname, "%s/%s", cob_config_dir, val);
		if (cb_load_conf (fname, 0) != 0)
		  return -1;
	      }
	    else
	      {
		*((const char **) var) = val;
	      }
	    break;
	  }
	case BOOLEAN:
	  {
	    if (strcmp (val, "yes") == 0)
	      *((int *) var) = 1;
	    else if (strcmp (val, "no") == 0)
	      *((int *) var) = 0;
	    else
	      goto invalid_value;
	    break;
	  }
	case SUPPORT:
	  {
	    if (strcmp (val, "ok") == 0)
	      *((enum cb_support *) var) = CB_OK;
	    else if (strcmp (val, "warning") == 0)
	      *((enum cb_support *) var) = CB_WARNING;
	    else if (strcmp (val, "archaic") == 0)
	      *((enum cb_support *) var) = CB_ARCHAIC;
	    else if (strcmp (val, "obsolete") == 0)
	      *((enum cb_support *) var) = CB_OBSOLETE;
	    else if (strcmp (val, "skip") == 0)
	      *((enum cb_support *) var) = CB_SKIP;
	    else if (strcmp (val, "ignore") == 0)
	      *((enum cb_support *) var) = CB_IGNORE;
	    else if (strcmp (val, "error") == 0)
	      *((enum cb_support *) var) = CB_ERROR;
	    else if (strcmp (val, "unconformable") == 0)
	      *((enum cb_support *) var) = CB_UNCONFORMABLE;
	    else
	      goto invalid_value;
	    break;
	  }
	invalid_value:
	  fprintf (stderr, _("%s:%d: invalid value for '%s'\n"),
		   fname, line, name);
	  ret = -1;
	  break;
	unsupported_value:
	  fprintf (stderr, _("%s:%d: '%s' not supported yet\n"),
		   fname, line, val);
	  ret = -1;
	  break;
	}
    }
  fclose (fp);

  /* checks for no definition */
  if (check_nodef)
    for (i = 1; config_table[i].name; i++)
      if (config_table[i].val == NULL)
	{
	  fprintf (stderr, "%s: no definition of '%s'\n",
		   fname, config_table[i].name);
	  ret = -1;
	}

  return ret;
}
