/*
 * Copyright (C) 2001-2002 Keisuke Nishida
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "common.h"
#include "call.h"
#include "lib/gettext.h"

extern void cob_init_numeric (void);
extern void cob_init_termio (void);
extern void cob_init_fileio (void);
extern void cob_init_call (void);

int cob_initialized = 0;

int cob_argc = 0;
char **cob_argv = NULL;

char *cob_source_file = NULL;
int cob_source_line = 0;

unsigned char cob_decimal_point = '.';
unsigned char cob_currency_symbol = '$';

int cob_status;
int cob_return_code = 0;

/* ZERO,SPACE,HIGH-VALUE,LOW-VALUE,QUOTE */

struct cob_field cob_zero =  {1, "0", 0};
struct cob_field cob_space = {1, " ", 0};
struct cob_field cob_high =  {1, "\xff", 0};
struct cob_field cob_low =   {1, "\0", 0};
struct cob_field cob_quote = {1, "\"", 0};

/* SWITCH-1/2/3/4/5/6/7/8 */

char cob_switch[8] = {1, 0, 1, 1, 1, 1, 1, 1};

long cob_exp10[10] = {
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000
};

long long cob_exp10LL[19] = {
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000,
  10000000000,
  100000000000,
  1000000000000,
  10000000000000,
  100000000000000,
  1000000000000000,
  10000000000000000,
  100000000000000000,
  1000000000000000000
};

static int ding_on_error = 0;


/*
 * Config file
 */

static struct config
{
  const char *key;
  const char *val;
  struct config *next;
} *config_list = NULL;

static void
config_insert (const char *key, const char *val)
{
  struct config *p = malloc (sizeof (struct config));
  p->key = strdup (key);
  p->val = strdup (val);
  p->next = config_list;
  config_list = p;
}

const char *
cob_config_lookup (const char *key)
{
  struct config *l;
  for (l = config_list; l; l = l->next)
    if (strcmp (key, l->key) == 0)
      return l->val;
  return NULL;
}

int
cob_config_compare (const char *key, const char *val)
{
  const char *tmp = cob_config_lookup (key);
  if (tmp != NULL && strcmp (tmp, val) == 0)
    return 1;
  return 0;
}

static void
config_load (void)
{
  FILE *fp;
  char buff[BUFSIZ];
  const char *filename = getenv ("COB_CONFIG_FILE");
  if (!filename)
    filename = COB_CONFIG_FILE;

  fp = fopen (filename, "r");
  if (fp == NULL)
    return;

  while (fgets (buff, BUFSIZ, fp) > 0)
    {
      char *key, *val;

      /* skip comment/blank lines */
      if (buff[0] == '#' || buff[0] == '\n')
	continue;

      /* get the key */
      key = strtok (buff, ": \t");
      if (key == NULL)
	continue;

      /* get the value */
      val = strtok (NULL, " \t\n");
      if (val == NULL)
	continue;

      config_insert (key, val);
    }

  fclose (fp);
}


/*
 * General functions
 */

void
cob_init (int argc, char **argv)
{
  cob_argc = argc;
  cob_argv = argv;

#if ENABLE_NLS
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif

  cob_init_numeric ();
  cob_init_termio ();
  cob_init_fileio ();
  cob_init_call ();

  config_load ();

  ding_on_error = cob_config_compare ("ding-on-error", "yes");

  cob_initialized = 1;
}

void
cob_module_init (void)
{
  if (!cob_initialized)
    {
      fputs (_("warning: cob_init expected in the main program\n"), stderr);
      cob_init (0, NULL);
    }
}

void
cob_stop_run (void)
{
  exit (cob_return_code);
}

int
cob_index (int i, int max, const char *name)
{
  if (i < 1 || i > max)
    {
      cob_runtime_error (_("index `%s' out of range: %d"), name, i);
      return (i < 1) ? 0 : max - 1;
    }
  return i - 1;
}

int
cob_index_depending (int i, int min, int max, int dep, const char *name, const char *depname)
{
  if (dep < min || max < dep)
    {
      cob_runtime_error (_("value of `%s' out of range: %d"), depname, dep);
      dep = (dep < min) ? min : max;
    }
  if (i < min || dep < i)
    {
      cob_runtime_error (_("index `%s' out of range: %d"), name, i);
      return (i < min) ? 0 : dep - 1;
    }
  return i - 1;
}


/*
 * Comparison
 */

int
cob_cmp_field (struct cob_field f1, struct cob_field f2)
{
  int i, ret = 0;
  int min = (f1.size < f2.size) ? f1.size : f2.size;
  int sign1 = cob_get_sign (f1);
  int sign2 = cob_get_sign (f1);

  /* compare common substring */
  for (i = 0; i < min; i++)
    if (f1.data[i] != f2.data[i])
      {
	ret = f1.data[i] - f2.data[i];
	goto end;
      }

  /* compare the rest (if any) with spaces */
  if (f1.size != f2.size)
    {
      int max = (f1.size > f2.size) ? f1.size : f2.size;
      unsigned char *data = (f1.size > f2.size) ? f1.data : f2.data;
      for (; i < max; i++)
	if (data[i] != ' ')
	  {
	    ret = data[i] - ' ';
	    if (f1.size < f2.size)
	      ret = -ret;
	    break;
	  }
    }

 end:
  cob_put_sign (f1, sign1);
  cob_put_sign (f2, sign2);
  return ret;
}

int
cob_cmp_str (struct cob_field f1, unsigned char *data2, size_t size2)
{
  int i, ret = 0;
  int min = (f1.size < size2) ? f1.size : size2;
  int sign = cob_get_sign (f1);

  /* compare common substring */
  for (i = 0; i < min; i++)
    if (f1.data[i] != data2[i])
      {
	ret = f1.data[i] - data2[i];
	goto end;
      }

  /* compare the rest (if any) with spaces */
  if (f1.size != size2)
    {
      int max = (f1.size > size2) ? f1.size : size2;
      unsigned char *data = (f1.size > size2) ? f1.data : data2;
      for (; i < max; i++)
	if (data[i] != ' ')
	  {
	    ret = data[i] - ' ';
	    if (f1.size < size2)
	      ret = -ret;
	    break;
	  }
    }

 end:
  cob_put_sign (f1, sign);
  return ret;
}

int
cob_cmp_all (unsigned char *data, unsigned char c, size_t size)
{
  int i;
  for (i = 0; i < size; i++)
    if (data[i] != c)
      return data[i] - c;
  return 0;
}

int
cob_cmp_all_str (unsigned char *data, unsigned char *str, size_t size)
{
  int i;
  unsigned char *s = str;
  for (i = 0; i < size; i++)
    {
      if (data[i] != *s)
	return data[i] - *s;
      if (*++s == 0)
	s = str;
    }
  return 0;
}


/*
 * Class check
 */

void
cob_check_numeric (struct cob_field f, const char *name)
{
  if (!cob_is_numeric (f))
    {
      int i;
      size_t size = f.size;
      unsigned char *data = f.data;
      char buff[size * 4 + 1];
      char *p = buff;
      for (i = 0; i < size; i++)
	if (isprint (data[i]))
	  *p++ = data[i];
	else
	  p += sprintf (p, "\\%03o", data[i]);
      *p = '\0';
      cob_runtime_error (_("value of `%s' not numeric: `%s'"), name, buff);
    }
}

int
cob_is_numeric (struct cob_field f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_BINARY:
    case COB_PACKED:
      return 1;
    case COB_DISPLAY:
      {
	int i;
	int sign = cob_get_sign (f);
	int size = COB_FIELD_LENGTH (f);
	unsigned char *data = COB_FIELD_BASE (f);
	for (i = 0; i < size; i++)
	  if (!isdigit (data[i]))
	    {
	      cob_put_sign (f, sign);
	      return 0;
	    }
	cob_put_sign (f, sign);
	return 1;
      }
    default:
      {
	int i;
	for (i = 0; i < f.size; i++)
	  if (!isdigit (f.data[i]))
	    return 0;
	return 1;
      }
    }
}

int
cob_is_alpha (struct cob_field f)
{
  int i;
  for (i = 0; i < f.size; i++)
    if (!isspace (f.data[i]) && !isalpha (f.data[i]))
      return 0;
  return 1;
}

int
cob_is_upper (struct cob_field f)
{
  int i;
  for (i = 0; i < f.size; i++)
    if (!isspace (f.data[i]) && !isupper (f.data[i]))
      return 0;
  return 1;
}

int
cob_is_lower (struct cob_field f)
{
  int i;
  for (i = 0; i < f.size; i++)
    if (!isspace (f.data[i]) && !islower (f.data[i]))
      return 0;
  return 1;
}


/*
 * Common functions
 */

/* {SIGN}
 * positive: 0123456789
 * negative: @ABCDEFGHI
 */

int
cob_get_sign (struct cob_field f)
{
  if (f.desc && f.desc->have_sign)
    {
      if (f.desc->sign_separate)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.size - 1;
	  return (*p == '+') ? 1 : -1;
	}
      else
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.size - 1;
	  if (*p <= '9')
	    return 1;
	  *p -= 0x10;
	  return -1;
	}
    }
  return 0;
}

void
cob_put_sign (struct cob_field f, int sign)
{
  if (f.desc && f.desc->have_sign)
    {
      if (f.desc->sign_separate)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.size - 1;
	  *p = (sign < 0) ? '-' : '+';
	}
      else if (sign < 0)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.size - 1;
	  *p += 0x10;
	}
    }
}

char *
cob_field_to_string (struct cob_field f, char *s)
{
  int i;
  memcpy (s, f.data, f.size);
  for (i = 0; i < f.size; i++)
    if (s[i] == ' ')
      break;
  s[i] = '\0';
  return s;
}

void
cob_runtime_error (char *fmt, ...)
{
  va_list ap;

  /* prefix */
  if (cob_source_line)
    fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
  fputs ("libcob: ", stderr);

  /* body */
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  /* postfix */
  if (ding_on_error)
    fputs ("\a", stderr);
  fputs ("\n", stderr);
}
