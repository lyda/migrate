/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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
#include "numeric.h"
#include "termio.h"
#include "fileio.h"
#include "call.h"
#include "lib/gettext.h"

int cob_argc = 0;
char **cob_argv = NULL;

const char *cob_source_file = NULL;
unsigned int cob_source_line = 0;
const char *cob_source_statement = NULL;

int cob_return_code = 0;
int cob_linage_counter = 0;
int cob_cmp_result;

cob_field_attr cob_group_attr = {COB_TYPE_GROUP, 0, 0, 0, NULL};
cob_field_attr cob_alnum_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
cob_field_attr cob_just_attr  = {COB_TYPE_ALPHANUMERIC, 0, 0, COB_FLAG_JUSTIFIED, NULL};
cob_field_attr cob_all_attr   = {COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL};

cob_field cob_zero =  {1, "0",    &cob_all_attr};
cob_field cob_space = {1, " ",    &cob_all_attr};
cob_field cob_high =  {1, "\xff", &cob_all_attr};
cob_field cob_low =   {1, "\0",   &cob_all_attr};
cob_field cob_quote = {1, "\"",   &cob_all_attr};

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
  1LL,
  10LL,
  100LL,
  1000LL,
  10000LL,
  100000LL,
  1000000LL,
  10000000LL,
  100000000LL,
  1000000000LL,
  10000000000LL,
  100000000000LL,
  1000000000000LL,
  10000000000000LL,
  100000000000000LL,
  1000000000000000LL,
  10000000000000000LL,
  100000000000000000LL,
  1000000000000000000LL
};

cob_module *cob_current_module = NULL;

int cob_exception_code;

struct cob_exception cob_exception_table[] = {
  {0, 0, 0},		/* COB_EC_ZERO */
#undef COB_EXCEPTION
#define COB_EXCEPTION(CODE,TAG,NAME,CRITICAL) { 0x##CODE, NAME, CRITICAL },
#include "exception.def"
  {0, 0, 0}		/* COB_EC_MAX */
};

int cob_initialized = 0;


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
  char buff[256];
  const char *filename = getenv ("COB_CONFIG_FILE");
  if (!filename)
    filename = COB_CONFIG_FILE;

  fp = fopen (filename, "r");
  if (fp == NULL)
    return;

  while (fgets (buff, 256, fp) > 0)
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

static void
cob_init_config (void)
{
  config_load ();
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

  cob_init_config ();
  cob_init_numeric ();
  cob_init_termio ();
  cob_init_fileio ();
  cob_init_call ();

  cob_initialized = 1;
}

void
cob_module_enter (cob_module *module)
{
  if (!cob_initialized)
    {
      fputs (_("warning: cob_init expected in the main program\n"), stderr);
      cob_init (0, NULL);
    }

  module->next = cob_current_module;
  cob_current_module = module;
}

void
cob_module_leave (cob_module *module)
{
  cob_current_module = cob_current_module->next;
}

void
cob_stop_run (void)
{
  exit (cob_return_code);
}


/*
 * Utilities
 */

/* {SIGN}
 * positive: 0123456789
 * negative: @ABCDEFGHI
 */

int
cob_real_get_sign (cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	unsigned char *p;

	/* locate sign */
	if (COB_FIELD_SIGN_LEADING (f))
	  p = f->data;
	else
	  p = f->data + f->size - 1;

	/* get sign */
	if (COB_FIELD_SIGN_SEPARATE (f))
	  {
	    return (*p == '+') ? 1 : -1;
	  }
	else
	  {
	    if (*p <= '9')
	      return 1;
	    *p -= 0x10;
	    return -1;
	  }
      }
    case COB_TYPE_NUMERIC_PACKED:
      {
	unsigned char *p = f->data + f->attr->digits / 2;
	return (*p & 0x01) ? -1 : 1;
      }
    default:
      return 0;
    }
}

void
cob_real_put_sign (cob_field *f, int sign)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	unsigned char *p;

	/* locate sign */
	if (COB_FIELD_SIGN_LEADING (f))
	  p = f->data;
	else
	  p = f->data + f->size - 1;

	/* put sign */
	if (COB_FIELD_SIGN_SEPARATE (f))
	  {
	    int c = (sign < 0) ? '-' : '+';
	    if (*p != c)
	      *p = c;
	  }
	else if (sign < 0)
	  {
	    *p += 0x10;
	  }
	return;
      }
    case COB_TYPE_NUMERIC_PACKED:
      {
	unsigned char *p = f->data + f->attr->digits / 2;
	if (sign < 0)
	  *p = (*p & 0xf0) | 0x0d;
	else
	  *p = (*p & 0xf0) | 0x0c;
	return;
      }
    default:
      return;
    }
}

char *
cob_field_to_string (cob_field *f, char *s)
{
  size_t i;
  memcpy (s, f->data, f->size);
  for (i = f->size - 1; i >= 0; i--)
    if (s[i] != ' ')
      break;
  s[i + 1] = '\0';
  return s;
}


/*
 * Switch
 */

static int cob_switch[8] = {1, 0, 1, 1, 1, 1, 1, 1};

int
cob_get_switch (int n)
{
  return cob_switch[n];
}

void
cob_set_switch (int n, int flag)
{
  cob_switch[n] = flag;
}


/*
 * Comparison
 */

static int
cmp_char (cob_field *f, unsigned char c)
{
  size_t i;
  int ret = 0;
  int sign = cob_get_sign (f);

  for (i = 0; i < f->size; i++)
    {
      ret = f->data[i] - c;
      if (ret != 0)
	break;
    }

  cob_put_sign (f, sign);
  return ret;
}

static int
cmp_all (cob_field *f1, cob_field *f2)
{
  size_t i;
  int ret = 0;
  int sign = cob_get_sign (f1);
  unsigned char *s = NULL;

  for (i = 0; i < f1->size; i++)
    {
      if (i % f2->size == 0)
	s = f2->data;
      ret = f1->data[i] - *s++;
      if (ret != 0)
	break;
    }

  cob_put_sign (f1, sign);
  return ret;
}

static int
cmp_alnum (cob_field *f1, cob_field *f2)
{
  size_t i;
  int ret = 0;
  int sign1 = cob_get_sign (f1);
  int sign2 = cob_get_sign (f2);
  size_t min = (f1->size < f2->size) ? f1->size : f2->size;
  size_t max = (f1->size > f2->size) ? f1->size : f2->size;

  /* compare common substring */
  for (i = 0; i < min; i++)
    {
      ret = f1->data[i] - f2->data[i];
      if (ret != 0)
	goto end;
    }

  /* compare the rest (if any) with spaces */
  for (; i < max; i++)
    {
      if (f1->size > f2->size)
	ret = f1->data[i] - ' ';
      else
	ret = ' ' - f2->data[i];
      if (ret != 0)
	goto end;
    }

 end:
  cob_put_sign (f1, sign1);
  cob_put_sign (f2, sign2);
  return ret;
}

int
cob_cmp (cob_field *f1, cob_field *f2)
{
  if (COB_FIELD_TYPE (f2) == COB_TYPE_ALPHANUMERIC_ALL)
    {
      if (f2 == &cob_zero && COB_FIELD_IS_NUMERIC (f1))
	cob_cmp_result = cob_cmp_int (f1, 0);
      else if (f2->size == 1)
	cob_cmp_result = cmp_char (f1, f2->data[0]);
      else
	cob_cmp_result = cmp_all (f1, f2);
    }
  else if (COB_FIELD_TYPE (f1) == COB_TYPE_ALPHANUMERIC_ALL)
    {
      if (f1 == &cob_zero && COB_FIELD_IS_NUMERIC (f2))
	cob_cmp_result = - cob_cmp_int (f2, 0);
      else if (f1->size == 1)
	cob_cmp_result = - cmp_char (f2, f1->data[0]);
      else
	cob_cmp_result = - cmp_all (f2, f1);
    }
  else
    {
      if (COB_FIELD_IS_NUMERIC (f1) && COB_FIELD_IS_NUMERIC (f2))
	cob_cmp_result = cob_numeric_cmp (f1, f2);
      else
	cob_cmp_result = cmp_alnum (f1, f2);
    }
  return cob_cmp_result;
}

int
cob_cmp_int (cob_field *f1, int n)
{
  cob_field_attr attr = {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN};
  cob_field temp = {sizeof (int), (unsigned char *) &n, &attr};
  cob_cmp_result = cob_numeric_cmp (f1, &temp);
  return cob_cmp_result;
}


/*
 * Class check
 */

int
cob_is_numeric (cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
    case COB_TYPE_NUMERIC_PACKED:
      return 1;
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	int i;
	int sign = cob_get_sign (f);
	int size = COB_FIELD_SIZE (f);
	unsigned char *data = COB_FIELD_DATA (f);
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
	size_t i;
	for (i = 0; i < f->size; i++)
	  if (!isdigit (f->data[i]))
	    return 0;
	return 1;
      }
    }
}

int
cob_is_alpha (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !isalpha (f->data[i]))
      return 0;
  return 1;
}

int
cob_is_upper (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !isupper (f->data[i]))
      return 0;
  return 1;
}

int
cob_is_lower (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !islower (f->data[i]))
      return 0;
  return 1;
}


/*
 * Run-time error checking
 */

void
cob_runtime_error (const char *fmt, ...)
{
  va_list ap;

  /* prefix */
  if (cob_source_file)
    fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
  fputs ("libcob: ", stderr);

  /* body */
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  /* postfix */
  fputs ("\n", stderr);
}

void
cob_exception (void)
{
  int i;
  for (i = 1; i < COB_EC_MAX; i++)
    if (cob_exception_code == cob_exception_table[i].code)
      {
	cob_runtime_error (cob_exception_table[i].name);
	if (cob_exception_table[i].critical)
	  exit (1);
      }
}

void
cob_check_numeric (cob_field *f, const char *name)
{
  if (!cob_is_numeric (f))
    {
      size_t i;
      unsigned char *data = f->data;
      char buff[f->size * 4 + 1];
      char *p = buff;
      for (i = 0; i < f->size; i++)
	if (isprint (data[i]))
	  *p++ = data[i];
	else
	  p += sprintf (p, "\\%03o", data[i]);
      *p = '\0';
      cob_runtime_error (_("`%s' not numeric: `%s'"), name, buff);
      exit (1);
    }
}

void
cob_check_subscript (int i, int max, const char *name)
{
  cob_check_subscript_depending (i, 1, max, max, name, 0);
}

void
cob_check_subscript_depending (int i, int min, int max, int dep, const char *name, const char *depname)
{
  COB_SET_EXCEPTION (COB_EC_ZERO);

  /* check the OCCURS DEPENDING ON item */
  if (dep < min || max < dep)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_ODO);
      cob_runtime_error (_("OCCURS DEPENDING ON `%s' out of bounds: %d"),
			 depname, dep);
      exit (1);
    }

  /* check the subscript */
  if (i < min || dep < i)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_SUBSCRIPT);
      cob_runtime_error (_("subscript of `%s' out of bounds: %d"), name, i);
      exit (1);
    }
}

void
cob_check_ref_mod (int offset, int length, int size, const char *name)
{
  COB_SET_EXCEPTION (COB_EC_ZERO);

  /* check the offset */
  if (offset < 1 || offset > size)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_REF_MOD);
      cob_runtime_error (_("offset of `%s' out of bounds: %d"),
			 name, offset);
      exit (1);
    }

  /* check the length */
  if (length < 1 || offset + length - 1 > size)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_REF_MOD);
      cob_runtime_error (_("length of `%s' out of bounds: %d"),
			 name, length);
      exit (1);
    }
}
