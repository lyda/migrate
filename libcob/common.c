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

int cob_initialized = 0;

int cob_argc = 0;
char **cob_argv = NULL;

char *cob_source_file = NULL;
int cob_source_line = 0;

unsigned char cob_decimal_point = '.';
unsigned char cob_currency_symbol = '$';

int cob_status;

/* ZERO,SPACE,HIGH-VALUE,LOW-VALUE,QUOTE */

static struct cob_field_desc x_desc = {1, COB_ALPHANUMERIC};
struct cob_field cob_zero =  {&x_desc, "0"};
struct cob_field cob_space = {&x_desc, " "};
struct cob_field cob_high =  {&x_desc, "\xff"};
struct cob_field cob_low =   {&x_desc, "\0"};
struct cob_field cob_quote = {&x_desc, "\""};

/* RETURN-CODE */

int cob_return_code_value = 0;
static struct cob_field_desc rc_desc = {4, COB_BINARY, 9, 0, 1};
struct cob_field cob_return_code = {&rc_desc, (char *)&cob_return_code_value};

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


void
cob_init (int argc, char **argv)
{
  cob_argc = argc;
  cob_argv = argv;

  cob_init_numeric ();
  cob_init_termio ();
  cob_init_fileio ();
  cob_init_call ();

  cob_initialized = 1;
}

void
cob_module_init (void)
{
  if (!cob_initialized)
    {
      fputs ("warning: cob_init expected in the main program\n", stderr);
      cob_init (0, NULL);
    }
}

void
cob_stop_run (void)
{
  exit (cob_return_code_value);
}

int
cob_index (int i, int max)
{
  if (i < 1 || i > max)
    {
      cob_runtime_error ("index out of range `%d'", i);
      return 0;
    }
  return i - 1;
}

int
cob_index_depending (int i, int min, int max, int dep)
{
  if (dep < min || max < dep)
    {
      cob_runtime_error ("value out of range `%d'", dep);
      return 0;
    }
  if (i < min || dep < i)
    {
      cob_runtime_error ("index out of range `%d'", i);
      return 0;
    }
  return i - 1;
}


/*
 * Comparison
 */

int
cob_str_cmp (struct cob_field f1, struct cob_field f2)
{
  int i, ret = 0;
  int len1 = f1.desc->size;
  int len2 = f2.desc->size;
  int min = (len1 < len2) ? len1 : len2;
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
  if (len1 != len2)
    {
      int max = (len1 > len2) ? len1 : len2;
      unsigned char *data = (len1 > len2) ? f1.data : f2.data;
      for (; i < max; i++)
	if (data[i] != ' ')
	  {
	    ret = data[i] - ' ';
	    if (len1 < len2)
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
cob_cmp_str (struct cob_field f1, unsigned char *data2, int len2)
{
  int i, ret = 0;
  int len1 = f1.desc->size;
  int min = (len1 < len2) ? len1 : len2;
  int sign = cob_get_sign (f1);

  /* compare common substring */
  for (i = 0; i < min; i++)
    if (f1.data[i] != data2[i])
      {
	ret = f1.data[i] - data2[i];
	goto end;
      }

  /* compare the rest (if any) with spaces */
  if (len1 != len2)
    {
      int max = (len1 > len2) ? len1 : len2;
      unsigned char *data = (len1 > len2) ? f1.data : data2;
      for (; i < max; i++)
	if (data[i] != ' ')
	  {
	    ret = data[i] - ' ';
	    if (len1 < len2)
	      ret = -ret;
	    break;
	  }
    }

 end:
  cob_put_sign (f1, sign);
  return ret;
}

int
cob_cmp_all (unsigned char *data, unsigned char c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (data[i] != c)
      return data[i] - c;
  return 0;
}


/*
 * Class check
 */

void
cob_check_numeric (struct cob_field f)
{
  char s[BUFSIZ];
  if (!cob_is_numeric (f))
    cob_runtime_error ("non-numeric value `%s'", cob_field_to_string (f, s));
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
	int ret = 1;
	int sign = cob_get_sign (f);
	int len = COB_FIELD_LENGTH (f);
	unsigned char *data = COB_FIELD_BASE (f);
	for (i = 0; i < len; i++)
	  if (!isdigit (data[i]))
	    {
	      ret = 0;
	      break;
	    }
	cob_put_sign (f, sign);
	return ret;
      }
    default:
      {
	int i;
	int size = COB_FIELD_SIZE (f);
	unsigned char *data = COB_FIELD_DATA (f);
	for (i = 0; i < size; i++)
	  if (!isdigit (data[i]))
	    return 0;
	return 1;
      }
    }
}

int
cob_is_alpha (struct cob_field f)
{
  int i;
  int size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !isalpha (data[i]))
      return 0;
  return 1;
}

int
cob_is_upper (struct cob_field f)
{
  int i;
  int size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !isupper (data[i]))
      return 0;
  return 1;
}

int
cob_is_lower (struct cob_field f)
{
  int i;
  int size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !islower (data[i]))
      return 0;
  return 1;
}


/*
 * Common functions
 */

int
cob_get_sign (struct cob_field f)
{
  if (f.desc->have_sign)
    {
      if (f.desc->sign_separate)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.desc->size - 1;
	  return (*p == '+') ? 1 : -1;
	}
      else
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.desc->size - 1;
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
  if (f.desc->have_sign)
    {
      if (f.desc->sign_separate)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.desc->size - 1;
	  *p = (sign < 0) ? '-' : '+';
	}
      else if (sign < 0)
	{
	  char *p = f.desc->sign_leading ? f.data : f.data + f.desc->size - 1;
	  *p += 0x10;
	}
    }
}

char *
cob_field_to_string (struct cob_field f, char *s)
{
  int i, size = COB_FIELD_SIZE (f);
  memcpy (s, COB_FIELD_DATA (f), size);
  for (i = 0; i < size; i++)
    if (s[i] == ' ')
      break;
  s[i] = '\0';
  return s;
}

void
cob_runtime_error (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  if (cob_source_line)
    fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
  fputs ("run-time error: ", stderr);
  vfprintf (stderr, fmt, ap);
  fputs ("\a\n", stderr);
  va_end (ap);
}
