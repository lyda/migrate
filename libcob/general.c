/*
 * Copyright (C) 2001, 2000, 1999,  Rildo Pragana, Jim Noeth, 
 *               Andrew Cameron, David Essex.
 * Copyright (C) 1993, 1991  Rildo Pragana.
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

#include "libcob.h"

int cob_status;
int cob_argc = 0;
char **cob_argv = NULL;

int cob_source_line = 0;
char *cob_source_file = NULL;

unsigned char cob_decimal_point = '.';
unsigned char cob_currency_symbol = '$';

int cob_reloading_flag = 0;

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

static struct cob_field_desc fig_desc = {1, 'X'};

struct cob_field cob_zero =  {&fig_desc, "0"};
struct cob_field cob_space = {&fig_desc, " "};
struct cob_field cob_high =  {&fig_desc, "\xff"};
struct cob_field cob_low =   {&fig_desc, "\0"};
struct cob_field cob_quote = {&fig_desc, "\""};

int
get_sign (struct cob_field f)
{
  int digit;

  switch (FIELD_TYPE (f))
    {
    case 'C':
      digit = f.desc->size / 2;
      return (f.desc->size & 1) ?	/* odd number of digits? */
	(((f.data[digit] & 0x0f) == 0x0d) ? 1 : 0) :
	(((f.data[digit] & 0xf0) == 0xd0) ? 1 : 0);
    case '9':
      if (!f.desc->have_sign)
	return 0;
      if (f.desc->separate_sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->size - 1;
	  int sign = (*p == '+') ? 0 : 1;
	  *p = '0';
	  return sign;
	}
      else
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->size - 1;
	  if (*p <= '9')
	    return 0;
	  *p -= 0x10;
	  return 1;
	}
    }
  return 0;
}

void
put_sign (struct cob_field f, int sign)
{
  int digit;

  switch (FIELD_TYPE (f))
    {
    case 'C':
      digit = f.desc->size / 2;
      f.data[digit] = (f.desc->size & 1) ?	/* odd number of digits */
	((f.data[digit] & 0xf0) | (sign ? 0x0d : 0x0c)) : (sign ? 0xd0 : 0xc0);
      return;
    case '9':
      if (!f.desc->have_sign)
	return;
      if (f.desc->separate_sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->size - 1;
	  *p = sign ? '-' : '+';
	}
      else if (sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->size - 1;
	  *p += 0x10;
	}
    }
}

char *
cob_field_to_string (struct cob_field f, char *s)
{
  int i, size = FIELD_SIZE (f);
  memcpy (s, FIELD_DATA (f), size);
  for (i = 0; i < size; i++)
    if (s[i] == ' ')
      break;
  s[i] = '\0';
  return s;
}



void
cob_init (int argc, char **argv)
{
  cob_argc = argc;
  cob_argv = argv;
  cob_source_file = 0;
  cob_source_line = 0;
}

void
cob_stop_run (void)
{
  exit (0);
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

void
cob_check_numeric (struct cob_field f)
{
  int i;
  int sign = get_sign (f);
  int len = FIELD_LENGTH (f);
  unsigned char *s = FIELD_BASE (f);
  for (i = 0; i < len; i++)
    if (!isdigit (s[i]))
      {
	cob_runtime_error ("non-numeric value `%s'", s);
	break;
      }
  put_sign (f, sign);
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
  int sign1 = get_sign (f1);
  int sign2 = get_sign (f1);

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
  put_sign (f1, sign1);
  put_sign (f2, sign2);
  return ret;
}

int
cob_cmp_str (struct cob_field f1, unsigned char *data2, int len2)
{
  int i, ret = 0;
  int len1 = f1.desc->size;
  int min = (len1 < len2) ? len1 : len2;
  int sign1 = get_sign (f1);

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
  put_sign (f1, sign1);
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

int
cob_is_numeric (struct cob_field f)
{
  if (FIELD_TYPE (f) == '9')
    {
      int i, sign;
      int ret = 1;
      int size = FIELD_LENGTH (f);
      unsigned char *data = FIELD_BASE (f);
      sign = get_sign (f);
      for (i = 0; i < size; i++)
	if (!isdigit (data[i]))
	  {
	    ret = 0;
	    break;
	  }
      put_sign (f, sign);
      return ret;
    }
  else
    {
      int i;
      int size = FIELD_SIZE (f);
      unsigned char *data = FIELD_DATA (f);
      for (i = 0; i < size; i++)
	if (!isdigit (data[i]))
	  return 0;
      return 1;
    }
}

int
cob_is_alpha (struct cob_field f)
{
  int i;
  int size = FIELD_SIZE (f);
  unsigned char *data = FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !isalpha (data[i]))
      return 0;
  return 1;
}

int
cob_is_upper (struct cob_field f)
{
  int i;
  int size = FIELD_SIZE (f);
  unsigned char *data = FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !isupper (data[i]))
      return 0;
  return 1;
}

int
cob_is_lower (struct cob_field f)
{
  int i;
  int size = FIELD_SIZE (f);
  unsigned char *data = FIELD_DATA (f);
  for (i = 0; i < size; i++)
    if (!isspace (data[i]) && !islower (data[i]))
      return 0;
  return 1;
}
