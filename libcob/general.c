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
#include <string.h>
#include <ctype.h>

#include "libcob.h"

int cob_status;
int cob_argc = 0;
char **cob_argv = NULL;

char *cob_source_file;
int cob_source_line;

char *cob_source_file = NULL;
int cob_source_line = 0;

unsigned char cob_decimal_point = '.';
unsigned char cob_currency_symbol = '$';

long long cob_exp10[19] = {
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

int
get_sign (struct cob_field f)
{
  int digit;

  switch (FIELD_TYPE (f))
    {
    case 'C':
      digit = f.desc->len / 2;
      return (f.desc->len & 1) ?	/* odd number of digits? */
	(((f.data[digit] & 0x0f) == 0x0d) ? 1 : 0) :
	(((f.data[digit] & 0xf0) == 0xd0) ? 1 : 0);
    case '9':
      if (!f.desc->have_sign)
	return 0;
      if (f.desc->separate_sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
	  int sign = (*p == '+') ? 0 : 1;
	  *p = '0';
	  return sign;
	}
      else
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
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
      digit = f.desc->len / 2;
      f.data[digit] = (f.desc->len & 1) ?	/* odd number of digits */
	((f.data[digit] & 0xf0) | (sign ? 0x0d : 0x0c)) : (sign ? 0xd0 : 0xc0);
      return;
    case '9':
      if (!f.desc->have_sign)
	return;
      if (f.desc->separate_sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
	  *p = sign ? '-' : '+';
	}
      else if (sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
	  *p += 0x10;
	}
    }
}

int
picCompLength (const char *pic)
{
  int len = 0, i;
  for (i = 0; pic[i]; i++)
    if (pic[i] == '9' || pic[i] == 'P')
      len += pic[++i];
  return len;
}

int
picCompDecimals (const char *pic)
{
  int decimals = -1;
  for (; *pic; pic += 2)
    {
      if (*pic == 'V' || *pic == '.')
	decimals = 0;
      else if (decimals >= 0)
	decimals += pic[1];
    }
  return (decimals < 0) ? 0 : decimals;
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
  cob_init_stack ();
}

int
get_index (struct cob_field f)
{
  int index;
  struct cob_field_desc desc =
    { 4, 'B', f.desc->decimals, 0, f.desc->have_sign, 0, 0, 0, "S9\x9" };
  struct cob_field d = {&desc, (unsigned char *) &index};
  cob_dis_check (f);
  cob_move (f, d);
  return index;
}

int
cob_str_cmp (struct cob_field f1, struct cob_field f2)
{
  int sign1, sign2;
  int len1 = f1.desc->len;
  int len2 = f2.desc->len;

  sign1 = get_sign (f1);
  sign2 = get_sign (f2);

  {
    int i;
    int maxi = (len1 < len2) ? len1 : len2;
    for (i = 0; i < maxi; i++)
      {
	if (f1.data[i] == f2.data[i])
	  continue;
	if (f1.data[i] > f2.data[i])
	  goto positive;
	if (f1.data[i] < f2.data[i])
	  goto negative;
      }
    if (len1 > len2)
      {
	while (i < len1)
	  if (f1.data[i++] != ' ')
	    goto positive;
      }
    else
      {
	while (i < len2)
	  if (f2.data[i++] != ' ')
	    goto negative;
      }
    goto zero;
  }

  {
    int ret;
  positive:
    ret = 1; goto end;
  zero:
    ret = 0; goto end;
  negative:
    ret = -1; goto end;
  end:
    put_sign (f1, sign1);
    put_sign (f2, sign2);
    return ret;
  }
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


void
_DUMP_ (unsigned char *caData, char *szCount, char *caOut)
{
  int i, k;
  unsigned char c;

  k = 0;
  for (i = 0; i < 4; ++i)
    {
      if (szCount[i] == '\0')
	break;
      k *= 10;
      k += (szCount[i] - '0');
    }

  for (i = 0; i < k; ++i)
    {
      c = (caData[i] >> 4) + '0';
      if (c > '9')
	c += 7;
      caOut[i * 2] = c;

      c = (caData[i] & 0xf) + '0';
      if (c > '9')
	c += 7;
      caOut[(i * 2) + 1] = c;
    }
}
