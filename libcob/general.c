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

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "_libcob.h"

int cob_argc = 0;
char **cob_argv = NULL;

int cob_status;
int cob_status_register;

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

void
cob_init (int argc, char **argv)
{
  cob_argc = argc;
  cob_argv = argv;
}

char
sign_to_char (int digit)
{
  if (!digit)
    return '{';
  if (digit == 0x80)
    return '}';
  if (digit > 0)
    return 'A' + (char) (digit - 1);
  digit = -digit;
  return 'J' + (char) (digit - 1);
}

int
char_to_sign (char ch)
{
  if (ch == '{')
    return 0;
  if (ch == '}')
    return 0x80;
  if (ch < 'J')
    return (int) (ch - 'A' + 1);
  return (-(int) (ch - 'J' + 1));
}

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
      if (f.desc->pic[0] != 'S')
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
	  digit = char_to_sign (*p);
	  if (digit == 0x80)
	    *p = '0';
	  else if (digit < 0)
	    *p = '0' - digit;
	  else
	    {
	      *p = '0' + digit;
	      return 0;
	    }
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
      if (f.desc->pic[0] != 'S')
	return;
      if (f.desc->separate_sign)
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
	  *p = sign ? '-' : '+';
	}
      else
	{
	  char *p = f.desc->leading_sign ? f.data : f.data + f.desc->len - 1;
	  digit = *p - '0';
	  if (sign)
	    digit = -digit;
	  *p = sign_to_char ((sign && digit == 0) ? 0x80 : digit);
	}
    }
}

int
get_index (struct cob_field f)
{
  int index;
  struct fld_desc desc = { 4, 'B', 0, 0, 0, 0, 0, 0, 0, "S9\x9" };
  struct cob_field d = {&desc, (unsigned char *) &index};
  cob_move (f, d);
  return index;
}

struct fld_desc *
cob_adjust_length (struct fld_desc *dep_desc, char *dep_val,
		   int min, int max, struct fld_desc *var_desc,
		   struct fld_desc *item, struct fld_desc *copy)
{
  int itocc = get_index ((struct cob_field) {dep_desc, dep_val});
  if (itocc < min || itocc > max)
    {
      /* should generate exception, for now just a warning */
      fprintf (stderr, "*** Warning: table size out of bounds ");
      fprintf (stderr, "(requested = %d, min = %d, max = %d)\n",
	       itocc, min, max);
      itocc = max;
    }

  memmove (copy, var_desc, sizeof (struct fld_desc));
  copy->len -= (max - itocc) * item->len;
  return copy;
}


/*
 * 
 */

int
cob_exit ()
{
  return 0;
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
