/* Genaral functions
 *
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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

int cob_status;

void
cob_init (int argc, char **argv)
{
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
extract_sign (struct fld_desc *f, char *s)
{
  char *tmp;
  int digit;

  if (f->type == 'C')
    {
      digit = f->len / 2;
      return (f->len & 1) ?	/* odd number of digits? */
	(((s[digit] & 0x0f) == 0x0d) ? 1 : 0) :
	(((s[digit] & 0xf0) == 0xd0) ? 1 : 0);
    }
  if (*f->pic != 'S')
    return 0;
  tmp = (f->leading_sign) ? s : s + f->len - 1;
  digit = char_to_sign (*tmp);
  if (digit == 0x80)
    *tmp = '0';
  else if (digit < 0)
    *tmp = '0' - digit;
  else
    {
      *tmp = '0' + digit;
      return 0;
    }
  return 1;
}

void
put_sign (struct fld_desc *f, char *s, int sign)
{
  char *tmp;
  int digit;

  if (f->type == 'C')
    {
      digit = f->len / 2;
      s[digit] = (f->len & 1) ?	/* odd number of digits */
	((s[digit] & 0xf0) | (sign ? 0x0d : 0x0c)) : (sign ? 0xd0 : 0xc0);
      return;
    }
  if (*f->pic != 'S')
    return;
  tmp = (f->leading_sign) ? s : s + f->len - 1;
  digit = *tmp - '0';
  if (sign)
    digit = -digit;
  *tmp = sign_to_char ((sign && digit == 0) ? 0x80 : digit);
}

int
get_index (struct fld_desc *f, char *s)
{
  int index;
  struct fld_desc fld = { 4, 'B', 0, 0, 0, 0, 0, 0, "S9\x9" };
  cob_move (f, s, &fld, (char *) &index);
  return index;
}

struct fld_desc *
cob_adjust_length (struct fld_desc *dep_desc, char *dep_val,
		   int min, int max, struct fld_desc *var_desc,
		   struct fld_desc *item, struct fld_desc *copy)
{
  int itocc = get_index (dep_desc, dep_val);
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
 * Class check
 */

int
cob_is_numeric (struct fld_desc *f, char *s)
{
  int i, dig = 0;

  if ((f->type == 'B') || (f->type == 'C') || (f->type == 'U'))
    /* the B and C formats have valid numbers always (?) */
    return 1;

  for (i = 0; i < f->len; i++)
    {
      char c = s[i];
      /* must have at least one digit */
      if (!dig && (c >= '0') && (c <= '9'))
	dig++;
      if (i == 0 && ((c == ' ') || (c == '+') || (c == '-')))
	continue;
      /* look for a number followed by several spaces (is this valid?) */
      if (c == ' ')
	{
	  while (i < f->len)
	    {
	      if (s[i] != ' ')
		return 0;
	      i++;
	    }
	  break;
	}
	/******** take care of NULL picture (bug in refmod's) ********/
      if (f->type != 'G' && f->pic != NULL)
	{
	  /* take care of signed numbers (non separate sign) */
	  if ((i == f->len - 1) && (*(f->pic) == 'S'))
	    {
	      if (strchr ("}ABCDEFGHI{JKLMNOPQR", c) != NULL)
		{
		  dig++;
		  break;
		}
	    }
	}
      if ((c > '9') || (c < '0'))
	return 0;
    }
  if (!dig)
    return 0;
  return 1;
}

int
cob_is_alphabetic (struct fld_desc *f, char *s)
{
  int i;
  for (i = 0; i < f->len; i++)
    if (!isspace (s[i]) && !isalpha (s[i]))
      return 0;
  return 1;
}

int
cob_is_upper (struct fld_desc *f, char *s)
{
  int i;
  for (i = 0; i < f->len; i++)
    if (!isspace (s[i]) && isupper (s[i]))
      return 0;
  return 1;
}

int
cob_is_lower (struct fld_desc *f, char *s)
{
  int i;
  for (i = 0; i < f->len; i++)
    if (!isspace (s[i]) && islower (s[i]))
      return 0;
  return 1;
}


/*
 * 
 */

void
cob_exit ()
{
//     do_scrio_finish();
}

int
fldLength (struct fld_desc *f)
{
  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: return 3;
	case 2: return 5;
	case 4: return 10;
	default: return 18;
	}
    case 'U':
      if (f->len == 4)
	return 14;
      else
	return 30;
    default:
      return f->len;
    }
}

int
picCompLength (struct fld_desc *f)
{
  int len = 0, i;
  unsigned char *pic = f->pic;
  for (i = 0; pic[i]; i++)
    {
      if (pic[i] == '9' || pic[i] == 'P')
	len += pic[++i];
    }
  return len;
}
