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

#include "_libcob.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

int
check_condition (struct cob_field f1, ...)
{
  struct cob_field f2, f3;
  va_list args;

  va_start (args, f1);
  f2.desc = va_arg (args, struct fld_desc *);
  while (f2.desc)
    {
      f2.data = va_arg (args, char *);
      f3 = va_arg (args, struct cob_field);

      if (FIELD_TYPE (f1) == '9' || FIELD_TYPE (f1) == 'B')
	{
	  cob_push_decimal (f2);
	  cob_push_decimal (f1);
	  cob_push_decimal (f3);
	  if (cob_between ())
	    goto success;
	}
      else
	{
	  int i;
	  for (i = 0; i < f1.desc->len; i++)
	    if ((i < f2.desc->len) && (f1.data[i] >= f2.data[i]))
	      if ((i < f3.desc->len) && (f1.data[i] <= f3.data[i]))
		goto success;
	}
      f2.desc = va_arg (args, struct fld_desc *);
    }
  /* fail */
  va_end (args);
  return 1;

 success:
  va_end (args);
  return 0;
}

int
cob_compare (struct cob_field f1, struct cob_field f2)
{
  int sign1, sign2;
  char type1 = FIELD_TYPE (f1);
  char type2 = FIELD_TYPE (f2);
  int len1 = f1.desc->len;
  int len2 = f2.desc->len;

  if ((type1 == '9' || type1 == 'C' || type1 == 'B' || type1 == 'U')
      && (type2 == '9' || type2 == 'C' || type2 == 'B' || type2 == 'U'))
    {
      cob_push_decimal (f1);
      cob_push_decimal (f2);
      return cob_cmp ();
    }

  sign1 = get_sign (f1);
  sign2 = get_sign (f2);

  if (f1.desc->all || f2.desc->all)
    {
      int i, j = 0, k = 0;
      int maxi = (len1 < len2) ? len1 : len2;
      for (i = 0; i < maxi; i++)
	{
	  if (f1.data[j] == f2.data[k])
	    continue;
	  if (f1.data[j] > f2.data[k])
	    goto positive;
	  if (f1.data[j] < f2.data[k])
	    goto negative;
	  j++;
	  k++;
	  if (f1.desc->all && j >= len1)
	    j = 0;
	  if (f2.desc->all && k >= len2)
	    k = 0;
	}

      if (len1 > len2)
	while (j < len1)
	  {
	    if (f1.data[j++] != f2.data[k++])
	      goto positive;
	    if (k >= len2)
	      k = 0;
	  }
      else
	while (k < len2)
	  {
	    if (f1.data[j++] != f2.data[k++])
	      goto negative;
	    if (j >= len1)
	      j = 0;
	  }
      goto zero;
    }

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

int
cob_in_range (struct cob_field low, struct cob_field val, struct cob_field up)
{
  if (cob_compare (low, val) <= 0 && cob_compare (val, up) <= 0)
    return 1;
  return 0;
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
	/* take care of signed numbers (non separate sign) */
	if (i == f->len - 1 && f->pic[0] == 'S')
	  if (strchr ("}ABCDEFGHI{JKLMNOPQR", c) != NULL)
	    {
	      dig++;
	      break;
	    }
      if ((c > '9') || (c < '0'))
	return 0;
    }
  if (!dig)
    return 0;
  return 1;
}

int
cob_is_alpha (struct cob_field f)
{
  int i;

  if (FIELD_NUMERIC_P (f))
    return 0;

  for (i = 0; i < FIELD_SIZE (f); i++)
    if (!isspace (FIELD_DATA (f)[i]) && !isalpha (FIELD_DATA (f)[i]))
      return 0;
  return 1;
}

int
cob_is_upper (struct cob_field f)
{
  int i;

  if (FIELD_NUMERIC_P (f))
    return 0;

  for (i = 0; i < FIELD_SIZE (f); i++)
    if (!isspace (FIELD_DATA (f)[i]) && !isupper (FIELD_DATA (f)[i]))
      return 0;
  return 1;
}

int
cob_is_lower (struct cob_field f)
{
  int i;

  if (FIELD_NUMERIC_P (f))
    return 0;

  for (i = 0; i < FIELD_SIZE (f); i++)
    if (!isspace (FIELD_DATA (f)[i]) && !islower (FIELD_DATA (f)[i]))
      return 0;
  return 1;
}
