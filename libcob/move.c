/*
 * Copyright (C) 2002-2003 Keisuke Nishida
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

#include "move.h"
#include "lib/gettext.h"

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

static void
store_common_region (cob_field *f, unsigned char *data, size_t size, int expt)
{
  int lf1 = expt;
  int lf2 = COB_FIELD_EXPT (f);
  int hf1 = size + lf1;
  int hf2 = COB_FIELD_SIZE (f) + lf2;
  int lcf = MAX (lf1, lf2);
  int gcf = MIN (hf1, hf2);
  unsigned char *s1 = data + (hf1 - gcf);
  unsigned char *s2 = COB_FIELD_DATA (f) + (hf2 - gcf);
  unsigned char *e1 = data + (hf1 - lcf);

  memset (f->data, '0', f->size);
  if (s1 < e1)
    memcpy (s2, s1, e1 - s1);
}


/*
 * Display
 */

void
cob_move_alphanum_to_display (cob_field *f1, cob_field *f2)
{
  int sign, count, size;
  unsigned char *p;
  unsigned char *s1 = f1->data;
  unsigned char *s2 = COB_FIELD_DATA (f2);
  unsigned char *e1 = s1 + f1->size;
  unsigned char *e2 = s2 + COB_FIELD_SIZE (f2);

  /* initialize */
  memset (f2->data, '0', f2->size);

  /* skip white spaces */
  for (; s1 < e1; s1++)
    if (!isspace (*s1))
      break;

  /* check for sign */
  sign = 0;
  if (*s1 == '+' || *s1 == '-')
    sign = (*s1++ == '+') ? 1 : -1;

  /* count the number of digits before decimal point */
  count = 0;
  for (p = s1; p < e1 && *p != cob_env->decimal_point; p++)
    if (isdigit (*p))
      count++;

  /* find the start position */
  size = COB_FIELD_SIZE (f2) + f2->attr->expt;
  if (count < size)
    s2 += size - count;
  else
    while (count-- > size)
      while (!isdigit (*s1++));

  /* move */
  count = 0;
  for (; s1 < e1 && s2 < e2; s1++)
    {
      unsigned char c = *s1;
      if (isdigit (c))
	*s2++ = c;
      else if (c == cob_env->decimal_point)
	{
	  if (count++ > 0)
	    goto error;
	}
      else if (!(isspace (c) || c == cob_env->numeric_separator))
	goto error;
    }

  cob_put_sign (f2, sign);
  return;

 error:
  memset (f2->data, '0', f2->size);
  cob_put_sign (f2, 0);
}

void
cob_move_display_to_display (cob_field *f1, cob_field *f2)
{
  int sign = cob_get_sign (f1);

  store_common_region (f2, COB_FIELD_DATA (f1), COB_FIELD_SIZE (f1),
		       COB_FIELD_EXPT (f1));

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
}

void
cob_move_display_to_alphanum (cob_field *f1, cob_field *f2)
{
  int sign = cob_get_sign (f1);
  size_t size1 = COB_FIELD_SIZE (f1);
  size_t size2 = f2->size;
  unsigned char *data1 = COB_FIELD_DATA (f1);
  unsigned char *data2 = f2->data;

  if (size1 >= size2)
    {
      memcpy (data2, data1, size2);
    }
  else
    {
      int diff = size2 - size1;
      int zero_size = 0;
      /* move */
      memcpy (data2, data1, size1);
      /* implied 0 ('P's) */
      if (f1->attr->expt > 0)
	{
	  zero_size = MIN (f1->attr->expt, diff);
	  memset (data2 + size1, '0', zero_size);
	}
      /* padding */
      if (diff - zero_size > 0)
	memset (data2 + size1 + zero_size, ' ', diff - zero_size);
    }

  cob_put_sign (f1, sign);
}

void
cob_move_alphanum_to_alphanum (cob_field *f1, cob_field *f2)
{
  size_t size1 = f1->size;
  size_t size2 = f2->size;
  unsigned char *data1 = f1->data;
  unsigned char *data2 = f2->data;

  if (size1 >= size2)
    {
      /* move string with truncation */
      if (COB_FIELD_JUSTIFIED (f2))
	memcpy (data2, data1 + size1 - size2, size2);
      else
	memcpy (data2, data1, size2);
    }
  else
    {
      /* move string with padding */
      if (COB_FIELD_JUSTIFIED (f2))
	{
	  memset (data2, ' ', size2 - size1);
	  memcpy (data2 + size2 - size1, data1, size1);
	}
      else
	{
	  memcpy (data2, data1, size1);
	  memset (data2 + size1, ' ', size2 - size1);
	}
    }
}


/*
 * Packed decimal
 */

void
cob_move_display_to_packed (cob_field *f1, cob_field *f2)
{
  size_t i;
  int sign = cob_get_sign (f1);
  int digits1 = COB_FIELD_DIGITS (f1);
  int digits2 = COB_FIELD_DIGITS (f2);
  int exp1 = COB_FIELD_EXPT (f1);
  int exp2 = COB_FIELD_EXPT (f2);
  unsigned char *data1 = COB_FIELD_DATA (f1);
  unsigned char *data2 = f2->data;
  unsigned char *p = data1 + (digits1 + exp1) - (digits2 + exp2);

  /* pack string */
  memset (f2->data, 0, f2->size);
  for (i = 0; i < digits2; i++, p++)
    {
      char n = (data1 <= p && p < data1 + digits1) ? *p - '0' : 0;
      if (i % 2 == 0)
	data2[i / 2] = n << 4;
      else
	data2[i / 2] |= n;
    }

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
}

void
cob_move_packed_to_display (cob_field *f1, cob_field *f2)
{
  size_t i;
  int sign = cob_get_sign (f1);
  unsigned char *data = f1->data;
  unsigned char buff[f1->size];

  /* unpack string */
  for (i = 0; i < f1->attr->digits; i++)
    if (i % 2 == 0)
      buff[i] = (data[i/2] >> 4) + '0';
    else
      buff[i] = (data[i/2] & 0x0f) + '0';

  /* store */
  store_common_region (f2, buff, COB_FIELD_DIGITS (f1), COB_FIELD_EXPT (f1));

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
}


/*
 * Binary integer
 */

void
cob_move_display_to_binary (cob_field *f1, cob_field *f2)
{
  size_t i, size;
  long long val = 0;
  int sign = cob_get_sign (f1);
  size_t size1 = COB_FIELD_SIZE (f1);
  size_t size2 = f2->size;
  unsigned char *data1 = COB_FIELD_DATA (f1);
  unsigned char *data2 = f2->data;

  /* get value */
  size = size1 + f1->attr->expt - f2->attr->expt;
  for (i = 0; i < size; ++i)
    if (i < size1)
      val = val * 10 + data1[i] - '0';
    else
      val = val * 10;
  if (sign < 0 && COB_FIELD_HAVE_SIGN (f2))
    val = -val;
  val %= cob_exp10LL[(int) f2->attr->digits];

  /* store */
  switch (size2)
    {
    case 1: *(char *) data2 = val; break;
    case 2: *(short *) data2 = val; break;
    case 4: *(long *) data2 = val; break;
    case 8: *(long long *) data2 = val; break;
    }

  cob_put_sign (f1, sign);
}

void
cob_move_binary_to_display (cob_field *f1, cob_field *f2)
{
  int i, sign;
  long long val = 0;
  char buff[20]; /* long long is at most 20 digits */

  /* get value */
  switch (f1->size)
    {
    case 1: val = *(char *) f1->data; break;
    case 2: val = *(short *) f1->data; break;
    case 4: val = *(long *) f1->data; break;
    case 8: val = *(long long *) f1->data; break;
    }

  /* get sign */
  sign = 1;
  if (val < 0)
    {
      sign = -1;
      val = -val;
    }

  /* convert to string */
  i = 20;
  while (val > 0)
    {
      buff[--i] = val % 10 + '0';
      val /= 10;
    }

  /* store */
  store_common_region (f2, buff + i, 20 - i, f1->attr->expt);

  cob_put_sign (f2, sign);
}


/*
 * Edited
 */

#define get()								\
  ({									\
    char _x = ((min <= src && src < max) ? *src++ : (src++, '0'));	\
    if (_x != '0')							\
      is_zero = suppress_zero = 0;					\
    _x;									\
  })

void
cob_move_display_to_edited (cob_field *f1, cob_field *f2)
{
  const char *p;
  int sign = cob_get_sign (f1);
  int neg = (sign < 0) ? 1 : 0;
  unsigned char *min, *max, *src, *dst, *end;
  unsigned char pad = ' ';
  int count = 0;
  int count_sign = 1;
  int trailing_sign = 0;
  int is_zero = 1;
  int suppress_zero = 1;
  unsigned char sign_symbol = 0;
  unsigned char *decimal_point = NULL;

  /* count the number of digit places before decimal point */
  for (p = f2->attr->pic; *p; p += 2)
    {
      unsigned char c = p[0];
      if (c == '9' || c == 'P' || c == 'Z' || c == '*' ||
	  c == cob_env->currency_symbol)
	count += p[1], count_sign = 0;
      else if (count_sign && (c == '+' || c == '-'))
	count += p[1];
      else if (p[0] == 'V' || p[0] == cob_env->decimal_point)
	break;
    }

  min = COB_FIELD_DATA (f1);
  max = min + COB_FIELD_SIZE (f1);
  src = max + f1->attr->expt - count;
  dst = f2->data;
  end = f2->data + f2->size;
  for (p = f2->attr->pic; *p; )
    {
      unsigned char c = *p++; /* PIC char */
      unsigned char n = *p++; /* PIC char count */
      for (; n > 0; n--, dst++)
	{
	  switch (c)
	    {
	    case '0':
	    case '/': *dst = c; break;
	    case 'B': *dst = suppress_zero ? pad : 'B'; break;
	    case 'P': break;

	    case '9':
	      *dst = get ();
	      suppress_zero = 0;
	      trailing_sign = 1;
	      break;

	    case 'V':
	    case '.':
	    case ',':
	      if (c == 'V' || c == cob_env->decimal_point)
		{
		  *dst = cob_env->decimal_point;
		  decimal_point = dst;
		  break;
		}
	      else
		{
		  *dst = suppress_zero ? pad : c;
		  break;
		}

	    case 'C':
	    case 'D':
	      end = dst;
	      memcpy (dst++, neg ? (c == 'C' ? "CR" : "DB") : "  ", 2);
	      break;

	    case 'Z':
	    case '*':
	      {
		char x = get ();
		pad = (c == '*') ? '*' : ' ';
		*dst = suppress_zero ? pad : x;
		break;
	      }

	    case '+':
	    case '-':
	      {
		char x = get ();
		if (trailing_sign)
		  {
		    *dst = neg ? '-' : (c == '+') ? '+' : ' ';
		    end--;
		  }
		else if (dst == f2->data || suppress_zero)
		  {
		    *dst = pad;
		    sign_symbol = neg ? '-' : (c == '+') ? '+' : ' ';
		  }
		else
		  *dst = x;
		break;
	      }

	    default:
	      if (c == cob_env->currency_symbol)
		{
		  char x = get ();
		  if (dst == f2->data || suppress_zero)
		    *dst = pad, sign_symbol = cob_env->currency_symbol;
		  else
		    *dst = x;
		  break;
		}

	      *dst = '?'; /* invalid PIC */
	    }
	}
    }

  if (suppress_zero || (is_zero && COB_FIELD_BLANK_ZERO (f2)))
    {
      /* all digits are zeros */
      if (pad == ' ' || COB_FIELD_BLANK_ZERO (f2))
	memset (f2->data, ' ', f2->size);
      else
	for (dst = f2->data; dst < f2->data + f2->size; dst++)
	  if (*dst != cob_env->decimal_point)
	    *dst = pad;
    }
  else
    {
      /* put zero after the decimal point if necessary */
      if (decimal_point)
	for (dst = decimal_point + 1; dst < end; dst++)
	  if (!isdigit (*dst) && !strchr (",+-/B", *dst))
	    *dst = '0';

      /* put sign or currency symbol at the beginning */
      if (sign_symbol)
	{
	  for (dst = end - 1; dst > f2->data; dst--)
	    if (*dst == ' ')
	      break;
	  *dst = sign_symbol;
	}

      /* replace all `B's by pad */
      for (dst = f2->data; dst < end; dst++)
	if (*dst == 'B')
	  *dst = pad;
    }

  cob_put_sign (f1, sign);
}

void
cob_move_alphanum_to_edited (cob_field *f1, cob_field *f2)
{
  const char *p;
  unsigned char *max, *src, *dst;
  int sign = cob_get_sign (f1);

  src = COB_FIELD_DATA (f1);
  max = src + COB_FIELD_SIZE (f1);
  dst = f2->data;
  for (p = f2->attr->pic; *p; )
    {
      unsigned char c = *p++; /* PIC char */
      unsigned char n = *p++; /* PIC char count */
      for (; n > 0; n--)
	{
	  switch (c)
	    {
	    case 'A':
	    case 'X':
	    case '9': *dst++ = (src < max) ? *src++ : ' '; break;
	    case '0':
	    case '/': *dst++ = c; break;
	    case 'B': *dst++ = ' '; break;
	    default:  *dst++ = '?'; /* invalid PIC */
	    }
	}
    }
  cob_put_sign (f1, sign);
}


/*
 * MOVE dispatcher
 */

static void
indirect_move (void (*func) (cob_field *src, cob_field *dst),
	       cob_field *src, cob_field *dst,
	       unsigned int size, char expt)
{
  unsigned char data[size];
  cob_field_attr attr =
    {COB_TYPE_NUMERIC_DISPLAY, size, expt, COB_FLAG_HAVE_SIGN};
  cob_field temp = {size, data, &attr};
  func (src, &temp);
  cob_move (&temp, dst);
}

void
cob_move_all (cob_field *src, cob_field *dst)
{
  int i;
  unsigned char data[dst->size];
  cob_field temp = {dst->size, data, &cob_alnum_attr};

  for (i = 0; i < dst->size; i++)
    data[i] = src->data[i % src->size];

  cob_move (&temp, dst);
}

void
cob_move (cob_field *src, cob_field *dst)
{
  /* non-elementary move (ISO+IEC+1989-2002 14.8.24.3-2) */
  if (COB_FIELD_TYPE (src) == COB_TYPE_GROUP
      || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP)
    return cob_move_alphanum_to_alphanum (src, dst);

  /* elementary move */
  switch (COB_FIELD_TYPE (src))
    {
    case COB_TYPE_ALPHANUMERIC_ALL:
      return cob_move_all (src, dst);

    case COB_TYPE_NUMERIC_DISPLAY:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_display_to_display (src, dst);
	case COB_TYPE_NUMERIC_PACKED:
	  return cob_move_display_to_packed (src, dst);
	case COB_TYPE_NUMERIC_BINARY:
	  return cob_move_display_to_binary (src, dst);
	case COB_TYPE_NUMERIC_EDITED:
	  return cob_move_display_to_edited (src, dst);
	case COB_TYPE_ALPHANUMERIC_EDITED:
	  if (src->attr->expt > 0
	      || - src->attr->expt > src->attr->digits)
	    /* expand P's */
	    return indirect_move (cob_move_display_to_display, src, dst,
				  MAX (src->attr->digits, - src->attr->expt),
				  MAX (0, - src->attr->expt));
	  else
	    return cob_move_alphanum_to_edited (src, dst);
	default:
	  return cob_move_display_to_alphanum (src, dst);
	}

    case COB_TYPE_NUMERIC_PACKED:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_packed_to_display (src, dst);
	default:
	  return indirect_move (cob_move_packed_to_display, src, dst,
				src->attr->digits, src->attr->expt);
	}

    case COB_TYPE_NUMERIC_BINARY:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_binary_to_display (src, dst);
	default:
	  return indirect_move (cob_move_binary_to_display, src, dst,
				src->attr->digits, src->attr->expt);
	}

    default:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_alphanum_to_display (src, dst);
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_EDITED:
	  return indirect_move (cob_move_alphanum_to_display,
				src, dst, 32, -16);
	case COB_TYPE_ALPHANUMERIC_EDITED:
	  return cob_move_alphanum_to_edited (src, dst);
	default:
	  return cob_move_alphanum_to_alphanum (src, dst);
	}
    }
}


/*
 * Convenience functions
 */

void
cob_memcpy (cob_field *dst, unsigned char *src, int size)
{
  cob_field temp = {size, src, &cob_alnum_attr};
  cob_move (&temp , dst);
}

void
cob_set_int (cob_field *f, int n)
{
  cob_field_attr attr =
    {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL};
  cob_field temp = {4, (unsigned char *) &n, &attr};
  cob_move (&temp, f);
}

void
cob_display_to_int (cob_field *f, int *n)
{
  size_t i;
  int val = 0;
  int sign = cob_get_sign (f);
  size_t size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);

  /* skip preceding zeros */
  for (i = 0; i < size; i++)
    if (data[i] != '0')
      break;

  /* get value */
  if (f->attr->expt > 0)
    {
      for (; i < size; ++i)
	val = val * 10 + data[i] - '0';
      val *= cob_exp10[f->attr->expt];
    }
  else
    {
      size += f->attr->expt;
      for (; i < size; ++i)
	val = val * 10 + data[i] - '0';
    }
  if (sign < 0)
    val = -val;
  *n = val;

  cob_put_sign (f, sign);
}

void
cob_binary_to_int (cob_field *f, int *n)
{
  switch (f->size)
    {
    case 1: *n = *(char *) f->data;
    case 2: *n = *(short *) f->data;
    default: *n = *(long *) f->data;
    }
}

int
cob_get_int (cob_field *f)
{
  int n;
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      cob_display_to_int (f, &n);
      break;
    case COB_TYPE_NUMERIC_BINARY:
      cob_binary_to_int (f, &n);
      break;
    default:
      {
	cob_field_attr attr =
	  {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field temp = {4, (unsigned char *) &n, &attr};
	cob_move (f, &temp);
      }
    }
  return n;
}
