/*
 * Copyright (C) 2002 Keisuke Nishida
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

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

#define COPY_COMMON_REGION(b1,l1,d1,b2,l2,d2)	\
{						\
  int lf1 = -(d1);				\
  int lf2 = -(d2);				\
  int hf1 = (l1) + lf1;				\
  int hf2 = (l2) + lf2;				\
  int lcf = MAX (lf1, lf2);			\
  int gcf = MIN (hf1, hf2);			\
  unsigned char *s1 = (b1) + (hf1 - gcf);	\
  unsigned char *s2 = (b2) + (hf2 - gcf);	\
  unsigned char *e1 = (b1) + (hf1 - lcf);	\
  if (s1 < e1)					\
    memcpy (s2, s1, e1 - s1);			\
}


/*
 * Display
 */

static void
finalize_display (struct cob_field f)
{
  if (f.desc->blank_zero)
    {
      int i, len = COB_FIELD_LENGTH (f);
      unsigned char *base = COB_FIELD_BASE (f);
      for (i = 0; i < len; i++)
	if (base[i] != '0')
	  return;
      memset (base, ' ', len);
    }
}

void
cob_move_alphanum_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign, count, size;
  unsigned char *p;
  unsigned char *s1 = COB_FIELD_DATA (f1);
  unsigned char *s2 = COB_FIELD_BASE (f2);
  unsigned char *e1 = s1 + COB_FIELD_SIZE (f1);
  unsigned char *e2 = s2 + COB_FIELD_LENGTH (f2);

  /* initialize */
  memset (f2.data, '0', f2.desc->size);

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
  for (p = s1; p < e1 && *p != cob_decimal_point; p++)
    if (isdigit (*p))
      count++;

  /* find the start position */
  size = COB_FIELD_LENGTH (f2) - f2.desc->decimals;
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
      else if (c == cob_decimal_point)
	{
	  if (count++ > 0)
	    goto error;
	}
      else if (!(isspace (c) || c == cob_numeric_separator))
	goto error;
    }

  cob_put_sign (f2, sign);
  finalize_display (f2);
  return;

 error:
  memset (f2.data, '0', f2.desc->size);
  cob_put_sign (f2, 0);
  finalize_display (f2);
}

void
cob_move_display_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign = cob_get_sign (f1);

  memset (f2.data, '0', f2.desc->size);
  COPY_COMMON_REGION (COB_FIELD_BASE (f1), COB_FIELD_LENGTH (f1),
		      COB_FIELD_DECIMALS (f1),
		      COB_FIELD_BASE (f2), COB_FIELD_LENGTH (f2),
		      COB_FIELD_DECIMALS (f2));

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
  finalize_display (f2);
}

void
cob_move_display_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  int sign = cob_get_sign (f1);
  int len1 = COB_FIELD_LENGTH (f1);
  int len2 = COB_FIELD_SIZE (f2);
  unsigned char *base1 = COB_FIELD_BASE (f1);
  unsigned char *base2 = COB_FIELD_DATA (f2);

  if (len1 >= len2)
    {
      memcpy (base2, base1, len2);
    }
  else
    {
      int diff = len2 - len1;
      int zero_len = 0;
      /* move */
      memcpy (base2, base1, len1);
      /* implied 0 ('P's) */
      if (f1.desc->decimals < 0)
	{
	  zero_len = MIN (-f1.desc->decimals, diff);
	  memset (base2 + len1, '0', zero_len);
	}
      /* padding */
      if (diff - zero_len > 0)
	memset (base2 + len1 + zero_len, ' ', diff - zero_len);
    }

  cob_put_sign (f1, sign);
}

void
cob_move_alphanum_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  unsigned int size1 = f1.desc->size;
  unsigned int size2 = f2.desc->size;
  unsigned char *base1 = f1.data;
  unsigned char *base2 = f2.data;

  if (size1 >= size2)
    {
      /* move string with truncation */
      if (f2.desc->justified)
	memcpy (base2, base1 + size1 - size2, size2);
      else
	memcpy (base2, base1, size2);
    }
  else
    {
      /* move string with padding */
      if (f2.desc->justified)
	{
	  memset (base2, ' ', size2 - size1);
	  memcpy (base2 + size2 - size1, base1, size1);
	}
      else
	{
	  memcpy (base2, base1, size1);
	  memset (base2 + size1, ' ', size2 - size1);
	}
    }
}


/*
 * Packed decimal
 */

void
cob_move_display_to_packed (struct cob_field f1, struct cob_field f2)
{
  int i;
  int sign = cob_get_sign (f1);
  int len1 = COB_FIELD_LENGTH (f1);
  int dec1 = COB_FIELD_DECIMALS (f1);
  int len2 = f2.desc->size;
  int dec2 = f2.desc->decimals;
  unsigned char *base1 = COB_FIELD_BASE (f1);
  unsigned char *base2 = f2.data;
  unsigned char *p = base1 + (len1 - dec1) - (len2 - dec2);

  /* pack string */
  memset (f2.data, 0, f2.desc->size);
  for (i = 0; i < len1; i++, p++)
    {
      char n = (base1 <= p && p < base1 + len1) ? *p - '0' : 0;
      if (i % 2 == 0)
	base2[i / 2] = n << 4;
      else
	base2[i / 2] |= n;
    }

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
}

void
cob_move_packed_to_display (struct cob_field f1, struct cob_field f2)
{
  int i;
  int sign = cob_get_sign (f1);
  int len = f1.desc->size;
  unsigned char *base = f1.data;
  unsigned char buff[len];

  /* unpack string */
  for (i = 0; i < len; i++)
    buff[i] = ((i % 2 == 0) ? (base[i/2] >> 4) : (base[i/2] & 0x0f)) + '0';

  /* store */
  memset (f2.data, '0', f2.desc->size);
  COPY_COMMON_REGION (buff, len, f1.desc->decimals,
		      COB_FIELD_BASE (f2), COB_FIELD_LENGTH (f2),
		      COB_FIELD_DECIMALS (f2));

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
  finalize_display (f2);
}


/*
 * Binary integer
 */

void
cob_move_display_to_binary (struct cob_field f1, struct cob_field f2)
{
  int i, len;
  long long val = 0;
  int sign = cob_get_sign (f1);
  int len1 = COB_FIELD_LENGTH (f1);
  int len2 = f2.desc->size;
  unsigned char *base1 = COB_FIELD_BASE (f1);
  unsigned char *base2 = f2.data;

  /* get value */
  len = len1 - f1.desc->decimals + f2.desc->decimals;
  for (i = 0; i < len; ++i)
    if (i < len1)
      val = val * 10 + base1[i] - '0';
    else
      val = val * 10;
  if (sign < 0 && f2.desc->have_sign)
    val = -val;
  val %= cob_exp10LL[(int) f2.desc->digits];

  /* store */
  switch (len2)
    {
    case 1: *(char *) base2 = val; break;
    case 2: *(short *) base2 = val; break;
    case 4: *(long *) base2 = val; break;
    case 8: *(long long *) base2 = val; break;
    }

  cob_put_sign (f1, sign);
}

void
cob_move_binary_to_display (struct cob_field f1, struct cob_field f2)
{
  int i, sign;
  long long val;
  char buff[20]; /* long long is at most 20 digits */

  /* get value */
  switch (f1.desc->size)
    {
    case 1: val = *(char *) f1.data; break;
    case 2: val = *(short *) f1.data; break;
    case 4: val = *(long *) f1.data; break;
    case 8: val = *(long long *) f1.data; break;
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
  memset (f2.data, '0', f2.desc->size);
  COPY_COMMON_REGION (buff + i, 20 - i, f1.desc->decimals,
		      COB_FIELD_BASE (f2), COB_FIELD_LENGTH (f2),
		      COB_FIELD_DECIMALS (f2));

  cob_put_sign (f2, sign);
  finalize_display (f2);
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
cob_move_display_to_edited (struct cob_field f1, struct cob_field f2)
{
  char *p;
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
  for (p = f2.desc->pic; *p; p += 2)
    {
      unsigned char c = p[0];
      if (c == '9' || c == 'P' || c == 'Z' || c == '*' ||
	  c == cob_currency_symbol)
	count += p[1], count_sign = 0;
      else if (count_sign && (c == '+' || c == '-'))
	count += p[1];
      else if (p[0] == 'V' || p[0] == cob_decimal_point)
	break;
    }

  min = COB_FIELD_BASE (f1);
  max = min + COB_FIELD_LENGTH (f1);
  src = max - f1.desc->decimals - count;
  dst = f2.data;
  end = f2.data + f2.desc->size;
  for (p = f2.desc->pic; *p; )
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
	      if (c == 'V' || c == cob_decimal_point)
		{
		  *dst = cob_decimal_point;
		  decimal_point = dst;
		  break;
		}
	      else
		{
		  *dst = suppress_zero ? pad : c; break;
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
		else if (dst == f2.data || suppress_zero)
		  {
		    *dst = pad;
		    sign_symbol = neg ? '-' : (c == '+') ? '+' : ' ';
		  }
		else
		  *dst = x;
		break;
	      }

	    default:
	      if (c == cob_currency_symbol)
		{
		  char x = get ();
		  if (dst == f2.data || suppress_zero)
		    *dst = pad, sign_symbol = cob_currency_symbol;
		  else
		    *dst = x;
		  break;
		}

	      *dst = '?';
	      cob_runtime_error ("invalid PIC char `%c'", c);
	    }
	}
    }

  if (suppress_zero || (is_zero && f2.desc->blank_zero))
    {
      /* all digits are zeros */
      if (pad == ' ' || f2.desc->blank_zero)
	memset (f2.data, ' ', f2.desc->size);
      else
	for (dst = f2.data; dst < f2.data + f2.desc->size; dst++)
	  if (*dst != cob_decimal_point)
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
	  for (dst = end - 1; dst > f2.data; dst--)
	    if (*dst == ' ')
	      break;
	  *dst = sign_symbol;
	}

      /* replace all `B's by pad */
      for (dst = f2.data; dst < end; dst++)
	if (*dst == 'B')
	  *dst = pad;
    }

  cob_put_sign (f1, sign);
}

void
cob_move_alphanum_to_edited (struct cob_field f1, struct cob_field f2)
{
  char *p;
  unsigned char *max, *src, *dst;
  int sign = cob_get_sign (f1);

  src = COB_FIELD_BASE (f1);
  max = src + COB_FIELD_LENGTH (f1);
  dst = f2.data;
  for (p = f2.desc->pic; *p; )
    {
      unsigned char c = *p++; /* PIC char */
      unsigned char n = *p++; /* PIC char count */
      for (; n > 0; n--, dst++)
	{
	  switch (c)
	    {
	    case 'A':
	    case 'X':
	    case '9': *dst = (src < max) ? *src++ : ' '; break;

	    case '0':
	    case '/': *dst = c; break;
	    case 'B': *dst = ' '; break;

	    default:
	      *dst = '?';
	      cob_runtime_error ("invalid PIC char `%c'", c);
	    }
	}
    }
  cob_put_sign (f1, sign);
}


/*
 * MOVE dispatcher
 */

static void
indirect_move (void (*move_func) (struct cob_field src, struct cob_field dst),
	       struct cob_field src, struct cob_field dst,
	       unsigned int size, char decimals)
{
  unsigned char data[size];
  struct cob_field_desc desc = {size, '9', size, decimals, 1};
  struct cob_field temp = {&desc, data};
  move_func (src, temp);
  cob_move (temp, dst);
}

void
cob_move (struct cob_field src, struct cob_field dst)
{
  if (COB_FIELD_TYPE (src) == COB_GROUP || COB_FIELD_TYPE (dst) == COB_GROUP)
    return cob_move_alphanum_to_alphanum (src, dst);

  switch (COB_FIELD_TYPE (src))
    {
    case COB_NUMERIC:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_NUMERIC:
	  return cob_move_display_to_display (src, dst);
	case COB_PACKED:
	  return cob_move_display_to_packed (src, dst);
	case COB_BINARY:
	  return cob_move_display_to_binary (src, dst);
	case COB_NUMERIC_EDITED:
	  return cob_move_display_to_edited (src, dst);
	case COB_ALPHANUMERIC_EDITED:
	  if (src.desc->decimals < 0 || src.desc->decimals > src.desc->digits)
	    /* expand P's */
	    return indirect_move (cob_move_display_to_display, src, dst,
				  MAX (src.desc->digits, src.desc->decimals),
				  MAX (0, src.desc->decimals));
	  else
	    return cob_move_alphanum_to_edited (src, dst);
	default:
	  return cob_move_display_to_alphanum (src, dst);
	}

    case COB_PACKED:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_NUMERIC:
	  return cob_move_packed_to_display (src, dst);
	default:
	  return indirect_move (cob_move_packed_to_display, src, dst,
				src.desc->digits, src.desc->decimals);
	}

    case COB_BINARY:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_NUMERIC:
	  return cob_move_binary_to_display (src, dst);
	default:
	  return indirect_move (cob_move_binary_to_display, src, dst,
				src.desc->digits, src.desc->decimals);
	}

    default:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_NUMERIC:
	  return cob_move_alphanum_to_display (src, dst);
	case COB_PACKED:
	case COB_BINARY:
	case COB_NUMERIC_EDITED:
	  return indirect_move (cob_move_alphanum_to_display,
				src, dst, 36, 18);
	case COB_ALPHANUMERIC_EDITED:
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
cob_mem_move (struct cob_field dst, unsigned char *src, int len)
{
  struct cob_field_desc fld = {len, COB_ALPHANUMERIC};
  cob_move ((struct cob_field) {&fld, src}, dst);
}

int
cob_to_int (struct cob_field f)
{
  int val;
  struct cob_field_desc desc =
    {4, COB_BINARY, f.desc->digits, f.desc->decimals, f.desc->have_sign};
  struct cob_field temp = {&desc, (unsigned char *) &val};
  cob_move (f, temp);
  return val;
}
