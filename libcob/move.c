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

#include "_libcob.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
 * ALL literal
 */

static struct fld_desc all_desc = {1, 'X', 0, 1, 0, 0, 0, 0, "X\001"};

void
cob_move_all (struct cob_field f1, struct cob_field f2)
{
  if (f1.desc->len == 1)
    memset (f2.data, f1.data[0], f2.desc->len);
  else
    {
      int rest = f2.desc->len;
      unsigned char *data = f2.data;
      while (rest > 0)
	{
	  int len = MIN (f1.desc->len, rest);
	  memcpy (data, f1.data, len);
	  data += len;
	  rest -= len;
	}
    }
}

void
cob_move_zero (struct cob_field f)
{
  switch (FIELD_TYPE (f))
    {
    case '9':
      memset (f.data, '0', f.desc->len);
      put_sign (f, 0);
      return;

    case 'C':
      memset (f.data, 0, f.desc->len);
      put_sign (f, 0);
      break;

    case 'B':
      switch (f.desc->len)
	{
	case 1: *(char *) f.data = 0; return;
	case 2: *(short *) f.data = 0; return;
	case 4: *(long *) f.data = 0; return;
	case 8: *(long long *) f.data = 0; return;
	}

    case 'U':
      switch (f.desc->len)
	{
	case 4: *(float *) f.data = 0; return;
	case 8: *(double *) f.data = 0; return;
	}

    default:
      cob_move ((struct cob_field) {&all_desc, "0"}, f);
      return;
    }
}

void
cob_move_space (struct cob_field f)
{
  cob_move ((struct cob_field) {&all_desc, " "}, f);
}

void
cob_move_high (struct cob_field f)
{
  switch (FIELD_TYPE (f))
    {
    case 'B':
      switch (f.desc->len)
	{
	case 1: *(char *) f.data = -1; return;
	case 2: *(short *) f.data = -1; return;
	case 4: *(long *) f.data = -1; return;
	case 8: *(long long *) f.data = -1; return;
	}

    case '9':
      memset (f.data, '9', f.desc->len);
      put_sign (f, 0);
      return;

    default:
      {
	unsigned char c = 255;
	cob_move ((struct cob_field) {&all_desc, &c}, f);
      }
    }
}

void
cob_move_low (struct cob_field f)
{
  switch (FIELD_TYPE (f))
    {
    case '9':
    case 'B':
    case 'C':
    case 'U':
      cob_move_zero (f);
      return;

    default:
      cob_move ((struct cob_field) {&all_desc, "\0"}, f);
    }
}

void
cob_move_quote (struct cob_field f)
{
  cob_move ((struct cob_field) {&all_desc, "\""}, f);
}


/*
 * Display
 */

void
cob_move_alphanum_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign, count, size;
  unsigned char *p;
  unsigned char *s1 = f1.data;
  unsigned char *s2 = FIELD_BASE (f2);
  unsigned char *e1 = s1 + f1.desc->len;
  unsigned char *e2 = s2 + FIELD_LENGTH (f2);

  /* initialize */
  memset (f2.data, '0', f2.desc->len);

  /* skip white spaces */
  for (; s1 < e1; s1++)
    if (!isspace (*s1))
      break;

  /* check for sign */
  sign = 0;
  if (*s1 == '+' || *s1 == '-')
    sign = (*s1++ == '+') ? 0 : 1;

  /* count the number of digits before decimal point */
  count = 0;
  for (p = s1; p < e1 && *p != '.'; p++)
    if (isdigit (*p))
      count++;

  /* find the start position */
  size = FIELD_LENGTH (f2) - f2.desc->decimals;
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
      else if (c == '.')
	{
	  if (count++ > 0)
	    goto error;
	}
      else if (!(isspace (c) || c == ','))
	goto error;
    }

  put_sign (f2, sign);
  return;

 error:
  memset (f2.data, '0', f2.desc->len);
  put_sign (f2, 0);
}

void
cob_move_display_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign = get_sign (f1);

  memset (f2.data, '0', f2.desc->len);
  COPY_COMMON_REGION (FIELD_BASE (f1), FIELD_LENGTH (f1), FIELD_DECIMALS (f1),
		      FIELD_BASE (f2), FIELD_LENGTH (f2), FIELD_DECIMALS (f2));

  put_sign (f1, sign);
  put_sign (f2, sign);
}

void
cob_move_display_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  int sign = get_sign (f1);
  int len1 = FIELD_LENGTH (f1);
  int len2 = f2.desc->len;
  unsigned char *base1 = FIELD_BASE (f1);
  unsigned char *base2 = f2.data;

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

  put_sign (f1, sign);
}

void
cob_move_alphanum_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  int len1 = f1.desc->len;
  int len2 = f2.desc->len;
  unsigned char *base1 = f1.data;
  unsigned char *base2 = f2.data;

  if (len1 >= len2)
    {
      /* just move string, truncating if necessary */
      if (f2.desc->just_r)
	memcpy (base2, base1 + len1 - len2, len2);
      else
	memcpy (base2, base1, len2);
    }
  else
    {
      /* move string with padding */
      if (f2.desc->just_r)
	{
	  memset (base2, ' ', len2 - len1);
	  memcpy (base2 + len2 - len1, base1, len1);
	}
      else
	{
	  memcpy (base2, base1, len1);
	  memset (base2 + len1, ' ', len2 - len1);
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
  int sign = get_sign (f1);
  int len1 = FIELD_LENGTH (f1);
  int dec1 = FIELD_DECIMALS (f1);
  int len2 = f2.desc->len;
  int dec2 = f2.desc->decimals;
  unsigned char *base1 = FIELD_BASE (f1);
  unsigned char *base2 = f2.data;
  unsigned char *p = base1 + (len1 - dec1) - (len2 - dec2);

  /* pack string */
  memset (f2.data, 0, f2.desc->len);
  for (i = 0; i < len1; i++, p++)
    {
      char n = (base1 <= p && p < base1 + len1) ? *p - '0' : 0;
      if (i % 2 == 0)
	base2[i / 2] = n << 4;
      else
	base2[i / 2] |= n;
    }

  put_sign (f1, sign);
  put_sign (f2, sign);
}

void
cob_move_packed_to_display (struct cob_field f1, struct cob_field f2)
{
  int i;
  int sign = get_sign (f1);
  int len = f1.desc->len;
  unsigned char *base = f1.data;
  unsigned char buff[len];

  /* unpack string */
  for (i = 0; i < len; i++)
    buff[i] = ((i % 2 == 0) ? (base[i/2] >> 4) : (base[i/2] & 0x0f)) + '0';

  /* store */
  memset (f2.data, '0', f2.desc->len);
  COPY_COMMON_REGION (buff, len, f1.desc->decimals,
		      FIELD_BASE (f2), FIELD_LENGTH (f2), FIELD_DECIMALS (f2));

  put_sign (f1, sign);
  put_sign (f2, sign);
}


/*
 * Binary integer
 */

void
cob_move_display_to_binary (struct cob_field f1, struct cob_field f2)
{
  int i, len;
  long long val = 0;
  int sign = get_sign (f1);
  int len1 = FIELD_LENGTH (f1);
  int len2 = f2.desc->len;
  unsigned char *base1 = FIELD_BASE (f1);
  unsigned char *base2 = f2.data;

  /* get value */
  len = len1 - f1.desc->decimals + f2.desc->decimals;
  for (i = 0; i < len; ++i)
    if (i < len1)
      val = val * 10 + base1[i] - '0';
    else
      val = val * 10;
  if (sign && FIELD_SIGNED (f2))
    val = -val;
  val %= cob_exp10[picCompLength (f2.desc->pic)];

  /* store */
  switch (len2)
    {
    case 1: *(char *) base2 = val; break;
    case 2: *(short *) base2 = val; break;
    case 4: *(long *) base2 = val; break;
    case 8: *(long long *) base2 = val; break;
    }

  put_sign (f1, sign);
}

void
cob_move_binary_to_display (struct cob_field f1, struct cob_field f2)
{
  int i, sign;
  long long val;
  char buff[20]; /* long long is at most 20 digits */

  /* get value */
  switch (f1.desc->len)
    {
    case 1: val = *(char *) f1.data; break;
    case 2: val = *(short *) f1.data; break;
    case 4: val = *(long *) f1.data; break;
    case 8: val = *(long long *) f1.data; break;
    }

  /* get sign */
  sign = 0;
  if (val < 0)
    {
      sign = 1;
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
  memset (f2.data, '0', f2.desc->len);
  COPY_COMMON_REGION (buff + i, 20 - i, f1.desc->decimals,
		      FIELD_BASE (f2), FIELD_LENGTH (f2), FIELD_DECIMALS (f2));

  put_sign (f2, sign);
}


/*
 * Floating point
 */

void
cob_move_display_to_float (struct cob_field f1, struct cob_field f2)
{
  int i;
  double val = 0;
  int sign = get_sign (f1);
  int len1 = FIELD_LENGTH (f1);
  int len2 = f2.desc->len;
  unsigned char *base1 = FIELD_BASE (f1);
  unsigned char *base2 = f2.data;

  /* get value as long long */
  for (i = 0; i < len1; ++i)
    val = val * 10 + base1[i] - '0';
  if (sign)
    val = -val;
  val /= cob_exp10[(int) FIELD_DECIMALS (f1)];
  // FIXME: we need modulo here
  // val = fmod (val, cob_exp10[picCompLength (f2.desc->pic)]);

  /* store */
  switch (len2)
    {
    case 4: *(float *) base2 = val; break;
    case 8: *(double *) base2 = val; break;
    }

  put_sign (f1, sign);
}

void
cob_move_float_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign = 0;
  double val;
  char buff[40];
  int len = FIELD_LENGTH (f2);
  int decimals = FIELD_DECIMALS (f2);
  unsigned char *base = FIELD_BASE (f2);

  /* get value */
  switch (f1.desc->len)
    {
    case 4: val = *(float *) f1.data; break;
    case 8: val = *(double *) f1.data; break;
    }

  /* get sign */
  if (val < 0)
    {
      sign = 1;
      val = -val;
    }

  /* convert to string */
  if (decimals > 0)
    sprintf (buff, "%%0%d.%df", len, decimals);
  else
    sprintf (buff, "%%0%df", len - decimals);
  sprintf (buff, buff, val);
  memmove (buff + len - decimals - 1, buff + len - decimals, decimals + 1);

  /* store */
  memset (f2.data, '0', f2.desc->len);
  COPY_COMMON_REGION (buff, len, decimals, base, len, decimals);

  put_sign (f2, sign);
}


/*
 * Edited
 */

#define get() ((min <= src && src < max) ? *src++ : (src++, '0'))

void
cob_move_display_to_edited (struct cob_field f1, struct cob_field f2)
{
  char *p;
  int sign = get_sign (f1);
  unsigned char *min, *max, *src, *dst, *end;
  unsigned char pad = ' ';
  int count = 0;
  int count_sign = 1;
  int trailing_sign = 0;
  int suppress_zero = 1;
  unsigned char sign_symbol = 0;
  unsigned char *decimal_point = NULL;

  /* count the number of digit places before decimal point */
  for (p = f2.desc->pic; *p; p += 2)
    {
      unsigned char c = p[0];
      if (c == '9' || c == 'P' || c == 'Z' || c == '*' || c == cCurrencySymbol)
	count += p[1], count_sign = 0;
      else if (count_sign && (c == '+' || c == '-'))
	count += p[1];
      else if (p[0] == '.')
	break;
    }

  min = FIELD_BASE (f1);
  max = min + FIELD_LENGTH (f1);
  src = max - f1.desc->decimals - count;
  dst = f2.data;
  end = f2.data + f2.desc->len;
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
	    case ',': *dst = suppress_zero ? pad : ','; break;
	    case 'P': break;

	    case 'V':
	    case '.': *dst = '.'; decimal_point = dst; break;

	    case '9':
	      *dst = get ();
	      suppress_zero = 0;
	      trailing_sign = 1;
	      break;

	    case 'C':
	    case 'D':
	      p += 2;
	      end = dst;
	      memcpy (dst++, sign ? (c == 'C' ? "CR" : "DB") : "  ", 2);
	      break;

	    case '+':
	    case '-':
	      {
		char x = get ();
		if (trailing_sign)
		  *dst = sign ? '-' : (c == '+') ? '+' : ' ';
		else if (dst == f2.data || (suppress_zero && x == '0'))
		  {
		    *dst = pad;
		    sign_symbol = sign ? '-' : (c == '+') ? '+' : ' ';
		  }
		else
		  *dst = x, suppress_zero = 0;
		break;
	      }

	    case 'Z':
	    case '*':
	      {
		char x = get ();
		pad = (c == '*') ? '*' : ' ';
		if (suppress_zero && x == '0')
		  *dst = pad;
		else
		  *dst = x, suppress_zero = 0;
		break;
	      }

	    default:
	      if (c == cCurrencySymbol)
		{
		  char x = get ();
		  if (dst == f2.data || (suppress_zero && x == '0'))
		    *dst = pad, sign_symbol = cCurrencySymbol;
		  else
		    *dst = x, suppress_zero = 0;
		  break;
		}

	      *dst = '?';
	      fprintf (stderr, "cob_move: invalid PIC char: `%c'\n", c);
	    }
	}
    }

  if (suppress_zero)
    {
      /* all digits are zeros */
      if (pad == ' ')
	memset (f2.data, ' ', f2.desc->len);
      else
	for (dst = f2.data; dst < f2.data + f2.desc->len; dst++)
	  if (*dst != '.')
	    *dst = pad;
    }
  else
    {
      /* put zero after the decimal point if necessary */
      if (decimal_point)
	for (dst = decimal_point + 1; dst < end; dst++)
	  if (!isdigit (*dst) && !strchr (",+-", *dst))
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

  put_sign (f1, sign);
}

void
cob_move_alphanum_to_edited (struct cob_field f1, struct cob_field f2)
{
  char *p;
  unsigned char *max, *src, *dst;

  max = f1.data + f1.desc->len;
  src = f1.data;
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
	      fprintf (stderr, "invalid PIC char: `%c'\n", c);
	    }
	}
    }
}


/*
 * MOVE dispatcher
 */

static int
is_numeric_edited (const char *pic)
{
  for (; *pic; pic += 2)
    if (*pic == 'A' || *pic == 'X')
      return 0;
  return 1;
}

static void
indirect_move (void (*move_func) (struct cob_field f1, struct cob_field f2),
	       struct cob_field f1, struct cob_field f2, char *pic)
{
  static unsigned char temp_data[36];
  static struct fld_desc temp_desc = {36, '9', 0, 0, 0, 0, 0, 0, 0};
  static struct cob_field temp = {&temp_desc, temp_data};
  temp.desc->pic = pic;
  temp.desc->len = picCompLength (pic);
  temp.desc->decimals = picCompDecimals (pic);
  move_func (f1, temp);
  cob_move (temp, f2);
}

void
cob_move (struct cob_field f1, struct cob_field f2)
{
  if (f1.desc->all)
    switch (FIELD_TYPE (f2))
      {
      case DTYPE_PACKED:
      case DTYPE_BININT:
      case DTYPE_FLOAT:
      case DTYPE_EDITED:
	return indirect_move (cob_move_all, f1, f2, f2.desc->pic);
      default:
	return cob_move_all (f1, f2);
      }

  switch (FIELD_TYPE (f1))
    {
    case DTYPE_DISPLAY:
      switch (FIELD_TYPE (f2))
	{
	case DTYPE_DISPLAY:
	  return cob_move_display_to_display (f1, f2);
	case DTYPE_PACKED:
	  return cob_move_display_to_packed (f1, f2);
	case DTYPE_BININT:
	  return cob_move_display_to_binary (f1, f2);
	case DTYPE_FLOAT:
	  return cob_move_display_to_float (f1, f2);
	case DTYPE_EDITED:
	  if (is_numeric_edited (f2.desc->pic))
	    return cob_move_display_to_edited (f1, f2);
	  else
	    return cob_move_alphanum_to_edited (f1, f2);
	default:
	  return cob_move_display_to_alphanum (f1, f2);
	}

    case DTYPE_PACKED:
      switch (FIELD_TYPE (f2))
	{
	case DTYPE_DISPLAY:
	  return cob_move_packed_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_packed_to_display,
				f1, f2, f1.desc->pic);
	}

    case DTYPE_BININT:
      switch (FIELD_TYPE (f2))
	{
	case DTYPE_DISPLAY:
	  return cob_move_binary_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_binary_to_display,
				f1, f2, f1.desc->pic);
	}

    case DTYPE_FLOAT:
      switch (FIELD_TYPE (f2))
	{
	case DTYPE_DISPLAY:
	  return cob_move_float_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_float_to_display,
				f1, f2, f1.desc->pic);
	}

    case DTYPE_GROUP:
      return cob_move_alphanum_to_alphanum (f1, f2);

    default:
      switch (FIELD_TYPE (f2))
	{
	case DTYPE_DISPLAY:
	  return cob_move_alphanum_to_display (f1, f2);
	case DTYPE_PACKED:
	case DTYPE_BININT:
	case DTYPE_FLOAT:
	indirect:
	  return indirect_move (cob_move_alphanum_to_display,
				f1, f2, "S\0019\022V\0019\022");
	case DTYPE_EDITED:
	  if (is_numeric_edited (f2.desc->pic))
	    goto indirect;
	  else
	    return cob_move_alphanum_to_edited (f1, f2);
	default:
	  return cob_move_alphanum_to_alphanum (f1, f2);
	}
    }
}

void
cob_move_2 (struct fld_desc *f1, char *s1, struct fld_desc *f2, char *s2)
{
  cob_move ((struct cob_field) {f1, s1}, (struct cob_field) {f2, s2});
}
