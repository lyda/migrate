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

void
print_field (struct cob_field f)
{
  int i;
  printf ("field 0x%p len=%ld type=%c decimals=%d data=[",
	  f.desc, f.desc->len, f.desc->type, f.desc->decimals);
  for (i = 0; i < f.desc->len; i++)
    putchar (f.data[i]);
  puts ("]");
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
  unsigned char *endp1 = f1.data + f1.desc->len;
  unsigned char *s2 = f2.data;
  unsigned char *endp2 = f2.data + f2.desc->len;

  /* initialize */
  memset (f2.data, '0', f2.desc->len);

  /* skip white spaces */
  for (; s1 < endp1; s1++)
    if (!isspace (*s1))
      break;

  /* check for sign */
  sign = 0;
  if (*s1 == '+' || *s1 == '-')
    sign = (*s1++ == '+') ? 0 : 1;

  /* count the number of digits before decimal point */
  count = 0;
  for (p = s1; p < endp1 && *p != '.'; p++)
    if (isdigit (*p))
      count++;

  /* find the start position */
  size = f2.desc->len - f2.desc->decimals;
  if (count < size)
    s2 += size - count;
  else
    while (count-- > size)
      while (!isdigit (*s1++));

  /* move */
  count = 0;
  for (; s1 < endp1 && s2 < endp2; s1++)
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

  put_sign (f2.desc, f2.data, sign);
  return;

 error:
  memset (f2.data, '0', f2.desc->len);
  put_sign (f2.desc, f2.data, 0);
}

void
cob_move_display_to_display (struct cob_field f1, struct cob_field f2)
{
  int sign = extract_sign (f1.desc, f1.data);
  int hf1 = f1.desc->len - f1.desc->decimals;
  int hf2 = f2.desc->len - f2.desc->decimals;
  int lf1 = -f1.desc->decimals;
  int lf2 = -f2.desc->decimals;
  int gcf = MIN (hf1, hf2);
  int lcf = MAX (lf1, lf2);
  unsigned char *s1 = f1.data + (hf1 - gcf);
  unsigned char *s2 = f2.data + (hf2 - gcf);
  unsigned char *e1 = f1.data + (hf1 - lcf);

  /* initialize by 0 */
  memset (f2.data, '0', f2.desc->len);

  /* copy common region if exists */
  if (s1 < e1)
    memcpy (s2, s1, e1 - s1);

  put_sign (f1.desc, f1.data, sign);
  put_sign (f2.desc, f2.data, sign);
}

void
cob_move_display_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  int sign = extract_sign (f1.desc, f1.data);

  if (f1.desc->len >= f2.desc->len)
    {
      memcpy (f2.data, f1.data, f2.desc->len);
    }
  else
    {
      int diff = f2.desc->len - f1.desc->len;
      int zero_len = 0;
      /* move */
      memcpy (f2.data, f1.data, f1.desc->len);
      /* implied 0 ('P's) */
      if (f1.desc->decimals < 0)
	{
	  zero_len = MIN (-f1.desc->decimals, diff);
	  memset (f2.data + f1.desc->len, '0', zero_len);
	}
      /* padding */
      if (diff - zero_len > 0)
	memset (f2.data + f1.desc->len + zero_len, ' ', diff - zero_len);
    }

  put_sign (f1.desc, f1.data, sign);
}

void
cob_move_alphanum_to_alphanum (struct cob_field f1, struct cob_field f2)
{
  int len1 = f1.desc->len;
  int len2 = f2.desc->len;
  if (len1 >= len2)
    {
      /* just move string, truncating if necessary */
      if (f2.desc->just_r)
	memcpy (f2.data, f1.data + len1 - len2, len2);
      else
	memcpy (f2.data, f1.data, len2);
    }
  else
    {
      /* move string with padding */
      if (f2.desc->just_r)
	{
	  memset (f2.data, ' ', len2 - len1);
	  memcpy (f2.data + len2 - len1, f1.data, len1);
	}
      else
	{
	  memcpy (f2.data, f1.data, len1);
	  memset (f2.data + len1, ' ', len2 - len1);
	}
    }
}

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


/*
 * Binary integer
 */

void
cob_move_display_to_binary (struct cob_field f1, struct cob_field f2)
{
  int i, len;
  int sign = extract_sign (f1.desc, f1.data);
  long long val = 0;

  len = f1.desc->len - f1.desc->decimals + f2.desc->decimals;
  for (i = 0; i < len; ++i)
    if (i < f1.desc->len)
      val = val * 10 + f1.data[i] - '0';
    else
      val = val * 10;
  if (sign)
    val *= -1;
  val %= cob_exp10[picCompLength (f2.desc->pic)];

  switch (f2.desc->len)
    {
    case 1: *(char *) f2.data = val; break;
    case 2: *(short *) f2.data = val; break;
    case 4: *(long *) f2.data = val; break;
    case 8: *(long long *) f2.data = val; break;
    }

  put_sign (f1.desc, f1.data, sign);
}

void
cob_move_binary_to_display (struct cob_field f1, struct cob_field f2)
{
  int i, sign = 0;
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

  /* copy string */
  {
    int temp_len = 20 - i;
    unsigned char *temp_data = buff + i;
    int hf1 = temp_len - f1.desc->decimals;
    int hf2 = f2.desc->len - f2.desc->decimals;
    int lf1 = -f1.desc->decimals;
    int lf2 = -f2.desc->decimals;
    int gcf = MIN (hf1, hf2);
    int lcf = MAX (lf1, lf2);
    unsigned char *s1 = temp_data + (hf1 - gcf);
    unsigned char *s2 = f2.data + (hf2 - gcf);
    unsigned char *e1 = temp_data + (hf1 - lcf);

    memset (f2.data, '0', f2.desc->len);
    if (s1 < e1)
      memcpy (s2, s1, e1 - s1);
  }

  put_sign (f2.desc, f2.data, sign);
}


/*
 * Floating point
 */

void
cob_move_display_to_float (struct cob_field f1, struct cob_field f2)
{
  puts ("not implemented");
}

void
cob_move_float_to_display (struct cob_field f1, struct cob_field f2)
{
  puts ("not implemented");
}


/*
 * Packed decimal
 */

void
cob_move_display_to_packed (struct cob_field f1, struct cob_field f2)
{
  puts ("not implemented");
}

void
cob_move_packed_to_display (struct cob_field f1, struct cob_field f2)
{
  puts ("not implemented");
}


/*
 * Edited
 */

#define get() ((min <= src && src < max) ? *src++ : (src++, '0'))

void
cob_move_display_to_edited (struct cob_field f1, struct cob_field f2)
{
  char *p;
  int sign = extract_sign (f1.desc, f1.data);
  unsigned char *min, *max, *src, *dst, *end;
  unsigned char pad = ' ';
  int count = 0;
  int count_sign = 1;
  int trailing_sign = 0;
  int have_digits = 0;
  unsigned char sign_symbol = 0;
  unsigned char *decimal_point = NULL;

  /* count the number of digit places before decimal point */
  for (p = f2.desc->pic; *p; p += 2)
    {
      unsigned char c = p[0];
      if (c == '9' || c == 'Z' || c == '*' || c == cCurrencySymbol)
	count += p[1], count_sign = 0;
      else if (count_sign && (c == '+' || c == '-'))
	count += p[1];
      else if (p[0] == '.')
	break;
    }

  min = f1.data;
  max = f1.data + f1.desc->len;
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
	    case '9': *dst = get (); have_digits = trailing_sign = 1; break;

	    case '0':
	    case '/': *dst = c; break;
	    case 'B': *dst = pad; break;
	    case 'P': *dst = have_digits ? '0' : pad; break;

	    case 'V':
	    case '.': *dst = pad; decimal_point = dst; break;
	    case ',': *dst = have_digits ? ',' : pad; break;

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
		else if (dst == f2.data || (!have_digits && x == '0'))
		  {
		    *dst = pad;
		    sign_symbol = sign ? '-' : (c == '+') ? '+' : ' ';
		  }
		else
		  *dst = x, have_digits = 1;
		break;
	      }

	    case 'Z':
	    case '*':
	      {
		char x = get ();
		pad = (c == '*') ? '*' : ' ';
		if (!have_digits && x == '0')
		  *dst = pad;
		else
		  *dst = x, have_digits = 1;
		break;
	      }

	    default:
	      if (c == cCurrencySymbol)
		{
		  char x = get ();
		  if (dst == f2.data || (!have_digits && x == '0'))
		    *dst = pad, sign_symbol = cCurrencySymbol;
		  else
		    *dst = x, have_digits = 1;
		  break;
		}

	      *dst = '?';
	      fprintf (stderr, "invalid PIC char: `%c'\n", c);
	    }
	}
    }

  if (have_digits && decimal_point)
    {
      *decimal_point = '.';
      for (dst = decimal_point + 1; dst < end; dst++)
	if (!isdigit (*dst) && *dst != ',')
	  *dst = '0';
    }

  if (have_digits && sign_symbol)
    {
      for (dst = end - 1; dst > f2.data; dst--)
	if (*dst == ' ' || *dst == '*')
	  break;
      *dst = sign_symbol;
    }

  put_sign (f1.desc, f1.data, sign);
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

static int
is_numeric_edited (const char *pic)
{
  for (; *pic; pic += 2)
    if (strchr ("AX9B0/", *pic) == NULL)
      return 1;
  return 0;
}

void
cob_move (struct cob_field f1, struct cob_field f2)
{
  if (f1.desc->all)
    switch (f1.desc->type)
      {
      case DTYPE_BININT:
	return indirect_move (cob_move_binary_to_display,
			      f1, f2, f1.desc->pic);
      case DTYPE_FLOAT:
	return indirect_move (cob_move_float_to_display,
			      f1, f2, f1.desc->pic);
      case DTYPE_PACKED:
	return indirect_move (cob_move_packed_to_display,
			      f1, f2, f1.desc->pic);
      default:
	return cob_move_all (f1, f2);
      }

  switch (f1.desc->type)
    {
    case DTYPE_DISPLAY:
      switch (f2.desc->type)
	{
	case DTYPE_DISPLAY:
	  return cob_move_display_to_display (f1, f2);
	case DTYPE_BININT:
	  return cob_move_display_to_binary (f1, f2);
	case DTYPE_FLOAT:
	  return cob_move_display_to_float (f1, f2);
	case DTYPE_PACKED:
	  return cob_move_display_to_packed (f1, f2);
	case DTYPE_EDITED:
	  if (is_numeric_edited (f2.desc->pic))
	    return cob_move_display_to_edited (f1, f2);
	  else
	    return cob_move_alphanum_to_edited (f1, f2);
	default:
	  return cob_move_display_to_alphanum (f1, f2);
	}

    case DTYPE_BININT:
      switch (f2.desc->type)
	{
	case DTYPE_DISPLAY:
	  return cob_move_binary_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_binary_to_display,
				f1, f2, f1.desc->pic);
	}

    case DTYPE_FLOAT:
      switch (f2.desc->type)
	{
	case DTYPE_DISPLAY:
	  return cob_move_float_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_float_to_display,
				f1, f2, f1.desc->pic);
	}

    case DTYPE_PACKED:
      switch (f2.desc->type)
	{
	case DTYPE_DISPLAY:
	  return cob_move_packed_to_display (f1, f2);
	default:
	  return indirect_move (cob_move_packed_to_display,
				f1, f2, f1.desc->pic);
	}

    default:
      switch (f2.desc->type)
	{
	case DTYPE_DISPLAY:
	  return cob_move_alphanum_to_display (f1, f2);
	case DTYPE_BININT:
	case DTYPE_FLOAT:
	case DTYPE_PACKED:
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


void
cob_move_zero (struct cob_field f)
{
  switch (f.desc->type)
    {
    case 'B':
      switch (f.desc->len)
	{
	case 1: *(char *) f.data = 0; return;
	case 2: *(short *) f.data = 0; return;
	case 4: *(long *) f.data = 0; return;
	case 8: *(long long *) f.data = 0; return;
	}

    case '9':
      memset (f.data, '0', f.desc->len);
      put_sign (f.desc, f.data, 0);
      return;

    default:
      {
	static struct fld_desc desc = {1, '9', 0, 1, 0, 0, 0, 0, "9\001"};
	static struct cob_field zero = {&desc, "0"};
	cob_move (zero, f);
	return;
      }
    }
}

void
cob_move_space (struct cob_field f)
{
  static struct fld_desc desc = {1, 'X', 0, 1, 0, 0, 0, 0, "X\001"};
  static struct cob_field space = {&desc, " "};
  cob_move (space, f);
}
