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
#include <math.h>

#include "move.h"
#include "byteswap.h"
#include "lib/gettext.h"

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

static void
store_common_region (cob_field *f, unsigned char *data, size_t size, int scale)
{
  int lf1 = -scale;
  int lf2 = -COB_FIELD_SCALE (f);
  int hf1 = size + lf1;
  int hf2 = COB_FIELD_SIZE (f) + lf2;
  int lcf = MAX (lf1, lf2);
  int gcf = MIN (hf1, hf2);

  if (gcf > lcf)
    {
      memset (COB_FIELD_DATA (f), '0', hf2 - gcf);
      memcpy (COB_FIELD_DATA (f) + hf2 - gcf, data + hf1 - gcf, gcf - lcf);
      memset (COB_FIELD_DATA (f) + hf2 - lcf, '0', lcf - lf2);
    }
  else
    memset (f->data, '0', f->size);
}


/*
 * Display
 */

static void
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
  for (p = s1; p < e1 && *p != cob_current_module->decimal_point; p++)
    if (isdigit (*p))
      count++;

  /* find the start position */
  size = COB_FIELD_SIZE (f2) - f2->attr->scale;
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
      else if (c == cob_current_module->decimal_point)
	{
	  if (count++ > 0)
	    goto error;
	}
      else if (!(isspace (c) || c == cob_current_module->numeric_separator))
	goto error;
    }

  cob_put_sign (f2, sign);
  return;

 error:
  memset (f2->data, '0', f2->size);
  cob_put_sign (f2, 0);
}

static void
cob_move_display_to_display (cob_field *f1, cob_field *f2)
{
  int sign = cob_get_sign (f1);

  store_common_region (f2, COB_FIELD_DATA (f1), COB_FIELD_SIZE (f1),
		       COB_FIELD_SCALE (f1));

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
}

static void
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
      if (f1->attr->scale < 0)
	{
	  zero_size = MIN (-f1->attr->scale, diff);
	  memset (data2 + size1, '0', zero_size);
	}
      /* padding */
      if (diff - zero_size > 0)
	memset (data2 + size1 + zero_size, ' ', diff - zero_size);
    }

  cob_put_sign (f1, sign);
}

static void
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

static void
cob_move_display_to_packed (cob_field *f1, cob_field *f2)
{
  size_t i;
  int offset;
  int sign = cob_get_sign (f1);
  int digits1 = COB_FIELD_DIGITS (f1);
  int digits2 = COB_FIELD_DIGITS (f2);
  int scale1 = COB_FIELD_SCALE (f1);
  int scale2 = COB_FIELD_SCALE (f2);
  unsigned char *data1 = COB_FIELD_DATA (f1);
  unsigned char *data2 = f2->data;
  unsigned char *p = data1 + (digits1 - scale1) - (digits2 - scale2);

  /* pack string */
  memset (f2->data, 0, f2->size);
  offset = 1 - (digits2 % 2);
  for (i = offset; i < digits2 + offset; i++, p++)
    {
      char n = (data1 <= p && p < data1 + digits1) ? cob_d2i (*p): 0;
      if (i % 2 == 0)
	data2[i/2] = n << 4;
      else
	data2[i/2] |= n;
    }

  cob_put_sign (f1, sign);
  cob_put_sign (f2, sign);
  if (!COB_FIELD_HAVE_SIGN (f2))
    data2[digits2 / 2] |= 0x0f;
}

static void
cob_move_packed_to_display (cob_field *f1, cob_field *f2)
{
  size_t i;
  int offset;
  int sign = cob_get_sign (f1);
  unsigned char *data = f1->data;
  unsigned char buff[f1->attr->digits];

  /* unpack string */
  offset = 1 - (f1->attr->digits % 2);
  for (i = offset; i < f1->attr->digits + offset; i++)
    if (i % 2 == 0)
      buff[i - offset] = cob_i2d (data[i/2] >> 4);
    else
      buff[i - offset] = cob_i2d (data[i/2] & 0x0f);

  /* store */
  store_common_region (f2, buff, COB_FIELD_DIGITS (f1), COB_FIELD_SCALE (f1));

  cob_put_sign (f2, sign);
}

/*
 * Floating point
 */

static void
cob_move_display_to_fp (cob_field *f1, cob_field *f2)
{
	double		val;
	size_t		size;
	int		sign = cob_get_sign (f1);
	size_t		size1 = COB_FIELD_SIZE (f1);
	char		*data1;
	char		buff2[64];

	memset(buff2, 0, sizeof(buff2));
	size = size1 - f1->attr->scale;
	if ( sign < 0 ) {
		buff2[0] = '-';
		data1 = &buff2[1];
	} else {
		data1 = buff2;
	}
	if ( f1->attr->scale <= 0 ) {
		sprintf(data1, "%*.*s.0", size, size, f1->data);
	} else {
		sprintf(data1, "%*.*s.%*.*s", size, size, f1->data,
			f1->attr->scale, f1->attr->scale,
			f1->data + size);
	}
	sscanf(buff2, "%lf", &val);
	if ( COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_FLOAT ) {
		float	flval = val;
		*(float *)f2->data = flval;
	} else {
		*(double *)f2->data = val;
	}
}

static void
cob_move_fp_to_display (cob_field *f1, cob_field *f2)
{
	double		val;
	double		frac;
	double		intgr;
	int		sign;
	int		decs;
	long long	res;
	char		*x, *y;
	char		buff[64];
	char		buff2[64];

	memset(buff, 0, sizeof(buff));
	memset(buff2, 0, sizeof(buff2));
	if ( COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_FLOAT ) {
		val = (double)(*(float *)f1->data);
	} else {
		val = *(double *)f1->data;
	}
	sign = 1;
	if (val < 0) {
		sign = -1;
		val = -val;
	}
	frac = modf(val, &intgr);
	res = intgr;
	decs = 0;
	for ( ; res ; res /= 10 ) {
		decs++;
	}
	sprintf(buff2, "%-18.*lf", 18 - decs, val);
	y = buff;
	for ( x = buff2; *x; x++ ) {
		if ( *x == '.' ) {
			continue;
		}
		if ( *x == ' ' ) {
			continue;
		}
		*y++ = *x;
	}
		
	store_common_region (f2, buff, strlen(buff), 18 - decs);
	cob_put_sign (f2, sign);
}

/*
 * Binary integer
 */

static void
cob_move_display_to_binary (cob_field *f1, cob_field *f2)
{
  size_t i, size;
  long long val = 0;
  int sign = cob_get_sign (f1);
  size_t size1 = COB_FIELD_SIZE (f1);
  unsigned char *data1 = COB_FIELD_DATA (f1);

  /* get value */
  size = size1 - f1->attr->scale + f2->attr->scale;
  for (i = 0; i < size; ++i)
    if (i < size1)
      val = val * 10 + cob_d2i (data1[i]);
    else
      val = val * 10;
  if (sign < 0 && COB_FIELD_HAVE_SIGN (f2))
    val = -val;
  if (cob_current_module->flag_binary_truncate)
    val %= cob_exp10LL[(int) f2->attr->digits];

  /* store */
  cob_binary_set_int64 (f2, val);

  cob_put_sign (f1, sign);
}

static void
cob_move_binary_to_display (cob_field *f1, cob_field *f2)
{
  int i, sign;
  long long val = 0;
  char buff[32]; /* long long is at most 20 digits */

  /* get value */
  val = cob_binary_get_int64 (f1);
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
      buff[--i] = cob_i2d (val % 10);
      val /= 10;
    }

  /* store */
  store_common_region (f2, buff + i, 20 - i, f1->attr->scale);

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

static void
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
	  c == cob_current_module->currency_symbol)
	count += p[1], count_sign = 0;
      else if (count_sign && (c == '+' || c == '-'))
	count += p[1];
      else if (p[0] == 'V' || p[0] == cob_current_module->decimal_point)
	break;
    }

  min = COB_FIELD_DATA (f1);
  max = min + COB_FIELD_SIZE (f1);
  src = max - f1->attr->scale - count;
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
	      if (c == 'V' || c == cob_current_module->decimal_point)
		{
		  if (c == 'V' && (*p == '.' || *p == ','))
		    {
		      /* When "V," or "V." is specified,
			 enforce to use it as decimal point */
		      *dst = *p;
		      p += 2;
		    }
		  else
		    *dst = cob_current_module->decimal_point;
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
		trailing_sign = 1;
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
	      if (c == cob_current_module->currency_symbol)
		{
		  char x = get ();
		  if (dst == f2->data || suppress_zero)
		    *dst = pad, sign_symbol = cob_current_module->currency_symbol;
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
	  if (*dst != cob_current_module->decimal_point)
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

      /* replace all 'B's by pad */
      count = 0;
      for (dst = f2->data; dst < end; dst++)
      {
	if ( *dst == 'B' ) {
		if ( count == 0 ) {
			*dst = pad;
		} else {
			*dst = ' ';
		}
	} else {
		count++;
	}
      }
    }

  cob_put_sign (f1, sign);
}

static void
cob_move_edited_to_display (cob_field *f1, cob_field *f2)
{
  int i;
  int sign = 0;
  int scale = 0;
  int have_point = 0;
  unsigned char buff[f1->size];
  unsigned char *p = buff;

  /* de-edit */
  for (i = 0; i < f1->size; i++)
    {
      int c = f1->data[i];
      switch (c)
	{
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  *p++ = c;
	  if (have_point)
	    scale++;
	  break;
	case '.': case ',':
	  if (c == cob_current_module->decimal_point)
	    have_point = 1;
	  break;
	case '-':
	case 'C':
	  sign = -1;
	  break;
	}
    }

  /* store */
  store_common_region (f2, buff, p - buff, scale);

  cob_put_sign (f2, sign);
}

static void
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
	       unsigned int size, char scale)
{
  cob_field temp;
  cob_field_attr attr =
    {COB_TYPE_NUMERIC_DISPLAY, size, scale, COB_FLAG_HAVE_SIGN, NULL};
/* Worse code
  cob_field temp = {size, data, &attr};
*/
  unsigned char data[64];

  temp.size = size;
  temp.data = data;
  temp.attr = &attr;
  func (src, &temp);
  cob_move (&temp, dst);
}

static void
cob_move_all (cob_field *src, cob_field *dst)
{
  int i;
  cob_field temp;
  cob_field_attr attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
/* Worse code
  cob_field temp = {dst->size, data, &attr};
*/
  unsigned char data[dst->size];

  temp.size = dst->size;
  temp.data = data;
  temp.attr = &attr;
  for (i = 0; i < dst->size; i++)
    data[i] = src->data[i % src->size];

  cob_move (&temp, dst);
}

void
cob_move (cob_field *src, cob_field *dst)
{
  if (COB_FIELD_TYPE (src) == COB_TYPE_ALPHANUMERIC_ALL)
    return cob_move_all (src, dst);

  /* non-elementary move */
  if (COB_FIELD_TYPE (src) == COB_TYPE_GROUP
      || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP)
    return cob_move_alphanum_to_alphanum (src, dst);

  /* elementary move */
  switch (COB_FIELD_TYPE (src))
    {
    case COB_TYPE_NUMERIC_FLOAT:
    case COB_TYPE_NUMERIC_DOUBLE:
	return indirect_move (cob_move_fp_to_display, src, dst, 40, 20);
    case COB_TYPE_NUMERIC_DISPLAY:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
		return cob_move_display_to_fp (src, dst);
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_display_to_display (src, dst);
	case COB_TYPE_NUMERIC_PACKED:
	  return cob_move_display_to_packed (src, dst);
	case COB_TYPE_NUMERIC_BINARY:
	  return cob_move_display_to_binary (src, dst);
	case COB_TYPE_NUMERIC_EDITED:
	  return cob_move_display_to_edited (src, dst);
	case COB_TYPE_ALPHANUMERIC_EDITED:
	  if (src->attr->scale < 0
	      || src->attr->scale > src->attr->digits)
	    /* expand P's */
	    return indirect_move (cob_move_display_to_display, src, dst,
				  MAX (src->attr->digits, src->attr->scale),
				  MAX (0, src->attr->scale));
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
				src->attr->digits, src->attr->scale);
	}

    case COB_TYPE_NUMERIC_BINARY:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_binary_to_display (src, dst);
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_EDITED:
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
	  return indirect_move (cob_move_binary_to_display, src, dst,
				20, src->attr->scale);
	default:
	  return indirect_move (cob_move_binary_to_display, src, dst,
				src->attr->digits, src->attr->scale);
	}

    case COB_TYPE_NUMERIC_EDITED:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_edited_to_display (src, dst);
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_EDITED:
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
	  return indirect_move (cob_move_edited_to_display,
				src, dst, 36, 18);
	case COB_TYPE_ALPHANUMERIC_EDITED:
	  return cob_move_alphanum_to_edited (src, dst);
	default:
	  return cob_move_alphanum_to_alphanum (src, dst);
	}

    default:
      switch (COB_FIELD_TYPE (dst))
	{
	case COB_TYPE_NUMERIC_DISPLAY:
	  return cob_move_alphanum_to_display (src, dst);
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_EDITED:
	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
	  return indirect_move (cob_move_alphanum_to_display,
				src, dst, 36, 18);
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

static int
cob_display_get_int (cob_field *f)
{
  size_t i;
  int val = 0;
  int sign = cob_get_sign (f);
  size_t size = COB_FIELD_SIZE (f);
  unsigned char *data = COB_FIELD_DATA (f);

  /* skip preceding zeros */
  for (i = 0; i < size; i++)
    if (cob_d2i (data[i]) != 0)
      break;

  /* get value */
  if (f->attr->scale < 0)
    {
      for (; i < size; ++i)
	val = val * 10 + cob_d2i (data[i]);
      val *= cob_exp10[(int) -f->attr->scale];
    }
  else
    {
      size -= f->attr->scale;
      for (; i < size; ++i)
	val = val * 10 + cob_d2i (data[i]);
    }
  if (sign < 0)
    val = -val;

  cob_put_sign (f, sign);
  return val;
}

int
cob_binary_get_int (cob_field *f)
{
  return cob_binary_get_int64 (f);
}

long long
cob_binary_get_int64 (cob_field *f)
{
  long long n = 0;
  int fsiz = 8 - f->size;
#ifndef WORDS_BIGENDIAN
  if (COB_FIELD_BINARY_SWAP (f))
    {
      if (COB_FIELD_HAVE_SIGN (f))
	{
	  memcpy (&n, f->data, f->size);
	  n = COB_BSWAP_64 (n);
	  n >>= 8 * fsiz; /* shift with sign */
	}
      else
	{
	  memcpy (((unsigned char *) &n) + fsiz, f->data, f->size);
	  n = COB_BSWAP_64 (n);
	}
    }
  else
    {
      if (COB_FIELD_HAVE_SIGN (f))
	{
	  memcpy (((unsigned char *) &n) + fsiz, f->data, f->size);
	  n >>= 8 * fsiz; /* shift with sign */
	}
      else
	{
	  memcpy (&n, f->data, f->size);
	}
    }
#else /* WORDS_BIGENDIAN */
  if (COB_FIELD_HAVE_SIGN (f))
    {
      memcpy (&n, f->data, f->size);
      n >>= 8 * (8 - f->size); /* shift with sign */
    }
  else
    {
      memcpy (((unsigned char *) &n) + 8 - f->size, f->data, f->size);
    }
#endif /* WORDS_BIGENDIAN */
  return n;
}

void
cob_binary_set_int (cob_field *f, int n)
{
  cob_binary_set_int64 (f, n);
}

void
cob_binary_set_int64 (cob_field *f, long long n)
{
#ifndef WORDS_BIGENDIAN
  if (COB_FIELD_BINARY_SWAP (f))
    {
      n = COB_BSWAP_64 (n);
      memcpy (f->data, ((unsigned char *) &n) + 8 - f->size, f->size);
    }
  else
    {
      memcpy (f->data, &n, f->size);
    }
#else /* WORDS_BIGENDIAN */
  memcpy (f->data, ((unsigned char *) &n) + 8 - f->size, f->size);
#endif /* WORDS_BIGENDIAN */
}

void
cob_set_int (cob_field *f, int n)
{
  cob_field temp;
  cob_field_attr attr = {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, 0};

/* Generates worse code
  cob_field temp = {4, (unsigned char *) &n, &attr};
*/
  temp.size = 4;
  temp.data = (unsigned char *) &n;
  temp.attr = &attr;
  cob_move (&temp, f);
}

int
cob_get_int (cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      return cob_display_get_int (f);
    case COB_TYPE_NUMERIC_BINARY:
      return cob_binary_get_int (f);
    default:
      {
	int n;
	cob_field temp;
	cob_field_attr attr =
	  {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL};
	/* Worse code 
	cob_field temp = {4, (unsigned char *) &n, &attr};
	*/
	temp.size = 4;
	temp.data = (unsigned char *) &n;
	temp.attr = &attr;
	cob_move (f, &temp);
	return n;
      }
    }
}

void
cob_memcpy (cob_field *dst, unsigned char *src, int size)
{
  cob_field temp;
  cob_field_attr attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/* Worse code
  cob_field temp = {size, src, &attr};
*/
  temp.size = size;
  temp.data = src;
  temp.attr = &attr;
  cob_move (&temp , dst);
}
