/*
 * Copyright (C) 2002-2007 Keisuke Nishida
 * Copyright (C) 2007 Roger While
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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "move.h"
#include "byteswap.h"
/*
#include "lib/gettext.h"
*/

static inline int
cob_min_int (const int x, const int y)
{
	if (x < y) {
		return x;
	}
	return y;
}

static inline int
cob_max_int (const int x, const int y)
{
	if (x > y) {
		return x;
	}
	return y;
}

static void
store_common_region (cob_field *f, const unsigned char *data,
		     const size_t size, const int scale)
{
	int	lf1 = -scale;
	int	lf2 = -COB_FIELD_SCALE (f);
	int	hf1 = (int) size + lf1;
	int	hf2 = (int) COB_FIELD_SIZE (f) + lf2;
	int	lcf;
	int	gcf;

	lcf = cob_max_int (lf1, lf2);
	gcf = cob_min_int (hf1, hf2);
	if (gcf > lcf) {
		own_memset (COB_FIELD_DATA (f), '0', (size_t)(hf2 - gcf));
		own_memcpy (COB_FIELD_DATA (f) + hf2 - gcf, data + hf1 - gcf,
				(size_t)(gcf - lcf));
		own_memset (COB_FIELD_DATA (f) + hf2 - lcf, '0', (size_t)(lcf - lf2));
	} else {
		own_memset (f->data, '0', f->size);
	}
}

/*
 * Display
 */

static void
cob_move_alphanum_to_display (cob_field *f1, cob_field *f2)
{
	int		sign, count, size;
	unsigned char	*p;
	unsigned char	*s1 = f1->data;
	unsigned char	*s2 = COB_FIELD_DATA (f2);
	unsigned char	*e1 = s1 + f1->size;
	unsigned char	*e2 = s2 + COB_FIELD_SIZE (f2);

	/* initialize */
	own_memset (f2->data, '0', f2->size);

	/* skip white spaces */
	for (; s1 < e1; s1++) {
		if (!isspace (*s1)) {
			break;
		}
	}

	/* check for sign */
	sign = 0;
	if (*s1 == '+' || *s1 == '-') {
		sign = (*s1++ == '+') ? 1 : -1;
	}

	/* count the number of digits before decimal point */
	count = 0;
	for (p = s1; p < e1 && *p != cob_current_module->decimal_point; p++) {
		if (isdigit (*p)) {
			count++;
		}
	}

	/* find the start position */
	size = (int) COB_FIELD_SIZE (f2) - f2->attr->scale;
	if (count < size) {
		s2 += size - count;
	} else {
		while (count-- > size) {
			while (!isdigit (*s1++)) { ; }
		}
	}

	/* move */
	count = 0;
	for (; s1 < e1 && s2 < e2; s1++) {
		unsigned char	c = *s1;

		if (isdigit (c)) {
			*s2++ = c;
		} else if (c == cob_current_module->decimal_point) {
			if (count++ > 0) {
				goto error;
			}
		} else if (!(isspace (c) || c == cob_current_module->numeric_separator)) {
			goto error;
		}
	}

	cob_put_sign (f2, sign);
	return;

error:
	own_memset (f2->data, '0', f2->size);
	cob_put_sign (f2, 0);
}

static void
cob_move_display_to_display (cob_field *f1, cob_field *f2)
{
	int	sign = cob_get_sign (f1);

	store_common_region (f2, COB_FIELD_DATA (f1), COB_FIELD_SIZE (f1),
			     COB_FIELD_SCALE (f1));

	cob_put_sign (f1, sign);
	cob_put_sign (f2, sign);
}

static void
cob_move_display_to_alphanum (cob_field *f1, cob_field *f2)
{
	int		diff;
	int		zero_size;
	int		sign = cob_get_sign (f1);
	size_t		size1 = COB_FIELD_SIZE (f1);
	size_t		size2 = f2->size;
	unsigned char	*data1 = COB_FIELD_DATA (f1);
	unsigned char	*data2 = f2->data;

	if (size1 >= size2) {
		own_memcpy (data2, data1, size2);
	} else {
		diff = (int)(size2 - size1);
		zero_size = 0;
		/* move */
		own_memcpy (data2, data1, size1);
		/* implied 0 ('P's) */
		if (f1->attr->scale < 0) {
			zero_size = cob_min_int ((int)-f1->attr->scale, diff);
			own_memset (data2 + size1, '0', zero_size);
		}
		/* padding */
		if (diff - zero_size > 0) {
			own_memset (data2 + size1 + zero_size, ' ', diff - zero_size);
		}
	}

	cob_put_sign (f1, sign);
}

static void
cob_move_alphanum_to_alphanum (cob_field *f1, cob_field *f2)
{
	size_t		size1 = f1->size;
	size_t		size2 = f2->size;
	unsigned char	*data1 = f1->data;
	unsigned char	*data2 = f2->data;

	if (size1 >= size2) {
		/* move string with truncation */
		if (COB_FIELD_JUSTIFIED (f2)) {
			own_memcpy (data2, data1 + size1 - size2, size2);
		} else {
			own_memcpy (data2, data1, size2);
		}
	} else {
		/* move string with padding */
		if (COB_FIELD_JUSTIFIED (f2)) {
			own_memset (data2, ' ', size2 - size1);
			own_memcpy (data2 + size2 - size1, data1, size1);
		} else {
			own_memcpy (data2, data1, size1);
			own_memset (data2 + size1, ' ', size2 - size1);
		}
	}
}

/*
 * Packed decimal
 */

static void
cob_move_display_to_packed (cob_field *f1, cob_field *f2)
{
	size_t		i;
	size_t		offset;
	int		sign = cob_get_sign (f1);
	size_t		digits1 = COB_FIELD_DIGITS (f1);
	size_t		digits2 = COB_FIELD_DIGITS (f2);
	int		scale1 = COB_FIELD_SCALE (f1);
	int		scale2 = COB_FIELD_SCALE (f2);
	unsigned char	*data1 = COB_FIELD_DATA (f1);
	unsigned char	*data2 = f2->data;
	unsigned char	*p = data1 + (digits1 - scale1) - (digits2 - scale2);

	/* pack string */
	own_memset (f2->data, 0, f2->size);
	offset = 1 - (digits2 % 2);
	for (i = offset; i < digits2 + offset; i++, p++) {
		unsigned char n = (data1 <= p && p < data1 + digits1) ? cob_d2i (*p) : 0;
		if (i % 2 == 0) {
			data2[i / 2] = n << 4;
		} else {
			data2[i / 2] |= n;
		}
	}

	cob_put_sign (f1, sign);
	if (!COB_FIELD_HAVE_SIGN (f2)) {
		data2[digits2 / 2] |= 0x0f;
	} else {
		cob_real_put_sign (f2, sign);
	}
}

static void
cob_move_packed_to_display (cob_field *f1, cob_field *f2)
{
	size_t		i;
	size_t		offset;
	int		sign = cob_get_sign (f1);
	unsigned char	*data = f1->data;
	unsigned char	buff[64];

	/* unpack string */
	offset = 1 - (f1->attr->digits % 2);
	for (i = offset; i < f1->attr->digits + offset; i++) {
		if (i % 2 == 0) {
			buff[i - offset] = cob_i2d (data[i / 2] >> 4);
		} else {
			buff[i - offset] = cob_i2d (data[i / 2] & 0x0f);
		}
	}

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

	own_memset ((ucharptr)buff2, 0, sizeof (buff2));
	size = size1 - f1->attr->scale;
	if (sign < 0) {
		buff2[0] = '-';
		data1 = &buff2[1];
	} else {
		data1 = buff2;
	}
	if (f1->attr->scale <= 0) {
		sprintf (data1, "%*.*s.0", (int)size, (int)size, f1->data);
	} else {
		sprintf (data1, "%*.*s.%*.*s", (int)size, (int)size, f1->data,
			 f1->attr->scale, f1->attr->scale, f1->data + size);
	}
	sscanf (buff2, "%lf", &val);
	if (COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_FLOAT) {
		float	flval = (float) val;

		memcpy (f2->data, (ucharptr)&flval, sizeof(float));
	} else {
		memcpy (f2->data, (ucharptr)&val, sizeof(double));
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

	own_memset ((ucharptr)buff, 0, sizeof (buff));
	own_memset ((ucharptr)buff2, 0, sizeof (buff2));
	if (COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_FLOAT) {
		float	flval;

		memcpy ((ucharptr)&flval, f1->data, sizeof (float));
		val = flval;
	} else {
		memcpy ((ucharptr)&val, f1->data, sizeof (double));
	}
	sign = 1;
	if (val < 0) {
		sign = -1;
		val = -val;
	}
	frac = modf (val, &intgr);
	res = (long long) intgr;
	decs = 0;
	for (; res; res /= 10) {
		decs++;
	}
	sprintf (buff2, "%-18.*lf", 18 - decs, val);
	y = buff;
	for (x = buff2; *x; x++) {
		if (*x == '.') {
			continue;
		}
		if (*x == ' ') {
			continue;
		}
		*y++ = *x;
	}

	store_common_region (f2, (ucharptr)buff, strlen (buff), 18 - decs);
	cob_put_sign (f2, sign);
}

/*
 * Binary integer
 */

static void
cob_move_display_to_binary (cob_field *f1, cob_field *f2)
{
	size_t		i, size;
	long long	val = 0;
	int		sign = cob_get_sign (f1);
	size_t		size1 = COB_FIELD_SIZE (f1);
	unsigned char	*data1 = COB_FIELD_DATA (f1);

	/* get value */
	size = size1 - f1->attr->scale + f2->attr->scale;
	for (i = 0; i < size; ++i) {
		if (i < size1) {
			val = val * 10 + cob_d2i (data1[i]);
		} else {
			val = val * 10;
		}
	}
	if (sign < 0 && COB_FIELD_HAVE_SIGN (f2)) {
		val = -val;
	}
	if (cob_current_module->flag_binary_truncate &&
	    !COB_FIELD_REAL_BINARY(f2)) {
		val %= cob_exp10LL[(int)f2->attr->digits];
	}

	/* store */
	cob_binary_set_int64 (f2, val);

	cob_put_sign (f1, sign);
}

static void
cob_move_binary_to_display (cob_field *f1, cob_field *f2)
{
	int			i, sign;
	unsigned long long	val;
	long long		val2;
	char			buff[64];	/* long long is at most 20 digits */

	sign = 1;
	/* get value */
	if (COB_FIELD_HAVE_SIGN (f1)) {
		val2 = cob_binary_get_int64 (f1);
		if (val2 < 0) {
			sign = -1;
			val = -val2;
		} else {
			val = val2;
		}
	} else {
		val = cob_binary_get_int64 (f1);
	}

	/* convert to string */
	i = 20;
	while (val > 0) {
		buff[--i] = (char) cob_i2d (val % 10);
		val /= 10;
	}

	/* store */
	store_common_region (f2, (ucharptr)buff + i, (size_t)(20 - i), f1->attr->scale);

	cob_put_sign (f2, sign);
}

/*
 * Edited
 */

static void
cob_move_display_to_edited (cob_field *f1, cob_field *f2)
{
	const char	*p;
	int		sign = cob_get_sign (f1);
	int		neg = (sign < 0) ? 1 : 0;
	unsigned char	*min, *max, *src, *dst, *end;
	unsigned char	pad = ' ';
	unsigned char	x;
	int		count = 0;
	int		count_sign = 1;
	int		count_curr = 1;
	int		trailing_sign = 0;
	int		trailing_curr = 0;
	int		is_zero = 1;
	int		suppress_zero = 1;
	int		sign_first = 0;
	int		p_is_left = 0;
	unsigned char	*decimal_point = NULL;
	unsigned char	sign_symbol = 0;
	unsigned char	curr_symbol = 0;

	/* count the number of digit places before decimal point */
	for (p = f2->attr->pic; *p; p += 2) {
		unsigned char	c = p[0];

		if (c == '9' || c == 'Z' || c == '*') {
			count += p[1];
			count_sign = 0;
			count_curr = 0;
		} else if (count_curr && c == cob_current_module->currency_symbol) {
			count += p[1];
		} else if (count_sign && (c == '+' || c == '-')) {
			count += p[1];
		} else if (c == 'P') {
			if (count == 0) {
				p_is_left = 1;
				break;
			} else {
				count += p[1];
				count_sign = 0;
				count_curr = 0;
			}
		} else if (c == 'V' || c == cob_current_module->decimal_point) {
			break;
		}
	}

	min = COB_FIELD_DATA (f1);
	max = min + COB_FIELD_SIZE (f1);
	src = max - f1->attr->scale - count;
	dst = f2->data;
	end = f2->data + f2->size;
	for (p = f2->attr->pic; *p;) {
		unsigned char	c = *p++;	/* PIC char */
		unsigned char	n = *p++;	/* PIC char count */

		for (; n > 0; n--, dst++) {
			switch (c) {
			case '0':
			case '/':
				*dst = c;
				break;

			case 'B':
				*dst = suppress_zero ? pad : 'B';
				break;

			case 'P':
				if (p_is_left) {
					src++;
					dst--;
				}
				break;

			case '9':
				*dst = (min <= src && src < max) ? *src++ : (src++, '0');
				if (*dst != '0') {
					is_zero = suppress_zero = 0;
				}
				suppress_zero = 0;
				trailing_sign = 1;
				trailing_curr = 1;
				break;

			case 'V':
				dst--;
				decimal_point = dst;
				break;

			case '.':
			case ',':
				if (c == cob_current_module->decimal_point) {
					*dst = cob_current_module->decimal_point;
					decimal_point = dst;
				} else {
					*dst = suppress_zero ? pad : c;
				}
				break;

			case 'C':
			case 'D':
				end = dst;
				memcpy (dst++, neg ? (c == 'C' ? "CR" : "DB") : "  ", 2);
				break;

			case 'Z':
			case '*':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				pad = (c == '*') ? '*' : ' ';
				*dst = suppress_zero ? pad : x;
				trailing_sign = 1;
				trailing_curr = 1;
				break;

			case '+':
			case '-':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				if (trailing_sign) {
					*dst = neg ? '-' : (c == '+') ? '+' : ' ';
					end--;
				} else if (dst == f2->data || suppress_zero) {
					*dst = pad;
					sign_symbol = neg ? '-' : (c == '+') ? '+' : ' ';
					if (!curr_symbol) {
						sign_first++;
					}
				} else {
					*dst = x;
				}
				break;

			default:
				if (c == cob_current_module->currency_symbol) {
					x = (min <= src && src < max) ? *src++ : (src++, '0');
					if (x != '0') {
						is_zero = suppress_zero = 0;
					}
					if (trailing_curr) {
						*dst = cob_current_module->currency_symbol;
						end--;
					} else if (dst == f2->data || suppress_zero) {
						*dst = pad;
						curr_symbol = cob_current_module->currency_symbol;
					} else {
						*dst = x;
					}
					break;
				}

				*dst = '?';	/* invalid PIC */
			}
		}
	}

	if (suppress_zero || (is_zero && COB_FIELD_BLANK_ZERO (f2))) {
		/* all digits are zeros */
		if (pad == ' ' || COB_FIELD_BLANK_ZERO (f2)) {
			own_memset (f2->data, ' ', f2->size);
		} else {
			for (dst = f2->data; dst < f2->data + f2->size; dst++) {
				if (*dst != cob_current_module->decimal_point) {
					*dst = pad;
				}
			}
		}
	} else {
		/* put zero after the decimal point if necessary */
		if (decimal_point) {
			for (dst = decimal_point + 1; dst < end; dst++) {
				if (!isdigit (*dst) && !strchr (",+-/B", *dst)) {
					*dst = '0';
				}
			}
		}

		/* put sign or currency symbol at the beginning */
		if (sign_symbol || curr_symbol) {
			for (dst = end - 1; dst > f2->data; dst--) {
				if (*dst == ' ') {
					break;
				}
			}
			if (sign_symbol && curr_symbol) {
				if (sign_first) {
					*dst = curr_symbol;
					dst--;
					if (dst >= f2->data) {
						*dst = sign_symbol;
					}
				} else {
					*dst = sign_symbol;
					dst--;
					if (dst >= f2->data) {
						*dst = curr_symbol;
					}
				}
			} else if (sign_symbol) {
				*dst = sign_symbol;
			} else {
				*dst = curr_symbol;
			}
		}

		/* replace all 'B's by pad */
		count = 0;
		for (dst = f2->data; dst < end; dst++) {
			if (*dst == 'B') {
				if (count == 0) {
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
	size_t		i;
	int		sign = 0;
	int		scale = 0;
	int		count = 0;
	int		have_point = 0;
	unsigned char	*p;
	const char	*p1;
	unsigned char	buff[64];

	p = buff;
	/* de-edit */
	for (i = 0; i < f1->size; i++) {
		int	c = f1->data[i];

		switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			*p++ = c;
			if (have_point) {
				scale++;
			}
			break;
		case '.':
		case ',':
			if (c == cob_current_module->decimal_point) {
				have_point = 1;
			}
			break;
		case '-':
		case 'C':
			sign = -1;
			break;
		}
	}
	/* count the number of digit places after decimal point in case of 'V', 'P' */
	if (scale == 0) {
		for (p1 = f1->attr->pic; *p1; p1 += 2) {
			unsigned char c = p1[0];

			if (c == '9'  || c == '0' || c == 'Z' || c == '*') {
				if (have_point) {
					scale += p1[1];
				} else {
					count += p1[1];
				}
			} else if (c == 'P') {
				if (count == 0) {
					have_point = 1;
					scale += p1[1];
				} else {
					scale -= p1[1];
				}
			} else if (c == 'V') {
				have_point = 1;
			}
		}
	}

	/* store */
	store_common_region (f2, buff, (size_t)(p - buff), scale);

	cob_put_sign (f2, sign);
}

static void
cob_move_alphanum_to_edited (cob_field *f1, cob_field *f2)
{
	const char	*p;
	unsigned char	*max, *src, *dst;
	int		sign = cob_get_sign (f1);

	src = COB_FIELD_DATA (f1);
	max = src + COB_FIELD_SIZE (f1);
	dst = f2->data;
	for (p = f2->attr->pic; *p;) {
		unsigned char	c = *p++;	/* PIC char */
		unsigned char	n = *p++;	/* PIC char count */

		for (; n > 0; n--) {
			switch (c) {
			case 'A':
			case 'X':
			case '9':
				*dst++ = (src < max) ? *src++ : ' ';
				break;
			case '0':
			case '/':
				*dst++ = c;
				break;
			case 'B':
				*dst++ = ' ';
				break;
			default:
				*dst++ = '?';	/* invalid PIC */
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
	       cob_field *src, cob_field *dst, size_t size, int scale)
{
	cob_field	temp;
	cob_field_attr	attr =
	    { COB_TYPE_NUMERIC_DISPLAY, size, scale, COB_FLAG_HAVE_SIGN, NULL };
	unsigned char	data[64];

	temp.size = size;
	temp.data = data;
	temp.attr = &attr;
	func (src, &temp);
	cob_move (&temp, dst);
}

static void
cob_move_all (cob_field *src, cob_field *dst)
{
	int			i;
	int			digcount;
	cob_field		temp;
	cob_field_attr		attr = { COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL };
	static int		lastsize = 0;
	static unsigned char	*data = NULL;

	if (COB_FIELD_IS_NUMERIC(dst)) {
		digcount = 18;
		attr.type = COB_TYPE_NUMERIC_DISPLAY;
		attr.digits = 18;
/*
		if (dst->attr->type & COB_TYPE_NUMERIC_EDITED) {
			digcount = dst->size;
		} else {
			digcount = dst->attr->digits;
		}
*/
	} else {
		digcount = (int) dst->size;
	}
	if (!data) {
		if (digcount <= COB_SMALL_BUFF) {
			data = cob_malloc (COB_SMALL_BUFF);
			lastsize = COB_SMALL_BUFF;
		} else {
			data = cob_malloc (digcount);
			lastsize = digcount;
		}
	} else {
		if (digcount > lastsize) {
			free (data);
			data = cob_malloc (digcount);
			lastsize = digcount;
		}
	}
	temp.size = digcount;
	temp.data = data;
	temp.attr = &attr;
	if (likely(src->size == 1)) {
		own_memset (data, src->data[0], (size_t)digcount);
	} else {
		for (i = 0; i < digcount; i++) {
			data[i] = src->data[i % src->size];
		}
	}

	cob_move (&temp, dst);
}

void
cob_move (cob_field *src, cob_field *dst)
{
	if (COB_FIELD_TYPE (src) == COB_TYPE_ALPHANUMERIC_ALL) {
		cob_move_all (src, dst);
		return;
	}

	/* non-elementary move */
	if (COB_FIELD_TYPE (src) == COB_TYPE_GROUP || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP) {
		cob_move_alphanum_to_alphanum (src, dst);
		return;
	}

	/* elementary move */
	switch (COB_FIELD_TYPE (src)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			cob_move_display_to_fp (src, dst);
			return;
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_display_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
			cob_move_display_to_packed (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
			cob_move_display_to_binary (src, dst);
			return;
		case COB_TYPE_NUMERIC_EDITED:
			cob_move_display_to_edited (src, dst);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			if (src->attr->scale < 0 || src->attr->scale > src->attr->digits) {
				/* expand P's */
				indirect_move (cob_move_display_to_display, src, dst,
					      (size_t)cob_max_int ((int)src->attr->digits, (int)src->attr->scale),
					      cob_max_int (0, (int)src->attr->scale));
				return;
			} else {
				cob_move_alphanum_to_edited (src, dst);
				return;
			}
		default:
			cob_move_display_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_PACKED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_packed_to_display (src, dst);
			return;
		default:
			indirect_move (cob_move_packed_to_display, src, dst,
				      src->attr->digits, src->attr->scale);
			return;
		}

	case COB_TYPE_NUMERIC_BINARY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_binary_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_binary_to_display, src, dst,
				      20, src->attr->scale);
			return;
		default:
			indirect_move (cob_move_binary_to_display, src, dst,
				      src->attr->digits, src->attr->scale);
			return;
		}

	case COB_TYPE_NUMERIC_EDITED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_edited_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_edited_to_display, src, dst, 36, 18);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
		indirect_move (cob_move_fp_to_display, src, dst, 40, 20);
		return;

	default:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_alphanum_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_EDITED:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
			indirect_move (cob_move_alphanum_to_display, src, dst, 36, 18);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}
	}
}

/*
 * Convenience functions
 */

static int
cob_packed_get_int (cob_field *f1)
{
	size_t		i;
	size_t		offset;
	int		val = 0;
	int		sign = cob_get_sign (f1);
	unsigned char	*data = f1->data;

	offset = 1 - (f1->attr->digits % 2);
	for (i = offset; i < f1->attr->digits + offset; i++) {
		val *= 10;
		if (i % 2 == 0) {
			val += data[i / 2] >> 4;
		} else {
			val += data[i / 2] & 0x0f;
		}
	}
	if (sign < 0) {
		val = -val;
	}
	return val;
}

static int
cob_display_get_int (cob_field *f)
{
	size_t		i;
	int		val = 0;
	int		sign = cob_get_sign (f);
	size_t		size = COB_FIELD_SIZE (f);
	unsigned char	*data = COB_FIELD_DATA (f);

	/* skip preceding zeros */
	for (i = 0; i < size; i++) {
		if (cob_d2i (data[i]) != 0) {
			break;
		}
	}

	/* get value */
	if (f->attr->scale < 0) {
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
		val *= cob_exp10[(int)-f->attr->scale];
	} else {
		size -= f->attr->scale;
		for (; i < size; ++i) {
			val = val * 10 + cob_d2i (data[i]);
		}
	}
	if (sign < 0) {
		val = -val;
	}

	cob_put_sign (f, sign);
	return val;
}

int
cob_binary_get_int (const cob_field * const f)
{
	return (int) cob_binary_get_int64 (f);
}

long long
cob_binary_get_int64 (const cob_field * const f)
{
	long long	n = 0;
	size_t		fsiz = 8 - f->size;

/* Experimental code - not activated */
#if 0
	unsigned char	*s;

	if ((COB_FIELD_BINARY_SWAP (f) && !COB_FIELD_HAVE_SIGN (f)) ||
	    (!COB_FIELD_BINARY_SWAP (f) && COB_FIELD_HAVE_SIGN (f))) {
		s = (unsigned char *)&n + fsiz;
	} else {
		s = (unsigned char *)&n;
	}
	own_byte_memcpy (s, f->data, f->size);
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
	}
	if (COB_FIELD_HAVE_SIGN (f)) {
		n >>= 8 * fsiz;	/* shift with sign */
	}
#endif
#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
			n = COB_BSWAP_64 (n);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n = COB_BSWAP_64 (n);
		}
	} else {
		if (COB_FIELD_HAVE_SIGN (f)) {
			own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
			n >>= 8 * fsiz;	/* shift with sign */
		} else {
			own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		}
	}
#else	/* WORDS_BIGENDIAN */
	if (COB_FIELD_HAVE_SIGN (f)) {
		own_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		n >>= 8 * fsiz;	/* shift with sign */
	} else {
		own_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
	}
#endif	/* WORDS_BIGENDIAN */
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
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	own_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	own_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

void
cob_set_int (cob_field *f, int n)
{
	cob_field	temp;
	cob_field_attr	attr = { COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, 0 };

	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	cob_move (&temp, f);
}

int
cob_get_int (cob_field *f)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_get_int (f);
	case COB_TYPE_NUMERIC_BINARY:
		return cob_binary_get_int (f);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_packed_get_int (f);
	default:
	{
		int		n;
		cob_field	temp;
		cob_field_attr	attr =
		    { COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL };

		temp.size = 4;
		temp.data = (unsigned char *)&n;
		temp.attr = &attr;
		cob_move (f, &temp);
		return n;
	}
	}
}

void
cob_memcpy (cob_field *dst, unsigned char *src, int size)
{
	cob_field	temp;
	cob_field_attr	attr = { COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL };

	temp.size = size;
	temp.data = src;
	temp.attr = &attr;
	cob_move (&temp, dst);
}
