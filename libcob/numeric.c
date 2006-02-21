/*
 * Copyright (C) 2001-2006 Keisuke Nishida
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

#include "common.h"
#include "move.h"
#include "numeric.h"
#include "byteswap.h"

#define DECIMAL_NAN	-128
#define DECIMAL_CHECK(d1,d2) \
  if (d1->scale == DECIMAL_NAN || d2->scale == DECIMAL_NAN) { \
      d1->scale = DECIMAL_NAN; \
      return; \
    }

static cob_decimal	cob_d1;
static cob_decimal	cob_d2;
static cob_decimal	cob_d3;
static cob_decimal	cob_d4;
static mpz_t		cob_mexp;
static unsigned char	packed_value[20];

/*
 * Decimal number
 */

void
cob_decimal_init (cob_decimal *d)
{
	mpz_init2 (d->value, 256);
	d->scale = 0;
}

/* Not used - comment out
void
cob_decimal_print (cob_decimal *d)
{
	mpz_out_str (stdout, 10, d->value);
	if (d->scale) {
		fprintf (stdout, " * 10^%d", -d->scale);
	}
	fputs ("\n", stdout);
}
end comment out */

/* d->value *= 10^n, d->scale += n */
static void
shift_decimal (cob_decimal *d, int n)
{
	if (n == 0) {
		return;
	}
	if (n > 0) {
		mpz_ui_pow_ui (cob_mexp, 10, n);
		mpz_mul (d->value, d->value, cob_mexp);
	} else if (n < 0) {
		mpz_ui_pow_ui (cob_mexp, 10, -n);
		mpz_tdiv_q (d->value, d->value, cob_mexp);
	}
	d->scale += n;
}

static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
	if (d1->scale < d2->scale) {
		shift_decimal (d1, d2->scale - d1->scale);
	} else if (d1->scale > d2->scale) {
		shift_decimal (d2, d1->scale - d2->scale);
	}
}

/*
 * Decimal set/get
 */

static void
cob_decimal_set (cob_decimal *dst, cob_decimal *src)
{
	mpz_set (dst->value, src->value);
	dst->scale = src->scale;
}

/* int */

/*
void
cob_decimal_set_int (cob_decimal *d, int n)
{
	mpz_set_si (d->value, n);
	d->scale = 0;
}
*/

int
cob_decimal_get_int (cob_decimal *d)
{
	cob_decimal_set (&cob_d1, d);
	shift_decimal (&cob_d1, -cob_d1.scale);
	return mpz_get_si (cob_d1.value);
}

/* double */

void
cob_decimal_set_double (cob_decimal *d, double v)
{
	mpz_set_d (d->value, v * 1.0e9);
	d->scale = 9;
}

double
cob_decimal_get_double (cob_decimal *d)
{
	int n = d->scale;
	double v = mpz_get_d (d->value);

	for (; n > 0; n--) {
		v /= 10;
	}
	for (; n < 0; n++) {
		v *= 10;
	}
	return v;
}

/* DISPLAY */

static void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
	int		sign = cob_get_sign (f);
	size_t		size = COB_FIELD_SIZE (f);
	unsigned char	*data = COB_FIELD_DATA (f);

	/* skip leading zeros */
	while (size > 1 && cob_d2i (data[0]) == 0) {
		size--;
		data++;
	}

	/* set value */
	if (size < 10) {
		unsigned int	n = 0;

		while ( size-- ) {
			n = n * 10 + cob_d2i (*data++);
		}
/*
		unsigned char	*endp = data + size;
		unsigned int	n = cob_d2i (*data++);

		while (data < endp) {
			n = n * 10 + cob_d2i (*data++);
		}
*/
		mpz_set_ui (d->value, n);
	} else {
		unsigned char buff[64];

		own_memcpy (buff, data, size);
		buff[size] = 0;
		mpz_set_str (d->value, (char *)buff, 10);
	}

	/* set sign and scale */
	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = f->attr->scale;
	cob_put_sign (f, sign);
}

static int
cob_decimal_get_display (cob_decimal *d, cob_field *f, int opt)
{
	int		diff;
	int		sign = mpz_sgn (d->value);
	size_t		size;
	unsigned char	*data = COB_FIELD_DATA (f);
	unsigned char	buff[256];

	/* build string */
	mpz_abs (d->value, d->value);
	mpz_get_str ((char *)buff, 10, d->value);
	size = strlen ((char *)buff);

	/* store number */
	diff = COB_FIELD_SIZE (f) - size;
	if (diff < 0) {
		/* overflow */
		COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);

		/* if the statement has ON SIZE ERROR or NOT ON SIZE ERROR,
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			return cob_exception_code;
		}

		/* othersize, truncate digits */
		own_memcpy (data, buff - diff, COB_FIELD_SIZE (f));
	} else {
		/* no overflow */
		own_memset (data, '0', diff);
		own_memcpy (data + diff, buff, size);
	}

	cob_put_sign (f, sign);
	return 0;
}

/* BINARY */

static void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
	if (f->size <= 4) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			mpz_set_si (d->value, cob_binary_get_int (f));
		} else {
			mpz_set_ui (d->value, cob_binary_get_int (f));
		}
	} else {
		long long	val = cob_binary_get_int64 (f);

		mpz_set_si (d->value, val >> 32);
		mpz_mul_2exp (d->value, d->value, 32);
		mpz_add_ui (d->value, d->value, val & 0xffffffff);
	}
	d->scale = f->attr->scale;
}

static int
cob_decimal_get_binary (cob_decimal *d, cob_field *f, int opt)
{
	int	overflow = 0;
	int	digits = f->attr->digits;

	if (f->size <= 4) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			int	val;

			if (!mpz_fits_sint_p (d->value)) {
				if (cob_current_module->flag_binary_truncate) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						mpz_tdiv_r_ui (d->value, d->value,
							       cob_exp10[digits]);
					}
					val = mpz_get_si (d->value);
				} else {
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					val = mpz_get_ui (d->value);
				}
			} else {
				val = mpz_get_si (d->value);
			}

			if (cob_current_module->flag_binary_truncate) {
				if (val <= -cob_exp10[digits] || val >= cob_exp10[digits]) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						val %= cob_exp10[digits];
					}
				}
			}
			cob_binary_set_int (f, val);
		} else {
			unsigned int	val;

			if (mpz_sgn (d->value) < 0) {
				mpz_abs (d->value, d->value);
			}
			if (!mpz_fits_uint_p (d->value)) {
				if (cob_current_module->flag_binary_truncate) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						mpz_tdiv_r_ui (d->value, d->value,
							       cob_exp10[digits]);
					}
				} else {
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
				}
			}
			val = mpz_get_ui (d->value);
			if (cob_current_module->flag_binary_truncate) {
				if (val >= cob_exp10[digits]) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						val %= cob_exp10[digits];
					}
				}
			}
			cob_binary_set_int (f, val);
		}
	} else {
		unsigned int	lo;

		mpz_fdiv_r_2exp (cob_d2.value, d->value, 32);
		mpz_fdiv_q_2exp (d->value, d->value, 32);
		lo = mpz_get_ui (cob_d2.value);

		if (COB_FIELD_HAVE_SIGN (f)) {
			long long	val;

			if (!mpz_fits_sint_p (d->value)) {
				/* overflow */
				overflow = 1;
				if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
					goto overflow;
				}
				if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
					/* FIXME: need truncation */ ;
				}
			}
			val = mpz_get_si (d->value);
			val = (val << 32) | lo;
			if (cob_current_module->flag_binary_truncate) {
				if (val <= -cob_exp10LL[digits] || val >= cob_exp10LL[digits]) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						val %= cob_exp10LL[digits];
					}
				}
			}
			cob_binary_set_int64 (f, val);
		} else {
			unsigned long long	val;

			if (mpz_sgn (d->value) < 0) {
				mpz_abs (d->value, d->value);
			}
			if (!mpz_fits_uint_p (d->value)) {
				/* overflow */
				overflow = 1;
				if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
					goto overflow;
				}
				if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
					/* FIXME: need truncation */ ;
				}
			}
			val = mpz_get_ui (d->value);
			val = (val << 32) | lo;
			if (cob_current_module->flag_binary_truncate) {
				if (val >= cob_exp10LL[digits]) {
					/* overflow */
					overflow = 1;
					if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
						goto overflow;
					}
					if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
						val %= cob_exp10LL[digits];
					}
				}
			}
			cob_binary_set_int64 (f, val);
		}
	}
	if (!overflow) {
		return 0;
	}

      overflow:
	COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
	return cob_exception_code;
}

static void
cob_add_binary (cob_field *f, int val)
{
	unsigned char	*p = f->data;

	switch (f->size) {
	case 1:
		if (COB_FIELD_HAVE_SIGN (f)) {
			*(char *)p += val;
		} else {
			*(unsigned char *)p += val;
		}
		return;
	case 2:
		if (COB_FIELD_HAVE_SIGN (f)) {
			short	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_16 (*(short *)p);
				n += val;
				*(short *)p = COB_BSWAP_16(n);
			} else {
				*(short *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(short));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
		} else {
			unsigned short	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_16 (*(unsigned short *)p);
				n += val;
				*(unsigned short *)p = COB_BSWAP_16(n);
			} else {
				*(unsigned short *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(short));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
		}
		return;
	case 4:
		if (COB_FIELD_HAVE_SIGN (f)) {
			int	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_32 (*(int *)p);
				n += val;
				*(int *)p = COB_BSWAP_32(n);
			} else {
				*(int *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(int));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
		} else {
			unsigned int	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_32 (*(unsigned int *)p);
				n += val;
				*(unsigned int *)p = COB_BSWAP_32(n);
			} else {
				*(unsigned int *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(int));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
		}
		return;
	case 8:
		if (COB_FIELD_HAVE_SIGN (f)) {
			long long	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_64 (*(long long *)p);
				n += val;
				*(long long *)p = COB_BSWAP_64(n);
			} else {
				*(long long *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(long long));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
		} else {
			unsigned long long	n;

#ifndef WORDS_BIGENDIAN
			if (COB_FIELD_BINARY_SWAP (f)) {
				n = COB_BSWAP_64 (*(unsigned long long *)p);
				n += val;
				*(unsigned long long *)p = COB_BSWAP_64(n);
			} else {
				*(unsigned long long *)p += val;
			}
#else
			memcpy ((unsigned char *)&n, p, sizeof(long long));
			n += val;
			memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
		}
		return;
	}
	return;
}


/* PACKED-DECIMAL */

static int
cob_packed_get_sign (cob_field *f)
{
	unsigned char *p;

	p = f->data + (f->attr->digits / 2);
	return ((*p & 0x0f) == 0x0d) ? -1 : 1;
}

static void
cob_add_packed (cob_field *f, int val)
{
	int		sign;
	unsigned char	*p;
	int		ndigs;
	unsigned int	msn;
	int		tval;
	int		carry = 0;
	unsigned int	subtr = 0;
	unsigned int	zeroes = 0;
	unsigned int	origdigs;

	ndigs = f->attr->digits - f->attr->scale;
	if (ndigs <= 0) {
		return;
	}
	sign = COB_FIELD_HAVE_SIGN (f) ? cob_packed_get_sign (f) : 0;
	msn = 1 - (f->attr->scale % 2);

	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign < 0) {
		val = -val;
	}
	if (val < 0) {
		val = -val;
		subtr = 1;
	}
	p = f->data + (ndigs / 2) - (1 - msn);
	origdigs = ndigs;
	while (ndigs--) {
		if (!msn) {
			tval = *p & 0x0f;
		} else {
			tval = (*p & 0xf0) >> 4;
		}
		if (val) {
			carry += (val % 10);
			val /= 10;
		}
		if (subtr) {
			tval -= carry;
			if (tval < 0) {
				tval += 10;
				carry = 1;
			} else {
				carry = 0;
			}
		} else {
			tval += carry;
			if (tval > 9) {
				tval %= 10;
				carry = 1;
			} else {
				carry = 0;
			}
		}
		if (tval == 0) {
			zeroes++;
		}
		if (!msn) {
			*p = (*p & 0xf0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0f) | (tval << 4);
			msn = 0;
			p--;
		}
	}
	if (sign) {
		p = f->data + f->size - 1;
		if (origdigs == zeroes) {
			*p = (*p & 0xf0) | 0x0c;
		} else if (subtr && carry) {
			sign = -sign;
			if (sign < 0) {
				*p = (*p & 0xf0) | 0x0d;
			} else {
				*p = (*p & 0xf0) | 0x0c;
			}
		}
	}
}

static void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
	int		sign;
	unsigned int	val;
	unsigned char	*p = f->data;
	int		digits = f->attr->digits;

	sign = COB_FIELD_HAVE_SIGN (f) ? cob_packed_get_sign (f) : 0;

	if (digits % 2 == 0) {
		val = *p & 0x0f;
		digits--;
		p++;
	} else {
		val = 0;
	}

	if (f->attr->digits < 10) {
		while (digits > 1) {
			if (val) {
				val *= 100;
			}
			if (*p) {
				val += ((*p >> 4) * 10) + (*p & 0x0f);
			}
			digits -= 2;
			p++;
		}
		if (val) {
			val *= 10;
		}
		val += *p >> 4;
		mpz_set_ui (d->value, val);
	} else {
		unsigned int	valseen = 0;

		mpz_set_ui (d->value, val);
		if (val) {
			valseen = 1;
		}
		while (digits > 1) {
			if (valseen) {
				mpz_mul_ui (d->value, d->value, 100);
			}
			if (*p) {
				mpz_add_ui (d->value, d->value,
					(*p >> 4) * 10 + (*p & 0x0f));
				valseen = 1;
			}
			digits -= 2;
			p++;
		}
		if (valseen) {
			mpz_mul_ui (d->value, d->value, 10);
		}
		mpz_add_ui (d->value, d->value, (*p >> 4));
	}

	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = f->attr->scale;
}

static int
cob_decimal_get_packed (cob_decimal *d, cob_field *f, int opt)
{
	int		diff;
	int		sign = mpz_sgn (d->value);
	size_t		size;
	size_t		n;
	size_t		i;
	int		digits = f->attr->digits;
	unsigned char	*data = f->data;
	unsigned char	*p;
	unsigned char	*q;
	unsigned char	buff[COB_MEDIUM_BUFF];

	/* build string */
	mpz_abs (d->value, d->value);
	mpz_get_str ((char *)buff, 10, d->value);
	size = strlen ((char *)buff);

	/* store number */
	q = buff;
	diff = digits - size;
	if (diff < 0) {
		/* overflow */
		COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);

		/* if the statement has ON SIZE ERROR or NOT ON SIZE ERROR,
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			return cob_exception_code;
		}
		q += size - digits;
		size = digits;
	}
	own_memset (data, 0, f->size);
	p = data + (digits / 2) - (size / 2);
	diff = 1 - (size % 2);
	for (i = diff, n = 0; i < size + diff; i++, n++) {
		unsigned char	x = cob_d2i (q[n]);

		if (i % 2 == 0) {
			*p = x << 4;
		} else {
			*p++ |= x;
		}
	}

	p = data + (digits / 2);
	if (!COB_FIELD_HAVE_SIGN (f)) {
                *p |= 0x0f;
        } else {
		if (sign < 0) {
			*p |= 0x0d;
		} else {
			*p |= 0x0c;
		}
	}

	return 0;
}

/* General field */

void
cob_decimal_set_field (cob_decimal *d, cob_field *f)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		cob_decimal_set_binary (d, f);
		break;
	case COB_TYPE_NUMERIC_PACKED:
		cob_decimal_set_packed (d, f);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		cob_decimal_set_double (d, (double)*(float *)f->data);
		break;
	case COB_TYPE_NUMERIC_DOUBLE:
		cob_decimal_set_double (d, (double)*(double *)f->data);
		break;
	default:
		cob_decimal_set_display (d, f);
		break;
	}
}

int
cob_decimal_get_field (cob_decimal *d, cob_field *f, int opt)
{
	if (d->scale == DECIMAL_NAN) {
		COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
		return cob_exception_code;
	}

	/* work copy */
	if (d != &cob_d1) {
		cob_decimal_set (&cob_d1, d);
		d = &cob_d1;
	}

	/* rounding */
	if (opt & COB_STORE_ROUND) {
		if (f->attr->scale < d->scale) {
			int	sign = mpz_sgn (d->value);

			if (sign != 0) {
				shift_decimal (d, f->attr->scale - d->scale + 1);
				if (sign > 0) {
					mpz_add_ui (d->value, d->value, 5);
				} else {
					mpz_sub_ui (d->value, d->value, 5);
				}
			}
		}
	}

	/* append or truncate decimal digits */
	shift_decimal (d, f->attr->scale - d->scale);

	/* store number */
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		return cob_decimal_get_binary (d, f, opt);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_decimal_get_packed (d, f, opt);
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_decimal_get_display (d, f, opt);
	case COB_TYPE_NUMERIC_FLOAT:
	{
		float	val;

		val = cob_decimal_get_double (d);
		memcpy (f->data, (ucharptr)&val, sizeof (float));
/*
		*(float *)f->data = val;
*/
		return 0;
	}
	case COB_TYPE_NUMERIC_DOUBLE:
	{
		double	val;

		val = cob_decimal_get_double (d);
		memcpy (f->data, (ucharptr)&val, sizeof (double));
/*
		*(double *)f->data = val;
*/
		return 0;
	}
	default:
	{
		cob_field	temp;
		cob_field_attr	attr = {
			COB_TYPE_NUMERIC_DISPLAY,
			f->attr->digits,
			f->attr->scale,
			COB_FLAG_HAVE_SIGN,
			NULL
		};
		unsigned char	data[64];

		temp.size = f->attr->digits;
		temp.data = data;
		temp.attr = &attr;
		if (cob_decimal_get_display (d, &temp, opt) == 0) {
			cob_move (&temp, f);
		}
		return cob_exception_code;
	}
	}
}

/*
 * Decimal arithmetic
 */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_add (d1->value, d1->value, d2->value);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_sub (d1->value, d1->value, d2->value);
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	d1->scale += d2->scale;
	mpz_mul (d1->value, d1->value, d2->value);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);

	/* check for division by zero */
	if (mpz_sgn (d2->value) == 0) {
		d1->scale = DECIMAL_NAN;
		COB_SET_EXCEPTION (COB_EC_SIZE_ZERO_DIVIDE);
		return;
	}

	d1->scale -= d2->scale;
	shift_decimal (d1, 19 + ((d1->scale < 0) ? -d1->scale : 0));
	mpz_tdiv_q (d1->value, d1->value, d2->value);
}

void
cob_decimal_pow (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);

	if (d2->scale == 0 && mpz_fits_ulong_p (d2->value)) {
		unsigned int n = mpz_get_ui (d2->value);
		mpz_pow_ui (d1->value, d1->value, n);
		d1->scale *= n;
	} else {
		cob_decimal_set_double (d1, pow (cob_decimal_get_double (d1),
						 cob_decimal_get_double (d2)));
	}
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
	align_decimal (d1, d2);
	return mpz_cmp (d1->value, d2->value);
}

/*
 * Optimized arithmetic for DISPLAY
 */

static int
display_add_int (unsigned char *data, size_t size, unsigned int n)
{
	int		carry = 0;
	unsigned char	*sp = data + size;

	while (n > 0) {
		int	i = n % 10;

		n = n / 10;

		/* check for overflow */
		if (--sp < data) {
			if (!cob_current_module->flag_binary_truncate) {
				return 0;
			}
			return 1;
		}

		/* perform addition */
		if ((*sp += i + carry) > '9') {
			carry = 1, *sp -= 10;
		} else {
			carry = 0;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* carry up */
	while (--sp >= data) {
		if ((*sp += 1) <= '9') {
			return 0;
		}
		*sp = '0';
	}
	if (!cob_current_module->flag_binary_truncate) {
		return 0;
	}
	return 1;
}

static int
display_sub_int (unsigned char *data, size_t size, unsigned int n)
{
	int		carry = 0;
	unsigned char	*sp = data + size;

	while (n > 0) {
		int	i = n % 10;

		n = n / 10;

		/* check for overflow */
		if (--sp < data) {
			return 1;
		}

		/* perform subtraction */
		if ((*sp -= i + carry) < '0') {
			carry = 1, *sp += 10;
		} else {
			carry = 0;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* carry up */
	while (--sp >= data) {
		if ((*sp -= 1) >= '0') {
			return 0;
		}
		*sp = '9';
	}
	return 1;
}

static int
cob_display_add_int (cob_field *f, int n)
{
	int		sign;
	size_t		osize;
	unsigned char	*data = COB_FIELD_DATA (f);
	size_t		size = COB_FIELD_SIZE (f);
	int		scale = COB_FIELD_SCALE (f);
	unsigned char	tfield[64];

	osize = size;
	own_memcpy (tfield, data, osize);
	sign = cob_get_sign (f);
	/* -x + n = -(x - n) */
	if (sign < 0) {
		n = -n;
	}

	if (scale < 0) {
		/* PIC 9(n)P(m) */
		if (-scale < 10) {
			n /= cob_exp10[-scale];
		} else {
			n = 0;
		}
	} else {
		/* PIC 9(n)V9(m) */
		size -= scale;
		/* Following can never be true as size is unsigned ?? */
		/* Comment out
		if (size < 0) {
			cob_put_sign (f, sign);
			goto overflow;
		}
		*/
	}

	if (n > 0) {
		/* add n to the field */
		if (display_add_int (data, size, n) != 0) {
			/* if there was an overflow, recover the last value */
			own_memcpy (data, tfield, osize);
			goto overflow;
		}
	} else if (n < 0) {
		/* subtract n from the field */
		if (display_sub_int (data, size, -n) != 0) {
			int	i;

			for (i = 0; i < size; i++) {
				data[i] = cob_i2d (9 - cob_d2i (data[i]));
			}
			display_add_int (data, size, 1);
			sign = -sign;
		}
	}

	cob_put_sign (f, sign);
	return 0;

      overflow:
	cob_put_sign (f, sign);
	COB_SET_EXCEPTION (COB_EC_SIZE_OVERFLOW);
	return cob_exception_code;
}

/*
 * Convenience functions
 */

int
cob_add (cob_field *f1, cob_field *f2, int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_add (&cob_d1, &cob_d2);
	return cob_decimal_get_field (&cob_d1, f1, opt);
}

int
cob_sub (cob_field *f1, cob_field *f2, int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_sub (&cob_d1, &cob_d2);
	return cob_decimal_get_field (&cob_d1, f1, opt);
}

int
cob_add_int (cob_field *f, int n)
{
	if (n == 0) {
		return 0;
	}
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_add_int (f, n);
	case COB_TYPE_NUMERIC_PACKED:
		cob_add_packed (f, n);
		return 0;
	default:
		/* not optimized */
		cob_decimal_set_field (&cob_d1, f);
		cob_decimal_set_int (&cob_d2, n);
		if (cob_d1.scale) {
			mpz_ui_pow_ui (cob_mexp, 10, cob_d1.scale);
			mpz_mul (cob_d2.value, cob_d2.value, cob_mexp);
			cob_d2.scale = cob_d1.scale;
		}
		mpz_add (cob_d1.value, cob_d1.value, cob_d2.value);
/*
		cob_decimal_set_int (&cob_d2, n);
		cob_decimal_add (&cob_d1, &cob_d2);
*/
		return cob_decimal_get_field (&cob_d1, f, 0);
	}
}

int
cob_sub_int (cob_field *f, int n)
{
	if (n == 0) {
		return 0;
	}
	return cob_add_int (f, -n);
}

int
cob_div_quotient (cob_field *dividend, cob_field *divisor, cob_field *quotient, int opt)
{
	int ret;

	cob_decimal_set_field (&cob_d1, dividend);
	cob_decimal_set_field (&cob_d2, divisor);
	cob_decimal_set (&cob_d3, &cob_d1);

	/* compute quotient */
	cob_decimal_div (&cob_d1, &cob_d2);
	if (cob_d1.scale == DECIMAL_NAN) {
		cob_d3.scale = DECIMAL_NAN;
		return cob_exception_code;
	}

	/* set quotient */
	cob_decimal_set (&cob_d4, &cob_d1);
	ret = cob_decimal_get_field (&cob_d1, quotient, opt);

	/* truncate digits from the quotient */
	shift_decimal (&cob_d4, quotient->attr->scale - cob_d4.scale);

	/* compute remainder */
	cob_decimal_mul (&cob_d4, &cob_d2);
	cob_decimal_sub (&cob_d3, &cob_d4);

	return ret;
}

int
cob_div_remainder (cob_field *fld_remainder, int opt)
{
	return cob_decimal_get_field (&cob_d3, fld_remainder, opt);
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_packed (cob_field *f, int n)
{
	int			sign;
	size_t			size;
	size_t			inc = 0;
	static int		lastval = 0;
	unsigned char		*p;
	unsigned char		val1[20];

	sign = COB_FIELD_HAVE_SIGN (f) ? cob_packed_get_sign (f) : 0;
	/* Field positive, value negative */
	if (sign >= 0 && n < 0) {
		return 1;
	}
	/* Field negative, value positive */
	if (sign < 0 && n >= 0) {
		return -1;
	}
	/* Both positive or both negative */
	p = f->data;
	for (size = 0; size < 20; size++) {
		if (size < 20 - f->size) {
			val1[size] = 0;
		} else {
			val1[size] = p[inc++];
		}
	}
	val1[19] &= 0xf0;
	if ((f->attr->digits % 2) == 0) {
		val1[size] &= 0x0f;
	}
	if (n != lastval) {
		lastval = n;
		if (n < 0) {
			n = -n;
		}
		own_memset (&packed_value[14], 0, 6);
		if (n) {
			p = &packed_value[19];
			*p =  (n % 10) << 4;
			p--;
			n /= 10;
			for ( ; n; ) {
				size = n % 100;
				*p = (size % 10) | ((size / 10) << 4);
				n /= 100;
/*
				*p = (n % 10);
				n /= 10;
				*p |= ((n % 10) << 4);
				n /= 10;
*/
				p--;
			}
		}
	}
	for (size = 0; size < 20; size++) {
		if (val1[size] != packed_value[size]) {
			if (sign < 0) {
				return packed_value[size] - val1[size];
			} else {
				return val1[size] - packed_value[size];
			}
		}
	}
	return 0;
}

void
cob_init_numeric (void)
{
	cob_decimal_init (&cob_d1);
	cob_decimal_init (&cob_d2);
	cob_decimal_init (&cob_d3);
	cob_decimal_init (&cob_d4);
	mpz_init2 (cob_mexp, 512);
	own_memset (packed_value, 0, sizeof(packed_value));
}
