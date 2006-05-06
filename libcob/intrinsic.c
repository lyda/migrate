/*
 * Copyright (C) 2005-2006 Roger While
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
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <math.h>

#include "byteswap.h"
#include "common.h"
#include "move.h"
#include "numeric.h"
#include "intrinsic.h"
#include "lib/gettext.h"

/* Stacked field level */
#define DEPTH_LEVEL	8

/* Working fields */
static cob_decimal	d1, d2, d3, d4, d5;

/* Stack definitions for created fields */
static int		curr_entry = 0;
static cob_field	*curr_field = NULL;
static cob_field_attr	*curr_attr = NULL;
static cob_field	calc_field[DEPTH_LEVEL];
static cob_field_attr	calc_attr[DEPTH_LEVEL];
	

/* Constants for date/day calculations */
static const int normal_days[] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
static const int leap_days[] =	 {0,31,60,91,121,152,182,213,244,274,305,335,366};
static const int normal_month_days[] = {0,31,28,31,30,31,30,31,31,30,31,30,31};
static const int leap_month_days[] =   {0,31,29,31,30,31,30,31,31,30,31,30,31};

/* Static function prototypes */
static void	make_double_entry (void);
static void	make_field_entry (cob_field *f);
static int	leap_year ( int year );
static double	intr_get_double (cob_decimal *d);
static int	comp_field (const void *m1, const void *m2);

/* Initialization routine */

void
cob_init_intrinsic ()
{
	int		i;

	cob_decimal_init (&d1);
	cob_decimal_init (&d2);
	cob_decimal_init (&d3);
	cob_decimal_init (&d4);
	cob_decimal_init (&d5);
	/* mpz_init2 (mp, 256); */
	memset ((char *)&calc_field[0], 0, sizeof (calc_field));
	memset ((char *)&calc_attr[0], 0, sizeof (calc_attr));
	for ( i = 0; i < DEPTH_LEVEL; i++ ) {
		calc_field[i].data = cob_malloc (1024);
		calc_field[i].size = 1024;
	}
	return;
}


/* Low level routines */

static void
make_double_entry ()
{
	unsigned char		*s;

	curr_field = &calc_field[curr_entry];
	curr_attr = &calc_attr[curr_entry];
	if ( curr_field->size < sizeof (double) ) {
		if ( curr_field->size == 0 ) {
			s = cob_malloc (sizeof (double) + 3);
		} else {
			s = realloc (curr_field->data, sizeof (double) + 3);
		}
		memset (s, 0, sizeof (double) + 3);
	} else {
		s = curr_field->data;
		memset (s, 0, curr_field->size);
	}

	curr_attr->type = COB_TYPE_NUMERIC_DOUBLE;
	curr_attr->digits = 18;
	curr_attr->scale = 9;
	curr_attr->flags = COB_FLAG_HAVE_SIGN;
	curr_attr->pic = NULL;

	curr_field->size = sizeof (double);
	curr_field->data = s;
	curr_field->attr = curr_attr;
		
	if ( ++curr_entry >= DEPTH_LEVEL ) {
		curr_entry = 0;
	}
	return;
}

static void
make_field_entry (cob_field *f)
{
	unsigned char		*s;

	curr_field = &calc_field[curr_entry];
	curr_attr = &calc_attr[curr_entry];
	if ( f->size > curr_field->size ) {
		if ( curr_field->size == 0 ) {
			s = cob_malloc (f->size + 3);
		} else {
			s = realloc (curr_field->data, f->size + 3);
			if ( !s ) {
				cob_runtime_error (_("Cannot acquire %d bytes of memory - Aborting"), f->size + 3);
				cob_stop_run (1);
			}
		}
		memset (s, 0, f->size + 3);
	} else {
		s = curr_field->data;
		memset (s, 0, curr_field->size);
	}
	*curr_field = *f;
	*curr_attr = *(f->attr);
	curr_field->data = s;
	curr_field->attr = curr_attr;
		
	if ( ++curr_entry >= DEPTH_LEVEL ) {
		curr_entry = 0;
	}
	return;
}

static int
leap_year ( int year )
{
	return ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)) ? 1 : 0;
}

/* Leave in
static void
intr_set_double (cob_decimal *d, double v)
{
	mpz_set_d (d->value, v * 1.0e9);
	d->scale = 9;
}
*/

static double
intr_get_double (cob_decimal *d)
{
	int n = d->scale;
	double v = mpz_get_d (d->value);

	for (; n > 0; n--) v /= 10;
	for (; n < 0; n++) v *= 10;
	return v;
}

static int
comp_field (const void *m1, const void *m2)
{
	cob_field	*f1 = *(cob_field **) m1;
	cob_field	*f2 = *(cob_field **) m2;
	return cob_cmp (f1, f2);
}

/* Numeric expressions */

cob_field *
cob_intr_binop (cob_field *f1, int op, cob_field *f2)
{
	make_double_entry ();

	cob_decimal_set_field (&d1, f1);
	cob_decimal_set_field (&d2, f2);
	switch (op) {
	case '+':
		cob_decimal_add (&d1, &d2);
		break;
	case '-':
		cob_decimal_sub (&d1, &d2);
		break;
	case '*':
		cob_decimal_mul (&d1, &d2);
		break;
	case '/':
		cob_decimal_div (&d1, &d2);
		break;
	case '^':
		cob_decimal_pow (&d1, &d2);
		break;
	default:
		break;
	}
	cob_decimal_get_field (&d1, curr_field, 0);
	
	return curr_field;
}

/* Intrinsics */

cob_field *
cob_intr_length (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);
	cob_set_int (curr_field, (int)srcfield->size);
	return curr_field;
}

cob_field *
cob_intr_integer (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};
	int		i, scale;

	make_field_entry (&field);
/*
	cob_move (srcfield, curr_field);
	if ( *(long long *)curr_field->data < 0 ) {
		if ( cob_cmp (srcfield, curr_field) ) {
			*(long long *)curr_field->data -= 1;
		}
	}
*/

	cob_decimal_set_field (&d1, srcfield);
	if ( mpz_sgn (d1.value) >= 0 ) {
		cob_decimal_get_field (&d1, curr_field, 0);
		return curr_field;
	}
	scale = 1;
	for ( i = 0; i < d1.scale; i++ ) {
		scale *= 10;
	}
	if ( mpz_fdiv_ui (d1.value, scale) ) {
		mpz_sub_ui (d1.value, d1.value, scale);
	}
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_integer_part (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	make_field_entry (&field);

	cob_move (srcfield, curr_field);
	return curr_field;
}

cob_field *
cob_intr_fraction_part (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 18, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	make_field_entry (&field);

	cob_move (srcfield, curr_field);
	return curr_field;
}

cob_field *
cob_intr_sign (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {4, NULL, &attr};
	int		n;

	make_field_entry (&field);

	cob_set_int (curr_field, 0);
	n = cob_cmp (srcfield, curr_field);
	if ( n < 0 ) {
		cob_set_int (curr_field, -1);
	} else if ( n > 0 ) {
		cob_set_int (curr_field, 1);
	}

	return curr_field;
}

cob_field *
cob_intr_upper_case (cob_field *sizefield, cob_field *srcfield)
{
	int		i, size;

	make_field_entry (srcfield);
	if ( sizefield ) {
		size = cob_get_int (sizefield);
		curr_field->size = size;
	} else {
		size = (int) srcfield->size;
	}
	for ( i = 0; i < size; i++ ) {
		curr_field->data[i] = toupper (srcfield->data[i]);
	}
	return curr_field;
}

cob_field *
cob_intr_lower_case (cob_field *sizefield, cob_field *srcfield)
{
	int		i, size;

	make_field_entry (srcfield);
	if ( sizefield ) {
		size = cob_get_int (sizefield);
		curr_field->size = size;
	} else {
		size = (int) srcfield->size;
	}
	for ( i = 0; i < size; i++ ) {
		curr_field->data[i] = tolower (srcfield->data[i]);
	}
	return curr_field;
}

cob_field *
cob_intr_reverse (cob_field *sizefield, cob_field *srcfield)
{
	int		i, size;

	make_field_entry (srcfield);
	if ( sizefield ) {
		size = cob_get_int (sizefield);
		curr_field->size = size;
	} else {
		size = (int) srcfield->size;
	}
	for ( i = 0; i < size; i++ ) {
		curr_field->data[i] = srcfield->data[size - i - 1];
	}
	return curr_field;
}

cob_field *
cob_intr_current_date ()
{
#if !defined(__linux__) && !defined(__CYGWIN__) && defined(HAVE_TIMEZONE)
	long    contz;
#endif
	time_t 		curtime;
	cob_field_attr	attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field	field = {21, NULL, &attr};
	char		buff[24];

	make_field_entry (&field);

	curtime = time (NULL);
#if defined(__linux__) || defined(__CYGWIN__)
	strftime (buff, 22, "%Y%m%d%H%M%S00%z", localtime (&curtime));
#elif defined(HAVE_TIMEZONE)
	strftime (buff, 17, "%Y%m%d%H%M%S00", localtime (&curtime));
	if (timezone <= 0) {
		contz = -timezone;
		buff[16] = '+';
	} else {
		contz = timezone;
		buff[16] = '-';
	}
	sprintf(&buff[17], "%2.2ld%2.2ld", contz / 3600, contz % 60);
#else
	strftime (buff, 22, "%Y%m%d%H%M%S0000000", localtime (&curtime));
#endif
	memcpy (curr_field->data, buff, 21);
	return curr_field;
}

cob_field *
cob_intr_char (cob_field *srcfield)
{
	int		i;
	cob_field_attr	attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field	field = {1, NULL, &attr};

	make_field_entry (&field);

	i = cob_get_int (srcfield);
	if ( i < 1 || i > 256 ) {
		*curr_field->data = 0;
	} else {
		*curr_field->data = i - 1;
	}
	return curr_field;
}

cob_field *
cob_intr_ord (cob_field *srcfield)
{
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);
	
	cob_set_int (curr_field, (int)(*srcfield->data + 1));
	return curr_field;
}

cob_field *
cob_intr_date_of_integer (cob_field *srcdays)
{
	int		i;
	int		days;
	int		baseyear = 1601;
	int		leapyear = 365;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, NULL};
	cob_field	field = {8, NULL, &attr};
	char		buff[16];

	make_field_entry (&field);

	/* Base 1601-01-01 */
	days = cob_get_int (srcdays);
	if ( days < 1 || days > 3067671 ) {
		memset (curr_field->data, '0', 8);
		return curr_field;
	}
	while ( days > leapyear ) {
		days -= leapyear;
		baseyear++;
		if ( leap_year (baseyear) ) {
			leapyear = 366;
		} else {
			leapyear = 365;
		}
	}
	for ( i = 0; i < 13; i++ ) {
		if ( leap_year (baseyear) ) {
			if ( days <= leap_days[i] ) {
				days -= leap_days[i-1];
				break;
			}
		} else {
			if ( days <= normal_days[i] ) {
				days -= normal_days[i-1];
				break;
			}
		}
	}
	sprintf (buff, "%4.4d%2.2d%2.2d", baseyear, i, days);
	memcpy (curr_field->data, buff, 8);
	return curr_field;
}

cob_field *
cob_intr_day_of_integer (cob_field *srcdays)
{
	int		days;
	int		baseyear = 1601;
	int		leapyear = 365;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_DISPLAY, 7, 0, 0, NULL};
	cob_field	field = {7, NULL, &attr};
	char		buff[16];

	make_field_entry (&field);

	/* Base 1601-01-01 */
	days = cob_get_int (srcdays);
	if ( days < 1 || days > 3067671 ) {
		memset (curr_field->data, '0', 7);
		return curr_field;
	}
	while ( days > leapyear ) {
		days -= leapyear;
		baseyear++;
		if ( leap_year (baseyear) ) {
			leapyear = 366;
		} else {
			leapyear = 365;
		}
	}
	sprintf (buff, "%4.4d%3.3d", baseyear, days);
	memcpy (curr_field->data, buff, 7);
	return curr_field;
}

cob_field *
cob_intr_integer_of_date (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		totaldays;
	int		month;
	int		year;
	int		baseyear = 1601;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 10000;
	if ( year < 1601 || year > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	indate %= 10000;
	month = indate / 100;
	if ( month < 1 || month > 12 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	days = indate % 100;
	if ( days < 1 || days > 31 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( leap_year (year) ) {
		if ( days > leap_month_days[month] ) {
			cob_set_int (curr_field, 0);
			return curr_field;
		}
	} else {
		if ( days > normal_month_days[month] ) {
			cob_set_int (curr_field, 0);
			return curr_field;
		}
	}
	totaldays = 0;
	while ( baseyear != year ) {
		if ( leap_year (baseyear) ) {
			totaldays += 366;
		} else {
			totaldays += 365;
		}
		baseyear++;
	}
	if ( leap_year (baseyear) ) {
		totaldays += leap_days[month - 1];
	} else {
		totaldays += normal_days[month - 1];
	}
	totaldays += days;
	cob_set_int (curr_field, totaldays);
	return curr_field;
}

cob_field *
cob_intr_integer_of_day (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		totaldays;
	int		year;
	int		baseyear = 1601;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 1000;
	if ( year < 1601 || year > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	days = indate % 1000;
	if ( days < 1 || days > 365 + leap_year (year) ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	totaldays = 0;
	while ( baseyear != year ) {
		if ( leap_year (baseyear) ) {
			totaldays += 366;
		} else {
			totaldays += 365;
		}
		baseyear++;
	}
	totaldays += days;
	cob_set_int (curr_field, totaldays);
	return curr_field;
}

cob_field *
cob_intr_test_date_yyyymmdd (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		month;
	int		year;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 10000;
	if ( year < 1601 || year > 9999 ) {
		cob_set_int (curr_field, 1);
		return curr_field;
	}
	indate %= 10000;
	month = indate / 100;
	if ( month < 1 || month > 12 ) {
		cob_set_int (curr_field, 2);
		return curr_field;
	}
	days = indate % 100;
	if ( days < 1 || days > 31 ) {
		cob_set_int (curr_field, 3);
		return curr_field;
	}
	if ( leap_year (year) ) {
		if ( days > leap_month_days[month] ) {
			cob_set_int (curr_field, 3);
			return curr_field;
		}
	} else {
		if ( days > normal_month_days[month] ) {
			cob_set_int (curr_field, 3);
			return curr_field;
		}
	}
	cob_set_int (curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_test_day_yyyyddd (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		year;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 1000;
	if ( year < 1601 || year > 9999 ) {
		cob_set_int (curr_field, 1);
		return curr_field;
	}
	days = indate % 1000;
	if ( days < 1 || days > 365 + leap_year (year) ) {
		cob_set_int (curr_field, 2);
		return curr_field;
	}
	cob_set_int (curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_factorial (cob_field *srcfield)
{
	int		srcval;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, 0, NULL};
	cob_field	field = {8, NULL, &attr};

	make_field_entry (&field);

	srcval = cob_get_int (srcfield);
	if ( srcval < 0 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	d1.scale = 0;
	mpz_fac_ui (d1.value, srcval);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_exp (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = pow (2.7182818284590452354, intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_exp10 (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = pow (10.0, intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_abs (cob_field *srcfield)
{

	make_field_entry (srcfield);
	cob_decimal_set_field (&d1, srcfield);
	mpz_abs (d1.value, d1.value);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_acos (cob_field *srcfield)
{
	int			i, tempres;
	unsigned long long	result;
	double			mathd2;
	cob_field_attr		attr = {COB_TYPE_NUMERIC_BINARY, 18, 17, 0, NULL};
	cob_field		field = {8, NULL, &attr};

	cob_decimal_set_field (&d1, srcfield);
	make_field_entry (&field);
	
	errno = 0;
	mathd2 = acos (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}

	result = (unsigned long long) mathd2;
	mathd2 -= result;
	for ( i = 0; i < 17; i++ ) {
		mathd2 *= 10;
		tempres = (int) mathd2;
		result *= 10;
		result += tempres;
		mathd2 -= tempres;
	}
	memcpy (curr_field->data, (char *)&result, 8);
	return curr_field;
}

cob_field *
cob_intr_asin (cob_field *srcfield)
{
	int			i, tempres;
	long long		result;
	double			mathd2;
	cob_field_attr		attr = {COB_TYPE_NUMERIC_BINARY, 18, 17, COB_FLAG_HAVE_SIGN, NULL};
	cob_field		field = {8, NULL, &attr};

	cob_decimal_set_field (&d1, srcfield);
	make_field_entry (&field);

	errno = 0;
	mathd2 = asin (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	result = (long long) mathd2;
	mathd2 -= result;
	for ( i = 0; i < 17; i++ ) {
		mathd2 *= 10;
		tempres = (int) mathd2;
		result *= 10;
		result += tempres;
		mathd2 -= tempres;
	}
	memcpy (curr_field->data, (char *)&result, 8);
	return curr_field;
}

cob_field *
cob_intr_atan (cob_field *srcfield)
{
	int			i, tempres;
	long long		result;
	double			mathd2;
	cob_field_attr		attr = {COB_TYPE_NUMERIC_BINARY, 18, 17, COB_FLAG_HAVE_SIGN, NULL};
	cob_field		field = {8, NULL, &attr};

	cob_decimal_set_field (&d1, srcfield);
	make_field_entry (&field);

	errno = 0;
	mathd2 = atan (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	result = (long long) mathd2;
	mathd2 -= result;
	for ( i = 0; i < 17; i++ ) {
		mathd2 *= 10;
		tempres = (int) mathd2;
		result *= 10;
		result += tempres;
		mathd2 -= tempres;
	}
	memcpy (curr_field->data, (char *)&result, 8);
	return curr_field;
}

cob_field *
cob_intr_cos (cob_field *srcfield)
{
	int			i, tempres;
	long long		result;
	double			mathd2;
	cob_field_attr		attr = {COB_TYPE_NUMERIC_BINARY, 18, 17, COB_FLAG_HAVE_SIGN, NULL};
	cob_field		field = {8, NULL, &attr};

	cob_decimal_set_field (&d1, srcfield);
	make_field_entry (&field);

	errno = 0;
	mathd2 = cos (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	result = (long long) mathd2;
	mathd2 -= result;
	for ( i = 0; i < 17; i++ ) {
		mathd2 *= 10;
		tempres = (int) mathd2;
		result *= 10;
		result += tempres;
		mathd2 -= tempres;
	}
	memcpy (curr_field->data, (char *)&result, 8);
	return curr_field;
}

cob_field *
cob_intr_log (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = log (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_log10 (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = log10(intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_sin (cob_field *srcfield)
{
	int			i, tempres;
	long long		result;
	double			mathd2;
	cob_field_attr		attr = {COB_TYPE_NUMERIC_BINARY, 18, 17, COB_FLAG_HAVE_SIGN, NULL};
	cob_field		field = {8, NULL, &attr};

	cob_decimal_set_field (&d1, srcfield);
	make_field_entry (&field);

	errno = 0;
	mathd2 = sin (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	result = (long long) mathd2;
	mathd2 -= result;
	for ( i = 0; i < 17; i++ ) {
		mathd2 *= 10;
		tempres = (int) mathd2;
		result *= 10;
		result += tempres;
		mathd2 -= tempres;
	}
	memcpy (curr_field->data, (char *)&result, 8);
	return curr_field;
}

cob_field *
cob_intr_sqrt (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = sqrt (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_tan (cob_field *srcfield)
{
	double		mathd2;

	cob_decimal_set_field (&d1, srcfield);
	make_double_entry ();

	errno = 0;
	mathd2 = tan (intr_get_double (&d1));
	if ( errno ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	memcpy (curr_field->data, (char *)&mathd2, 8);
	return curr_field;
}

cob_field *
cob_intr_numval (cob_field *srcfield)
{
	size_t		i;
	int		integer_digits = 0;
	int		decimal_digits = 0;
	int		sign = 0;
	int		decimal_seen = 0;
	long long	llval = 0;
	double		val;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};
	unsigned char	rec_buff[64];
	unsigned char	integer_buff[64];
	unsigned char	decimal_buff[64];
	unsigned char	final_buff[64];

	memset (rec_buff, 0, sizeof (rec_buff));
	memset (integer_buff, 0, sizeof (integer_buff));
	memset (decimal_buff, 0, sizeof (decimal_buff));
	memset (final_buff, 0, sizeof (final_buff));

	memcpy (rec_buff, srcfield->data, srcfield->size);
	for ( i = 0; i < srcfield->size; i++ ) {
		if ( strcasecmp ((char *)&rec_buff[i], "CR") == 0
		     || strcasecmp ((char *)&rec_buff[i], "DB") == 0 ) {
			sign = 1;
			break;
		}
		if ( rec_buff[i] == ' ' ) {
			continue;
		}
		if ( rec_buff[i] == '+' ) {
			continue;
		}
		if ( rec_buff[i] == '-' ) {
			sign = 1;
			continue;
		}
		if ( rec_buff[i] == cob_current_module->decimal_point ) {
			decimal_seen = 1;
			continue;
		}
		if ( rec_buff[i] >= '0' && rec_buff[i] <= '9' ) {
			llval *= 10;
			llval += rec_buff[i] - '0';
			if ( decimal_seen ) {
				decimal_buff[decimal_digits++] = rec_buff[i];
			} else {
				integer_buff[integer_digits++] = rec_buff[i];
			}
		}
	}
	if ( !integer_digits ) {
		integer_buff[0] = '0';
	}
	if ( !decimal_digits ) {
		decimal_buff[0] = '0';
	}
	if ( sign ) {
		llval = -llval;
	}
	if ( (integer_digits + decimal_digits) <= 18 ) {
		attr.scale = decimal_digits;
		make_field_entry (&field);
		memcpy (curr_field->data, (char *)&llval, 8);
	} else {
		sprintf ((char *)final_buff, "%s%s.%s", sign ? "-" : "", integer_buff, decimal_buff);
		sscanf ((char *)final_buff, "%lf", &val);
		make_double_entry ();
		memcpy (curr_field->data, (char *)&val, sizeof (double));
	}
	return curr_field;
}

cob_field *
cob_intr_numval_c (cob_field *srcfield)
{
	size_t		i;
	int		integer_digits = 0;
	int		decimal_digits = 0;
	int		sign = 0;
	int		decimal_seen = 0;
	long long	llval = 0;
	double		val;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};
	unsigned char	rec_buff[64];
	unsigned char	integer_buff[64];
	unsigned char	decimal_buff[64];
	unsigned char	final_buff[64];

	memset (rec_buff, 0, sizeof (rec_buff));
	memset (integer_buff, 0, sizeof (integer_buff));
	memset (decimal_buff, 0, sizeof (decimal_buff));
	memset (final_buff, 0, sizeof (final_buff));

	memcpy (rec_buff, srcfield->data, srcfield->size);
	for ( i = 0; i < srcfield->size; i++ ) {
		if ( strcasecmp ((char *)&rec_buff[i], "CR") == 0
		     || strcasecmp ((char *)&rec_buff[i], "DB") == 0 ) {
			sign = 1;
			break;
		}
		if ( rec_buff[i] == ' ' ) {
			continue;
		}
		if ( rec_buff[i] == '+' ) {
			continue;
		}
		if ( rec_buff[i] == '-' ) {
			sign = 1;
			continue;
		}
		if ( rec_buff[i] == cob_current_module->decimal_point ) {
			decimal_seen = 1;
			continue;
		}
		if ( rec_buff[i] >= '0' && rec_buff[i] <= '9' ) {
			llval *= 10;
			llval += rec_buff[i] - '0';
			if ( decimal_seen ) {
				decimal_buff[decimal_digits++] = rec_buff[i];
			} else {
				integer_buff[integer_digits++] = rec_buff[i];
			}
		}
	}
	if ( !integer_digits ) {
		integer_buff[0] = '0';
	}
	if ( !decimal_digits ) {
		decimal_buff[0] = '0';
	}
	if ( sign ) {
		llval = -llval;
	}
	if ( (integer_digits + decimal_digits) <= 18 ) {
		attr.scale = decimal_digits;
		make_field_entry (&field);
		memcpy (curr_field->data, (char *)&llval, 8);
	} else {
		sprintf ((char *)final_buff, "%s%s.%s", sign ? "-" : "", integer_buff, decimal_buff);
		sscanf ((char *)final_buff, "%lf", &val);
		make_double_entry ();
		memcpy (curr_field->data, (char *)&val, sizeof (double));
	}
	return curr_field;
}

cob_field *
cob_intr_annuity (cob_field *srcfield1, cob_field *srcfield2)
{
	double		mathd1, mathd2;

	make_double_entry ();

	cob_decimal_set_field (&d1, srcfield1);
	cob_decimal_set_field (&d2, srcfield2);
	
	mathd1 = intr_get_double (&d1);
	mathd2 = intr_get_double (&d2);
	if ( mathd1 == 0 ) {
		mathd1 = 1.0 / mathd2;
		memcpy (curr_field->data, (char *)&mathd1, sizeof (double));
		return curr_field;
	}
	mathd1 /= (1.0 - pow (mathd1 + 1.0, 0.0 - mathd2));
	memcpy (curr_field->data, (char *)&mathd1, sizeof (double));
	return curr_field;
}

cob_field *
cob_intr_sum (int params, ...)
{
	int		i;
	int		digits = 0;
	int		scale = 0;
	cob_field	*f;
	va_list		args;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};


	va_start (args, params);

	mpz_set_ui (d1.value, 0);
	d1.scale = 0;

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( (f->attr->digits - f->attr->scale) > digits ) {
			digits = f->attr->digits - f->attr->scale;
		}
		if ( f->attr->scale > scale ) {
			scale = f->attr->scale;
		}
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);

	attr.scale = scale;
	make_field_entry (&field);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_ord_min (int params, ...)
{
	int		i;
	cob_field	*f, *basef;
	int		ordmin = 0;
	va_list		args;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	if ( params <= 1 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basef) < 0 ) {
			basef = f;
			ordmin = i;
		}
	}
	va_end (args);

	cob_set_int (curr_field, ordmin + 1);
	return curr_field;
}

cob_field *
cob_intr_ord_max (int params, ...)
{
	int		i;
	cob_field	*f, *basef;
	int		ordmin = 0;
	va_list		args;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	if ( params <= 1 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basef) > 0 ) {
			basef = f;
			ordmin = i;
		}
	}
	va_end (args);

	cob_set_int (curr_field, ordmin + 1);
	return curr_field;
}

cob_field *
cob_intr_min (int params, ...)
{
	int		i;
	cob_field	*f, *basef;
	va_list		args;

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basef) < 0 ) {
			basef = f;
		}
	}
	va_end (args);

	return basef;
}

cob_field *
cob_intr_max (int params, ...)
{
	int		i;
	cob_field	*f, *basef;
	va_list		args;

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basef) > 0 ) {
			basef = f;
		}
	}
	va_end (args);

	return basef;
}

cob_field *
cob_intr_midrange (int params, ...)
{
	int		i;
	cob_field	*f, *basemin, *basemax;
	va_list		args;

	make_double_entry ();
	va_start (args, params);

	basemin = va_arg (args, cob_field *);
	basemax = basemin;
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basemin) < 0 ) {
			basemin = f;
		}
		if ( cob_cmp (f, basemax) > 0 ) {
			basemax = f;
		}
	}
	va_end (args);

	cob_decimal_set_field (&d1, basemin);
	cob_decimal_set_field (&d2, basemax);
	cob_decimal_add (&d1, &d2);
	mpz_set_ui (d2.value, 2);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_median (int params, ...)
{
	int		i;
	cob_field	*f;
	cob_field	**field_alloc;
	va_list		args;

	va_start (args, params);

	f = va_arg (args, cob_field *);
	if ( params == 1 ) {
		return f;
	}

	field_alloc = cob_malloc (params * sizeof (cob_field *));
	field_alloc[0] = f;

	for ( i = 1; i < params; i++ ) {
		field_alloc[i] = va_arg (args, cob_field *);
	}
	va_end (args);

	qsort (field_alloc, (size_t)params, (size_t)sizeof (cob_field *), comp_field);

	i = params / 2;
	if ( params % 2 ) {
		f = field_alloc[i];
	} else {
		make_double_entry ();
		cob_decimal_set_field (&d1, field_alloc[i-1]);
		cob_decimal_set_field (&d2, field_alloc[i]);
		cob_decimal_add (&d1, &d2);
		mpz_set_ui (d2.value, 2);
		d2.scale = 0;
		cob_decimal_div (&d1, &d2);
		cob_decimal_get_field (&d1, curr_field, 0);
		f = curr_field;
	}
	
	free (field_alloc);
	return f;
}

cob_field *
cob_intr_mean (int params, ...)
{
	int		i;
	cob_field	*f;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};
	unsigned char	data[16];
	long long	n;
	va_list		args;


	va_start (args, params);
	mpz_set_ui (d1.value, 0);
	d1.scale = 0;

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);

	mpz_set_ui (d2.value, params);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);
	field.data = data;
	cob_decimal_get_field (&d1, &field, 0);
	n = *(long long *)data;
	i = 0;
	for ( ; n; n /= 10, i++ ) ;
	field.data = NULL;
	if ( i <= 18 ) {
		attr.scale = 18 - i;
	}
	make_field_entry (&field);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_mod (cob_field *srcfield1, cob_field *srcfield2)
{
	cob_field	*f1;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	make_field_entry (&field);

	f1 = cob_intr_integer (cob_intr_binop (srcfield1, '/', srcfield2));
	cob_decimal_set_field (&d1, srcfield2);
	cob_decimal_set_field (&d2, f1);
	cob_decimal_mul (&d2, &d1);
	cob_decimal_set_field (&d1, srcfield1);
	cob_decimal_sub (&d1, &d2);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_range (int params, ...)
{
	int		i;
	cob_field	*f, *basemin, *basemax;
	va_list		args;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	va_start (args, params);

	basemin = va_arg (args, cob_field *);
	basemax = basemin;
	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		if ( cob_cmp (f, basemin) < 0 ) {
			basemin = f;
		}
		if ( cob_cmp (f, basemax) > 0 ) {
			basemax = f;
		}
	}
	va_end (args);

	attr.scale = basemin->attr->scale;
	if ( basemax->attr->scale > attr.scale ) {
		attr.scale = basemax->attr->scale;
	}
	make_field_entry (&field);
	cob_decimal_set_field (&d1, basemax);
	cob_decimal_set_field (&d2, basemin);
	cob_decimal_sub (&d1, &d2);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_rem (cob_field *srcfield1, cob_field *srcfield2)
{
	cob_field	*f1;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	f1 = cob_intr_integer_part (cob_intr_binop (srcfield1, '/', srcfield2));
	cob_decimal_set_field (&d1, srcfield2);
	cob_decimal_set_field (&d2, f1);
	cob_decimal_mul (&d2, &d1);
	cob_decimal_set_field (&d1, srcfield1);
	cob_decimal_sub (&d1, &d2);

	attr.scale = srcfield1->attr->scale;
	if ( srcfield2->attr->scale > attr.scale ) {
		attr.scale = srcfield2->attr->scale;
	}
	make_field_entry (&field);
	cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_random (int params, ...)
{
	int		seed = 1;
	int		randnum, i;
	cob_field	*f;
	va_list		args;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 9, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};

	va_start (args, params);

	if ( params ) {
		f = va_arg (args, cob_field *);
		seed = cob_get_int (f);
		if ( seed < 0 ) {
			seed = 0;
		}
		srand ((unsigned int)seed);
	}
	va_end (args);

	randnum = rand ();
	for ( i = 0; i < 10; i++ ) {
		if ( (randnum / cob_exp10[i]) == 0 ) {
			break;
		}
	}
	if ( i == 0 ) {
		i = 1;
	}
	attr.scale = i;
	make_field_entry (&field);
	*(long long *)curr_field->data = (long long)randnum;
	return curr_field;
}

cob_field *
cob_intr_variance (int params, ...)
{
	int		i;
	cob_field	*f;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
	cob_field	field = {8, NULL, &attr};
	long long	n;
	unsigned char	data[16];
	va_list		args;

	if ( params == 1 ) {
		make_field_entry (&field);
		cob_set_int (curr_field, 0);
		return curr_field;
	}

	va_start (args, params);

	/* MEAN for all params */
	mpz_set_ui (d1.value, 0);
	d1.scale = 0;

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);
	mpz_set_ui (d2.value, params);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);

	/* Got the MEAN in d1, iterate again */

	mpz_set_ui (d4.value, 0);
	d4.scale = 0;

	va_start (args, params);

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_sub (&d2, &d1);
		cob_decimal_mul (&d2, &d2);
		cob_decimal_add (&d4, &d2);
	}
	va_end (args);

	mpz_set_ui (d3.value, params);
	d3.scale = 0;
	cob_decimal_div (&d4, &d3);
	field.data = data;
	cob_decimal_get_field (&d4, &field, 0);
	n = *(long long *)data;
	i = 0;
	for ( ; n; n /= 10, i++ ) ;
	field.data = NULL;
	if ( i <= 18 ) {
		attr.scale = 18 - i;
	}
	make_field_entry (&field);
	cob_decimal_get_field (&d4, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_standard_deviation (int params, ...)
{
	int		i;
	cob_field	*f;
	va_list		args;

	va_start (args, params);
	make_double_entry ();

	if ( params == 1 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}

	/* MEAN for all params */
	mpz_set_ui (d1.value, 0);
	d1.scale = 0;

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);
	mpz_set_ui (d2.value, params);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);

	/* Got the MEAN in d1, iterate again */

	mpz_set_ui (d4.value, 0);
	d4.scale = 0;

	va_start (args, params);

	for ( i = 0; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_sub (&d2, &d1);
		cob_decimal_mul (&d2, &d2);
		cob_decimal_add (&d4, &d2);
	}
	va_end (args);

	mpz_set_ui (d3.value, params);
	d3.scale = 0;
	cob_decimal_div (&d4, &d3);
	/* We have the VARIANCE in d4, sqrt = STANDARD-DEVIATION */

/* Do not know why this does not work
	d5.scale = d4.scale;
	mpz_mul_ui (d5.value, d4.value, 1000000000);
	mpz_mul_ui (d4.value, d5.value, 1000000000);
	mpz_sqrt (d5.value, d4.value);
	mpz_div_ui (d4.value, d5.value, 1000000000);
	cob_decimal_get_field (&d4, curr_field, 0);
	return curr_field;
*/

	cob_decimal_get_field (&d4, curr_field, 0);
	f = cob_intr_sqrt (curr_field);
	return f;
}

cob_field *
cob_intr_present_value (int params, ...)
{
	int		i;
	cob_field	*f;
	va_list		args;

	va_start (args, params);
	make_double_entry ();

	if ( params < 2 ) {
		fprintf (stderr, "Wrong number of parameters for FUNCTION PRESENT-VALUE\n");
		fflush (stderr);
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	f = va_arg (args, cob_field *);
	cob_decimal_set_field (&d1, f);
	mpz_set_ui (d2.value, 1);
	d2.scale = 0;
	cob_decimal_add (&d1, &d2);

	mpz_set_ui (d4.value, 0);
	d4.scale = 0;

	for ( i = 1; i < params; i++ ) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		mpz_set (d3.value, d1.value);
		d3.scale = d1.scale;
		if ( i > 1 ) {
			mpz_set_ui (d5.value, i);
			d5.scale = 0;
			cob_decimal_pow (&d3, &d5);
		}
		cob_decimal_div (&d2, &d3);
		cob_decimal_add (&d4, &d2);
	}
	va_end (args);

	cob_decimal_get_field (&d4, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_year_to_yyyy (int params, ...)
{
	int		year;
	int		interval;
	int		xqtyear;
	int		maxyear;
	cob_field	*f;
	va_list		args;
	time_t		t;
	struct tm	*timeptr;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	va_start (args, params);
	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	if ( params > 1 ) {
		f = va_arg (args, cob_field *);
		interval = cob_get_int (f);
	} else {
		interval = 50;
	}
	if ( params > 2 ) {
		f = va_arg (args, cob_field *);
		xqtyear = cob_get_int (f);
	} else {
		t = time (NULL);
		timeptr = localtime (&t);
		xqtyear = 1900 + timeptr->tm_year;
	}
	va_end (args);

	if ( year < 0 || year > 99 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( xqtyear < 1601 || xqtyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	maxyear = xqtyear + interval;
	if ( maxyear < 1700 || maxyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( maxyear % 100 >= year ) {
		year += 100 * ( maxyear / 100 );
	} else {
		year += 100 * ( (maxyear / 100) - 1 );
	}
	cob_set_int (curr_field, year);
	return curr_field;
}

cob_field *
cob_intr_date_to_yyyymmdd (int params, ...)
{
	int		year;
	int		mmdd;
	int		interval;
	int		xqtyear;
	int		maxyear;
	cob_field	*f;
	va_list		args;
	time_t		t;
	struct tm	*timeptr;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	va_start (args, params);
	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	mmdd = year % 10000;
	year /= 10000;
	if ( params > 1 ) {
		f = va_arg (args, cob_field *);
		interval = cob_get_int (f);
	} else {
		interval = 50;
	}
	if ( params > 2 ) {
		f = va_arg (args, cob_field *);
		xqtyear = cob_get_int (f);
	} else {
		t = time (NULL);
		timeptr = localtime (&t);
		xqtyear = 1900 + timeptr->tm_year;
	}
	va_end (args);

	if ( year < 0 || year > 999999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( xqtyear < 1601 || xqtyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	maxyear = xqtyear + interval;
	if ( maxyear < 1700 || maxyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( maxyear % 100 >= year ) {
		year += 100 * ( maxyear / 100 );
	} else {
		year += 100 * ( (maxyear / 100) - 1 );
	}
	year *= 10000;
	year += mmdd;
	cob_set_int (curr_field, year);
	return curr_field;
}

cob_field *
cob_intr_day_to_yyyyddd (int params, ...)
{
	int		year;
	int		days;
	int		interval;
	int		xqtyear;
	int		maxyear;
	cob_field	*f;
	va_list		args;
	time_t		t;
	struct tm	*timeptr;
	cob_field_attr	attr = {COB_TYPE_NUMERIC_BINARY, 8, 0, 0, NULL};
	cob_field	field = {4, NULL, &attr};

	make_field_entry (&field);

	va_start (args, params);
	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	days = year % 1000;
	year /= 1000;
	if ( params > 1 ) {
		f = va_arg (args, cob_field *);
		interval = cob_get_int (f);
	} else {
		interval = 50;
	}
	if ( params > 2 ) {
		f = va_arg (args, cob_field *);
		xqtyear = cob_get_int (f);
	} else {
		t = time (NULL);
		timeptr = localtime (&t);
		xqtyear = 1900 + timeptr->tm_year;
	}
	va_end (args);

	if ( year < 0 || year > 999999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( xqtyear < 1601 || xqtyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	maxyear = xqtyear + interval;
	if ( maxyear < 1700 || maxyear > 9999 ) {
		cob_set_int (curr_field, 0);
		return curr_field;
	}
	if ( maxyear % 100 >= year ) {
		year += 100 * ( maxyear / 100 );
	} else {
		year += 100 * ( (maxyear / 100) - 1 );
	}
	year *= 1000;
	year += days;
	cob_set_int (curr_field, year);
	return curr_field;
}
