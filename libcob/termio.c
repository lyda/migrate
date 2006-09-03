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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <time.h>

#include "move.h"
#include "termio.h"

static const int	bin_digits[] = { 1, 3, 5, 8, 10, 13, 15, 17, 20 };

/*
 * DISPLAY
 */

static void
display_numeric (cob_field *f, FILE *fp)
{
	int		i;
	int		digits = COB_FIELD_DIGITS (f);
	int		scale = COB_FIELD_SCALE (f);
	int		size = digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0);
	unsigned char	data[128];
	cob_field_attr	attr = { COB_TYPE_NUMERIC_DISPLAY, digits, scale, 0, NULL };
	cob_field	temp = { size, data, &attr };

	if (COB_FIELD_HAVE_SIGN (f)) {
		attr.flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
		if (COB_FIELD_SIGN_LEADING (f)
		    || COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_BINARY) {
			attr.flags |= COB_FLAG_SIGN_LEADING;
		}
	}

	cob_move (f, &temp);
	for (i = 0; i < size; i++) {
		putc (data[i], fp);
	}
}

static void
pretty_display_numeric (cob_field *f, FILE *fp)
{
	int		i;
	unsigned char	*p;
	int		digits = COB_FIELD_DIGITS (f);
	int		scale = COB_FIELD_SCALE (f);
	int		size = (digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0)
				+ (scale > 0 ? 1 : 0));
	cob_field_attr	attr = { COB_TYPE_NUMERIC_EDITED, digits, scale, 0, NULL };
	cob_field	temp;
	unsigned char	pic[16];
	unsigned char	data[128];

	p = pic;
	temp.size = size;
	temp.data = data;
	temp.attr = &attr;
	attr.pic = (char *)pic;
	memset (pic, 0, sizeof (pic));
	memset (data, 0, sizeof (data));
	if (COB_FIELD_HAVE_SIGN (f)) {
		p += sprintf ((char *)p, "+\001");
	}
	if (scale > 0) {
		p += sprintf ((char *)p, "9%c", digits - scale);
		p += sprintf ((char *)p, "%c%c", cob_current_module->decimal_point, 1);
		p += sprintf ((char *)p, "9%c", scale);
	} else {
		p += sprintf ((char *)p, "9%c", digits);
	}

	cob_move (f, &temp);
	for (i = 0; i < size; i++) {
		putc (data[i], fp);
	}
}

static void
display_alnum (cob_field *f, FILE *fp)
{
	size_t	i;

	for (i = 0; i < f->size; i++) {
		putc (f->data[i], fp);
	}
}

static void
display (cob_field *f, FILE *fp)
{
	if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DOUBLE) {
		double f1doub;

		memcpy ((char *)&f1doub, f->data, sizeof (double));
		fprintf (fp, "%-.18lf", f1doub);
	} else if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_FLOAT) {
		float f1float;

		memcpy ((char *)&f1float, f->data, sizeof (float));
		fprintf (fp, "%-.18lf", (double)f1float);
	} else if (f->attr->type == COB_TYPE_NUMERIC_BINARY
		   && !cob_current_module->flag_pretty_display) {
		cob_field_attr	attr = *f->attr;
		cob_field	temp = *f;

		attr.digits = bin_digits[f->size];
		temp.attr = &attr;
		display_numeric (&temp, fp);
	} else if (COB_FIELD_IS_NUMERIC (f)) {
		if (cob_current_module->flag_pretty_display) {
			pretty_display_numeric (f, fp);
		} else {
			display_numeric (f, fp);
		}
	} else {
		display_alnum (f, fp);
	}
}

void
cob_new_display (const int outorerr, const int newline, const int varcnt, ...)
{
	FILE		*fp;
	cob_field	*f;
	int		i;
	va_list		args;

	if (!outorerr) {
		fp = stdout;
	} else {
		fp = stderr;
	}
	va_start (args, varcnt);
	for (i = 0; i < varcnt; i++) {
		f = va_arg (args, cob_field *);
		display (f, fp);
	}
	va_end (args);
	if (newline) {
		putc ('\n', fp);
		fflush (fp);
	}
}

/*
 * ACCEPT
 */

void
cob_accept (cob_field *f)
{
	unsigned char	buff[COB_MEDIUM_BUFF];

	if (isatty (fileno (stdin))) {
		/* terminal input */
		cob_field_attr	attr = { COB_TYPE_ALPHANUMERIC, 0, 0, 0, 0 };
		cob_field	temp = { 0, buff, &attr };

		/* read a line */
		fgets ((char *)buff, COB_MEDIUM_BUFF, stdin);
		temp.size = strlen ((char *)buff) - 1;

		/* move it to the field */
		cob_move (&temp, f);
	} else {
		/* non-terminal input */
		size_t	size;

		fgets ((char *)buff, COB_MEDIUM_BUFF, stdin);
		size = strlen ((char *)buff) - 1;
		if (size > f->size) {
			size = f->size;
		}
		memcpy (f->data, buff, size);
		memset (f->data + size, ' ', f->size - size);
	}
}

#if 0
void
cob_init_termio (void)
{
}
#endif
