/*
 * Copyright (C) 2002-2006 Keisuke Nishida
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
#include <sys/types.h>

#include "move.h"
#include "numeric.h"
#include "strings.h"

#define INSPECT_ALL		0
#define INSPECT_LEADING		1
#define INSPECT_FIRST	      	2
#define INSPECT_TRAILING      	3

static cob_field	*inspect_var;
static cob_field	inspect_var_copy;
static int		inspect_replacing;
static int		inspect_sign;
static size_t		inspect_size;
static unsigned char	*inspect_data;
static unsigned char	*inspect_start;
static unsigned char	*inspect_end;
static int		*inspect_mark = NULL;

static cob_field	*string_dst;
static cob_field	string_dst_copy;
static cob_field	*string_ptr;
static cob_field	string_ptr_copy;
static cob_field	*string_dlm;
static cob_field	string_dlm_copy;
static int		string_offset;

static cob_field	*unstring_src;
static cob_field	unstring_src_copy;
static cob_field	*unstring_ptr;
static cob_field	unstring_ptr_copy;
static int		unstring_offset;
static int		unstring_count;
static int		unstring_ndlms;
static struct {
	cob_field	*uns_dlm;
	int		uns_all;
} dlm_list[256];	/* FIXME - Needs to be dynamic */

static inline int
cob_min_int (const int x, const int y)
{
	if (x < y) {
		return x;
	}
	return y;
}

static void
inspect_common (cob_field *f1, cob_field *f2, int type)
{
	size_t	n = 0;
	int	i;
	int	len = (int)(inspect_end - inspect_start);
	int	*mark = &inspect_mark[inspect_start - inspect_data];

	if (inspect_replacing && f1->size != f2->size) {
		COB_SET_EXCEPTION (COB_EC_RANGE_INSPECT_SIZE);
		return;
	}

	if (type == INSPECT_TRAILING) {
		for (i = len - f2->size; i >= 0; i--) {
			/* find matching substring */
			if (memcmp (inspect_start + i, f2->data, f2->size) == 0) {
				size_t j;
				/* check if it is already marked */
				for (j = 0; j < f2->size; j++) {
					if (mark[i + j] != -1) {
						break;
					}
				}
				/* if not, mark and count it */
				if (j == f2->size) {
					for (j = 0; j < f2->size; j++) {
						mark[i + j] = inspect_replacing ? f1->data[j] : 1;
					}
					i -= f2->size - 1;
					n++;
				}
			} else {
				break;
			}
		}
	} else {
		for (i = 0; i < len - f2->size + 1; i++) {
			/* find matching substring */
			if (memcmp (inspect_start + i, f2->data, f2->size) == 0) {
				size_t j;
				/* check if it is already marked */
				for (j = 0; j < f2->size; j++) {
					if (mark[i + j] != -1) {
						break;
					}
				}
				/* if not, mark and count it */
				if (j == f2->size) {
					for (j = 0; j < f2->size; j++) {
						mark[i + j] = inspect_replacing ? f1->data[j] : 1;
					}
					i += f2->size - 1;
					n++;
					if (type == INSPECT_FIRST) {
						break;
					}
				}
			} else if (type == INSPECT_LEADING) {
				break;
			}
		}
	}

	if (n > 0 && !inspect_replacing) {
		cob_add_int (f1, (int) n);
	}
}

/*
 * INSPECT
 */

void
cob_inspect_init (cob_field *var, int replacing)
{
	size_t		i;
	size_t		digcount;
	static size_t	lastsize = 0;

	inspect_var_copy = *var;
	inspect_var = &inspect_var_copy;
	inspect_replacing = replacing;
	inspect_sign = cob_get_sign (var);
	inspect_size = COB_FIELD_SIZE (var);
	inspect_data = COB_FIELD_DATA (var);
	inspect_start = NULL;
	inspect_end = NULL;
	digcount = inspect_size * sizeof (int);
	if (!inspect_mark) {
		if (digcount <= COB_LARGE_BUFF) {
			inspect_mark = cob_malloc (COB_LARGE_BUFF);
			lastsize = COB_LARGE_BUFF;
		} else {
			inspect_mark = cob_malloc (digcount);
			lastsize = digcount;
		}
	} else {
		if (digcount > lastsize) {
			free (inspect_mark);
			inspect_mark = cob_malloc (digcount);
			lastsize = digcount;
		}
	}
	for (i = 0; i < inspect_size; i++) {
		inspect_mark[i] = -1;
	}
	cob_exception_code = 0;
}

void
cob_inspect_start (void)
{
	inspect_start = inspect_data;
	inspect_end = inspect_data + inspect_size;
}

void
cob_inspect_before (cob_field *str)
{
	unsigned char	*p;

	for (p = inspect_start; p < inspect_end - str->size + 1; p++) {
		if (memcmp (p, str->data, str->size) == 0) {
			inspect_end = p;
			return;
		}
	}
}

void
cob_inspect_after (cob_field *str)
{
	unsigned char	*p;

	for (p = inspect_start; p < inspect_end - str->size + 1; p++) {
		if (memcmp (p, str->data, str->size) == 0) {
			inspect_start = p + str->size;
			return;
		}
	}
	inspect_start = inspect_end;
}

void
cob_inspect_characters (cob_field *f1)
{
	int	i;
	int	len = (int)(inspect_end - inspect_start);
	int	*mark = &inspect_mark[inspect_start - inspect_data];

	if (inspect_replacing) {
		/* INSPECT REPLACING CHARACTERS f1 */
		for (i = 0; i < len; i++) {
			if (mark[i] == -1) {
				mark[i] = f1->data[0];
			}
		}
	} else {
		/* INSPECT TALLYING f1 CHARACTERS */
		int n = 0;

		for (i = 0; i < len; i++) {
			if (mark[i] == -1) {
				mark[i] = 1;
				n++;
			}
		}
		if (n > 0) {
			cob_add_int (f1, n);
		}
	}
}

void
cob_inspect_all (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_ALL);
}

void
cob_inspect_leading (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_LEADING);
}

void
cob_inspect_first (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_FIRST);
}

void
cob_inspect_trailing (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_TRAILING);
}

void
cob_inspect_converting (cob_field *f1, cob_field *f2)
{
	size_t	i, j;
	int	len = (int)(inspect_end - inspect_start);

	for (j = 0; j < f1->size; j++) {
		for (i = 0; i < len; i++) {
			if (inspect_mark[i] == -1 && inspect_start[i] == f1->data[j]) {
				inspect_start[i] = f2->data[j];
				inspect_mark[i] = 1;
			}
		}
	}
}

void
cob_inspect_finish (void)
{
	if (inspect_replacing) {
		size_t	i;
		for (i = 0; i < inspect_size; i++) {
			if (inspect_mark[i] != -1) {
				inspect_data[i] = inspect_mark[i];
			}
		}
	}

	cob_put_sign (inspect_var, inspect_sign);
}

/*
 * STRING
 */

void
cob_string_init (cob_field *dst, cob_field *ptr)
{
	string_dst_copy = *dst;
	string_dst = &string_dst_copy;
	string_ptr = 0;
	if (ptr) {
		string_ptr_copy = *ptr;
		string_ptr = &string_ptr_copy;
	}
	string_offset = 0;
	cob_exception_code = 0;

	if (string_ptr) {
		string_offset = cob_get_int (string_ptr) - 1;
		if (string_offset < 0 || string_offset >= string_dst->size) {
			COB_SET_EXCEPTION (COB_EC_OVERFLOW_STRING);
		}
	}
}

void
cob_string_delimited (cob_field *dlm)
{
	string_dlm = 0;
	if (dlm) {
		string_dlm_copy = *dlm;
		string_dlm = &string_dlm_copy;
	}
}

void
cob_string_append (cob_field *src)
{
	size_t	src_size = src->size;

	if (cob_exception_code) {
		return;
	}

	if (string_dlm) {
		int	i;
		int	size = (int)(src_size - string_dlm->size + 1);

		for (i = 0; i < size; i++) {
			if (memcmp (src->data + i, string_dlm->data, string_dlm->size) == 0) {
				src_size = i;
				break;
			}
		}
	}

	if (src_size <= string_dst->size - string_offset) {
		own_memcpy (string_dst->data + string_offset, src->data, src_size);
		string_offset += (int) src_size;
	} else {
		int size = (int)(string_dst->size - string_offset);
		own_memcpy (string_dst->data + string_offset, src->data, size);
		string_offset += size;
		COB_SET_EXCEPTION (COB_EC_OVERFLOW_STRING);
	}
}

void
cob_string_finish (void)
{
	if (string_ptr) {
		cob_set_int (string_ptr, string_offset + 1);
	}
}

/*
 * UNSTRING
 */

void
cob_unstring_init (cob_field *src, cob_field *ptr)
{
	unstring_src_copy = *src;
	unstring_src = &unstring_src_copy;
	unstring_ptr = 0;
	if (ptr) {
		unstring_ptr_copy = *ptr;
		unstring_ptr = &unstring_ptr_copy;
	}

	unstring_offset = 0;
	unstring_count = 0;
	unstring_ndlms = 0;
	cob_exception_code = 0;

	if (unstring_ptr) {
		unstring_offset = cob_get_int (unstring_ptr) - 1;
		if (unstring_offset < 0 || unstring_offset >= unstring_src->size) {
			COB_SET_EXCEPTION (COB_EC_OVERFLOW_UNSTRING);
		}
	}
}

void
cob_unstring_delimited (cob_field *dlm, int all)
{
	dlm_list[unstring_ndlms].uns_dlm = dlm;
	dlm_list[unstring_ndlms].uns_all = all;
	unstring_ndlms++;
}

void
cob_unstring_into (cob_field *dst, cob_field *dlm, cob_field *cnt)
{
	int			match_size = 0;
	int			brkpt = 0;
	size_t			dlm_size = 0;
	unsigned char		*dlm_data = NULL;
	unsigned char		*start = unstring_src->data + unstring_offset;

	if (cob_exception_code) {
		return;
	}

	if (unstring_offset >= unstring_src->size) {
		return;
	}

	if (unstring_ndlms == 0) {
		match_size = cob_min_int ((int)COB_FIELD_SIZE (dst),
					  (int)unstring_src->size - unstring_offset);
		cob_memcpy (dst, start, match_size);
		unstring_offset += match_size;
	} else {
		int		i;
		unsigned char	*p;
		unsigned char	*dp;
		unsigned char	*s;
		int		srsize;
		int		dlsize;

		srsize = (int) unstring_src->size;
		s = unstring_src->data + srsize;
		for (p = start; p < s; p++) {
			for (i = 0; i < unstring_ndlms; i++) {
				dlsize = (int) dlm_list[i].uns_dlm->size;
				dp = dlm_list[i].uns_dlm->data;
				if (p + dlsize > s) {
					break;
				}
				if (!memcmp (p, dp, dlsize)) {
					match_size = (int)(p - start);
					cob_memcpy (dst, start, match_size);
					unstring_offset += match_size + dlsize;
					dlm_data = dp;
					dlm_size = dlsize;
					if (dlm_list[i].uns_all) {
						for (p++ ; p < s; p++) {
							if (p + dlsize > s) {
								break;
							}
							if (memcmp (p, dp, dlsize)) {
								break;
							}
							unstring_offset += dlsize;
						}
					}
					brkpt = 1;
					break;
				}
			}
			if (brkpt) {
				break;
			}
		}
		if (!brkpt) {
			/* no match */
			match_size = (int)(unstring_src->size - unstring_offset);
			cob_memcpy (dst, start, match_size);
			unstring_offset = (int) unstring_src->size;
			dlm_data = NULL;
		}
	}
	unstring_count++;

	if (dlm) {
		if (dlm_data) {
			cob_memcpy (dlm, dlm_data, (int) dlm_size);
		} else if (COB_FIELD_IS_NUMERIC (dlm)) {
			cob_move (&cob_zero, dlm);
		} else {
			cob_move (&cob_space, dlm);
		}
	}

	if (cnt) {
		cob_set_int (cnt, match_size);
	}
}

void
cob_unstring_tallying (cob_field *f)
{
	cob_add_int (f, unstring_count);
}

void
cob_unstring_finish (void)
{
	if (unstring_offset < unstring_src->size) {
		COB_SET_EXCEPTION (COB_EC_OVERFLOW_UNSTRING);
	}

	if (unstring_ptr) {
		cob_set_int (unstring_ptr, unstring_offset + 1);
	}
}
