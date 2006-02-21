/*
 * Copyright (C) 2006 Roger While
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

#ifndef COB_CODEGEN_H
#define COB_CODEGEN_H

#include	<string.h>

static inline int
cob_get_numdisp (unsigned char *data, int size)
{
	int     retval = 0;

	if ( size > 0 ) {
		while ( size-- ) {
			retval *= 10;
			retval += (*data - '0');
			data++;;
		}
	}
	return retval;
}

static inline int
cob_cmp_u8_binary (cob_field *f, const int n)
{
	if (n < 0) {
		return 1;
	}
	if (*(f->data) < n) {
		return -1;
	} else if (*(f->data) > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s8_binary (cob_field *f, const int n)
{
	if (*(char *)(f->data) < n) {
		return -1;
	} else if (*(char *)(f->data) > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u16_binary (cob_field *f, const int n)
{
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char*)&val, f->data, sizeof(short));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_16 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s16_binary (cob_field *f, const int n)
{
	short	val;

	memcpy ((unsigned char*)&val, f->data, sizeof(short));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_16 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u32_binary (cob_field *f, const int n)
{
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char*)&val, f->data, sizeof(int));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_32 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s32_binary (cob_field *f, const int n)
{
	int	val;

	memcpy ((unsigned char*)&val, f->data, sizeof(int));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_32 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u64_binary (cob_field *f, const int n)
{
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char*)&val, f->data, sizeof(long long));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_64 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s64_binary (cob_field *f, const int n)
{
	long long	val;

	memcpy ((unsigned char*)&val, f->data, sizeof(long long));
	if (COB_FIELD_BINARY_SWAP(f)) {
		val = COB_BSWAP_64 (val);
	}
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline void
cob_addsub_u8_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;

	if (!addsub) {
		*(unsigned char *)p += val;
	} else {
		*(unsigned char *)p -= val;
	}
}

static inline void
cob_addsub_s8_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;

	if (!addsub) {
		*(signed char *)p += val;
	} else {
		*(signed char *)p -= val;
	}
}

static inline void
cob_addsub_u16_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;
	unsigned short	n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_16 (*(unsigned short *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(unsigned short *)p = COB_BSWAP_16(n);
	} else {
		*(unsigned short *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(short));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_addsub_s16_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;
	short		n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_16 (*(short *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(short *)p = COB_BSWAP_16(n);
	} else {
		*(short *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(short));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_addsub_u32_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;
	unsigned int	n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_32 (*(unsigned int *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(unsigned int *)p = COB_BSWAP_32(n);
	} else {
		*(unsigned int *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(int));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_addsub_s32_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;
	int		n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_32 (*(int *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(int *)p = COB_BSWAP_32(n);
	} else {
		*(int *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(int));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_addsub_u64_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char		*p = f->data;
	unsigned long long	n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (*(unsigned long long *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(unsigned long long *)p = COB_BSWAP_64(n);
	} else {
		*(unsigned long long *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

static inline void
cob_addsub_s64_binary (cob_field *f, const int addsub, const int val)
{
	unsigned char	*p = f->data;
	long long	n;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (*(long long *)p);
		if (!addsub) {
			n += val;
		} else {
			n -= val;
		}
		*(long long *)p = COB_BSWAP_64(n);
	} else {
		*(long long *)p += val;
	}
#else
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	if (!addsub) {
		n += val;
	} else {
		n -= val;
	}
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

#endif	/* COB_CODEGEN_H */
