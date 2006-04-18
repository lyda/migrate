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

#ifdef	COB_LOCAL_INLINE

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

/* Binary compare inlines */

static inline int
cob_cmp_u8_binary (const cob_field *f, const int n)
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
cob_cmp_s8_binary (const cob_field *f, const int n)
{
	if (*(signed char *)(f->data) < n) {
		return -1;
	} else if (*(signed char *)(f->data) > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u16_binary (const cob_field *f, const int n)
{
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(short));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s16_binary (const cob_field *f, const int n)
{
	short	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(short));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u32_binary (const cob_field *f, const int n)
{
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(int));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s32_binary (const cob_field *f, const int n)
{
	int	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(int));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_u64_binary (const cob_field *f, const int n)
{
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(long long));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmp_s64_binary (const cob_field *f, const int n)
{
	long long	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(long long));
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_u16_binary (const cob_field *f, const int n)
{
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(short));
	val = COB_BSWAP_16 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_s16_binary (const cob_field *f, const int n)
{
	short	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(short));
	val = COB_BSWAP_16 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_u32_binary (const cob_field *f, const int n)
{
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(int));
	val = COB_BSWAP_32 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_s32_binary (const cob_field *f, const int n)
{
	int	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(int));
	val = COB_BSWAP_32 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_u64_binary (const cob_field *f, const int n)
{
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
	memcpy ((unsigned char *)&val, f->data, sizeof(long long));
	val = COB_BSWAP_64 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

static inline int
cob_cmpswp_s64_binary (const cob_field *f, const int n)
{
	long long	val;

	memcpy ((unsigned char *)&val, f->data, sizeof(long long));
	val = COB_BSWAP_64 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

/* Add/Subtract inlines */

static inline void
cob_add_u8_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

	*p += val;
}

static inline void
cob_add_s8_binary (cob_field *f, const int val)
{
	signed char	*p = (signed char *)f->data;

	*p += val;
}

static inline void
cob_add_u16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned short *)p += val;
#else
	unsigned short	n;
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_add_s16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(short *)p += val;
#else
	short		n;
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_add_u32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned int *)p += val;
#else
	unsigned int	n;
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_add_s32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(int *)p += val;
#else
	int		n;
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_add_u64_binary (cob_field *f, const int val)
{
	unsigned char		*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned long long *)p += val;
#else
	unsigned long long	n;
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

static inline void
cob_add_s64_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(long long *)p += val;
#else
	long long	n;
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

#ifndef WORDS_BIGENDIAN
static inline void
cob_addswp_u16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	unsigned short	n;

	n = COB_BSWAP_16 (*(unsigned short *)p);
	n += val;
	*(unsigned short *)p = COB_BSWAP_16(n);
}

static inline void
cob_addswp_s16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	short		n;

	n = COB_BSWAP_16 (*(short *)p);
	n += val;
	*(short *)p = COB_BSWAP_16(n);
}

static inline void
cob_addswp_u32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	unsigned int	n;

	n = COB_BSWAP_32 (*(unsigned int *)p);
	n += val;
	*(unsigned int *)p = COB_BSWAP_32(n);
}

static inline void
cob_addswp_s32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	int		n;

	n = COB_BSWAP_32 (*(int *)p);
	n += val;
	*(int *)p = COB_BSWAP_32(n);
}

static inline void
cob_addswp_u64_binary (cob_field *f, const int val)
{
	unsigned char		*p = f->data;
	unsigned long long	n;

	n = COB_BSWAP_64 (*(unsigned long long *)p);
	n += val;
	*(unsigned long long *)p = COB_BSWAP_64(n);
}

static inline void
cob_addswp_s64_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	long long	n;

	n = COB_BSWAP_64 (*(long long *)p);
	n += val;
	*(long long *)p = COB_BSWAP_64(n);
}
#endif

static inline void
cob_sub_u8_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

	*p -= val;
}

static inline void
cob_sub_s8_binary (cob_field *f, const int val)
{
	signed char	*p = (signed char *)f->data;

	*p -= val;
}

static inline void
cob_sub_u16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned short *)p -= val;
#else
	unsigned short	n;
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_sub_s16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(short *)p -= val;
#else
	short		n;
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

static inline void
cob_sub_u32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned int *)p -= val;
#else
	unsigned int	n;
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_sub_s32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(int *)p -= val;
#else
	int		n;
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

static inline void
cob_sub_u64_binary (cob_field *f, const int val)
{
	unsigned char		*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(unsigned long long *)p -= val;
#else
	unsigned long long	n;
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

static inline void
cob_sub_s64_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;

#ifndef WORDS_BIGENDIAN
	*(long long *)p -= val;
#else
	long long	n;
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

#ifndef WORDS_BIGENDIAN
static inline void
cob_subswp_u16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	unsigned short	n;

	n = COB_BSWAP_16 (*(unsigned short *)p);
	n -= val;
	*(unsigned short *)p = COB_BSWAP_16(n);
}

static inline void
cob_subswp_s16_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	short		n;

	n = COB_BSWAP_16 (*(short *)p);
	n -= val;
	*(short *)p = COB_BSWAP_16(n);
}

static inline void
cob_subswp_u32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	unsigned int	n;

	n = COB_BSWAP_32 (*(unsigned int *)p);
	n -= val;
	*(unsigned int *)p = COB_BSWAP_32(n);
}

static inline void
cob_subswp_s32_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	int		n;

	n = COB_BSWAP_32 (*(int *)p);
	n -= val;
	*(int *)p = COB_BSWAP_32(n);
}

static inline void
cob_subswp_u64_binary (cob_field *f, const int val)
{
	unsigned char		*p = f->data;
	unsigned long long	n;

	n = COB_BSWAP_64 (*(unsigned long long *)p);
	n -= val;
	*(unsigned long long *)p = COB_BSWAP_64(n);
}

static inline void
cob_subswp_s64_binary (cob_field *f, const int val)
{
	unsigned char	*p = f->data;
	long long	n;

	n = COB_BSWAP_64 (*(long long *)p);
	n -= val;
	*(long long *)p = COB_BSWAP_64(n);
}
#endif

#else	/* COB_LOCAL_INLINE */

extern int cob_get_numdisp (unsigned char *data, int size);
extern int cob_cmp_u8_binary (const cob_field *f, const int n);
extern int cob_cmp_s8_binary (const cob_field *f, const int n);
extern int cob_cmp_u16_binary (const cob_field *f, const int n);
extern int cob_cmp_s16_binary (const cob_field *f, const int n);
extern int cob_cmp_u32_binary (const cob_field *f, const int n);
extern int cob_cmp_s32_binary (const cob_field *f, const int n);
extern int cob_cmp_u64_binary (const cob_field *f, const int n);
extern int cob_cmp_s64_binary (const cob_field *f, const int n);
extern int cob_cmpswp_u16_binary (const cob_field *f, const int n);
extern int cob_cmpswp_s16_binary (const cob_field *f, const int n);
extern int cob_cmpswp_u32_binary (const cob_field *f, const int n);
extern int cob_cmpswp_s32_binary (const cob_field *f, const int n);
extern int cob_cmpswp_u64_binary (const cob_field *f, const int n);
extern int cob_cmpswp_s64_binary (const cob_field *f, const int n);
extern void cob_add_u8_binary (cob_field *f, const int val);
extern void cob_add_s8_binary (cob_field *f, const int val);
extern void cob_add_u16_binary (cob_field *f, const int val);
extern void cob_add_s16_binary (cob_field *f, const int val);
extern void cob_add_u32_binary (cob_field *f, const int val);
extern void cob_add_s32_binary (cob_field *f, const int val);
extern void cob_add_u64_binary (cob_field *f, const int val);
extern void cob_add_s64_binary (cob_field *f, const int val);
extern void cob_addswp_u16_binary (cob_field *f, const int val);
extern void cob_addswp_s16_binary (cob_field *f, const int val);
extern void cob_addswp_u32_binary (cob_field *f, const int val);
extern void cob_addswp_s32_binary (cob_field *f, const int val);
extern void cob_addswp_u64_binary (cob_field *f, const int val);
extern void cob_addswp_s64_binary (cob_field *f, const int val);
extern void cob_sub_u8_binary (cob_field *f, const int val);
extern void cob_sub_s8_binary (cob_field *f, const int val);
extern void cob_sub_u16_binary (cob_field *f, const int val);
extern void cob_sub_s16_binary (cob_field *f, const int val);
extern void cob_sub_u32_binary (cob_field *f, const int val);
extern void cob_sub_s32_binary (cob_field *f, const int val);
extern void cob_sub_u64_binary (cob_field *f, const int val);
extern void cob_sub_s64_binary (cob_field *f, const int val);
extern void cob_subswp_u16_binary (cob_field *f, const int val);
extern void cob_subswp_s16_binary (cob_field *f, const int val);
extern void cob_subswp_u32_binary (cob_field *f, const int val);
extern void cob_subswp_s32_binary (cob_field *f, const int val);
extern void cob_subswp_u64_binary (cob_field *f, const int val);
extern void cob_subswp_s64_binary (cob_field *f, const int val);

#endif	/* COB_LOCAL_INLINE */

#endif	/* COB_CODEGEN_H */
