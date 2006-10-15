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
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_CODEGEN_H
#define COB_CODEGEN_H

#include	<string.h>

#ifdef	COB_LOCAL_INLINE
#define	COB_STATIC	static
#else
#define	COB_STATIC
extern int cob_get_numdisp (const unsigned char *data, const size_t size);
extern int cob_cmp_u8_binary (const unsigned char *p, const int n);
extern int cob_cmp_s8_binary (const unsigned char *p, const int n);
extern int cob_cmp_u16_binary (const unsigned char *p, const int n);
extern int cob_cmp_s16_binary (const unsigned char *p, const int n);
extern int cob_cmp_u24_binary (const unsigned char *p, const int n);
extern int cob_cmp_s24_binary (const unsigned char *p, const int n);
extern int cob_cmp_u32_binary (const unsigned char *p, const int n);
extern int cob_cmp_s32_binary (const unsigned char *p, const int n);
extern int cob_cmp_u40_binary (const unsigned char *p, const int n);
extern int cob_cmp_s40_binary (const unsigned char *p, const int n);
extern int cob_cmp_u48_binary (const unsigned char *p, const int n);
extern int cob_cmp_s48_binary (const unsigned char *p, const int n);
extern int cob_cmp_u56_binary (const unsigned char *p, const int n);
extern int cob_cmp_s56_binary (const unsigned char *p, const int n);
extern int cob_cmp_u64_binary (const unsigned char *p, const int n);
extern int cob_cmp_s64_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u16_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s16_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u24_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s24_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u32_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s32_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u40_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s40_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u48_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s48_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u56_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s56_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u64_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s64_binary (const unsigned char *p, const int n);
extern void cob_add_u8_binary (unsigned char *p, const int val);
extern void cob_add_s8_binary (unsigned char *p, const int val);
extern void cob_add_u16_binary (unsigned char *p, const int val);
extern void cob_add_s16_binary (unsigned char *p, const int val);
extern void cob_add_u24_binary (unsigned char *p, const int val);
extern void cob_add_s24_binary (unsigned char *p, const int val);
extern void cob_add_u32_binary (unsigned char *p, const int val);
extern void cob_add_s32_binary (unsigned char *p, const int val);
extern void cob_add_u64_binary (unsigned char *p, const int val);
extern void cob_add_s64_binary (unsigned char *p, const int val);
extern void cob_addswp_u16_binary (unsigned char *p, const int val);
extern void cob_addswp_s16_binary (unsigned char *p, const int val);
extern void cob_addswp_u32_binary (unsigned char *p, const int val);
extern void cob_addswp_s32_binary (unsigned char *p, const int val);
extern void cob_addswp_u64_binary (unsigned char *p, const int val);
extern void cob_addswp_s64_binary (unsigned char *p, const int val);
extern void cob_sub_u8_binary (unsigned char *p, const int val);
extern void cob_sub_s8_binary (unsigned char *p, const int val);
extern void cob_sub_u16_binary (unsigned char *p, const int val);
extern void cob_sub_s16_binary (unsigned char *p, const int val);
extern void cob_sub_u32_binary (unsigned char *p, const int val);
extern void cob_sub_s32_binary (unsigned char *p, const int val);
extern void cob_sub_u64_binary (unsigned char *p, const int val);
extern void cob_sub_s64_binary (unsigned char *p, const int val);
extern void cob_subswp_u16_binary (unsigned char *p, const int val);
extern void cob_subswp_s16_binary (unsigned char *p, const int val);
extern void cob_subswp_u32_binary (unsigned char *p, const int val);
extern void cob_subswp_s32_binary (unsigned char *p, const int val);
extern void cob_subswp_u64_binary (unsigned char *p, const int val);
extern void cob_subswp_s64_binary (unsigned char *p, const int val);
#endif

#if	defined(COB_LOCAL_INLINE) || defined(COB_LIB_INCLUDE)

#if !defined(__i386__) && !defined(__x86_64__) && !defined(__powerpc__) && !defined(__powerpc64__) && !defined(__ppc__) && !defined(__amd64__)
	#if defined(_MSC_VER)
		#define ALLOW_MISALIGNED
		#define MISALIGNED __unaligned
	#else
		#define MISALIGNED
	#endif
#else
	#define ALLOW_MISALIGNED
	#define MISALIGNED
#endif

COB_STATIC int
cob_get_numdisp (const unsigned char *data, const size_t size)
{
	int     retval = 0;
	size_t	n;

	for (n = 0; n < size; n++, data++) {
		retval *= 10;
		retval += (*data - (unsigned char)'0');
	}
	return retval;
}

/* Binary compare */

COB_STATIC int
cob_cmp_u8_binary (const unsigned char *p, const int n)
{
	if (*p < n) {
		return -1;
	} else if (*p > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s8_binary (const unsigned char *p, const int n)
{
	if (*(signed char *)(p) < n) {
		return -1;
	} else if (*(signed char *)(p) > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u16_binary (const unsigned char *p, const int n)
{
	unsigned short	val;

#ifdef ALLOW_MISALIGNED
	val = *(unsigned short MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(short));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s16_binary (const unsigned char *p, const int n)
{
	short	val;

#ifdef ALLOW_MISALIGNED
	val = *(short MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(short));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u24_binary (const unsigned char *p, const int n)
{
	union {
		unsigned int	val;
		unsigned char	c[4];
	} u;

	u.val = 0;
#ifdef	WORDS_BIGENDIAN
	u.c[1] = *p;
	u.c[2] = *(p + 1);
	u.c[3] = *(p + 2);
#else
	u.c[0] = *p;
	u.c[1] = *(p + 1);
	u.c[2] = *(p + 2);
#endif
	if (u.val < n) {
		return -1;
	} else if (u.val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s24_binary (const unsigned char *p, const int n)
{
	union {
		int		val;
		unsigned char	c[4];
	} u;

	u.val = 0;
	int	val = 0;

#ifdef	WORDS_BIGENDIAN
	u.c[0] = *p;
	u.c[1] = *(p + 1);
	u.c[2] = *(p + 2);
#else
	u.c[1] = *p;
	u.c[2] = *(p + 1);
	u.c[3] = *(p + 2);
#endif
	val >>= 8;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u32_binary (const unsigned char *p, const int n)
{
	unsigned int	val;

#ifdef ALLOW_MISALIGNED
	val = *(unsigned int MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(int));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s32_binary (const unsigned char *p, const int n)
{
	int	val;

#ifdef ALLOW_MISALIGNED
	val = *(int MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(int));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u40_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 3;
/*
	memcpy (&u.c[3], p, 5);
*/
#else
	x = (unsigned char *)&val;
/*
	memcpy (u.c, p, 5);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s40_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
/*
	memcpy ((unsigned char *)&val, p, 5);
*/
#else
	x = ((unsigned char *)&val) + 3;
/*
	memcpy (((unsigned char *)&val) + 3, p, 5);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	val >>= 24;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u48_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 2;
/*
	memcpy (&u.c[2], p, 6);
*/
#else
	x = (unsigned char *)&val;
/*
	memcpy (u.c, p, 6);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s48_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
/*
	memcpy ((unsigned char *)&val, p, 6);
*/
#else
	x = ((unsigned char *)&val) + 2;
/*
	memcpy (((unsigned char *)&val) + 2, p, 6);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	val >>= 16;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u56_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 1;
/*
	memcpy (&u.c[1], p, 7);
*/
#else
	x = (unsigned char *)&val;
/*
	memcpy (u.c, p, 7);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s56_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
/*
	memcpy ((unsigned char *)&val, p, 7);
*/
#else
	x = ((unsigned char *)&val) + 1;
/*
	memcpy (((unsigned char *)&val) + 1, p, 7);
*/
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	val >>= 8;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_u64_binary (const unsigned char *p, const int n)
{
	unsigned long long	val;

#ifdef ALLOW_MISALIGNED
	val = *(unsigned long long MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(long long));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmp_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

#ifdef ALLOW_MISALIGNED
	val = *(long long MISALIGNED *)p;
#else
	memcpy ((unsigned char *)&val, p, sizeof(long long));
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u16_binary (const unsigned char *p, const int n)
{
	unsigned short	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(short));
	val = COB_BSWAP_16 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s16_binary (const unsigned char *p, const int n)
{
	short	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_16 (*(short MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(short));
	val = COB_BSWAP_16 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u24_binary (const unsigned char *p, const int n)
{
	unsigned int	val = 0;
	unsigned char	*x;

	x = ((unsigned char *)&val) + 1;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
/*
	memcpy (((unsigned char *)&val) + 1, p, 3);
*/
	val = COB_BSWAP_32 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s24_binary (const unsigned char *p, const int n)
{
	int		val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
/*
	memcpy ((unsigned char *)&val, p, 3);
*/
	val = COB_BSWAP_32 (val);
	val >>= 8;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u32_binary (const unsigned char *p, const int n)
{
	unsigned int	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(int));
	val = COB_BSWAP_32 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s32_binary (const unsigned char *p, const int n)
{
	int	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_32 (*(int MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(int));
	val = COB_BSWAP_32 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u40_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	x = ((unsigned char *)&val) + 3;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
/*
	memcpy (((unsigned char *)&val) + 3, p, 5);
*/
	val = COB_BSWAP_64 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s40_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
/*
	memcpy ((unsigned char *)&val, p, 5);
*/
	val = COB_BSWAP_64 (val);
	val >>= 24;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u48_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	x = ((unsigned char *)&val) + 2;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
/*
	memcpy (((unsigned char *)&val) + 2, p, 6);
*/
	val = COB_BSWAP_64 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s48_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
/*
	memcpy ((unsigned char *)&val, p, 6);
*/
	val = COB_BSWAP_64 (val);
	val >>= 16;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u56_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	x = ((unsigned char *)&val) + 1;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
/*
	memcpy (((unsigned char *)&val) + 1, p, 7);
*/
	val = COB_BSWAP_64 (val);
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s56_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
/*
	memcpy ((unsigned char *)&val, p, 7);
*/
	val = COB_BSWAP_64 (val);
	val >>= 8;	/* shift with sign */
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_u64_binary (const unsigned char *p, const int n)
{
	unsigned long long	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(long long));
	val = COB_BSWAP_64 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

COB_STATIC int
cob_cmpswp_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_64 (*(long long MISALIGNED *)p);
#else
	memcpy ((unsigned char *)&val, p, sizeof(long long));
	val = COB_BSWAP_64 (val);
#endif
	if (val < n) {
		return -1;
	} else if (val > n) {
		return 1;
	}
	return 0;
}

/* Add/Subtract */

COB_STATIC void
cob_add_u8_binary (unsigned char *p, const int val)
{
	*p += val;
}

COB_STATIC void
cob_add_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p += val;
}

COB_STATIC void
cob_add_u16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned short MISALIGNED *)p += val;
#else
	unsigned short	n;
	if ((((int)p) & 1) == 0) {
		*(unsigned short *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

COB_STATIC void
cob_add_s16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(short MISALIGNED *)p += val;
#else
	short		n;
	if ((((int)p) & 1) == 0) {
		*(short *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

COB_STATIC void
cob_add_u24_binary (unsigned char *p, const int val)
{
	unsigned int	n = 0;
	unsigned char	*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_add_s24_binary (unsigned char *p, const int val)
{
	int	n = 0;
	unsigned char	*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n >>= 8;	/* shift with sign */
	n += val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_add_u32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned int MISALIGNED *)p += val;
#else
	unsigned int	n;
	if ((((int)p) & 3) == 0) {
		*(unsigned int *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

COB_STATIC void
cob_add_s32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(int MISALIGNED *)p += val;
#else
	int		n;
	if ((((int)p) & 3) == 0) {
		*(int *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

COB_STATIC void
cob_add_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned long long MISALIGNED *)p += val;
#else
	unsigned long long	n;
	if ((((int)p) & 7) == 0) {
		*(unsigned long long *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

COB_STATIC void
cob_add_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(long long MISALIGNED *)p += val;
#else
	long long	n;
	if ((((int)p) & 7) == 0) {
		*(long long *)p += val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n += val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

#ifndef WORDS_BIGENDIAN
COB_STATIC void
cob_addswp_u16_binary (unsigned char *p, const int val)
{
	unsigned short	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
	n += val;
	*(unsigned short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (unsigned short)((p[0] << 8) | p[1]);
	n += val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_s16_binary (unsigned char *p, const int val)
{
	short		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(short MISALIGNED *)p);
	n += val;
	*(short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (short)((p[0] << 8) | p[1]);
	n += val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_u32_binary (unsigned char *p, const int val)
{
	unsigned int	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
	n += val;
	*(unsigned int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
	n += val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_s32_binary (unsigned char *p, const int val)
{
	int		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(int MISALIGNED *)p);
	n += val;
	*(int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n += val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_u64_binary (unsigned char *p, const int val)
{
	union {
		unsigned long long	n;
		unsigned char		c[8];
	} u;

#ifdef ALLOW_MISALIGNED
	u.n = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
	u.n += val;
	*(unsigned long long MISALIGNED *)p = COB_BSWAP_64(u.n);
#else
	int i;
	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n += val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_addswp_s64_binary (unsigned char *p, const int val)
{
	union {
		long long	n;
		unsigned char	c[8];
	} u;

#ifdef ALLOW_MISALIGNED
	u.n = COB_BSWAP_64 (*(long long MISALIGNED *)p);
	u.n += val;
	*(long long MISALIGNED *)p = COB_BSWAP_64(u.n);
#else
	int i;
	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n += val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}
#endif	/* WORDS_BIGENDIAN */

COB_STATIC void
cob_sub_u8_binary (unsigned char *p, const int val)
{
	*p -= val;
}

COB_STATIC void
cob_sub_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p -= val;
}

COB_STATIC void
cob_sub_u16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned short MISALIGNED *)p -= val;
#else
	unsigned short	n;
	if ((((int)p) & 1) == 0) {
		*(unsigned short *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

COB_STATIC void
cob_sub_s16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(short MISALIGNED *)p -= val;
#else
	short		n;
	if ((((int)p) & 1) == 0) {
		*(short *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(short));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(short));
#endif
}

COB_STATIC void
cob_sub_u32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned int MISALIGNED *)p -= val;
#else
	unsigned int	n;
	if ((((int)p) & 3) == 0) {
		*(unsigned int *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

COB_STATIC void
cob_sub_s32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(int MISALIGNED *)p -= val;
#else
	int		n;
	if ((((int)p) & 3) == 0) {
		*(int *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(int));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(int));
#endif
}

COB_STATIC void
cob_sub_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned long long MISALIGNED *)p -= val;
#else
	unsigned long long	n;
	if ((((int)p) & 7) == 0) {
		*(unsigned long long *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

COB_STATIC void
cob_sub_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(long long MISALIGNED *)p -= val;
#else
	long long	n;
	if ((((int)p) & 7) == 0) {
		*(long long *)p -= val;
		return;
	}
	memcpy ((unsigned char *)&n, p, sizeof(long long));
	n -= val;
	memcpy (p, (unsigned char *)&n, sizeof(long long));
#endif
}

#ifndef WORDS_BIGENDIAN
COB_STATIC void
cob_subswp_u16_binary (unsigned char *p, const int val)
{
	unsigned short	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
	n -= val;
	*(unsigned short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (unsigned short)((p[0] << 8) | p[1]);
	n -= val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_s16_binary (unsigned char *p, const int val)
{
	short		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(short MISALIGNED *)p);
	n -= val;
	*(short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (short)((p[0] << 8) | p[1]);
	n -= val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_u32_binary (unsigned char *p, const int val)
{
	unsigned int	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
	n -= val;
	*(unsigned int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (unsigned int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n -= val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_s32_binary (unsigned char *p, const int val)
{
	int		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(int MISALIGNED *)p);
	n -= val;
	*(int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n -= val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_u64_binary (unsigned char *p, const int val)
{
	union {
		unsigned long long	n;
		unsigned char		c[8];
	} u;

#ifdef ALLOW_MISALIGNED
	u.n = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
	u.n -= val;
	*(unsigned long long MISALIGNED *)p = COB_BSWAP_64(u.n);
#else
	int i;
	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n -= val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_subswp_s64_binary (unsigned char *p, const int val)
{
	union {
		long long	n;
		unsigned char	c[8];
	} u;

#ifdef ALLOW_MISALIGNED
	u.n = COB_BSWAP_64 (*(long long MISALIGNED *)p);
	u.n -= val;
	*(long long MISALIGNED *)p = COB_BSWAP_64(u.n);
#else
	int i;
	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n -= val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}
#endif	/* WORDS_BIGENDIAN */

#endif	/* COB_LOCAL_INLINE || COB_LIB_INCLUDE */

#endif	/* COB_CODEGEN_H */
