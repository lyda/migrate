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

#ifdef	COB_LOCAL_INLINE

static int
cob_get_numdisp (unsigned char *data, int size)
{
	int     retval = 0;

	while (size--) {
		retval *= 10;
		retval += (*data - (unsigned char)'0');
		data++;
	}
	return retval;
}

/* Binary compare inlines */

static int
cob_cmp_u8_binary (const unsigned char *p, const int n)
{
	if (*p < n) {
		return -1;
	} else if (*p > n) {
		return 1;
	}
	return 0;
}

static int
cob_cmp_s8_binary (const unsigned char *p, const int n)
{
	if (*(signed char *)(p) < n) {
		return -1;
	} else if (*(signed char *)(p) > n) {
		return 1;
	}
	return 0;
}

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

static int
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

/* Add/Subtract inlines */

static void
cob_add_u8_binary (unsigned char *p, const int val)
{
	*p += val;
}

static void
cob_add_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p += val;
}

static void
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

static void
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

static void
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

static void
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

static void
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

static void
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
static void
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

static void
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

static void
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

static void
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

static void
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

static void
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
#endif

static void
cob_sub_u8_binary (unsigned char *p, const int val)
{
	*p -= val;
}

static void
cob_sub_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p -= val;
}

static void
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

static void
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

static void
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

static void
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

static void
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

static void
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
static void
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

static void
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

static void
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

static void
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

static void
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

static void
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
#endif

#else	/* COB_LOCAL_INLINE */

extern int cob_get_numdisp (unsigned char *data, int size);
extern int cob_cmp_u8_binary (const unsigned char *p, const int n);
extern int cob_cmp_s8_binary (const unsigned char *p, const int n);
extern int cob_cmp_u16_binary (const unsigned char *p, const int n);
extern int cob_cmp_s16_binary (const unsigned char *p, const int n);
extern int cob_cmp_u32_binary (const unsigned char *p, const int n);
extern int cob_cmp_s32_binary (const unsigned char *p, const int n);
extern int cob_cmp_u64_binary (const unsigned char *p, const int n);
extern int cob_cmp_s64_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u16_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s16_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u32_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s32_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_u64_binary (const unsigned char *p, const int n);
extern int cob_cmpswp_s64_binary (const unsigned char *p, const int n);
extern void cob_add_u8_binary (unsigned char *p, const int val);
extern void cob_add_s8_binary (unsigned char *p, const int val);
extern void cob_add_u16_binary (unsigned char *p, const int val);
extern void cob_add_s16_binary (unsigned char *p, const int val);
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

#endif	/* COB_LOCAL_INLINE */

#endif	/* COB_CODEGEN_H */
