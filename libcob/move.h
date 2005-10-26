/*
 * Copyright (C) 2002-2005 Keisuke Nishida
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

#ifndef COB_MOVE_H
#define COB_MOVE_H

#include <libcob/common.h>

extern void		cob_move (cob_field *src, cob_field *dst);
extern void		cob_memcpy (cob_field *dst, unsigned char *src, int size);
extern void		cob_set_int (cob_field *f, int n);
extern int		cob_get_int (cob_field *f);
extern int		cob_binary_get_int (cob_field *f);
extern long long	cob_binary_get_int64 (cob_field *f);
extern void		cob_binary_set_int (cob_field *f, int n);
extern void		cob_binary_set_int64 (cob_field *f, long long n);

/* memcpy/memset optimizations */

static inline void
own_memset_generic (unsigned char *x, const int y, size_t count)
{
	while (count--) {
		*x++ = y;
	}
}

static inline void
own_memcpy_generic (unsigned char *x, const unsigned char *y, size_t count)
{
	while (count--) {
		*x++ = *y++;
	}
	return;
}

#if defined (__GNUC__) && defined (__i386__)
static inline void
own_byte_memcpy (unsigned char *x, const unsigned char *y, size_t count)
{
	int d0, d1, d2;
	__asm__ __volatile__ (
	"cld\n\t"
	"rep; movsb"
	:"=&c" (d0), "=&D" (d1), "=&S" (d2)
	:"0" (count), "1" (x), "2" (y)
	:"memory");
}

static inline void
own_word_memcpy(void * to, const void * from, size_t n)
{
int d0, d1, d2;
__asm__ __volatile__(
	"cld\n\t"
	"rep ; movsl\n\t"
	"movl %4,%%ecx\n\t"
	"andl $3,%%ecx\n\t"
	"rep ; movsb\n\t"
	: "=&c" (d0), "=&D" (d1), "=&S" (d2)
	: "0" (n/4), "g" (n), "1" ((long) to), "2" ((long) from)
	: "memory");
}

static inline void 
own_memset_gg (void *s, int c, size_t n)
{
	register unsigned long int d0, d1;

	asm volatile
	("cld\n\t"
	"rep; stosb"
	: "=&D" (d0), "=&c" (d1)
	: "a" (c), "0" (s), "1" (n)
	: "memory");
}

#ifdef	SUPER_OPTIMIZE

static inline void
own_constant_memcpy(void * to, const void * from, size_t n)
{
	long esi, edi;
	if (!n)
		return;
	esi = (long) from;
	edi = (long) to;
	if (n >= 5*4) {
		/* large block: use rep prefix */
		int ecx;
		__asm__ __volatile__(
			"cld\n\t"
			"rep ; movsl"
			: "=&c" (ecx), "=&D" (edi), "=&S" (esi)
			: "0" (n/4), "1" (edi),"2" (esi)
			: "memory");
	} else {
		/* small block: don't clobber ecx + smaller code */
		if (n >= 4*4)
			__asm__ __volatile__("movsl"
			:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		if (n >= 3*4)
			__asm__ __volatile__("movsl"
			:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		if (n >= 2*4)
			__asm__ __volatile__("movsl"
			:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		if (n >= 1*4)
			__asm__ __volatile__("movsl"
			:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
	}
	switch (n % 4) {
	case 0:
		return;
	case 1:
		__asm__ __volatile__("movsb"
		:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		return;
	case 2:
		__asm__ __volatile__("movsw"
		:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		return;
	default:
		__asm__ __volatile__("movsw\n\tmovsb"
		:"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
		return;
	}
}

static inline void 
own_memset_cc (void *s, unsigned long int pattern, size_t n)
{
	register unsigned long int d0, d1;
	union {
		unsigned int ui;
		unsigned short int usi;
		unsigned char uc;
	} *u = s;

	switch (n) {
	case 0:
		return;
	case 1:
		u->uc = pattern;
		return;
	case 2:
		u->usi = pattern;
		return;
	case 3:
		u->usi = pattern;
		u = __extension__ ((void *) u + 2);
		u->uc = pattern;
		return;
	case 4:
		u->ui = pattern;
		return;
	}
#define COMMON_CODE(x) \
	asm volatile					\
	("cld\n\t"					\
	"rep; stosl"					\
	x						\
	: "=&c" (d0), "=&D" (d1)			\
	: "a" (pattern), "0" (n	/ 4), "1" (&u->uc)	\
	: "memory")

	switch (n % 4) {
	case 0:
		COMMON_CODE ("");
		return;
	case 1:
		COMMON_CODE ("\n\tstosb");
		return;
	case 2:
		COMMON_CODE ("\n\tstosw");
		return;
	case 3:
		COMMON_CODE ("\n\tstosw\n\tstosb");
		return;
	}
#undef COMMON_CODE
}


static inline void 
own_memset_cg (void *s, unsigned long c, size_t n)
{
	register unsigned long int d0, d1;

	asm volatile
	("cld\n\t"
	"rep; stosl\n\t"
	"testb	$2,%b3\n\t"
	"je	1f\n\t"
	"stosw\n"
	"1:\n\t"
	"testb	$1,%b3\n\t"
	"je	2f\n\t"
	"stosb\n"
	"2:"
	: "=&c" (d0), "=&D" (d1)
	: "a" (c), "q" (n), "0" (n / 4), "1" (s)
	: "memory");
}


#define own_memcpy(t, f, n) \
	(__builtin_constant_p(n) ? \
	own_constant_memcpy((t),(f),(n)) : \
	own_word_memcpy((t),(f),(n)))

#define own_memset(s, c, n) \
	(__extension__ (__builtin_constant_p (c)	\
	? (__builtin_constant_p (n)	\
	   ? own_memset_cc (s, 0x01010101UL * (unsigned char) (c), n) \
		: own_memset_cg (s, 0x01010101UL * (unsigned char) (c), n)) \
		: own_memset_gg (s, c, n)))

#else	/* SUPER_OPTIMIZE */

#define own_memcpy(x, y, z)		own_word_memcpy(x, y, z)
#define own_memset(x, y, z)		own_memset_gg(x, y, z)

#endif	/* SUPER_OPTIMIZE */

#else	/*  __i386__ */

#define own_byte_memcpy(x, y, z)	own_memcpy_generic(x, y, z)
#define own_memcpy(x, y, z)		own_memcpy_generic(x, y, z)
#define own_memset(x, y, z)		own_memset_generic(x, y, z)

#endif	/* __i386__ */

#endif /* COB_MOVE_H_ */
