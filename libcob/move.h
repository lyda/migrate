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

#ifndef COB_MOVE_H
#define COB_MOVE_H

#include <libcob/common.h>

extern void		cob_move (cob_field *src, cob_field *dst);
extern void		cob_set_int (cob_field *f, int n);
extern int		cob_get_int (cob_field *f);

/* memcpy/memset optimizations */

#define own_memcpy(x, y, z)		memcpy(x, y, z)

#if defined (__GNUC__) && defined (__i386__)

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

#define own_memset(s, c, n) \
	(__extension__ (__builtin_constant_p (c)	\
	? (__builtin_constant_p (n)	\
	   ? own_memset_cc (s, 0x01010101UL * (unsigned char) (c), n) \
		: own_memset_cg (s, 0x01010101UL * (unsigned char) (c), n)) \
		: own_memset_gg (s, c, n)))

#else	/* SUPER_OPTIMIZE */

#define own_memset(x, y, z)		own_memset_gg(x, y, z)

#endif	/* SUPER_OPTIMIZE */

#else	/*  __GNUC__ && __i386__ */

#define own_memset(x, y, z)		memset(x, y, z)

#endif	/* __GNUC__ && __i386__ */

#endif /* COB_MOVE_H */
