/*
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Extracted from glib/gtypes.h in GLIB-2.2.2.
 * Modified by Keisuke Nishida in 2003.
 */

#ifndef COB_BYTESWAP_H
#define COB_BYTESWAP_H

#include <sys/types.h>

#ifndef	__BIT_TYPES_DEFINED__
typedef signed char int8_t;
typedef unsigned char u_int8_t;
typedef short int16_t;
typedef unsigned short u_int16_t;
typedef int int32_t;
typedef unsigned int u_int32_t;
typedef long long int64_t;
typedef unsigned long long u_int64_t;
typedef int32_t register_t;
#endif

/* Basic bit swapping functions
 */
#define COB_BSWAP_16_CONSTANT(val)	((u_int16_t) (	\
    (u_int16_t) ((u_int16_t) (val) >> 8) |		\
    (u_int16_t) ((u_int16_t) (val) << 8)))
#define COB_BSWAP_32_CONSTANT(val)	((u_int32_t) (		\
    (((u_int32_t) (val) & (u_int32_t) 0x000000ffU) << 24) |	\
    (((u_int32_t) (val) & (u_int32_t) 0x0000ff00U) <<  8) |	\
    (((u_int32_t) (val) & (u_int32_t) 0x00ff0000U) >>  8) |	\
    (((u_int32_t) (val) & (u_int32_t) 0xff000000U) >> 24)))
#define COB_BSWAP_64_CONSTANT(val)	((u_int64_t) (	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x00000000000000ffULL) << 56) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x000000000000ff00ULL) << 40) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x0000000000ff0000ULL) << 24) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x00000000ff000000ULL) <<  8) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x000000ff00000000ULL) >>  8) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x0000ff0000000000ULL) >> 24) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0x00ff000000000000ULL) >> 40) |	\
    (((u_int64_t) (val) &				\
      (u_int64_t) 0xff00000000000000ULL) >> 56)))

/* Arch specific stuff for speed
 */
#if defined (__GNUC__) && (__GNUC__ >= 2)
#  if defined (__i386__)
#    define COB_BSWAP_16_IA32(val)				\
       (__extension__						\
	({ register u_int16_t __v, __x = ((u_int16_t) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_16_CONSTANT (__x);			\
	   else							\
	     __asm__ ("rorw $8, %w0"				\
		      : "=r" (__v)				\
		      : "0" (__x)				\
		      : "cc");					\
	    __v; }))
#    define COB_BSWAP_32_IA32(val)				\
       (__extension__						\
	({ register u_int32_t __v, __x = ((u_int32_t) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_32_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswap %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	    __v; }))
#    define COB_BSWAP_64_IA32(val)				\
       (__extension__						\
	({ union { u_int64_t __ll;				\
		   u_int32_t __l[2]; } __w, __r;		\
	   __w.__ll = ((u_int64_t) (val));			\
	   if (__builtin_constant_p (__w.__ll))			\
	     __r.__ll = COB_BSWAP_64_CONSTANT (__w.__ll);	\
	   else							\
	     {							\
	       __r.__l[0] = COB_BSWAP_32 (__w.__l[1]);		\
	       __r.__l[1] = COB_BSWAP_32 (__w.__l[0]);		\
	     }							\
	   __r.__ll; }))
     /* Possibly just use the constant version and let gcc figure it out? */
#    define COB_BSWAP_16(val) (COB_BSWAP_16_IA32 (val))
#    define COB_BSWAP_32(val) (COB_BSWAP_32_IA32 (val))
#    define COB_BSWAP_64(val) (COB_BSWAP_64_IA32 (val))
#  elif defined (__ia64__)
#    define COB_BSWAP_16_IA64(val)				\
       (__extension__						\
	({ register u_int16_t __v, __x = ((u_int16_t) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_16_CONSTANT (__x);			\
	   else							\
	     __asm__ __volatile__ ("shl %0 = %1, 48 ;;"		\
				   "mux1 %0 = %0, @rev ;;"	\
				    : "=r" (__v)		\
				    : "r" (__x));		\
	    __v; }))
#    define COB_BSWAP_32_IA64(val)				\
       (__extension__						\
	 ({ register u_int32_t __v, __x = ((u_int32_t) (val));	\
	    if (__builtin_constant_p (__x))			\
	      __v = COB_BSWAP_32_CONSTANT (__x);		\
	    else						\
	     __asm__ __volatile__ ("shl %0 = %1, 32 ;;"		\
				   "mux1 %0 = %0, @rev ;;"	\
				    : "=r" (__v)		\
				    : "r" (__x));		\
	    __v; }))
#    define COB_BSWAP_64_IA64(val)				\
       (__extension__						\
	({ register u_int64_t __v,				\
	     __x = ((u_int64_t) (val));				\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ __volatile__ ("mux1 %0 = %1, @rev ;;"	\
				   : "=r" (__v)			\
				   : "r" (__x));		\
	   __v; }))
#    define COB_BSWAP_16(val) (COB_BSWAP_16_IA64 (val))
#    define COB_BSWAP_32(val) (COB_BSWAP_32_IA64 (val))
#    define COB_BSWAP_64(val) (COB_BSWAP_64_IA64 (val))
#  elif defined (__x86_64__)
#    define COB_BSWAP_32_X86_64(val)				\
       (__extension__						\
	 ({ register u_int32_t __v, __x = ((u_int32_t) (val));	\
	    if (__builtin_constant_p (__x))			\
	      __v = COB_BSWAP_32_CONSTANT (__x);		\
	    else						\
	     __asm__ ("bswapl %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	    __v; }))
#    define COB_BSWAP_64_X86_64(val)				\
       (__extension__						\
	({ register u_int64_t __v, __x = ((u_int64_t) (val));	\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswapq %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	   __v; }))
     /* gcc seems to figure out optimal code for this on its own */
#    define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#    define COB_BSWAP_32(val) (COB_BSWAP_32_X86_64 (val))
#    define COB_BSWAP_64(val) (COB_BSWAP_64_X86_64 (val))
#  else /* generic gcc */
#    define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#    define COB_BSWAP_32(val) (COB_BSWAP_32_CONSTANT (val))
#    define COB_BSWAP_64(val) (COB_BSWAP_64_CONSTANT (val))
#  endif
#else /* generic */
#  define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#  define COB_BSWAP_32(val) (COB_BSWAP_32_CONSTANT (val))
#  define COB_BSWAP_64(val) (COB_BSWAP_64_CONSTANT (val))
#endif /* generic */

#endif /* COB_BYTESWAP_H */
