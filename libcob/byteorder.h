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

#ifndef COB_BYTEORDER_H
#define COB_BYTEORDER_H

/* Basic bit swapping functions
 */
#define COB_UINT16_SWAP_LE_BE_CONSTANT(val)	((unsigned short) (	\
    (unsigned short) ((unsigned short) (val) >> 8) |			\
    (unsigned short) ((unsigned short) (val) << 8)))
#define COB_UINT32_SWAP_LE_BE_CONSTANT(val)	((unsigned long) (	\
    (((unsigned long) (val) & (unsigned long) 0x000000ffU) << 24) |	\
    (((unsigned long) (val) & (unsigned long) 0x0000ff00U) <<  8) |	\
    (((unsigned long) (val) & (unsigned long) 0x00ff0000U) >>  8) |	\
    (((unsigned long) (val) & (unsigned long) 0xff000000U) >> 24)))
#define COB_UINT64_SWAP_LE_BE_CONSTANT(val)	((unsigned long long) ( \
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x00000000000000ffU) << 56) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x000000000000ff00U) << 40) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x0000000000ff0000U) << 24) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x00000000ff000000U) <<  8) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x000000ff00000000U) >>  8) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x0000ff0000000000U) >> 24) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0x00ff000000000000U) >> 40) |		\
    (((unsigned long long) (val) &					\
      (unsigned long long) 0xff00000000000000U) >> 56)))

/* Arch specific stuff for speed
 */
#if defined (__GNUC__) && (__GNUC__ >= 2) && defined (__OPTIMIZE__)
#  if defined (__i386__)
#    define COB_UINT16_SWAP_LE_BE_IA32(val)				\
       (__extension__							\
	({ register unsigned short __v, __x = ((unsigned short) (val));	\
	   if (__builtin_constant_p (__x))				\
	     __v = COB_UINT16_SWAP_LE_BE_CONSTANT (__x);		\
	   else								\
	     __asm__ ("rorw $8, %w0"					\
		      : "=r" (__v)					\
		      : "0" (__x)					\
		      : "cc");						\
	    __v; }))
#    if !defined (__i486__) && !defined (__i586__) \
	&& !defined (__pentium__) && !defined (__i686__) \
	&& !defined (__pentiumpro__)
#       define COB_UINT32_SWAP_LE_BE_IA32(val)				 \
	  (__extension__						 \
	   ({ register unsigned long __v, __x = ((unsigned long) (val)); \
	      if (__builtin_constant_p (__x))				 \
		__v = COB_UINT32_SWAP_LE_BE_CONSTANT (__x);		 \
	      else							 \
		__asm__ ("rorw $8, %w0\n\t"				 \
			 "rorl $16, %0\n\t"				 \
			 "rorw $8, %w0"					 \
			 : "=r" (__v)					 \
			 : "0" (__x)					 \
			 : "cc");					 \
	      __v; }))
#    else /* 486 and higher has bswap */
#       define COB_UINT32_SWAP_LE_BE_IA32(val)				 \
	  (__extension__						 \
	   ({ register unsigned long __v, __x = ((unsigned long) (val)); \
	      if (__builtin_constant_p (__x))				 \
		__v = COB_UINT32_SWAP_LE_BE_CONSTANT (__x);		 \
	      else							 \
		__asm__ ("bswap %0"					 \
			 : "=r" (__v)					 \
			 : "0" (__x));					 \
	      __v; }))
#    endif /* processor specific 32-bit stuff */
#    define COB_UINT64_SWAP_LE_BE_IA32(val)				\
       (__extension__							\
	({ union { unsigned long long __ll;				\
		   unsigned long __l[2]; } __w, __r;			\
	   __w.__ll = ((unsigned long long) (val));			\
	   if (__builtin_constant_p (__w.__ll))				\
	     __r.__ll = COB_UINT64_SWAP_LE_BE_CONSTANT (__w.__ll);	\
	   else								\
	     {								\
	       __r.__l[0] = COB_UINT32_SWAP_LE_BE (__w.__l[1]);		\
	       __r.__l[1] = COB_UINT32_SWAP_LE_BE (__w.__l[0]);		\
	     }								\
	   __r.__ll; }))
     /* Possibly just use the constant version and let gcc figure it out? */
#    define COB_UINT16_SWAP_LE_BE(val) (COB_UINT16_SWAP_LE_BE_IA32 (val))
#    define COB_UINT32_SWAP_LE_BE(val) (COB_UINT32_SWAP_LE_BE_IA32 (val))
#    define COB_UINT64_SWAP_LE_BE(val) (COB_UINT64_SWAP_LE_BE_IA32 (val))
#  elif defined (__ia64__)
#    define COB_UINT16_SWAP_LE_BE_IA64(val)				\
       (__extension__							\
	({ register unsigned short __v, __x = ((unsigned short) (val));	\
	   if (__builtin_constant_p (__x))				\
	     __v = COB_UINT16_SWAP_LE_BE_CONSTANT (__x);		\
	   else								\
	     __asm__ __volatile__ ("shl %0 = %1, 48 ;;"			\
				   "mux1 %0 = %0, @rev ;;"		\
				    : "=r" (__v)			\
				    : "r" (__x));			\
	    __v; }))
#    define COB_UINT32_SWAP_LE_BE_IA64(val)				\
       (__extension__							\
	 ({ register unsigned long __v, __x = ((unsigned long) (val));	\
	    if (__builtin_constant_p (__x))				\
	      __v = COB_UINT32_SWAP_LE_BE_CONSTANT (__x);		\
	    else							\
	     __asm__ __volatile__ ("shl %0 = %1, 32 ;;"			\
				   "mux1 %0 = %0, @rev ;;"		\
				    : "=r" (__v)			\
				    : "r" (__x));			\
	    __v; }))
#    define COB_UINT64_SWAP_LE_BE_IA64(val)			\
       (__extension__						\
	({ register unsigned long long __v,			\
	     __x = ((unsigned long long) (val));		\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_UINT64_SWAP_LE_BE_CONSTANT (__x);	\
	   else							\
	     __asm__ __volatile__ ("mux1 %0 = %1, @rev ;;"	\
				   : "=r" (__v)			\
				   : "r" (__x));		\
	   __v; }))
#    define COB_UINT16_SWAP_LE_BE(val) (COB_UINT16_SWAP_LE_BE_IA64 (val))
#    define COB_UINT32_SWAP_LE_BE(val) (COB_UINT32_SWAP_LE_BE_IA64 (val))
#    define COB_UINT64_SWAP_LE_BE(val) (COB_UINT64_SWAP_LE_BE_IA64 (val))
#  elif defined (__x86_64__)
#    define COB_UINT32_SWAP_LE_BE_X86_64(val)				\
       (__extension__							\
	 ({ register unsigned long __v, __x = ((unsigned long) (val));	\
	    if (__builtin_constant_p (__x))				\
	      __v = COB_UINT32_SWAP_LE_BE_CONSTANT (__x);		\
	    else							\
	     __asm__ ("bswapl %0"					\
		      : "=r" (__v)					\
		      : "0" (__x));					\
	    __v; }))
#    define COB_UINT64_SWAP_LE_BE_X86_64(val)			\
       (__extension__						\
	({ register unsigned long long __v,			\
	     __x = ((unsigned long long) (val));		\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_UINT64_SWAP_LE_BE_CONSTANT (__x);	\
	   else							\
	     __asm__ ("bswapq %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	   __v; }))
     /* gcc seems to figure out optimal code for this on its own */
#    define COB_UINT16_SWAP_LE_BE(val) (COB_UINT16_SWAP_LE_BE_CONSTANT (val))
#    define COB_UINT32_SWAP_LE_BE(val) (COB_UINT32_SWAP_LE_BE_X86_64 (val))
#    define COB_UINT64_SWAP_LE_BE(val) (COB_UINT64_SWAP_LE_BE_X86_64 (val))
#  else /* generic gcc */
#    define COB_UINT16_SWAP_LE_BE(val) (COB_UINT16_SWAP_LE_BE_CONSTANT (val))
#    define COB_UINT32_SWAP_LE_BE(val) (COB_UINT32_SWAP_LE_BE_CONSTANT (val))
#    define COB_UINT64_SWAP_LE_BE(val) (COB_UINT64_SWAP_LE_BE_CONSTANT (val))
#  endif
#else /* generic */
#  define COB_UINT16_SWAP_LE_BE(val) (COB_UINT16_SWAP_LE_BE_CONSTANT (val))
#  define COB_UINT32_SWAP_LE_BE(val) (COB_UINT32_SWAP_LE_BE_CONSTANT (val))
#  define COB_UINT64_SWAP_LE_BE(val) (COB_UINT64_SWAP_LE_BE_CONSTANT (val))
#endif /* generic */

#endif /* COB_BYTEORDER_H */
