/*
 * Copyright (C) 2001-2002 Keisuke Nishida
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

#ifndef _RESERVED_H_
#define _RESERVED_H_

enum builtin_token {
  BUILTIN_ZERO,
  BUILTIN_STDIN,
  BUILTIN_STDOUT,
  BUILTIN_STDERR,
  BUILTIN_SWITCH_1,
  BUILTIN_SWITCH_2,
  BUILTIN_SWITCH_3,
  BUILTIN_SWITCH_4,
  BUILTIN_SWITCH_5,
  BUILTIN_SWITCH_6,
  BUILTIN_SWITCH_7,
  BUILTIN_SWITCH_8,
};

extern int lookup_builtin_word (const char *name);
extern int lookup_reserved_word (const char *name);
extern void init_reserved_words (void);

#endif /* _RESERVED_H_ */
