/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

#ifndef CB_RESERVED_H
#define CB_RESERVED_H

#include "tree.h"

enum cb_device_name {
  CB_SYSIN,
  CB_SYSOUT,
  CB_SYSERR,
  CB_CONSOLE
};

enum cb_switch_name {
  CB_SWITCH_1,
  CB_SWITCH_2,
  CB_SWITCH_3,
  CB_SWITCH_4,
  CB_SWITCH_5,
  CB_SWITCH_6,
  CB_SWITCH_7,
  CB_SWITCH_8
};

extern cb_tree lookup_system_name (const char *name);
extern int lookup_reserved_word (const char *name);
extern void cb_init_reserved (void);

#endif /* CB_RESERVED_H */
