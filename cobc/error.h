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

#ifndef CB_ERROR_H
#define CB_ERROR_H

#include "config.h"

#include "tree.h"

extern int errorcount;
extern int warningcount;

extern void cb_warning (const char *fmt, ...);
extern void cb_error (const char *fmt, ...);
extern void cb_warning_x (cb_tree x, const char *fmt, ...);
extern void cb_error_x (cb_tree x, const char *fmt, ...);
extern void cb_archaic (const char *feature);
extern void cb_obsolete_85 (const char *feature);
extern void cb_obsolete_2002 (const char *feature);

extern void redefinition_error (cb_tree x);
extern void undefined_error (cb_tree x);
extern void ambiguous_error (cb_tree x);

#endif /* CB_ERROR_H */
