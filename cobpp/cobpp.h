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

#ifndef COBPP_H
#define COBPP_H

#include "config.h"

#include <stdio.h>

#define COBPP_PACKAGE	"cobpp (" PACKAGE_NAME ")"
#define COBPP_VERSION	PACKAGE_VERSION
#define COBPP_COPYRIGHT	"Copyright (C) 2001-2003 Keisuke Nishida\n"

#define COBPP_FORMAT_UNKNOWN	0
#define COBPP_FORMAT_FREE	1
#define COBPP_FORMAT_FIXED	2
#define COBPP_FORMAT_SEMI_FIXED	3

struct cobpp_path {
  const char *dir;
  struct cobpp_path *next;
};

extern int cobpp_tab_width;
extern int cobpp_debug_flag;
extern int cobpp_exit_status;
extern int cobpp_warn_trailing_line;
extern int cobpp_source_format;
extern int cobpp_source_format_inferred;
extern struct cobpp_path *cobpp_include_path;
extern struct cobpp_path *cobpp_depend_list;
extern FILE *cobpp_depend_file;

#endif /* COBPP_H */
