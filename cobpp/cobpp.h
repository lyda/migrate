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

#define COBPP_FORMAT_FREE		1
#define COBPP_FORMAT_FIXED		2

#define COBPP_DEFAULT_TAB_WIDTH		8
#define COBPP_DEFAULT_TEXT_COLUMN	72

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK 1	/* These are in libmingwex.a */
#endif

struct cobpp_name_list {
  const char *name;
  struct cobpp_name_list *next;
};

/* enable debugging lines */
extern int cobpp_flag_debugging_line;

/* warn if any text after cobpp_text_column */
extern int cobpp_warn_column_overflow;

int cobpp_source_format;

extern int cobpp_tab_width;
extern int cobpp_text_column;
extern int cobpp_exit_status;

extern struct cobpp_name_list *cobpp_include_list;
extern struct cobpp_name_list *cobpp_depend_list;
extern FILE *cobpp_depend_file;
extern char *cobpp_depend_target;

#endif /* COBPP_H */
