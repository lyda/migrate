/* 
 * Copyright (C) 2001 Keisuke Nishida
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

#ifndef _COBPP_H_
#define _COBPP_H_

#include <stdio.h>

#define COB_PACKAGE	"cobpp (OpenCOBOL)"
#define COB_VERSION	VERSION
#define COB_COPYRIGHT	"Copyright (C) 2001-2002 Keisuke Nishida\n"

#define COB_FORMAT_FREE		0
#define COB_FORMAT_FIXED	1

struct cob_path {
  const char *dir;
  struct cob_path *next;
};

extern int cob_tab_width;
extern int cob_debug_flag;
extern int cob_exit_status;
extern int cob_file_format;
extern struct cob_path *cob_include_path;
extern struct cob_path *cob_depend_list;
extern FILE *cob_depend_file;

#endif /* _COBPP_H_ */
