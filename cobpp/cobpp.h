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

#define COB_PACKAGE	"cobpp"
#define COB_VERSION	VERSION
#define COB_COPYRIGHT	"Copyright (C) 2001 Keisuke Nishida\n"

enum cob_format {
  COB_FORMAT_FREE,
  COB_FORMAT_FIXED
};

extern enum cob_format cob_format;
extern int cob_tab_width;
extern int cob_debug_flag;

#endif /* _COBPP_H_ */
