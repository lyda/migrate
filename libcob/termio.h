/*
 * Copyright (C) 2002 Keisuke Nishida
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
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#ifndef COB_TERMIO_H
#define COB_TERMIO_H

#include <stdio.h>
#include <libcob/common.h>

#define COB_SYSIN	0
#define COB_SYSOUT	1
#define COB_SYSERR	2

#define cob_puti(i,fd)	fprintf (cob_stream[fd], "%d", (i))
#define cob_puts(s,fd)	fputs ((s), cob_stream[fd])

extern FILE *cob_stream[];

extern void cob_init_termio (void);
#ifdef COB_DEBUG
extern void cob_debug_print (cob_field *f);
#endif

extern void cob_display (cob_field *f, int fd);
extern void cob_newline (int fd);
extern void cob_accept (cob_field *f, int fd);
extern void cob_accept_date (cob_field *f);
extern void cob_accept_day (cob_field *f);
extern void cob_accept_day_of_week (cob_field *f);
extern void cob_accept_time (cob_field *f);
extern void cob_accept_command_line (cob_field *f);
extern void cob_accept_environment (cob_field *f, cob_field *env);

#endif /* COB_TERMIO_H_ */
