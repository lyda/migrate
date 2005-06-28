/*
 * Copyright (C) 2002-2004 Keisuke Nishida
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

#include <libcob/common.h>

extern void cob_display (cob_field *f);
extern void cob_new_display (int outorerr, int newline, int varcnt, ...);
extern void cob_display_error (cob_field *f);
extern void cob_newline (void);
extern void cob_newline_error (void);
extern void cob_field_print (cob_field *f);
extern void cob_accept (cob_field *f);
extern void cob_accept_date (cob_field *f);
extern void cob_accept_date_yyyymmdd (cob_field *f);
extern void cob_accept_day (cob_field *f);
extern void cob_accept_day_yyyyddd (cob_field *f);
extern void cob_accept_day_of_week (cob_field *f);
extern void cob_accept_time (cob_field *f);
extern void cob_accept_command_line (cob_field *f);
extern void cob_display_environment (cob_field *f);
extern void cob_accept_environment (cob_field *f);
extern void cob_display_env_value (cob_field *f);
extern void cob_display_arg_number (cob_field *f);
extern void cob_accept_arg_number (cob_field *f);
extern void cob_accept_arg_value (cob_field *f);

extern void cob_init_termio (void);

#endif /* COB_TERMIO_H_ */
