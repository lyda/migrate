/*
 * Copyright (C) 2005 Roger While
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

#ifndef COB_INTRINSIC_H
#define COB_INTRINSIC_H

#include <libcob/common.h>

extern void		cob_init_intrinsic (void);
extern cob_field	*cob_intr_binop (cob_field *f1, int op, cob_field *f2);
extern cob_field	*cob_intr_current_date (void);
extern cob_field	*cob_intr_char (cob_field *srcfld);
extern cob_field	*cob_intr_ord (cob_field *srcfld);
extern cob_field	*cob_intr_date_of_integer (cob_field *srcdays);
extern cob_field	*cob_intr_day_of_integer (cob_field *srcdays);
extern cob_field	*cob_intr_integer_of_date (cob_field *srcfield);
extern cob_field	*cob_intr_integer_of_day (cob_field *srcfield);
extern cob_field	*cob_intr_test_date_yyyymmdd (cob_field *srcfield);
extern cob_field	*cob_intr_test_day_yyyyddd (cob_field *srcfield);
extern cob_field	*cob_intr_factorial (cob_field *srcfield);
extern cob_field	*cob_intr_exp (cob_field *srcfield);
extern cob_field	*cob_intr_exp10 (cob_field *srcfield);
extern cob_field	*cob_intr_abs (cob_field *srcfield);
extern cob_field	*cob_intr_acos (cob_field *srcfield);
extern cob_field	*cob_intr_asin (cob_field *srcfield);
extern cob_field	*cob_intr_atan (cob_field *srcfield);
extern cob_field	*cob_intr_cos (cob_field *srcfield);
extern cob_field	*cob_intr_log (cob_field *srcfield);
extern cob_field	*cob_intr_log10 (cob_field *srcfield);
extern cob_field	*cob_intr_sin (cob_field *srcfield);
extern cob_field	*cob_intr_sqrt (cob_field *srcfield);
extern cob_field	*cob_intr_tan (cob_field *srcfield);
extern cob_field	*cob_intr_upper_case (cob_field *sizefield, cob_field *srcfield);
extern cob_field	*cob_intr_lower_case (cob_field *sizefield, cob_field *srcfield);
extern cob_field	*cob_intr_reverse (cob_field *sizefield, cob_field *srcfield);
extern cob_field	*cob_intr_length (cob_field *srcfield);
extern cob_field	*cob_intr_integer (cob_field *srcfield);
extern cob_field	*cob_intr_integer_part (cob_field *srcfield);
extern cob_field	*cob_intr_numval (cob_field *srcfield);
extern cob_field	*cob_intr_numval_c (cob_field *srcfield);
extern cob_field	*cob_intr_annuity (cob_field *srcfield1, cob_field *srcfield2);
extern cob_field	*cob_intr_mod (cob_field *srcfield1, cob_field *srcfield2);
extern cob_field	*cob_intr_rem (cob_field *srcfield1, cob_field *srcfield2);
extern cob_field	*cob_intr_sum (int params, ...);
extern cob_field	*cob_intr_ord_min (int params, ...);
extern cob_field	*cob_intr_ord_max (int params, ...);
extern cob_field	*cob_intr_min (int params, ...);
extern cob_field	*cob_intr_max (int params, ...);
extern cob_field	*cob_intr_midrange (int params, ...);
extern cob_field	*cob_intr_median (int params, ...);
extern cob_field	*cob_intr_mean (int params, ...);
extern cob_field	*cob_intr_range (int params, ...);
extern cob_field	*cob_intr_random (int params, ...);
extern cob_field	*cob_intr_variance (int params, ...);
extern cob_field	*cob_intr_standard_deviation (int params, ...);
extern cob_field	*cob_intr_present_value (int params, ...);
extern cob_field	*cob_intr_year_to_yyyy (int params, ...);
extern cob_field	*cob_intr_date_to_yyyymmdd (int params, ...);
extern cob_field	*cob_intr_day_to_yyyyddd (int params, ...);

#endif /* COB_INTRINSIC_H */
