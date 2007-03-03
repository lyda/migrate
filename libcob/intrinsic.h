/*
 * Copyright (C) 2005-2007 Roger While
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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_INTRINSIC_H
#define COB_INTRINSIC_H

#include <libcob/common.h>

extern cob_field	*cob_intr_binop (cob_field *f1, int op, cob_field *f2);
extern cob_field	*cob_intr_current_date (void);
extern cob_field	*cob_intr_exception_file (void);
extern cob_field	*cob_intr_exception_location (void);
extern cob_field	*cob_intr_exception_status (void);
extern cob_field	*cob_intr_exception_statement (void);
extern cob_field	*cob_intr_char (cob_field *srcfield);
extern cob_field	*cob_intr_ord (cob_field *srcfield);
extern cob_field	*cob_intr_stored_char_length (cob_field *srcfield);
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
extern cob_field	*cob_intr_upper_case (cob_field *srcfield);
extern cob_field	*cob_intr_lower_case (cob_field *srcfield);
extern cob_field	*cob_intr_reverse (cob_field *srcfield);
extern cob_field	*cob_intr_trim (cob_field *srcfield, const int direction);
extern cob_field	*cob_intr_length (cob_field *srcfield);
extern cob_field	*cob_intr_integer (cob_field *srcfield);
extern cob_field	*cob_intr_integer_part (cob_field *srcfield);
extern cob_field	*cob_intr_fraction_part (cob_field *srcfield);
extern cob_field	*cob_intr_sign (cob_field *srcfield);
extern cob_field	*cob_intr_numval (cob_field *srcfield);
extern cob_field	*cob_intr_numval_c (cob_field *srcfield, cob_field *currency);
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
extern cob_field	*cob_intr_seconds_past_midnight (void);
extern cob_field	*cob_intr_seconds_from_formatted_time (cob_field *format, cob_field *value);
extern cob_field	*cob_intr_locale_date (cob_field *srcfield, cob_field *locale_field);
extern cob_field	*cob_intr_locale_time (cob_field *srcfield, cob_field *locale_field);

#endif /* COB_INTRINSIC_H */
