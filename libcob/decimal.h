/*
 * Copyright (C) 2001 Keisuke Nishida
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

#ifndef _COB_DECIMAL_H_
#define _COB_DECIMAL_H_

#include <gmp.h>

struct decimal_number {
  mpz_t number;
  int decimals;
};

typedef struct decimal_number *decimal;

union numeric_type {
  double n_double;
  decimal n_decimal;
};

extern int cob_size_error_flag;

extern void cob_fld_to_decimal (struct fld_desc *f, unsigned char *s, union numeric_type *p);
extern void cob_decimal_to_fld (struct fld_desc *f, char *s, int round, union numeric_type val);

extern decimal make_decimal (void);
extern void free_decimal (decimal d);
extern void print_decimal (decimal d);
extern void add_decimal (union numeric_type *v1, union numeric_type v2);
extern void subtract_decimal (union numeric_type *v1, union numeric_type v2);
extern void multiply_decimal (union numeric_type *v1, union numeric_type v2);
extern void divide_decimal (union numeric_type *v1, union numeric_type v2);
extern void pow_decimal (union numeric_type *v1, union numeric_type v2);
extern int compare_decimal (union numeric_type v1, union numeric_type v2);

#endif /* _COB_DECIMAL_H_ */
