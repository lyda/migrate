/*
 * Copyright (C) 2002 Keisuke Nishida
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

#ifndef _CODEGEN_H_
#define _CODEGEN_H_

#undef COBC_DEFINE_FUNCTION
#undef COBC_DEFINE_INLINE
#define COBC_DEFINE_FUNCTION(tag,func,argc) tag,
#define COBC_DEFINE_INLINE(tag,func,argc) tag,
enum {
#include "functions.h"
  COBC_FUNCTION_END
};

struct program_spec {
  char *program_id;
  int initial_program;
  struct cobc_list *index_list;
  struct cobc_list *file_name_list;
  struct cobc_list *label_list;
  struct cobc_list *using_list;
  struct cobc_list *exec_list;
  struct cobc_field *working_storage;
  struct cobc_field *linkage_storage;
};

extern struct program_spec program_spec;

extern void codegen (struct program_spec *spec);

#endif /* _CODEGEN_H_ */
