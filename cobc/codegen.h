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

struct cobc_program_spec {
  char *program_id;
  int initial_program;
  int loop_counter;
  int decimal_index;
  int decimal_index_max;
  int enable_screen;
  struct cobc_list *class_list;
  struct cobc_list *index_list;
  struct cobc_list *file_list;
  struct cobc_list *using_list;
  struct cobc_list *exec_list;
  struct cobc_list *label_list;
  struct cobc_list *reference_list;
  struct cobc_field *working_storage;
  struct cobc_field *linkage_storage;
  struct cobc_field *screen_storage;
  struct cobc_label *file_handler[5];
};

extern void codegen (struct cobc_program_spec *spec);

#endif /* _CODEGEN_H_ */
