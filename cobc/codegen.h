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
  int enable_screen;
  struct cobc_list *class_list;
  struct cobc_list *index_list;
  struct cobc_list *file_name_list;
  struct cobc_list *using_list;
  struct cobc_list *exec_list;
  struct cobc_field *working_storage;
  struct cobc_field *linkage_storage;
  struct cobc_field *screen_storage;
  struct cobc_label_name *input_handler;
  struct cobc_label_name *output_handler;
  struct cobc_label_name *i_o_handler;
  struct cobc_label_name *extend_handler;
};

extern void codegen (struct cobc_program_spec *spec);

extern void output_file_handler (struct cobc_file_name *f, int type, cobc_tree st1, cobc_tree st2);
extern void output_goto (struct cobc_label_name *p);
extern void output_goto_depending (struct cobc_list *labels, cobc_tree index);
extern void output_move (cobc_tree src, cobc_tree dst);
extern void output_initialize (cobc_tree x);
extern void output_initialize_replacing (cobc_tree x, struct cobc_list *l);
extern void output_display (cobc_tree x, cobc_tree fd);
extern void output_search (cobc_tree table, cobc_tree var, cobc_tree sentence, cobc_tree whens);
extern void output_search_all (cobc_tree table, cobc_tree sentence, cobc_tree when);
extern void output_call_statement (cobc_tree name, struct cobc_list *args, cobc_tree st1, cobc_tree st2);

#endif /* _CODEGEN_H_ */
