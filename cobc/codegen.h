/*
 * Copyright (C) 2001  Keisuke Nishida
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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

#include <stdio.h>

#include "scanner.h"

/* symbol table selector */
#define SYTB_LIT 1
#define SYTB_VAR 2
#define SYTB_LAB 3

/* set mode (see gen_set) */
#define SET_TO		0
#define SET_UP		1
#define SET_DOWN	2

/* asm sections or pseudo-sections */
#define SEC_CONST 2
#define SEC_DATA 3
#define SEC_STACK 4
#define SEC_ARGS 5
#define SEC_FIRST_NAMED 7

/* SPECIAL VARIABLES  */

#define SVAR_RCODE  	"RETURN-CODE"

#include "tree.h"

extern int screen_io_enable,scr_line,scr_column;
extern int decimal_comma;
extern char currency_symbol;
extern FILE *lexin;
extern cob_tree curr_field;
extern int curr_call_mode;
extern cob_tree pgm_id;
extern unsigned stack_offset;   /* offset das variaveis na pilha */
extern unsigned global_offset;  /* offset das variaveis globais (DATA) */
extern int paragr_num;
extern int loc_label;
extern int at_linkage, stack_plus;
extern char *toktext;
extern int yylex(void);
extern struct index_to_table_list *index2table;
extern int pgm_segment;
extern char *yytext;
extern int substring_slots;

extern cob_tree spe_lit_ZE;
extern cob_tree spe_lit_SP;
extern cob_tree spe_lit_LV;
extern cob_tree spe_lit_HV;
extern cob_tree spe_lit_QU;

/* htcobgen.c */
extern cob_tree lookup_symbol (char *s);
extern cob_tree lookup (char *s, int tab);
extern cob_tree install (char *name, int tab, int cloning);
extern cob_tree install_label (char *name);
extern cob_tree install_literal (const char *name);
extern cob_tree lookup_label (cob_tree sy, cob_tree parent);
extern cob_tree lookup_variable (cob_tree sy, cob_tree parent);
extern cob_tree lookup_for_redefines (cob_tree sy);
extern void init_program (const char *id);
extern void gen_loadvar (cob_tree sy);
extern void stabs_line (void);
extern void data_trail (void);
extern int adjust_linkage_vars (int start_offset);
extern void proc_header (int using);
extern void proc_trail (int using);
extern cob_tree save_literal (cob_tree x, int type);
extern void save_named_sect (cob_tree sy);
extern void put_disp_list (cob_tree sy);
extern void add_alternate_key (cob_tree sy, int duplicates);
extern struct scr_info *alloc_scr_info (void);
extern struct unstring_delimited *alloc_unstring_delimited (int all, cob_tree var);
extern struct unstring_destinations *alloc_unstring_dest (cob_tree var, cob_tree delim, cob_tree count);
extern struct string_from *alloc_string_from (cob_tree var, cob_tree delim);
extern void gen_unstring (cob_tree var, struct unstring_delimited *delim, struct unstring_destinations *dest, cob_tree ptr, cob_tree tally);
extern void gen_string (cob_tree sy, cob_tree_list list);
extern void gen_display_screen (cob_tree sy, int main);
extern void gen_display (int dupon, int nl);
extern void gen_gotoxy_expr (cob_tree x, cob_tree y);
extern void gen_accept (cob_tree sy, int echo, int main);
extern void gen_accept_from_time (cob_tree sy);
extern void gen_accept_from_date (cob_tree sy);
extern void gen_accept_from_day (cob_tree sy);
extern void gen_accept_from_day_of_week (cob_tree sy);
extern void gen_accept_from_inkey (cob_tree sy);
extern void gen_accept_from_cmdline (cob_tree sy);
extern void gen_accept_env_var (cob_tree sy, cob_tree v);
extern struct perf_info *create_perf_info (cob_tree sy1, cob_tree sy2, unsigned long lj, unsigned long le);
extern struct perform_info *create_perform_info (void);
extern char *check_perform_variables (cob_tree sy1, struct perform_info *pi1);
extern struct math_var *create_mathvar_info (struct math_var *mv, cob_tree sy, unsigned int opt);
extern int gen_status_branch (int status, int flag);
extern void gen_compute (struct math_var *vl1, cob_tree sy1);
extern void gen_add (cob_tree n1, cob_tree n2, int rnd);
extern void gen_add_to (cob_tree_list nums, struct math_var *list);
extern void gen_add_giving (cob_tree_list nums, struct math_var *list);
extern void gen_sub (cob_tree n1, cob_tree n2, int rnd);
extern void gen_subtract_from (cob_tree_list subtrahend_list, struct math_var *list);
extern void gen_subtract_giving (cob_tree_list subtrahend_list, cob_tree minuend, struct math_var *list);
extern void gen_multiply_by (cob_tree multiplicand, struct math_var *list);
extern void gen_multiply_giving (cob_tree multiplicand, cob_tree multiplier, struct math_var *list);
extern void gen_divide_into (cob_tree divisor, struct math_var *list);
extern void gen_divide_giving (cob_tree divisor, cob_tree dividend, struct math_var *list);
extern void gen_divide_giving_remainder (cob_tree divisor, cob_tree dividend, cob_tree quotient, cob_tree remainder, int rnd);
extern void gen_store_fnres (cob_tree sy);
extern void gen_class_check (cob_tree sy, int class);
extern void gen_inspect_tallying (cob_tree var, cob_tree_list list);
extern void gen_inspect_replacing (cob_tree var, cob_tree_list list);
extern void gen_inspect_converting (cob_tree var, cob_tree_list list);
extern void gen_move (cob_tree src, cob_tree dst);
extern void gen_corresponding (void (*func)(), cob_tree g1, cob_tree g2, int opt);
extern void gen_set (cob_tree_list l, int mode, cob_tree v);
extern void gen_set_true (cob_tree_list l);
extern void gen_goto (cob_tree_list l, cob_tree x);
extern void gen_evaluate_when (cob_tree_list subs, cob_tree_list whens, int next_lbl);
extern int gen_check_zero (void);
extern int gen_testif (void);
extern int gen_orstart (void);
extern void gen_dstlabel (int lbl);
extern int gen_passlabel (void);
extern int gen_marklabel (void);
extern void gen_jmplabel (int lbl);
extern void gen_push_int (cob_tree sy);
extern void gen_cancel (cob_tree sy);
extern void gen_perform_test_counter (int lbl);
extern void gen_perform_times (int lbl);
extern void gen_perform_thru (cob_tree s1, cob_tree s2);
extern void gen_perform (cob_tree sy);
extern void gen_SearchLoopCheck (unsigned long lbl5, cob_tree syidx, cob_tree sytbl);
extern void gen_SearchAllLoopCheck (unsigned long lbl3, cob_tree sytbl, cob_tree syvar, unsigned long lstart, unsigned long lend);
extern void Initialize_SearchAll_Boundaries (cob_tree sy);
extern cob_tree determine_table_index_name (cob_tree sy);
extern void define_field (int level, cob_tree sy);
extern void define_implicit_field (cob_tree sy, cob_tree sykey);
extern void scr_set_column (struct scr_info *si, int val, int plus_minus);
extern void scr_set_line (struct scr_info *si, int val, int plus_minus);
extern void update_report_field (cob_tree sy);
extern void update_screen_field (cob_tree sy, struct scr_info *si);
extern void update_field (void);
extern void close_fields (void);
extern void resolve_labels (void);
extern void gen_begin_label (cob_tree label);
extern void gen_end_label (cob_tree label);
extern void gen_stoprun (void);
extern void gen_exit (cob_tree label);
extern void gen_exit_program (void);
extern void set_variable_values (cob_tree v1, cob_tree v2);
extern void gen_condition (cob_tree x);
extern void gen_save_filedesc (cob_tree f);
extern void alloc_file_entry (cob_tree f);
extern void gen_fdesc (cob_tree f, cob_tree r);
extern struct sortfile_node *alloc_sortfile_node (cob_tree sy);
extern cob_tree create_status_register (char *name);
extern void gen_sort_using (cob_tree f, struct sortfile_node *sn);
extern void gen_sort_giving (cob_tree f, struct sortfile_node *sn);
extern void gen_sort (cob_tree f);
extern void gen_open (int mode, cob_tree f);
extern void gen_close_sort (cob_tree f);
extern void gen_close (cob_tree f);
extern void gen_read (cob_tree f, cob_tree buf, cob_tree key);
extern void gen_read_next (cob_tree f, cob_tree buf, int next_prev);
extern int gen_reads (cob_tree f, cob_tree buf, cob_tree key, int next_prev);
extern void gen_return (cob_tree f, cob_tree buf);
extern void gen_release (cob_tree r, cob_tree buf);
extern void gen_write (cob_tree r, int opt, cob_tree buf);
extern void gen_rewrite (cob_tree r, cob_tree buf);
extern void gen_start (cob_tree f, int cond, cob_tree key);
extern void gen_delete (cob_tree f);
extern void set_rec_varying_info (cob_tree f, cob_tree lmin, cob_tree lmax, cob_tree reclen);
extern void gen_save_using (cob_tree sy);
extern int gen_call (cob_tree v, struct call_parameter *parameter_list, int exceplabel, int notexceplabel);
extern void check_call_except (int excep, int notexcep, int exceplabel, int notexceplabel, int endlabel);
extern void gen_initialize (cob_tree sy_start);

/* parser.y */
extern int yyparse (void);
extern void yywarn (char *fmt, ...);
extern void yyerror (char *fmt, ...);

#endif /* _CODEGEN_H_ */
