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

/* set operations */
#define SET_TO 1
#define SET_UP_BY 2
#define SET_DOWN_BY 3

/* call modes */
#define CM_REF 1
#define CM_VAL 2
#define CM_CONT 3

/* asm sections or pseudo-sections */
#define SEC_CONST 2
#define SEC_DATA 3
#define SEC_STACK 4
#define SEC_ARGS 5
#define SEC_FIRST_NAMED 7

/* selection subject types (evaluate statement) */
#define SSUBJ_BOOLEAN	0
#define SSUBJ_EXPR	2
#define SSUBJ_STR	4

/* selection object types (evaluate statement) */
#define SOBJ_ANY	0x00
#define SOBJ_ZERO	0x10
#define SOBJ_EXPR	0x20
#define SOBJ_RANGE	0x30
#define SOBJ_STR	0x40
#define SOBJ_BOOLEAN	0x50
#define SOBJ_TYPE_MASK	0xf0

/* standard field classes */
#define CLASS_NUMERIC 0
#define CLASS_ALPHABETIC 1
#define CLASS_ALPHABETIC_LOWER 2
#define CLASS_ALPHABETIC_UPPER 3

/* extended condition flags */
#define COND_UNARY 0x10
#define COND_CLASS 0x20

/* minor token numbers */

/* CONDITIONAL */

#define EQUAL 	1
#define LESS  	2
#define GREATER	4
#define GEQ 	5
#define LEQ 	3
#define NEQ 	6

/* SPECIAL VARIABLES  */

#define SVAR_RCODE  	"RETURN-CODE"

#include "tree.h"

/* htcobgen.c */
extern struct sym *lookup_symbol (char *s);
extern struct sym *lookup (char *s, int tab);
extern struct sym *install (char *name, int tab, int cloning);
extern struct sym *install_label (char *name);
extern struct lit *install_literal (const char *name);
extern struct sym *lookup_label (struct sym *sy, struct sym *parent);
extern struct sym *lookup_variable (struct sym *sy, struct sym *parent);
extern struct sym *lookup_for_redefines (struct sym *sy);
extern void init_program (const char *id);
extern void gen_loadvar (struct sym *sy);
extern char sign_to_char (int digit);
extern struct lit *invert_literal_sign (struct lit *sy);
extern char *sch_convert (char *s);
extern int is_variable (struct sym *sy);
extern int is_subscripted (struct sym *sy);
extern void emit_lit (char *s, int len);
extern void emit_lit_fill (int c, int len);
extern void gen_init_value (struct lit *sy, int var_len);
extern void stabs_line (void);
extern void data_trail (void);
extern int adjust_linkage_vars (int start_offset);
extern void proc_header (int using);
extern void proc_trail (int using);
extern void save_field_in_list (struct sym *sy);
extern void save_literal (struct lit *v, int type);
extern void save_named_sect (struct sym *sy);
extern void put_disp_list (struct sym *sy);
extern int pic_digits (struct sym *sy);
extern int query_comp_len (struct sym *sy);
extern int symlen (struct sym *sy);
extern int varsize_ch (struct sym *sy);
extern void add_alternate_key (struct sym *sy, int duplicates);
extern struct list *insert_list (struct list *l, void *item);
extern void free_list (struct list *l);
extern struct scr_info *alloc_scr_info (void);
extern struct inspect_before_after *alloc_inspect_before_after (struct
								inspect_before_after
								*ba,
								int
								before_after,
								struct sym
								*var);
extern struct converting_struct *alloc_converting_struct (struct sym *fromvar,
							  struct sym *tovar,
							  struct
							  inspect_before_after
							  *ba);
extern struct tallying_list *alloc_tallying_list (struct tallying_list *tl,
						  struct sym *count,
						  struct tallying_for_list
						  *tfl);
extern struct tallying_for_list *alloc_tallying_for_list (struct
							  tallying_for_list
							  *tfl, int options,
							  struct sym *forvar,
							  struct
							  inspect_before_after
							  *ba);
extern struct replacing_list *alloc_replacing_list (struct replacing_list *rl,
						    int options,
						    struct replacing_by_list
						    *rbl, struct sym *byvar,
						    struct
						    inspect_before_after *ba);
extern struct replacing_by_list *alloc_replacing_by_list (struct
							  replacing_by_list
							  *rbl,
							  struct sym *replvar,
							  struct sym *byvar,
							  struct
							  inspect_before_after
							  *ba);
extern struct unstring_delimited *alloc_unstring_delimited (short int all,
							    struct sym *var);
extern struct unstring_destinations *alloc_unstring_dest (struct sym *var,
							  struct sym *delim,
							  struct sym *count);
extern struct string_from *alloc_string_from (struct sym *var,
					      struct sym *delim);
extern void gen_unstring (struct sym *var, struct unstring_delimited *delim,
			  struct unstring_destinations *dest, struct sym *ptr,
			  struct sym *tally);
extern void gen_string (struct string_from *sf, struct sym *sy,
			   struct sym *ptr);
extern void gen_display_screen (struct sym *sy, int main);
extern void gen_display (int dupon, int nl);
extern void gen_gotoxy_expr (void);
extern void gen_accept (struct sym *sy, int echo, int main);
extern void gen_accept_from_time (struct sym *sy);
extern void gen_accept_from_date (struct sym *sy);
extern void gen_accept_from_day (struct sym *sy);
extern void gen_accept_from_day_of_week (struct sym *sy);
extern void gen_accept_from_inkey (struct sym *sy);
extern void gen_accept_from_cmdline (struct sym *sy);
extern void gen_accept_env_var (struct sym *sy, struct lit *v);
extern struct perf_info *create_perf_info (struct sym *sy1, struct sym *sy2,
					   unsigned long lj,
					   unsigned long le);
extern struct perform_info *create_perform_info (void);
extern char *check_perform_variables (struct sym *sy1,
				      struct perform_info *pi1);
extern struct sym *create_expr (char op, struct sym *left, struct sym *right);
extern void free_expr (struct expr *e);
extern void free_expr_list (void);
extern struct math_var *create_mathvar_info (struct math_var *mv,
					     struct sym *sy,
					     unsigned int opt);
extern struct math_ose *math_on_size_error0 (void);
extern struct math_ose *math_on_size_error1 (struct math_ose *v);
extern void math_on_size_error2 (void);
extern void math_on_size_error3 (struct math_ose *v);
extern struct math_ose *math_on_size_error4 (struct math_ose *v,
					     unsigned long ty);
extern void gen_add (struct sym *s1, struct sym *s2, int rnd);
extern void gen_subtract (struct sym *s1, struct sym *s2, int rnd);
extern void gen_multiply (struct sym *s1, struct sym *s2, struct sym *s3,
			  int rnd);
extern void gen_divide (struct sym *s1, struct sym *s2, struct sym *s3,
			struct sym *s4, int rnd);
extern void gen_compute (struct math_var *vl1, struct sym *sy1, struct math_ose *ose);
extern void gen_add1 (struct math_var *vl0, struct math_var *vl2,
		      struct math_ose *v1);
extern void gen_add2 (struct math_var *vl1, struct math_var *vl2,
		      struct sym *sy1, struct math_ose *v1);
extern void gen_subtract1 (struct math_var *vl0, struct math_var *vl2,
			   struct math_ose *v1);
extern void gen_subtract2 (struct math_var *vl1, struct math_var *vl2,
			   struct sym *sy1, struct math_ose *v1);
extern void gen_multiply1 (struct math_var *vl1, struct sym *sy1,
			   struct math_ose *v1);
extern void gen_multiply2 (struct math_var *vl1, struct sym *sy1,
			   struct sym *sy2, struct math_ose *v1);
extern void gen_divide1 (struct math_var *vl1, struct sym *sy1,
			 struct math_ose *v1);
extern void gen_divide2 (struct math_var *vl1, struct sym *sy1,
			 struct sym *sy2, struct math_ose *v1);
extern struct subref *make_subref (struct sym *sy, struct subref *subs);
extern struct subref *create_subscript (struct sym *sy);
extern struct subref *add_subscript_item (struct subref *subs, char op,
					struct sym *item);
extern struct subref *add_subscript (struct subref *ref, struct subref *subs);
extern struct refmod *create_refmoded_var (struct sym *sy, struct sym *syoff,
					   struct sym *sylen);
extern void gen_subscripted (struct subref *subs);
extern struct sym *get_variable_item (struct sym *sy);
extern void gen_temp_storage (int size);
extern void adjust_desc_length (struct sym *sy);
extern void gen_store_fnres (struct sym *sy);
extern int is_numeric_sy (struct sym *sy);
extern void gen_class_check (struct sym *sy, int class);
extern void gen_inspect (struct sym *var, void *list, int operation);
extern void gen_move (struct sym *sy_src, struct sym *sy_dst);
extern void gen_movecorr (struct sym *sy1, struct sym *sy2);
extern void gen_addcorr (struct sym *sy1, struct sym *sy2, int rnd);
extern void gen_subtractcorr (struct sym *sy1, struct sym *sy2, int rnd);
extern void gen_set (struct sym *idx, int which, struct sym *var,
		     int adrof_idx, int adrof_var);
extern int gen_evaluate_start (void);
extern int push_selection_subject_copy (int level, struct selsubject *ssbj,
					int stkadd, int objtype);
extern int selection_subject_type (int level, struct selsubject *ssbj);
extern void gen_when_check (int level, struct selsubject *ssbj, int type, int endcase);
extern void gen_bypass_when_case (int bypass);
extern int gen_end_when (int n, int endcase, int sentence);
extern void push_boolean (int flag);
extern void push_condition (void);
extern void push_field (struct sym *sy);
extern void gen_goto_depending (struct list *l, struct sym *sy);
extern void gen_goto (struct list *l);
extern int gen_check_zero (void);
extern int gen_at_end (int status);
extern int gen_testif (void);
extern void gen_not (void);
extern int gen_andstart (void);
extern int gen_orstart (void);
extern void gen_dstlabel (int lbl);
extern int gen_passlabel (void);
extern int gen_marklabel (void);
extern void gen_jmplabel (int lbl);
extern void gen_push_int (struct sym *sy);
extern void gen_cancel (struct sym *sy);
extern void gen_perform_test_counter (int lbl);
extern void gen_perform_times (int lbl);
extern void gen_perform_thru (struct sym *s1, struct sym *s2);
extern void gen_perform (struct sym *sy);
extern int save_pic_char (char c, int n);
extern void gen_SearchLoopCheck (unsigned long lbl5, struct sym *syidx,
				 struct sym *sytbl);
extern void gen_SearchAllLoopCheck (unsigned long lbl3, struct sym *syidx,
				    struct sym *sytbl, struct sym *syvar,
				    unsigned long lstart, unsigned long lend);
extern void define_implicit_field (struct sym *sy, struct sym *sykey, int idxlen);
extern void Initialize_SearchAll_Boundaries (struct sym *sy,
					     struct sym *syidx);
extern struct sym *determine_table_index_name (struct sym *sy);
extern void define_field (int level, struct sym *sy);
extern struct selsubject *save_sel_subject (struct selsubject *ssubj, int type);
extern void release_sel_subject (int label, struct selsubject *ssbj);
extern int set_field_value_sw (struct sym *sy, int times);
extern int set_field_length (struct sym *sy, int times);
extern unsigned field_alignment (struct sym *sy, unsigned location);
extern void set_field_location (struct sym *sy, unsigned location);
extern void scr_set_column (struct scr_info *si, int val, int plus_minus);
extern void scr_set_line (struct scr_info *si, int val, int plus_minus);
extern void save_report (struct sym *rep, struct sym *file);
extern void update_report_field (struct sym *sy);
extern void update_screen_field (struct sym *sy, struct scr_info *si);
extern void update_field (void);
extern void close_fields (void);
extern void resolve_labels (void);
extern void open_section (struct sym *sect);
extern void close_section (void);
extern char *label_name (struct sym *lab);
extern char *var_name (struct sym *sy);
extern void close_paragr (void);
extern void open_paragr (struct sym *paragr);
extern void gen_stoprun (void);
extern void gen_exit (int code);
extern void set_variable_values (struct lit *v1, struct lit *v2);
extern void gen_condition (struct sym *sy);
extern void gen_compare_exp (int value);
extern void gen_compare (struct sym *s1, int value, struct sym *s2);
extern void assign_expr (struct sym *sy, int rnd);
extern int push_expr (struct sym *sy);
extern void gen_save_filedesc (struct sym *f);
extern void alloc_file_entry (struct sym *f);
extern void dump_alternate_keys (struct sym *r, struct alternate_list *alt);
extern void dump_fdesc (void);
extern void gen_fdesc (struct sym *f, struct sym *r);
extern void gen_status (struct sym *f);
extern struct sortfile_node *alloc_sortfile_node (struct sym *sy);
extern struct sym *create_status_register (char *name);
extern void gen_sort_using (struct sym *f, struct sortfile_node *sn);
extern void gen_sort_giving (struct sym *f, struct sortfile_node *sn);
extern void gen_sort (struct sym *f);
extern void gen_open (int mode, struct sym *f);
extern void gen_close_sort (struct sym *f);
extern void gen_close (struct sym *f);
extern int gen_reads (struct sym *f, struct sym *buf, struct sym *key,
		      int next_prev, int sel);
extern void gen_return (struct sym *f, struct sym *buf);
extern void gen_read (struct sym *f, struct sym *buf, struct sym *key);
extern void gen_read_next (struct sym *f, struct sym *buf, int next_prev);
extern void gen_release (struct sym *r, struct sym *buf);
extern void gen_write (struct sym *r, int opt, struct sym *buf);
extern void gen_rewrite (struct sym *r, struct sym *buf);
extern void gen_start (struct sym *f, int cond, struct sym *key);
extern void gen_delete (struct sym *f);
extern void set_rec_varying_info (struct sym *f, struct lit *lmin,
				  struct lit *lmax, struct sym *reclen);
extern void gen_check_varying (struct sym *f);
extern void gen_push_using (struct sym *sy);
extern void gen_save_using (struct sym *sy);
extern unsigned long int gen_call (struct lit *v, int stack_size,
				   int exceplabel, int notexceplabel);
extern int begin_on_except (void);
extern void check_call_except (int excep, int notexcep, int exceplabel,
			       int notexceplabel, int endlabel);
extern void gen_initialize (struct sym *sy_start);
extern void mark_actives (int first, int last);
extern struct ginfo *ginfo_container0 (void);
extern struct ginfo *ginfo_container1 (struct ginfo *v);
extern void ginfo_container2 (struct ginfo *v, unsigned long ty);
extern struct ginfo *ginfo_container3 (struct ginfo *v, unsigned long ty);
extern void ginfo_container4 (struct ginfo *v);
extern struct invalid_key_element *gen_before_invalid_key ();
extern struct invalid_key_element *gen_after_invalid_key (struct
							  invalid_key_element
							  *p);
extern struct invalid_keys *gen_invalid_keys (struct invalid_key_element *p1,
					      struct invalid_key_element *p2);
extern void gen_test_invalid_keys (struct invalid_keys *p);
extern int sort_exref_compare (const void *z1, const void *z2);

/* parser.y */
extern int yyparse (void);
extern void yywarn (char *fmt, ...);
extern void yyerror (char *fmt, ...);

#endif /* _CODEGEN_H_ */
