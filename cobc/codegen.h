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
#define CM_DESC 4

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

#include "tree.h"

/*
 * compile-time list for value ranges of 88-level variables.
 * the first range is stored at the "struct sym", with sym->vr
 * being a pointer to the remaining "struct vrange" nodes.
 */
struct vrange
{
  struct vrange *next;		/* pointer to next range of values (88 var) */
  struct lit *value;		/* pointer to literal with initial value */
  struct lit *value2;		/* pointer to first/next key (sort files) */
};

/*
 * Node for external data (named sections).
 */
struct named_sect
{
  struct named_sect *next;	/* pointer to next named section */
  short sec_no;			/* key: section id */
  char *os_name;		/* name of 01 or 77 data as known by OS */
};

/* this (struct rd) is aliased with (struct sym), so tail data is garbage! */
struct rd
{
  char litflag;
  struct rd *next;
  char *name;
  char type;			/* 'W' for report (RD) */
  struct sym *file;		/* file for writing this report */
  struct list *controls;	/* list of controls (listing breaks) */
  struct list *items;		/* list of all report items */
  int page_limit;
  int heading;
  int footing;
  int first_detail;
  int last_detail;
};

/* additional information for report items */
struct report_info
{
  int line;
  int line_offset;		/* PLUS <offset> given */
  int column;
  int value_source;		/* SUM, SOURCE (from a variable), literal */
  /* the actual source symbol is in (struct sym *)->value */
};

/* varying record range and actual size */
struct rec_varying
{
  struct lit *lmin;
  struct lit *lmax;
  struct sym *reclen;
};

/* selection subject set (evaluate statement) */
struct selsubject
{
  struct selsubject *next;
  int type;
};

/* sort file list for using/giving clauses*/
struct sortfile_node
{
  struct sortfile_node *next;
  struct sym *sy;
};

/* information required by the 'perform ... varying ... after' statements */
struct perf_info
{
  struct sym *pname1;		/* symbol name */
  struct sym *pname2;		/* symbol name */
  unsigned long ljmp;		/* jump label  */
  unsigned long lend;		/* end  label  */
};

struct perform_info
{
  struct perf_info *pf[4];
};

/* information required by the math verbs statements */
struct math_var
{
  struct sym *sname;		/* symbol name */
  unsigned int rounded;		/* rounded option: 0=false, 1=true */
  struct math_var *next;
};

/* information required by the math ON SIZE ERROR statement */
struct math_ose
{
  unsigned long ose;		/* 1=on_size, 2=not_on_size, 3=both */
  unsigned long lbl1;		/* call label name 1 - on_size */
  unsigned long lbl2;		/* call label name 2 - not_on_size */
  unsigned long lbl4;		/* bypass label name  */
};

/* generic information container used by the [NOT] AT END cluases */
struct ginfo
{
  unsigned long sel;		/* 1=true, 2=not true, 3=both */
  unsigned long lbl1;		/* call label name 1 - true */
  unsigned long lbl2;		/* call label name 2 - not true */
  unsigned long lbl3;		/* retrun 1 label name  */
  unsigned long lbl4;		/* retrun 2 label name  */
  unsigned long lbl5;		/* test bypass label name  */
};

/* information required by [NOT] INVALID KEY clauses */
struct invalid_key_element
{
  unsigned long lbl1;		/* skip label */
  unsigned long lbl2;		/* start label */
  unsigned long lbl3;		/* finish label */
};

struct invalid_keys
{
  struct invalid_key_element *invalid_key;
  struct invalid_key_element *not_invalid_key;
};

/******* supplemental information for screen items **********/
/* this is linked at the sym->index (aliased scrinfo) */
struct scr_info
{
  int attr;
  int line;
  int column;
  short int foreground;
  short int background;
  struct sym *from;
  struct sym *to;
  int label;
};

struct converting_struct
{
  struct sym *fromvar;
  struct sym *tovar;
  struct inspect_before_after *before_after;
};

struct tallying_list
{
  struct tallying_list *next;
  struct tallying_for_list *tflist;
  struct sym *count;
};

struct tallying_for_list
{
  struct tallying_for_list *next;
  int options;
  struct sym *forvar;
  struct inspect_before_after *before_after;
};

struct replacing_list
{
  struct replacing_list *next;
  int options;
  struct sym *byvar;
  struct replacing_by_list *replbylist;
  struct inspect_before_after *before_after;
};

struct replacing_by_list
{
  struct replacing_by_list *next;
  struct sym *replvar;
  struct sym *byvar;
  struct inspect_before_after *before_after;
};

struct inspect_before_after
{
  struct sym *before;
  struct sym *after;
};


struct alternate_list
{
  struct alternate_list *next;
  struct sym *key;
  int duplicates;
};

struct unstring_delimited
{
  struct unstring_delimited *next;
  short int all;
  struct sym *var;
};

struct unstring_destinations
{
  struct unstring_destinations *next;
  struct sym *var;
  struct sym *delim;
  struct sym *count;
};

struct string_from
{
  struct string_from *next;
  struct sym *var;
  struct sym *delim;
};

struct list
{
  struct list *next;
  void *var;
};

struct parm_list
{
  struct parm_list *next;
  void *var;
  unsigned location;
  short sec_no;
};

struct coord_pair
{
  int lin;
  int col;
};

struct index_to_table_list
{
  struct index_to_table_list *next;
  char *idxname;
  char *tablename;
  char *keyname;
  char seq;			/* '0' = none, '1' = ASCENDING, '2' = DESCENDING */
};

union label_def
{
  struct
  {
    unsigned n:15;
    unsigned defined:1;
    unsigned off;
  }
  l;
  unsigned long x;
};

struct condition
{
  struct sym *sy;		/* implied first operand */
  int oper;			/* operator */
};

/* OCCURS ... DEPENDING ON info */
struct occurs
{
  struct sym *depend;
  int min, max;
};

/* standard field classes */
#define CLASS_NUMERIC 0
#define CLASS_ALPHABETIC 1
#define CLASS_ALPHABETICLOWER 2
#define CLASS_ALPHABETICUPPER 3

/* extended condition flags */
#define COND_UNARY 0x10
#define COND_CLASS 0x20

/* minor token numbers */

/* USAGETOK - USAGENUM */

#define COMP 			0
#define USAGE_COMP1 		1
#define USAGE_COMP2 		2
#define COMP3 			3
#define INDEX 			4
#define USAGE_BINARY_CHAR 	5
#define USAGE_BINARY_SHORT 	6
#define USAGE_BINARY_LONG    	7
#define USAGE_BINARY_DOUBLE  	8
#define USAGE_DISPLAY  		9
#define USAGE_POINTER  		10

/*  DISPLAY  e' um token independente deste grupo */

/* DATE-TIME */
#define DATE 0
#define TIME 1
#define INKEY 2
#define DAY 3
#define DAY_OF_WEEK 4

/* ZERONUM */

#define ZERO 0
#define ZEROS 1
#define ZEROES 2

/* CONDITIONAL */

#define EQUAL 	1
#define LESS  	2
#define GREATER	4
#define GEQ 	5
#define LEQ 	3
#define NEQ 	6

/* DIRECTION */

#define ASCENDING  1
#define DESCENDING 2

/* SPECIAL VARIABLES  */

#define SVAR_RCODE  	"RETURN-CODE"

/* htcobgen.c */
extern char *upcase (char *s, char *buf);
extern struct sym *lookup_symbol (char *s);
extern struct sym *lookup (char *s, int tab);
extern struct sym *install (char *name, int tab, int cloning);
extern struct sym *install_label (char *name);
extern struct lit *install_literal (const char *name);
extern struct sym *lookup_label (struct sym *sy, struct sym *parent);
extern struct sym *lookup_variable (struct sym *sy, struct sym *parent);
extern struct sym *lookup_for_redefines (struct sym *sy);
extern void clear_symtab (void);
extern void clear_offsets (void);
extern char sign_to_char (int digit);
extern struct lit *invert_literal_sign (struct lit *sy);
extern void set_sign_flags (int flags);
extern char *sch_convert (char *s);
extern int is_variable (struct sym *sy);
extern int is_subscripted (struct sym *sy);
extern void emit_lit (char *s, int len);
extern void emit_lit_fill (int c, int len);
extern void gen_init_value (struct lit *sy, int var_len);
extern void stabs_line (void);
extern void pgm_header (char *id);
extern void data_trail (void);
extern int adjust_linkage_vars (int start_offset);
extern void proc_header (int using);
extern void proc_trail (int using);
extern void dump_working (void);
extern void save_field_in_list (struct sym *sy);
extern void save_literal (struct lit *v, int type);
extern void save_named_sect (struct sym *sy);
extern struct lit *save_special_literal (char val, char picc, char *nick);
extern void put_disp_list (struct sym *sy);
extern void push_immed (int i);
extern void push_eax (void);
extern void push_edx (void);
extern void pop_eax (void);
extern void push_ebx (void);
extern void push_at_ebx (struct sym *sy);
extern void push_at_eax (struct sym *sy);
extern void load_at_eax (struct sym *sy);
extern void cleanup_rt_stack (void);
extern void asm_call (char *s);
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
extern void gen_stringcmd (struct string_from *sf, struct sym *sy,
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
extern void create_occurs_info (int min, int max, struct sym *depend);
extern struct refmod *create_refmoded_var (struct sym *sy, struct sym *syoff,
					   struct sym *sylen);
extern void gen_subscripted (struct subref *subs);
extern struct sym *get_variable_item (struct sym *sy);
extern void gen_temp_storage (int size);
extern void adjust_desc_length (struct sym *sy);
extern void value_to_eax (struct sym *sy);
extern void load_address (struct sym *var);
extern void load_location (struct sym *var, char *cpureg);
extern void loadloc_to_eax (struct sym *sy_p);
extern void gen_loadloc (struct sym *sy_p);
extern void set_ptr (struct sym *sy);
extern void gen_loaddesc1 (struct sym *sy, int variable_length);
extern void gen_loaddesc (struct sym *sy);
extern void gen_loadvar (struct sym *sy);
extern void gen_loadval (struct sym *sy);
extern void gen_pushval (struct sym *sy);
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
extern unsigned long gen_at_end (int status);
extern unsigned long gen_testif (void);
extern void gen_not (void);
extern unsigned long gen_andstart (void);
extern unsigned long gen_orstart (void);
extern void gen_dstlabel (unsigned long lbl);
extern unsigned long gen_passlabel (void);
extern unsigned long gen_marklabel (void);
extern void gen_jmplabel (unsigned long lbl);
extern void gen_push_int (struct sym *sy);
extern void gen_perform_test_counter (unsigned long lbl);
extern void gen_perform_times (unsigned long lbl);
extern void gen_perform_thru (struct sym *s1, struct sym *s2);
extern void gen_perform (struct sym *sy);
extern int save_pic_char (char c, int n);
extern void gen_SearchLoopCheck (unsigned long lbl5, struct sym *syidx,
				 struct sym *sytbl);
extern void gen_SearchAllLoopCheck (unsigned long lbl3, struct sym *syidx,
				    struct sym *sytbl, struct sym *syvar,
				    unsigned long lstart, unsigned long lend);
extern void define_special_fields (void);
extern struct sym *define_temp_field (char desired_type, int desired_len);
extern int define_implicit_field (struct sym *sy, struct sym *sykey,
				  int idxlen);
extern void Initialize_SearchAll_Boundaries (struct sym *sy,
					     struct sym *syidx);
extern struct sym *determine_table_index_name (struct sym *sy);
extern void define_field (int level, struct sym *sy);
extern struct sym *alloc_filler (void);
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
extern void dump_symbols (void);
extern void chg_underline (char *s);
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
extern int main (int argc, char *argv[]);

/* htcobol.c */
extern void yywarn (char *fmt, ...);
extern void yyerror (char *fmt, ...);
extern int yyparse (void);

#endif /* _CODEGEN_H_ */
