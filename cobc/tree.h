/*
 * Copyright (C) 2001  Keisuke Nishida
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

#ifndef _TREE_H_
#define _TREE_H_


/*
 * Tree
 */

struct cob_tree_common {
  int litflag;
};

typedef struct sym *cob_tree;

#define COB_TREE(x)		((struct sym *) (x))
#define COB_TREE_TYPE(x)	(COB_TREE (x)->litflag)

extern void print_tree (cob_tree x);

/*
 * Tree list
 */

struct cob_tree_list {
  cob_tree tree;
  struct cob_tree_list *next;
};

typedef struct cob_tree_list *cob_tree_list;

extern cob_tree_list make_list (cob_tree x);
extern cob_tree_list list_append (cob_tree_list l, cob_tree x);


/*
 * Field
 */

struct cob_field {
  struct cob_tree_common common;
  cob_tree next;
  char *name;
  char type;
  int decimals;
};

#define FIELD(x)		((struct cob_field *) (x))
#define FIELD_NEXT(x)		(FIELD (x)->next)
#define FIELD_NAME(x)		(FIELD (x)->name)
#define FIELD_TYPE(x)		(FIELD (x)->type)

/*
 * Literals
 */

struct lit
{
  char litflag;			/* 1 for literals */
  struct lit *next;		/* next in literals list */
  char *name;			/* name (value) of literal */
  char type;
  int decimals;
  unsigned location;		/* data area for literal @lit+n */
  unsigned descriptor;		/* descriptor @lit+n */
  short sec_no;			/* asm section number */
  unsigned char all;
  char *nick;
  int access_mode;		/* ie call_mode */
  int len;			/* length of (possibly including NULLs) string */
};

#define LITERAL(x)		((struct lit *) (x))
#define LITERAL_P(x)		(COB_TREE_TYPE (x) == 1)

extern cob_tree make_literal (char *name);


/*
 * Symbol
 */

#define organization decimals
#define fdesc location
#define direction access_mode
#define call_mode access_mode
#define sort_data value2
#define record pic
#define recordsym redefines
#define filenamevar son
#define alternate brother
#define rec_varying picstr

struct sym
{
  char litflag;			/* 1 for literals, 2 for variables */
  struct sym *next;		/* pointer to next symbol with same hash */
  char *name;			/* symbol (variable) name */
  char type;			/* label or elementary item or group item 
				   9,A,X,B,C=elem; 
				   G=group;
				   F=file; 
				   R=record; 
				   S=screen */
  int decimals;			/* decimal places 
				   or organization (files ) */
  unsigned location;		/* offset of variable in stack area */
  /* or offset file descriptor in data area */
  unsigned descriptor;		/* field descriptor offset in data seg */
  /* or index field descriptor (files) */
  short sec_no;			/* asm section number */
  cob_tree clone;		/* NULL if this symbol is unique
				   otherwise, it must be further qualified */
  int times;			/* occurs times */
  char *picstr;			/* pointer to picture string saved or 
				   pointer to rec_varying (files) */
  cob_tree parent;		/* pointer to parent node (level)
				   pointer to STATUS var (files) */
  cob_tree son;		/* used in field hierarchy
				   for files this is the assign 
				   variable (filename) */
  cob_tree brother;		/* field variable at the same level or
				   alternate key list pointer (for indexed files) */
  cob_tree ix_desc;		/* key variable (in file descriptor)
				   pointer to fdesc (in record) */
  struct scr_info *scr;		/*  screen info in screen items */
  struct report_info *ri;	/*  report info in report items */
  struct sym *value;		/* pointer to literal with initial value */
  struct sym *value2;		/* pointer to first/next key (sort files) */
  union
  {
    struct vrange *vr;		/* pointer to next range of values (88 var) */
    struct refmod *rfm;		/* offset and length of refmod */
  }
  refmod_redef;
  int level;			/* level of field 
				   or ASSIGN TO DISK/PRINTER (files) */
  int access_mode;		/* access mode (files) */
  /* or direction (sort files data ) or call_mode for variables */
  int sign;			/* signal type (0=no signal,1=trailing) */
  int len;			/* length of item */
  char defined;			/* first time defined? */
  unsigned pic;			/* picture offset in data segment */
  int linkage_flg;		/* or record offset in stack (files) */
  cob_tree redefines;	/* points to a redefined field 
				   or record symbol (files) */
  struct
  {
    unsigned int just_r:1;
    unsigned int is_pointer:1;
    unsigned int blank:1;
    unsigned int spec_value:1;
    unsigned int value:1;
    unsigned int sync:1;
    unsigned int in_redefinition:1;
    unsigned int separate_sign:1;
    unsigned int leading_sign:1;
    unsigned int optional:1;
    unsigned int reserved:6;
  }
  flags;
  unsigned char slack;		/* slack bytes inserted */
  struct occurs *occurs;	/* for DEPENDING ON or null if fixed table */
};

#define SYMBOL(x)		((struct sym *) (x))
#define SYMBOL_P(x)		(COB_TREE_TYPE (x) == 0)

extern cob_tree make_symbol (char *name);
extern cob_tree make_filler (void);


/*
 * Storage for subscripted variable references.
 */
struct subref
{
  char litflag;
  cob_tree_list subs;
  cob_tree sym;
};

#define SUBREF(x)	((struct subref *) (x))
#define SUBREF_P(x)	(COB_TREE_TYPE (x) == 2)
#define SUBREF_SUBS(x)	(SUBREF (x)->subs)
#define SUBREF_SYM(x)	(SUBREF (x)->sym)

extern cob_tree make_subref (cob_tree sy, cob_tree_list subs);



/* Node for refmod's */
struct refmod
{
  char litflag;			/* 4 = refmod */
  cob_tree off;		/* offset from normal start address */
  cob_tree sym;		/* pointer to original var: must be at the same relative offset as sym in subref */
  cob_tree len;		/* corrected length */
  int slot;			/* slot in the data section */
};

#define REFMOD(x)	((struct refmod *) (x))
#define REFMOD_P(x)	(COB_TREE_TYPE (x) == 4)

extern cob_tree create_refmoded_var (cob_tree sy, cob_tree syoff, cob_tree sylen);


/*
 * compile-time list for value ranges of 88-level variables.
 * the first range is stored at the "struct sym", with sym->vr
 * being a pointer to the remaining "struct vrange" nodes.
 */
struct vrange
{
  struct vrange *next;		/* pointer to next range of values (88 var) */
  cob_tree value;		/* pointer to literal with initial value */
  cob_tree value2;		/* pointer to first/next key (sort files) */
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
  cob_tree file;		/* file for writing this report */
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
  /* the actual source symbol is in cob_tree->value */
};

/* varying record range and actual size */
struct rec_varying
{
  cob_tree lmin;
  cob_tree lmax;
  cob_tree reclen;
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
  cob_tree sy;
};

/* information required by the 'perform ... varying ... after' statements */
struct perf_info
{
  cob_tree pname1;		/* symbol name */
  cob_tree pname2;		/* symbol name */
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
  cob_tree sname;		/* symbol name */
  unsigned int rounded;		/* rounded option: 0=false, 1=true */
  struct math_var *next;
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
  int foreground;
  int background;
  cob_tree from;
  cob_tree to;
  int label;
};

struct converting_struct
{
  cob_tree fromvar;
  cob_tree tovar;
  struct inspect_before_after *before_after;
};

struct tallying_list
{
  struct tallying_list *next;
  struct tallying_for_list *tflist;
  cob_tree count;
};

struct tallying_for_list
{
  struct tallying_for_list *next;
  int options;
  cob_tree forvar;
  struct inspect_before_after *before_after;
};

struct replacing_list
{
  struct replacing_list *next;
  int options;
  cob_tree byvar;
  struct replacing_by_list *replbylist;
  struct inspect_before_after *before_after;
};

struct replacing_by_list
{
  struct replacing_by_list *next;
  cob_tree replvar;
  cob_tree byvar;
  struct inspect_before_after *before_after;
};

struct inspect_before_after
{
  cob_tree before;
  cob_tree after;
};


struct alternate_list
{
  struct alternate_list *next;
  cob_tree key;
  int duplicates;
};

struct unstring_delimited
{
  struct unstring_delimited *next;
  int all;
  cob_tree var;
};

struct unstring_destinations
{
  struct unstring_destinations *next;
  cob_tree var;
  cob_tree delim;
  cob_tree count;
};

struct string_from
{
  struct string_from *next;
  cob_tree var;
  cob_tree delim;
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
  char seq;		/* '0' = none, '1' = ASCENDING, '2' = DESCENDING */
};

struct condition
{
  cob_tree sy;		/* implied first operand */
  int oper;			/* operator */
};

/* OCCURS ... DEPENDING ON info */
struct occurs
{
  cob_tree depend;
  int min, max;
};


/*
 * Expression
 */

struct expr
{
  char litflag;			/* 5 for expr */
  char op;
  cob_tree left;
  cob_tree right;
};

#define EXPR(x)		((struct expr *) (x))
#define EXPR_P(x)	(COB_TREE_TYPE (x) == 5)
#define EXPR_OP(x)	(EXPR (x)->op)
#define EXPR_LEFT(x)	(EXPR (x)->left)
#define EXPR_RIGHT(x)	(EXPR (x)->right)

extern cob_tree make_expr (cob_tree left, char op, cob_tree right);


/*
 * Condition
 */

enum cond_type {
  COND_EQ,			/* x = y */
  COND_GT,			/* x > y */
  COND_LT,			/* x < y */
  COND_GE,			/* x >= y */
  COND_LE,			/* x <= y */
  COND_NE,			/* x != y */
  COND_NUMERIC,			/* x is NUMERIC */
  COND_ALPHABETIC,		/* x is ALPHABETIC */
  COND_LOWER,			/* x is ALPHABETIC-LOWER */
  COND_UPPER,			/* x is ALPHABETIC-UPPER */
  COND_POSITIVE,		/* x is POSITIVE */
  COND_NEGATIVE,		/* x is NEGATIVE */
  COND_ZERO,			/* x is ZERO */
  COND_NOT,			/* not x */
  COND_AND,			/* x and y */
  COND_OR,			/* x or y */
  COND_VAR			/* 88 variable */
};

struct cond
{
  char litflag;			/* 8 for condition */
  enum cond_type type;
  cob_tree x;
  cob_tree y;
};

#define COND(x)		((struct cond *) (x))
#define COND_P(x)	(COB_TREE_TYPE (x) == 8)
#define COND_TYPE(c)	(COND (c)->type)
#define COND_X(c)	(COND (c)->x)
#define COND_Y(c)	(COND (c)->y)

#define COND_IS_UNARY(c) (COND_Y (c) == 0)

extern cob_tree make_cond (cob_tree x, enum cond_type type, cob_tree y);
extern cob_tree make_unary_cond (cob_tree x, enum cond_type type);

#endif /* _TREE_H_ */
