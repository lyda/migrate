/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#define COBC_USAGE_BINARY	'2'
#define COBC_USAGE_INDEX	'6'
#define COBC_USAGE_DISPLAY	'9'

#define COBC_BEFORE		0
#define COBC_AFTER		1

enum cobc_tag {
  cobc_tag_const,
  cobc_tag_integer,
  cobc_tag_index,
  cobc_tag_literal,
  cobc_tag_field,
  cobc_tag_subref,
  cobc_tag_refmod,
  cobc_tag_expr,
  cobc_tag_cond,
  cobc_tag_pair,
  cobc_tag_file,
  cobc_tag_label,
  cobc_tag_call,
  cobc_tag_sequence,
  cobc_tag_perform,
  cobc_tag_assign,
  cobc_tag_if,
  cobc_tag_evaluate,
  cobc_tag_predefined,
  cobc_tag_class,
  cobc_tag_builtin,
};


/*
 * List
 */

struct cobc_list {
  void *item;
  struct cobc_list *next;
};

extern struct cobc_list *cons (void *x, struct cobc_list *l);
extern struct cobc_list *list (void *x);
extern struct cobc_list *list_last (struct cobc_list *l);
extern struct cobc_list *list_add (struct cobc_list *l, void *x);
extern struct cobc_list *list_append (struct cobc_list *l1, struct cobc_list *l2);
extern struct cobc_list *list_reverse (struct cobc_list *l);
extern int list_length (struct cobc_list *l);


/*
 * Location
 */

struct cobc_location {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
  char *text;
};

extern struct cobc_location cobc_location;


/*
 * Tree
 */

struct cobc_tree_common {
  char tag;
  char class;			/* A,9,X,N,1 */
  struct cobc_location loc;	/* source location */
};

typedef struct cobc_tree_common *cobc_tree;

#define COBC_TREE(x)		((struct cobc_tree_common *) (x))
#define COBC_TREE_TAG(x)	(COBC_TREE (x)->tag)
#define COBC_TREE_LOC(x)	(COBC_TREE (x)->loc)
#define COBC_TREE_CLASS(x)	(COBC_TREE (x)->class)

#ifdef COB_DEBUG
#define COBC_TREE_CAST(tg,ty,x)						\
  ({									\
    cobc_tree _x = (x);							\
    if (!_x || COBC_TREE_TAG (_x) != tg)				\
      {									\
	fprintf (stderr, "%s:%d: invalid type cast from `%s'\n",	\
		__FILE__, __LINE__, tree_to_string (_x));		\
	abort ();							\
      }									\
    ((ty *) (_x));							\
  })
#else
#define COBC_TREE_CAST(x,tg,ty)	((ty *) (x))
#endif

extern char *tree_to_string (cobc_tree x);


/*
 * Constants
 */

extern cobc_tree cobc_any;
extern cobc_tree cobc_true;
extern cobc_tree cobc_false;
extern cobc_tree cobc_zero;
extern cobc_tree cobc_space;
extern cobc_tree cobc_low;
extern cobc_tree cobc_high;
extern cobc_tree cobc_quote;
extern cobc_tree cobc_dt;
extern cobc_tree cobc_status;
extern cobc_tree cobc_return_code;
extern cobc_tree cobc_switch[];
extern cobc_tree cobc_int0;
extern cobc_tree cobc_int1;
extern cobc_tree cobc_int2;

extern struct cobc_label *cobc_default_error_handler;
extern struct cobc_label *cobc_standard_error_handler;

struct cobc_const {
  struct cobc_tree_common common;
  char *val;
};

#define COBC_CONST(x)		(COBC_TREE_CAST (cobc_tag_const, struct cobc_const, x))
#define COBC_CONST_P(x)		(COBC_TREE_TAG (x) == cobc_tag_const)

extern void init_constants (void);


/*
 * Builtin
 */

struct cobc_builtin {
  struct cobc_tree_common common;
  int id;
};

#define COBC_BUILTIN(x)		(COBC_TREE_CAST (cobc_tag_builtin, struct cobc_builtin, x))
#define COBC_BUILTIN_P(x)	(COBC_TREE_TAG (x) == cobc_tag_builtin)

extern cobc_tree make_builtin (int id);


/*
 * Integer
 */

struct cobc_integer {
  struct cobc_tree_common common;
  int val;
};

#define COBC_INTEGER(x)		(COBC_TREE_CAST (cobc_tag_integer, struct cobc_integer, x))
#define COBC_INTEGER_P(x)	(COBC_TREE_TAG (x) == cobc_tag_integer)

extern cobc_tree make_integer (int val);


/*
 * Index
 */

struct cobc_index {
  struct cobc_tree_common common;
  cobc_tree val;
};

#define COBC_INDEX(x)		(COBC_TREE_CAST (cobc_tag_index, struct cobc_index, x))
#define COBC_INDEX_P(x)		(COBC_TREE_TAG (x) == cobc_tag_index)

extern cobc_tree make_index (cobc_tree val);


/*
 * Pair
 */

struct cobc_pair {
  struct cobc_tree_common common;
  void *x;
  void *y;
};

#define COBC_PAIR(x)		(COBC_TREE_CAST (cobc_tag_pair, struct cobc_pair, x))
#define COBC_PAIR_P(x)		(COBC_TREE_TAG (x) == cobc_tag_pair)

extern cobc_tree make_pair (void *x, void *y);


/*
 * Literal
 */

struct cobc_literal {
  struct cobc_tree_common common;
  int all;
  int size;
  int sign;
  int decimals;
  unsigned char *str;
};

#define COBC_LITERAL(x)		(COBC_TREE_CAST (cobc_tag_literal, struct cobc_literal, x))
#define COBC_LITERAL_P(x)	(COBC_TREE_TAG (x) == cobc_tag_literal)

extern cobc_tree make_numeric_literal (int sign, unsigned char *digits, int decimals);
extern cobc_tree make_nonnumeric_literal (unsigned char *str);
extern long long literal_to_int (struct cobc_literal *p);


/*
 * Field
 */

struct cobc_field {
  struct cobc_tree_common common;
  int size;			/* field size */
  int memory_size;		/* memory size */
  int offset;			/* byte offset from the top (ie, 01 field) */
  int level;			/* level number */
  int occurs;			/* OCCURS */
  int occurs_min;
  int indexes;			/* the number of parents who have OCCURS */
  char usage;			/* USAGE IS */
  char *cname;			/* the name used in C files */
  cobc_tree value;		/* VALUE */
  cobc_tree occurs_depending;	/* OCCURS ... DEPENDING ON */
  struct cobc_word *word;	/* the word of this field */
  struct cobc_list *values;	/* VALUES used by level 88 item */
  struct cobc_list *index_list;	/* INDEXED BY */
  struct cobc_field *parent;	/* upper level field (NULL for 01 fields) */
  struct cobc_field *children;	/* top of lower level fields */
  struct cobc_field *sister;	/* fields in the same level */
  struct cobc_field *redefines;	/* REDEFIENS */
  struct cobc_field *rename_thru; /* RENAMES THRU */
  struct cobc_file *file;	/* file name associated in FD section */
  struct cobc_key {
    int dir;			/* ASCENDING or DESCENDING */
    cobc_tree key;		/* KEY */
    cobc_tree ref;		/* reference used in SEARCH ALL */
    cobc_tree val;		/* value to be compared in SEARCH ALL */
  } *keys;
  int nkeys;			/* the number of keys */
  struct cobc_picture {
    int size;			/* byte size */
    char *str;			/* picture string */
    char category;		/* field category */
    char digits;		/* the number of digit places */
    char decimals;		/* the number of decimal digits */
    char have_sign;		/* have `S' */
  } *pic;			/* PICTURE */
  struct {
    int external      : 1;	/* EXTERNAL */
    int blank_zero    : 1;	/* BLANK WHEN ZERO */
    int justified     : 1;	/* JUSTIFIED RIGHT */
    int sign_leading  : 1;	/* SIGN IS LEADING */
    int sign_separate : 1;	/* SIGN IS SEPARATE */
    int synchronized  : 1;	/* SYNCHRONIZED */
    int have_occurs   : 1;	/* if OCCURS clause exists */
    int in_redefines  : 1;	/* if any parent has REDEFINES clause */
    int used          : 1;	/* if used more than once */
    int referenced    : 1;	/* if any reference modification exists */
    int screen        : 1;	/* if defined in SCREEN SECTION */
  } f;
  /* screen parameters */
  cobc_tree screen_line;
  cobc_tree screen_column;
  struct cobc_field *screen_from;
  struct cobc_field *screen_to;
  long screen_flag;		/* flags used in SCREEN SECTION */
};

#ifdef COB_DEBUG
#define COBC_FIELD(x)						\
  ({								\
    cobc_tree __x = (x);					\
    if (COBC_REFMOD_P (__x))					\
      __x = ((struct cobc_refmod *) __x)->field;		\
    if (COBC_SUBREF_P (__x))					\
      __x = ((struct cobc_subref *) __x)->field;		\
    COBC_TREE_CAST (cobc_tag_field, struct cobc_field, __x);	\
  })
#else
#define COBC_FIELD(x)		((struct cobc_field *) (x))
#endif
#define COBC_FIELD_P(x)		(COBC_TREE_TAG (x) == cobc_tag_field)
#define COBC_FILLER_P(x) \
  (COBC_FIELD_P (x) && COBC_FIELD (x)->cname[0] == '$')
#define COBC_INDEX_NAME_P(x) \
  (COBC_FIELD_P (x) && COBC_FIELD (x)->usage == COBC_USAGE_INDEX)

extern cobc_tree make_field (struct cobc_word *word);
extern cobc_tree make_field_3 (struct cobc_word *word, char *pic, int usage);
extern cobc_tree make_filler (void);
extern struct cobc_field *field_founder (struct cobc_field *p);
extern int field_used_any_parent (struct cobc_field *p);
extern int field_used_any_child (struct cobc_field *p);
extern void finalize_field_tree (struct cobc_field *p);

struct cobc_picture *make_picture (void);


/*
 * Subscripted variable
 */

struct cobc_subref {
  struct cobc_tree_common common;
  cobc_tree field;
  struct cobc_list *subs;
};

#define COBC_SUBREF(x)		(COBC_TREE_CAST (cobc_tag_subref, struct cobc_subref, x))
#define COBC_SUBREF_P(x)	(COBC_TREE_TAG (x) == cobc_tag_subref)

extern cobc_tree make_subref (cobc_tree field, struct cobc_list *subs);


/*
 * Reference modifier
 */

struct cobc_refmod {
  struct cobc_tree_common common;
  cobc_tree field;
  cobc_tree offset;
  cobc_tree length;
};

#define COBC_REFMOD(x)		(COBC_TREE_CAST (cobc_tag_refmod, struct cobc_refmod, x))
#define COBC_REFMOD_P(x)	(COBC_TREE_TAG (x) == cobc_tag_refmod)

extern cobc_tree make_refmod (cobc_tree field, cobc_tree offset, cobc_tree length);


/*
 * Predefined name
 */

struct cobc_predefined {
  struct cobc_tree_common common;
  struct cobc_list *words;
};

#define COBC_PREDEFINED(x)	(COBC_TREE_CAST (cobc_tag_predefined, struct cobc_predefined, x))
#define COBC_PREDEFINED_P(x)	(COBC_TREE_TAG (x) == cobc_tag_predefined)

extern cobc_tree make_predefined (struct cobc_list *words);


/*
 * File name
 */

struct cobc_file {
  struct cobc_tree_common common;
  struct cobc_word *word;
  char *cname;
  /* SELECT */
  cobc_tree assign;		/* ASSIGN */
  int optional;			/* OPTIONAL */
  int organization;		/* ORGANIZATION */
  int access_mode;		/* ACCESS MODE */
  cobc_tree file_status;	/* FILE STATUS */
  cobc_tree key;		/* RELATIVE/RECORD KEY */
  struct cobc_alt_key {
    cobc_tree key;
    int duplicates;
    struct cobc_alt_key *next;
  } *alt_key_list;		/* ALTERNATE RECORD KEY list */
  /* FD/SD */
  struct cobc_field *record;	/* record descriptor */
  int record_min, record_max;	/* RECORD CONTAINS */
  cobc_tree record_depending;	/* RECORD DEPENDING */
  /* STANDARD ERROR PROCEDURE */
  struct cobc_label *handler;	/* error handler */
};

#define COBC_FILE(x)	(COBC_TREE_CAST (cobc_tag_file, struct cobc_file, x))
#define COBC_FILE_P(x)	(COBC_TREE_TAG (x) == cobc_tag_file)

extern cobc_tree make_file (struct cobc_word *word);


/*
 * Label name
 */

struct cobc_label {
  struct cobc_tree_common common;
  const char *cname;
  struct cobc_word *word;
  struct cobc_word *in_word;
  struct cobc_label *section;
  struct cobc_list *children;
  char need_begin;
  char need_return;
};

#define COBC_LABEL(x)		(COBC_TREE_CAST (cobc_tag_label, struct cobc_label, x))
#define COBC_LABEL_P(x)		(COBC_TREE_TAG (x) == cobc_tag_label)

extern cobc_tree make_label_nodef (struct cobc_word *word, struct cobc_word *in_word);
extern cobc_tree make_label (struct cobc_word *word);
extern void finalize_label (struct cobc_label *p);


/*
 * Expression
 */

struct cobc_expr {
  struct cobc_tree_common common;
  char op;
  cobc_tree left;
  cobc_tree right;
};

#define COBC_EXPR(x)		(COBC_TREE_CAST (cobc_tag_expr, struct cobc_expr, x))
#define COBC_EXPR_P(x)		(COBC_TREE_TAG (x) == cobc_tag_expr)

extern cobc_tree make_expr (cobc_tree left, char op, cobc_tree right);
extern int is_numeric (cobc_tree x);


/*
 * Class
 */

struct cobc_class {
  struct cobc_tree_common common;
  char *cname;
  struct cobc_list *list;
};

#define COBC_CLASS(x)		(COBC_TREE_CAST (cobc_tag_class, struct cobc_class, x))
#define COBC_CLASS_P(x)		(COBC_TREE_TAG (x) == cobc_tag_class)

extern cobc_tree make_class (struct cobc_word *word, struct cobc_list *list);


/*
 * Condition
 */

enum cobc_cond_type {
  COBC_COND_EQ,			/* x = y */
  COBC_COND_GT,			/* x > y */
  COBC_COND_LT,			/* x < y */
  COBC_COND_GE,			/* x >= y */
  COBC_COND_LE,			/* x <= y */
  COBC_COND_NE,			/* x != y */
  COBC_COND_CLASS,		/* x is class */
  COBC_COND_NOT,		/* not x */
  COBC_COND_AND,		/* x and y */
  COBC_COND_OR,			/* x or y */
};

struct cobc_cond
{
  struct cobc_tree_common common;
  enum cobc_cond_type type;
  cobc_tree left;
  cobc_tree right;
};

#define COBC_COND(x)		(COBC_TREE_CAST (cobc_tag_cond, struct cobc_cond, x))
#define COBC_COND_P(x)	(COBC_TREE_TAG (x) == cobc_tag_cond)

extern cobc_tree make_cond (cobc_tree x, enum cobc_cond_type type, cobc_tree y);
extern cobc_tree make_negative (cobc_tree x);


/*
 * If
 */

struct cobc_if {
  struct cobc_tree_common common;
  cobc_tree test;
  cobc_tree stmt1;
  cobc_tree stmt2;
};

#define COBC_IF(x)		(COBC_TREE_CAST (cobc_tag_if, struct cobc_if, x))
#define COBC_IF_P(x)		(COBC_TREE_TAG (x) == cobc_tag_if)

extern cobc_tree make_if (cobc_tree test, cobc_tree stmt1, cobc_tree stmt2);


/*
 * Evaluate
 */

struct cobc_evaluate {
  struct cobc_tree_common common;
  struct cobc_list *subject_list;
  struct cobc_list *case_list;
};

#define COBC_EVALUATE(x)	(COBC_TREE_CAST (cobc_tag_evaluate, struct cobc_evaluate, x))
#define COBC_EVALUATE_P(x)	(COBC_TREE_TAG (x) == cobc_tag_evaluate)

extern cobc_tree make_evaluate (struct cobc_list *subject_list, struct cobc_list *case_list);


/*
 * Perform
 */

#define COBC_PERFORM_EXIT	0
#define COBC_PERFORM_ONCE	1
#define COBC_PERFORM_TIMES	2
#define COBC_PERFORM_UNTIL	3

struct cobc_perform {
  struct cobc_tree_common common;
  int type;
  int test;
  cobc_tree body;
  cobc_tree data;
  struct cobc_perform_varying {
    cobc_tree name;
    cobc_tree from;
    cobc_tree by;
    cobc_tree until;
    struct cobc_perform_varying *next;
  } *varying;
};

#define COBC_PERFORM(x)		(COBC_TREE_CAST (cobc_tag_perform, struct cobc_perform, x))
#define COBC_PERFORM_P(x)	(COBC_TREE_TAG (x) == cobc_tag_perform)

extern cobc_tree make_perform (int type);
extern cobc_tree make_perform_once (cobc_tree body);
extern void add_perform_varying (struct cobc_perform *perf, cobc_tree name, cobc_tree from, cobc_tree by, cobc_tree until);


/*
 * Call
 */

struct cobc_call {
  struct cobc_tree_common common;
  const char *name;
  void (*func) ();
  int argc;
  void *argv[4];
};

#define COBC_CALL(x)		(COBC_TREE_CAST (cobc_tag_call, struct cobc_call, x))
#define COBC_CALL_P(x)		(COBC_TREE_TAG (x) == cobc_tag_call)

extern cobc_tree make_call (const char *name, void (*func)(), int argc, void *a1, void *a2, void *a3, void *a4);

#define make_call_0(f)			make_call (f, 0, 0, 0, 0, 0, 0)
#define make_call_1(f,a1)		make_call (f, 0, 1, a1, 0, 0, 0)
#define make_call_1_list(f,a1,ls)	make_call (f, 0, -1, a1, ls, 0, 0)
#define make_call_2(f,a1,a2)		make_call (f, 0, 2, a1, a2, 0, 0)
#define make_call_3(f,a1,a2,a3)		make_call (f, 0, 3, a1, a2, a3, 0)
#define make_call_4(f,a1,a2,a3,a4)	make_call (f, 0, 4, a1, a2, a3, a4)

#define make_inline_0(f)		make_call (0, f, 0, 0, 0, 0, 0)
#define make_inline_1(f,a1)		make_call (0, f, 1, a1, 0, 0, 0)
#define make_inline_2(f,a1,a2)		make_call (0, f, 2, a1, a2, 0, 0)
#define make_inline_3(f,a1,a2,a3)	make_call (0, f, 3, a1, a2, a3, 0)
#define make_inline_4(f,a1,a2,a3,a4)	make_call (0, f, 4, a1, a2, a3, a4)


/*
 * Assignment
 */

struct cobc_assign {
  struct cobc_tree_common common;
  cobc_tree field;
  cobc_tree value;
  int rounded;
};

#define COBC_ASSIGN(x)		(COBC_TREE_CAST (cobc_tag_assign, struct cobc_assign, x))
#define COBC_ASSIGN_P(x)	(COBC_TREE_TAG (x) == cobc_tag_assign)

extern cobc_tree make_assign (cobc_tree field, cobc_tree value, int rounded);
extern cobc_tree make_op_assign (cobc_tree field, char op, cobc_tree value);


/*
 * Sequence
 */

struct cobc_sequence {
  struct cobc_tree_common common;
  struct cobc_list *list;
  int save_status;
};

#define COBC_SEQUENCE(x)	(COBC_TREE_CAST (cobc_tag_sequence, struct cobc_sequence, x))
#define COBC_SEQUENCE_P(x)	(COBC_TREE_TAG (x) == cobc_tag_sequence)

extern cobc_tree make_sequence (struct cobc_list *list);
extern cobc_tree make_status_sequence (struct cobc_list *list);


/*
 * Word table
 */

struct cobc_word {
  char *name;			/* word name */
  int count;			/* the number of words with the same name */
  int error;			/* set to 1 if error displayed */
  cobc_tree item;		/* tree item associated with this word */
  struct cobc_word *link;	/* next word with the same name */
  struct cobc_word *next;	/* next word with the same hash value */
};

extern struct cobc_word *make_word (const char *name);
extern struct cobc_word *set_word_item (struct cobc_word *word, cobc_tree item);
extern struct cobc_word *lookup_user_word (const char *name);
extern struct cobc_word *lookup_qualified_word (struct cobc_word *word, struct cobc_field *parent);
extern void init_word_table (void);


/*
 * General parameter
 */

#define COBC_CALL_BY_REFERENCE	1
#define COBC_CALL_BY_CONTENT	2
#define COBC_CALL_BY_LENGTH	3
#define COBC_CALL_BY_VALUE	4

struct cobc_parameter {
  int type;
  cobc_tree x;
  cobc_tree y;
};

extern struct cobc_parameter *make_parameter (int type, cobc_tree x, cobc_tree y);
#define make_parameter_1(type,x) make_parameter (type, x, 0)

#endif /* _TREE_H_ */
