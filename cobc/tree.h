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

#define cobc_tag_const		0
#define cobc_tag_integer	1
#define cobc_tag_index		2
#define cobc_tag_literal	8
#define cobc_tag_field		9
#define cobc_tag_subref		10
#define cobc_tag_refmod		11
#define cobc_tag_expr		12
#define cobc_tag_cond		13
#define cobc_tag_pair		14
#define cobc_tag_file_name	15
#define cobc_tag_label_name	16
#define cobc_tag_call		17
#define cobc_tag_sequence	18
#define cobc_tag_perform	19
#define cobc_tag_assign		20
#define cobc_tag_register	21
#define cobc_tag_if		22
#define cobc_tag_evaluate	23

#define USAGE_DISPLAY	'9'
#define USAGE_BINARY	'B'
#define USAGE_PACKED	'C'
#define USAGE_FLOAT	'U'
#define USAGE_INDEX	'I'

#define COBC_BEFORE		0
#define COBC_AFTER		1

/* source location */
struct cobc_location {
  char *file;
  int line;
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
extern struct cobc_list *list_add (struct cobc_list *l, void *x);
extern struct cobc_list *list_append (struct cobc_list *l1, struct cobc_list *l2);
extern struct cobc_list *list_reverse (struct cobc_list *l);
extern int list_length (struct cobc_list *l);


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
#define COBC_TREE_CLASS(x)	(COBC_TREE (x)->class)

#ifdef COB_DEBUG
#define COBC_TREE_CAST(tg,ty,x)						\
  ({									\
    cobc_tree _x = (x);							\
    if (!_x || COBC_TREE_TAG (_x) != tg)				\
      {									\
	printf ("%s:%d: invalid type cast from `%s'\n",			\
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
extern cobc_tree cobc_status;
extern cobc_tree cobc_int0;
extern cobc_tree cobc_int1;

extern void init_constants (void);


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
  int size;			/* byte size */
  int offset;			/* byte offset from the top (ie, 01 field) */
  int level;			/* level number */
  int occurs;			/* OCCURS */
  int indexes;			/* the number of parents who have OCCURS */
  char usage;			/* USAGE IS */
  char category;		/* 9,A,X,E,N,M */
  char *cname;			/* the name used in C files */
  cobc_tree file;		/* file name associated in FD section */
  cobc_tree value;		/* VALUE */
  cobc_tree cond;		/* condition for level 88 item */
  struct cobc_word *word;	/* the word of this field */
  struct cobc_list *index_list;	/* INDEXED BY */
  struct cobc_field *parent;	/* upper level field (NULL for 01 fields) */
  struct cobc_field *children;	/* top of lower level fields */
  struct cobc_field *sister;	/* fields in the same level */
  struct cobc_field *redefines;	/* REDEFIENS */
  struct cobc_picture {
    int size;			/* byte size */
    char *str;			/* picture string */
    char category;		/* field category */
    char decimals;		/* the number of decimal digits */
    char have_sign;		/* if the field may hold sign */
  } *pic;			/* PICTURE */
  struct {
    int blank_zero    : 1;	/* BLANK WHEN ZERO */
    int justified     : 1;	/* JUSTIFIED RIGHT */
    int sign_leading  : 1;	/* SIGN IS LEADING */
    int sign_separate : 1;	/* SIGN IS SEPARATE */
    int synchronized  : 1;	/* SYNCHRONIZED */
    int have_occurs   : 1;	/* if OCCURS clause exists */
    int referenced    : 1;	/* if any reference modification exists */
  } f;
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
#define COBC_INDEX_NAME_P(x) \
  (COBC_FIELD_P (x) && COBC_FIELD (x)->usage == USAGE_INDEX)

extern cobc_tree make_field (struct cobc_word *word);
extern cobc_tree make_filler (void);
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
 * File name
 */

struct cobc_file_name {
  struct cobc_tree_common common;
  struct cobc_word *word;
  char *cname;
  struct cobc_field *record;	/* record descriptor */
  char *assign;			/* ASSIGN */
  int optional;			/* OPTIONAL */
  int organization;		/* ORGANIZATION */
  int access_mode;		/* ACCESS MODE */
  struct cobc_field *status;	/* FILE STATUS */
  struct cobc_field *key;	/* RELATIVE/RECORD KEY */
  struct cobc_alt_key {
    struct cobc_field *key;
    int duplicates;
    struct cobc_alt_key *next;
  } *alt_key_list;		/* ALTERNATE RECORD KEY list */
};

#define COBC_FILE_NAME(x)	(COBC_TREE_CAST (cobc_tag_file_name, struct cobc_file_name, x))
#define COBC_FILE_NAME_P(x)	(COBC_TREE_TAG (x) == cobc_tag_file_name)

extern cobc_tree make_file_name (struct cobc_word *word);


/*
 * Label name
 */

struct cobc_label_name {
  struct cobc_tree_common common;
  const char *cname;
  struct cobc_word *word;
  struct cobc_word *in_word;
  struct cobc_label_name *section;
  struct cobc_list *children;
};

#define COBC_LABEL_NAME(x)	(COBC_TREE_CAST (cobc_tag_label_name, struct cobc_label_name, x))
#define COBC_LABEL_NAME_P(x)	(COBC_TREE_TAG (x) == cobc_tag_label_name)

extern cobc_tree make_label_name_nodef (struct cobc_word *word, struct cobc_word *in_word);
extern cobc_tree make_label_name (struct cobc_word *word);
extern void finalize_label_name (struct cobc_label_name *p);


/*
 * Register
 */

struct cobc_register {
  struct cobc_tree_common common;
  int id;
};

#define COBC_REGISTER(x)	(COBC_TREE_CAST (cobc_tag_register, struct cobc_register, x))
#define COBC_REGISTER_P(x)	(COBC_TREE_TAG (x) == cobc_tag_register)

extern cobc_tree make_register ();


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
 * Condition
 */

enum cobc_cond_type {
  COBC_COND_EQ,			/* x = y */
  COBC_COND_GT,			/* x > y */
  COBC_COND_LT,			/* x < y */
  COBC_COND_GE,			/* x >= y */
  COBC_COND_LE,			/* x <= y */
  COBC_COND_NE,			/* x != y */
  COBC_COND_NUMERIC,		/* x is NUMERIC */
  COBC_COND_ALPHABETIC,		/* x is ALPHABETIC */
  COBC_COND_LOWER,		/* x is ALPHABETIC-LOWER */
  COBC_COND_UPPER,		/* x is ALPHABETIC-UPPER */
  COBC_COND_POSITIVE,		/* x is POSITIVE */
  COBC_COND_NEGATIVE,		/* x is NEGATIVE */
  COBC_COND_ZERO,		/* x is ZERO */
  COBC_COND_CLASS,		/* x is class-name */
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

#define COND(x)		(COBC_TREE_CAST (cobc_tag_cond, struct cobc_cond, x))
#define COND_P(x)	(COBC_TREE_TAG (x) == cobc_tag_cond)
#define COND_TYPE(c)	(COND (c)->type)
#define COND_LEFT(c)	(COND (c)->left)
#define COND_RIGHT(c)	(COND (c)->right)

#define COND_IS_UNARY(c) (COND_RIGHT (c) == 0)

extern cobc_tree make_cond (cobc_tree x, enum cobc_cond_type type, cobc_tree y);
extern cobc_tree make_unary_cond (cobc_tree x, enum cobc_cond_type type);


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
  cobc_tree init;
  cobc_tree step;
  cobc_tree cond;
};

#define COBC_PERFORM(x)		(COBC_TREE_CAST (cobc_tag_perform, struct cobc_perform, x))
#define COBC_PERFORM_P(x)	(COBC_TREE_TAG (x) == cobc_tag_perform)

extern cobc_tree make_perform (int type);


/*
 * Call
 */

struct cobc_call {
  struct cobc_tree_common common;
  int tag;
  int argc;
  cobc_tree argv[3];
};

#define COBC_CALL(x)		(COBC_TREE_CAST (cobc_tag_call, struct cobc_call, x))
#define COBC_CALL_P(x)		(COBC_TREE_TAG (x) == cobc_tag_call)

extern cobc_tree make_call_0 (int tag);
extern cobc_tree make_call_1 (int tag, cobc_tree a1);
extern cobc_tree make_call_2 (int tag, cobc_tree a1, cobc_tree a2);
extern cobc_tree make_call_3 (int tag, cobc_tree a1, cobc_tree a2, cobc_tree a3);


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


struct inspect_item {
  int type;
  cobc_tree sy1;
  cobc_tree sy2;
  struct cobc_list *list;
};

extern struct inspect_item *make_inspect_item (int type, cobc_tree sy1, cobc_tree sy2, struct cobc_list *list);

struct string_item {
  int type;
  cobc_tree sy;
};

extern struct string_item *make_string_item (int type, cobc_tree sy);


/*
 * CALL
 */

#define COBC_CALL_BY_REFERENCE	1
#define COBC_CALL_BY_CONTENT	2
#define COBC_CALL_BY_VALUE	3

struct call_item {
  int mode;
  cobc_tree var;
};

extern struct call_item *make_call_item (int mode, cobc_tree var);


/*
 * Word table
 */

struct cobc_word {
  char *name;			/* word name */
  int count;			/* the number of words with the same name */
  cobc_tree item;		/* tree item associated with this word */
  struct cobc_word *link;	/* next word with the same name */
  struct cobc_word *next;	/* next word with the same hash value */
};

extern struct cobc_word *make_word (char *name);
extern struct cobc_word *set_word_item (struct cobc_word *word, cobc_tree item);
extern struct cobc_word *lookup_user_word (const char *name);
extern struct cobc_word *lookup_qualified_word (struct cobc_word *word, struct cobc_field *parent);
extern void init_word_table (void);

#endif /* _TREE_H_ */
