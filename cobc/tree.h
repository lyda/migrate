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

#define COBC_CALL_BY_REFERENCE	1
#define COBC_CALL_BY_CONTENT	2
#define COBC_CALL_BY_LENGTH	3
#define COBC_CALL_BY_VALUE	4

#define COBC_BEFORE		0
#define COBC_AFTER		1


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
 * Word table
 */

struct cobc_word {
  char *name;			/* word name */
  int count;			/* the number of words with the same name */
  int error;			/* set to 1 if error displayed */
  struct cobc_list *items;	/* objects associated with this word */
  struct cobc_word *next;	/* next word with the same hash value */
};

extern struct cobc_word *lookup_word (const char *name);
extern void init_word_table (void);


/*
 * Tree
 */

enum cobc_tag {
  /* primitives */
  cobc_tag_const,		/* constant value */
  cobc_tag_integer,		/* native integer */
  cobc_tag_string,		/* native string */
  cobc_tag_literal,		/* numeric/alphanumeric literal */
  cobc_tag_decimal,		/* decimal number */
  cobc_tag_field,		/* user-defined variable */
  cobc_tag_file,		/* file description */
  /* expressions */
  cobc_tag_reference,		/* reference to a field, file, or label */
  cobc_tag_binary_op,		/* binary operation */
  cobc_tag_funcall,		/* run-time function call */
  cobc_tag_cast_int32,		/* cast to int32 */
  /* statements */
  cobc_tag_location,		/* source location */
  cobc_tag_label,		/* label statement */
  cobc_tag_if,			/* IF statement */
  cobc_tag_perform,		/* PERFORM statement */
  cobc_tag_sequence,		/* multiple statements */
  /* miscellaneous */
  cobc_tag_class,		/* CLASS definition */
  cobc_tag_builtin,
  cobc_tag_parameter,
};

struct cobc_tree_common {
  enum cobc_tag tag;
  char class;			/* A,9,X,N,1 */
  char *source_file;
  int source_line;
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
	fprintf (stderr,						\
		 "%s:%d: invalid type cast from `%s' at %d in %s\n",	\
		 __FILE__, __LINE__, _x ? tree_name (_x) : "null",	\
		 cobc_source_line, cobc_source_file);			\
	abort ();							\
      }									\
    ((ty *) (_x));							\
  })
#else
#define COBC_TREE_CAST(x,tg,ty)	((ty *) (x))
#endif

extern char *tree_name (cobc_tree x);

extern cobc_tree make_location (char *file, int line);


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
extern cobc_tree cobc_return_code;
extern cobc_tree cobc_switch[];
extern cobc_tree cobc_int0;
extern cobc_tree cobc_int1;
extern cobc_tree cobc_int2;

extern struct cobc_label *cobc_main_label;
extern struct cobc_label *cobc_standard_error_handler;

struct cobc_const {
  struct cobc_tree_common common;
  char *val;
};

#define COBC_CONST(x)		(COBC_TREE_CAST (cobc_tag_const, struct cobc_const, x))
#define COBC_CONST_P(x)		(COBC_TREE_TAG (x) == cobc_tag_const)

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
 * String
 */

struct cobc_string {
  struct cobc_tree_common common;
  unsigned char *str;
};

#define COBC_STRING(x)		(COBC_TREE_CAST (cobc_tag_string, struct cobc_string, x))
#define COBC_STRING_P(x)	(COBC_TREE_TAG (x) == cobc_tag_string)

extern cobc_tree make_string (unsigned char *str);


/*
 * Literal
 */

struct cobc_literal {
  struct cobc_tree_common common;
  size_t size;
  unsigned char *data;
  char id;
  char all;
  char sign;
  char decimals;
};

#define COBC_LITERAL(x)		(COBC_TREE_CAST (cobc_tag_literal, struct cobc_literal, x))
#define COBC_LITERAL_P(x)	(COBC_TREE_TAG (x) == cobc_tag_literal)

extern cobc_tree make_numeric_literal (int sign, unsigned char *digits, int decimals);
extern cobc_tree make_nonnumeric_literal (unsigned char *str);
extern long long literal_to_int (struct cobc_literal *l);


/*
 * Decimal
 */

struct cobc_decimal {
  struct cobc_tree_common common;
  char id;
};

#define COBC_DECIMAL(x)		(COBC_TREE_CAST (cobc_tag_decimal, struct cobc_decimal, x))
#define COBC_DECIMAL_P(x)	(COBC_TREE_TAG (x) == cobc_tag_decimal)

extern cobc_tree make_decimal (char id);


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
  int reference_id;
  int reference_max;
  char usage;			/* USAGE IS */
  char *name;			/* the original name */
  char *cname;			/* the name used in C */
  cobc_tree value;		/* VALUE */
  cobc_tree occurs_depending;	/* OCCURS ... DEPENDING ON */
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
    int used          : 1;	/* if used more than once */
    int screen        : 1;	/* if defined in SCREEN SECTION */
  } f;
  /* screen parameters */
  cobc_tree screen_line;
  cobc_tree screen_column;
  struct cobc_field *screen_from;
  struct cobc_field *screen_to;
  long screen_flag;		/* flags used in SCREEN SECTION */
};

#define COBC_FIELD(x)		(COBC_TREE_CAST (cobc_tag_field, struct cobc_field, x))
#define COBC_FIELD_P(x)		(COBC_TREE_TAG (x) == cobc_tag_field)
#define COBC_FILLER_P(x) \
  (COBC_FIELD_P (x) && COBC_FIELD (x)->cname[0] == '$')
#define COBC_INDEX_NAME_P(x) \
  (COBC_FIELD_P (x) && COBC_FIELD (x)->usage == COBC_USAGE_INDEX)

extern cobc_tree make_field (cobc_tree name);
extern cobc_tree make_field_3 (cobc_tree name, char *pic, int usage);
extern cobc_tree make_field_x (char *name, char *pic, int usage);
extern struct cobc_field *field (cobc_tree x);
extern int field_size (cobc_tree x);
extern struct cobc_field *field_founder (struct cobc_field *p);
extern int field_used_any_parent (struct cobc_field *p);
extern int field_used_any_child (struct cobc_field *p);
extern void finalize_field_tree (struct cobc_field *p);

struct cobc_picture *make_picture (void);


/*
 * File
 */

struct cobc_file {
  struct cobc_tree_common common;
  char *name;			/* the original name */
  char *cname;			/* the name used in C */
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

extern cobc_tree make_file (cobc_tree name);


/*
 * Reference
 */

struct cobc_reference {
  struct cobc_tree_common common;
  struct cobc_word *word;
  char id;
  cobc_tree value;
  struct cobc_list *subs;
  cobc_tree offset;
  cobc_tree length;
  struct cobc_reference *next;
};

#define COBC_REFERENCE(x)	(COBC_TREE_CAST (cobc_tag_reference, struct cobc_reference, x))
#define COBC_REFERENCE_P(x)	(COBC_TREE_TAG (x) == cobc_tag_reference)

#define COBC_NAME(x)		(COBC_REFERENCE (x)->word->name)
#define COBC_VALUE(x) \
  (COBC_REFERENCE_P (x) ? COBC_REFERENCE (x)->value : (x))

extern cobc_tree make_reference (struct cobc_word *word);
extern cobc_tree copy_reference (cobc_tree ref, cobc_tree value);
extern void set_value (cobc_tree ref, cobc_tree value);
extern char *associate (cobc_tree name, cobc_tree val);

extern cobc_tree make_filler (void);


/*
 * Binary operation
 */

/*
  '+'	x + y
  '-'	x - y
  '*'	x * y
  '/'	x / y
  '^'	x ** y
  '='	x = y
  '>'	x > y
  '<'	x < y
  '['	x <= y
  ']'	x >= y
  '~'	x != y
  '@'	( x )
  '!'	not x
  '&'	x and y
  '|'	x or y
*/

struct cobc_binary_op {
  struct cobc_tree_common common;
  char op;
  cobc_tree x;
  cobc_tree y;
};

#define COBC_BINARY_OP(x)	(COBC_TREE_CAST (cobc_tag_binary_op, struct cobc_binary_op, x))
#define COBC_BINARY_OP_P(x)	(COBC_TREE_TAG (x) == cobc_tag_binary_op)

#define make_parenthesize(x)	make_binary_op (x, '@', 0)
#define make_negative(x)	make_binary_op (x, '!', 0)

extern cobc_tree make_binary_op (cobc_tree x, char op, cobc_tree y);


/*
 * Function call
 */

struct cobc_funcall {
  struct cobc_tree_common common;
  const char *name;
  int argc;
  void *argv[4];
};

#define COBC_FUNCALL(x)		(COBC_TREE_CAST (cobc_tag_funcall, struct cobc_funcall, x))
#define COBC_FUNCALL_P(x)	(COBC_TREE_TAG (x) == cobc_tag_funcall)

extern cobc_tree make_funcall (const char *name, int argc, void *a1, void *a2, void *a3, void *a4);

#define make_funcall_0(f)		make_funcall (f, 0, 0, 0, 0, 0)
#define make_funcall_1(f,a1)		make_funcall (f, 1, a1, 0, 0, 0)
#define make_funcall_2(f,a1,a2)		make_funcall (f, 2, a1, a2, 0, 0)
#define make_funcall_3(f,a1,a2,a3)	make_funcall (f, 3, a1, a2, a3, 0)
#define make_funcall_4(f,a1,a2,a3,a4)	make_funcall (f, 4, a1, a2, a3, a4)


/*
 * Cast to int32
 */

struct cobc_cast_int32 {
  struct cobc_tree_common common;
  cobc_tree val;
};

#define COBC_CAST_INT32(x)	(COBC_TREE_CAST (cobc_tag_cast_int32, struct cobc_cast_int32, x))
#define COBC_CAST_INT32_P(x)	(COBC_TREE_TAG (x) == cobc_tag_cast_int32)

extern cobc_tree make_cast_int32 (cobc_tree val);


/*
 * Label
 */

struct cobc_label {
  struct cobc_tree_common common;
  char *name;
  char *cname;
  struct cobc_label *section;
  struct cobc_list *children;
  char need_begin;
  char need_return;
};

#define COBC_LABEL(x)		(COBC_TREE_CAST (cobc_tag_label, struct cobc_label, x))
#define COBC_LABEL_P(x)		(COBC_TREE_TAG (x) == cobc_tag_label)

extern cobc_tree make_label (cobc_tree name, struct cobc_label *section);


/*
 * IF
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
 * PERFORM
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
    cobc_tree step;
    cobc_tree until;
    struct cobc_perform_varying *next;
  } *varying;
};

#define COBC_PERFORM(x)		(COBC_TREE_CAST (cobc_tag_perform, struct cobc_perform, x))
#define COBC_PERFORM_P(x)	(COBC_TREE_TAG (x) == cobc_tag_perform)

extern cobc_tree make_perform (int type);
extern cobc_tree make_perform_once (cobc_tree body);
extern cobc_tree make_perform_exit (struct cobc_label *label);
extern void add_perform_varying (struct cobc_perform *perf, cobc_tree name, cobc_tree from, cobc_tree step, cobc_tree until);


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


/*
 * Class
 */

struct cobc_class {
  struct cobc_tree_common common;
  char *name;
  char *cname;
  struct cobc_list *list;
};

#define COBC_CLASS(x)		(COBC_TREE_CAST (cobc_tag_class, struct cobc_class, x))
#define COBC_CLASS_P(x)		(COBC_TREE_TAG (x) == cobc_tag_class)

extern cobc_tree make_class (cobc_tree name, struct cobc_list *list);


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
 * Parameter
 */

struct cobc_parameter {
  struct cobc_tree_common common;
  int type;
  cobc_tree x;
  cobc_tree y;
};

#define COBC_PARAMETER(x)		(COBC_TREE_CAST (cobc_tag_parameter, struct cobc_parameter, x))
#define COBC_PARAMETER_P(x)		(COBC_TREE_TAG (x) == cobc_tag_parameter)

extern cobc_tree make_parameter (int type, cobc_tree x, cobc_tree y);
#define make_parameter_1(type,x) make_parameter (type, x, 0)
#define make_pair(x,y)		 make_parameter (0, x, y)

#endif /* _TREE_H_ */
