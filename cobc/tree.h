/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

#define CB_BEFORE		0
#define CB_AFTER		1

#define CB_CALL_BY_REFERENCE	1
#define CB_CALL_BY_CONTENT	2
#define CB_CALL_BY_LENGTH	3
#define CB_CALL_BY_VALUE	4


/*
 * List
 */

struct cb_list {
  void *item;
  struct cb_list *next;
};

extern struct cb_list *cons (void *x, struct cb_list *l);
extern struct cb_list *list (void *x);
extern struct cb_list *list_last (struct cb_list *l);
extern struct cb_list *list_add (struct cb_list *l, void *x);
extern struct cb_list *list_append (struct cb_list *l1, struct cb_list *l2);
extern struct cb_list *list_reverse (struct cb_list *l);
extern int list_length (struct cb_list *l);


/*
 * Word table
 */

struct cb_word {
  const char *name;		/* word name */
  int count;			/* the number of words with the same name */
  int error;			/* set to 1 if error displayed */
  struct cb_list *items;	/* objects associated with this word */
  struct cb_word *next;		/* next word with the same hash value */
};


/*
 * Tree
 */

enum cb_tag {
  /* primitives */
  cb_tag_const,			/* constant value */
  cb_tag_integer,		/* native integer */
  cb_tag_string,		/* native string */
  cb_tag_literal,		/* numeric/alphanumeric literal */
  cb_tag_decimal,		/* decimal number */
  cb_tag_field,			/* user-defined variable */
  cb_tag_file,			/* file description */
  /* expressions */
  cb_tag_reference,		/* reference to a field, file, or label */
  cb_tag_binary_op,		/* binary operation */
  cb_tag_funcall,		/* run-time function call */
  cb_tag_cast_int32,		/* cast to int32 */
  /* statements */
  cb_tag_label,			/* label statement */
  cb_tag_if,			/* IF statement */
  cb_tag_perform,		/* PERFORM statement */
  cb_tag_sequence,		/* multiple statements */
  cb_tag_statement,		/* general statement */
  /* miscellaneous */
  cb_tag_class,			/* CLASS definition */
  cb_tag_builtin,
  cb_tag_parameter,
};

struct cb_tree_common {
  enum cb_tag tag;
  char class;
  char type;
  unsigned char *source_file;
  int source_line;
};

typedef struct cb_tree_common *cb_tree;

#define CB_TREE(x)		((struct cb_tree_common *) (x))
#define CB_TREE_TAG(x)		(CB_TREE (x)->tag)
#define CB_TREE_CLASS(x)	(CB_TREE (x)->class)
#define CB_TREE_TYPE(x)		(CB_TREE (x)->type)

#ifdef COB_DEBUG
#define CB_TREE_CAST(tg,ty,x)						\
  ({									\
    cb_tree _x = (x);							\
    if (!_x || CB_TREE_TAG (_x) != tg)					\
      {									\
	fprintf (stderr,						\
		 "%s:%d: invalid type cast from `%s' at %d in %s\n",	\
		 __FILE__, __LINE__, _x ? tree_name (_x) : "null",	\
		 cb_source_line, cb_source_file);			\
	abort ();							\
      }									\
    ((ty *) (_x));							\
  })
#else
#define CB_TREE_CAST(tg,ty,x)	((ty *) (x))
#endif

extern char *tree_name (cb_tree x);
extern int tree_category (cb_tree x);


/*
 * Constants
 */

extern cb_tree cb_any;
extern cb_tree cb_true;
extern cb_tree cb_false;
extern cb_tree cb_zero;
extern cb_tree cb_space;
extern cb_tree cb_low;
extern cb_tree cb_high;
extern cb_tree cb_quote;
extern cb_tree cb_return_code;
extern cb_tree cb_switch[];
extern cb_tree cb_int0;
extern cb_tree cb_int1;
extern cb_tree cb_int2;
extern cb_tree cb_error_node;

extern struct cb_label *cb_standard_error_handler;

struct cb_const {
  struct cb_tree_common common;
  char *val;
};

#define CB_CONST(x)		(CB_TREE_CAST (cb_tag_const, struct cb_const, x))
#define CB_CONST_P(x)		(CB_TREE_TAG (x) == cb_tag_const)

extern void init_constants (void);


/*
 * Integer
 */

struct cb_integer {
  struct cb_tree_common common;
  int val;
};

#define CB_INTEGER(x)		(CB_TREE_CAST (cb_tag_integer, struct cb_integer, x))
#define CB_INTEGER_P(x)		(CB_TREE_TAG (x) == cb_tag_integer)

extern cb_tree make_integer (int val);


/*
 * String
 */

struct cb_string {
  struct cb_tree_common common;
  const unsigned char *str;
};

#define CB_STRING(x)		(CB_TREE_CAST (cb_tag_string, struct cb_string, x))
#define CB_STRING_P(x)		(CB_TREE_TAG (x) == cb_tag_string)

extern cb_tree make_string (const unsigned char *str);


/*
 * Literal
 */

struct cb_literal {
  struct cb_tree_common common;
  size_t size;
  unsigned char *data;
  char all;
  char sign;
  char expt;
};

#define CB_LITERAL(x)		(CB_TREE_CAST (cb_tag_literal, struct cb_literal, x))
#define CB_LITERAL_P(x)		(CB_TREE_TAG (x) == cb_tag_literal)

extern cb_tree make_numeric_literal (int sign, unsigned char *digits, int expt);
extern cb_tree make_nonnumeric_literal (size_t size, unsigned char *data);
extern long long literal_to_int (struct cb_literal *l);


/*
 * Decimal
 */

struct cb_decimal {
  struct cb_tree_common common;
  char id;
};

#define CB_DECIMAL(x)		(CB_TREE_CAST (cb_tag_decimal, struct cb_decimal, x))
#define CB_DECIMAL_P(x)		(CB_TREE_TAG (x) == cb_tag_decimal)

extern cb_tree make_decimal (char id);


/*
 * Picture
 */

struct cb_picture {
  int size;			/* byte size */
  char *orig;			/* original picture string */
  char *str;			/* packed picture string */
  char category;		/* field category */
  char digits;			/* the number of digit places */
  char expt;			/* 10 ^ expt */
  char have_sign;		/* have `S' */
};

extern struct cb_picture *parse_picture (const char *str);


/*
 * Field
 */

enum cb_usage {
  CB_USAGE_BINARY,
  CB_USAGE_BIT,			/* not supported yet */
  CB_USAGE_DISPLAY,
  CB_USAGE_FLOAT,		/* not supported yet */
  CB_USAGE_INDEX,
  CB_USAGE_NATIONAL,		/* not supported yet */
  CB_USAGE_OBJECT,		/* not supported yet */
  CB_USAGE_PACKED,		/* not supported yet */
  CB_USAGE_POINTER,		/* not supported yet */
  CB_USAGE_PROGRAM		/* not supported yet */
};

enum cb_storage {
  CB_STORAGE_FILE,		/* FILE SECTION */
  CB_STORAGE_WORKING,		/* WORKING-STORAGE SECTION */
  CB_STORAGE_LINKAGE,		/* LINKAGE SECTION */
  CB_STORAGE_SCREEN		/* SCREEN SECTION */
};

struct cb_field {
  struct cb_tree_common common;
  int size;			/* field size */
  int memory_size;		/* memory size */
  int offset;			/* byte offset from the top (ie, 01 field) */
  int level;			/* level number */
  int occurs;			/* OCCURS */
  int occurs_min;
  int indexes;			/* the number of parents who have OCCURS */
  const char *name;		/* the original name */
  char *cname;			/* the name used in C */
  cb_tree occurs_depending;	/* OCCURS ... DEPENDING ON */
  enum cb_usage usage;		/* USAGE */
  enum cb_storage storage;
  struct cb_list *values;	/* VALUE */
  struct cb_list *index_list;	/* INDEXED BY */
  struct cb_field *parent;	/* upper level field (NULL for 01 fields) */
  struct cb_field *children;	/* top of lower level fields */
  struct cb_field *sister;	/* fields in the same level */
  struct cb_field *redefines;	/* REDEFIENS */
  struct cb_field *rename_thru; /* RENAMES THRU */
  struct cb_file *file;		/* file name associated in FD section */
  struct cb_key {
    int dir;			/* ASCENDING or DESCENDING */
    cb_tree key;		/* KEY */
    cb_tree ref;		/* reference used in SEARCH ALL */
    cb_tree val;		/* value to be compared in SEARCH ALL */
  } *keys;
  int nkeys;			/* the number of keys */
  struct cb_picture *pic;	/* PICTURE */
  /* flags */
  long flag_external      : 1;	/* EXTERNAL */
  long flag_blank_zero    : 1;	/* BLANK WHEN ZERO */
  long flag_justified     : 1;	/* JUSTIFIED RIGHT */
  long flag_sign_leading  : 1;	/* SIGN IS LEADING */
  long flag_sign_separate : 1;	/* SIGN IS SEPARATE */
  long flag_synchronized  : 1;	/* SYNCHRONIZED */
  long flag_occurs        : 1;	/* if OCCURS clause exists */
  long flag_base          : 1;
  long flag_field         : 1;
  /* screen parameters */
  cb_tree screen_line;
  cb_tree screen_column;
  struct cb_field *screen_from;
  struct cb_field *screen_to;
  long screen_flag;		/* flags used in SCREEN SECTION */
};

#define CB_FIELD(x)		(CB_TREE_CAST (cb_tag_field, struct cb_field, x))
#define CB_FIELD_P(x)		(CB_TREE_TAG (x) == cb_tag_field)

extern cb_tree make_field (cb_tree name);
extern cb_tree make_field_3 (cb_tree name, const char *pic, int usage);
extern cb_tree make_field_x (const char *name, const char *pic, int usage);
extern struct cb_field *field (cb_tree x);
extern int field_size (cb_tree x);
extern struct cb_field *field_founder (struct cb_field *p);

extern struct cb_field *build_field (int level, cb_tree name, struct cb_field *last_field, enum cb_storage storage);
extern struct cb_field *validate_redefines (struct cb_field *field, cb_tree redefines);
extern int validate_field (struct cb_field *p);
extern void finalize_field (struct cb_field *p);


/*
 * File
 */

struct cb_file {
  struct cb_tree_common common;
  const char *name;		/* the original name */
  char *cname;			/* the name used in C */
  /* SELECT */
  cb_tree assign;		/* ASSIGN */
  int optional;			/* OPTIONAL */
  int organization;		/* ORGANIZATION */
  int access_mode;		/* ACCESS MODE */
  cb_tree file_status;		/* FILE STATUS */
  cb_tree key;			/* RELATIVE/RECORD KEY */
  struct cb_alt_key {
    cb_tree key;
    int duplicates;
    struct cb_alt_key *next;
  } *alt_key_list;		/* ALTERNATE RECORD KEY list */
  /* FD/SD */
  struct cb_field *record;	/* record descriptor */
  int record_min, record_max;	/* RECORD CONTAINS */
  cb_tree record_depending;	/* RECORD DEPENDING */
  /* STANDARD ERROR PROCEDURE */
  struct cb_label *handler;	/* error handler */
};

#define CB_FILE(x)	(CB_TREE_CAST (cb_tag_file, struct cb_file, x))
#define CB_FILE_P(x)	(CB_TREE_TAG (x) == cb_tag_file)

extern struct cb_file *build_file (cb_tree name);
extern void validate_file (struct cb_file *f, cb_tree name);
extern void finalize_file (struct cb_file *f, struct cb_field *records);


/*
 * Reference
 */

struct cb_reference {
  struct cb_tree_common common;
  struct cb_word *word;
  cb_tree value;
  struct cb_list *subs;
  cb_tree offset;
  cb_tree length;
  struct cb_reference *next;
};

#define CB_REFERENCE(x)		(CB_TREE_CAST (cb_tag_reference, struct cb_reference, x))
#define CB_REFERENCE_P(x)	(CB_TREE_TAG (x) == cb_tag_reference)

#define CB_NAME(x)		(CB_REFERENCE (x)->word->name)

extern cb_tree make_reference (const char *name);
extern cb_tree copy_reference (cb_tree ref, cb_tree value);
extern void set_value (cb_tree ref, cb_tree value);
extern const char *associate (cb_tree name, cb_tree val);

extern cb_tree make_filler (void);

extern cb_tree resolve_data_name (cb_tree x);
extern int validate_data_name (cb_tree x);
extern cb_tree resolve_label (cb_tree x);
extern cb_tree resolve_file_name (cb_tree x);
extern cb_tree resolve_class_name (cb_tree x);
extern cb_tree resolve_mnemonic_name (cb_tree x);


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

struct cb_binary_op {
  struct cb_tree_common common;
  char op;
  cb_tree x;
  cb_tree y;
};

#define CB_BINARY_OP(x)		(CB_TREE_CAST (cb_tag_binary_op, struct cb_binary_op, x))
#define CB_BINARY_OP_P(x)	(CB_TREE_TAG (x) == cb_tag_binary_op)

#define make_parenthesize(x)	make_binary_op (x, '@', 0)
#define make_negative(x)	make_binary_op (x, '!', 0)

extern cb_tree make_binary_op (cb_tree x, char op, cb_tree y);


/*
 * Function call
 */

struct cb_funcall {
  struct cb_tree_common common;
  const char *name;
  int argc;
  void *argv[4];
};

#define CB_FUNCALL(x)		(CB_TREE_CAST (cb_tag_funcall, struct cb_funcall, x))
#define CB_FUNCALL_P(x)		(CB_TREE_TAG (x) == cb_tag_funcall)

extern cb_tree make_funcall (const char *name, int argc, void *a1, void *a2, void *a3, void *a4);

#define make_funcall_0(f)		make_funcall (f, 0, 0, 0, 0, 0)
#define make_funcall_1(f,a1)		make_funcall (f, 1, a1, 0, 0, 0)
#define make_funcall_2(f,a1,a2)		make_funcall (f, 2, a1, a2, 0, 0)
#define make_funcall_3(f,a1,a2,a3)	make_funcall (f, 3, a1, a2, a3, 0)
#define make_funcall_4(f,a1,a2,a3,a4)	make_funcall (f, 4, a1, a2, a3, a4)


/*
 * Cast to int32
 */

struct cb_cast_int32 {
  struct cb_tree_common common;
  cb_tree val;
};

#define CB_CAST_INT32(x)	(CB_TREE_CAST (cb_tag_cast_int32, struct cb_cast_int32, x))
#define CB_CAST_INT32_P(x)	(CB_TREE_TAG (x) == cb_tag_cast_int32)

extern cb_tree make_cast_int32 (cb_tree val);


/*
 * Label
 */

struct cb_label {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  struct cb_label *section;
  struct cb_list *children;
  char need_begin;
  char need_return;
};

#define CB_LABEL(x)		(CB_TREE_CAST (cb_tag_label, struct cb_label, x))
#define CB_LABEL_P(x)		(CB_TREE_TAG (x) == cb_tag_label)

extern cb_tree make_label (cb_tree name, struct cb_label *section);


/*
 * IF
 */

struct cb_if {
  struct cb_tree_common common;
  cb_tree test;
  cb_tree stmt1;
  cb_tree stmt2;
};

#define CB_IF(x)		(CB_TREE_CAST (cb_tag_if, struct cb_if, x))
#define CB_IF_P(x)		(CB_TREE_TAG (x) == cb_tag_if)

extern cb_tree make_if (cb_tree test, cb_tree stmt1, cb_tree stmt2);


/*
 * PERFORM
 */

#define CB_PERFORM_EXIT		0
#define CB_PERFORM_ONCE		1
#define CB_PERFORM_TIMES	2
#define CB_PERFORM_UNTIL	3

struct cb_perform {
  struct cb_tree_common common;
  int type;
  int test;
  cb_tree body;
  cb_tree data;
  struct cb_perform_varying {
    cb_tree name;
    cb_tree from;
    cb_tree step;
    cb_tree until;
    struct cb_perform_varying *next;
  } *varying;
};

#define CB_PERFORM(x)		(CB_TREE_CAST (cb_tag_perform, struct cb_perform, x))
#define CB_PERFORM_P(x)		(CB_TREE_TAG (x) == cb_tag_perform)

extern cb_tree make_perform (int type);
extern cb_tree make_perform_once (cb_tree body);
extern cb_tree make_perform_exit (struct cb_label *label);
extern void add_perform_varying (struct cb_perform *perf, cb_tree name, cb_tree from, cb_tree step, cb_tree until);


/*
 * Sequence
 */

struct cb_sequence {
  struct cb_tree_common common;
  struct cb_list *list;
  int save_status;
};

#define CB_SEQUENCE(x)		(CB_TREE_CAST (cb_tag_sequence, struct cb_sequence, x))
#define CB_SEQUENCE_P(x)	(CB_TREE_TAG (x) == cb_tag_sequence)

extern cb_tree make_sequence (struct cb_list *list);


/*
 * Statement
 */

struct cb_statement {
  struct cb_tree_common common;
  const char *name;
  int need_terminator;
};

#define CB_STATEMENT(x)		(CB_TREE_CAST (cb_tag_statement, struct cb_statement, x))
#define CB_STATEMENT_P(x)	(CB_TREE_TAG (x) == cb_tag_statement)

extern struct cb_statement *build_statement (const char *name);


/*
 * Class
 */

struct cb_class {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  struct cb_list *list;
};

#define CB_CLASS(x)		(CB_TREE_CAST (cb_tag_class, struct cb_class, x))
#define CB_CLASS_P(x)		(CB_TREE_TAG (x) == cb_tag_class)

extern cb_tree make_class (cb_tree name, struct cb_list *list);


/*
 * Builtin
 */

struct cb_builtin {
  struct cb_tree_common common;
  int id;
};

#define CB_BUILTIN(x)		(CB_TREE_CAST (cb_tag_builtin, struct cb_builtin, x))
#define CB_BUILTIN_P(x)		(CB_TREE_TAG (x) == cb_tag_builtin)

extern cb_tree make_builtin (int id);


/*
 * Parameter
 */

struct cb_parameter {
  struct cb_tree_common common;
  int type;
  cb_tree x;
  cb_tree y;
};

#define CB_PARAMETER(x)		(CB_TREE_CAST (cb_tag_parameter, struct cb_parameter, x))
#define CB_PARAMETER_P(x)	(CB_TREE_TAG (x) == cb_tag_parameter)

extern cb_tree make_parameter (int type, cb_tree x, cb_tree y);
#define make_parameter_1(type,x) make_parameter (type, x, 0)
#define make_pair(x,y)		 make_parameter (0, x, y)


/*
 * Program
 */

struct cb_program {
  /* program variables */
  const char *program_id;
  unsigned char decimal_point;		/* '.' or ',' */
  unsigned char currency_symbol;	/* '$' or user-specified */
  unsigned char numeric_separator;	/* ',' or '.' */
  struct cb_list *class_list;
  struct cb_list *entry_list;
  struct cb_list *index_list;
  struct cb_list *file_list;
  struct cb_list *exec_list;
  struct cb_list *label_list;
  struct cb_list *reference_list;
  struct cb_field *working_storage;
  struct cb_field *linkage_storage;
  struct cb_field *screen_storage;
  struct cb_label *file_handler[5];
  long flag_common      : 1;		/* COMMON PROGRAM */
  long flag_initial     : 1;		/* INITIAL PROGRAM */
  long flag_recursive   : 1;		/* RECURSIVE PROGRAM */
  long flag_screen      : 1;		/* have SCREEN SECTION */
  /* internal variables */
  int loop_counter;
  int decimal_index;
  int decimal_index_max;
  struct cb_word **word_table;
};

extern struct cb_program *build_program (void);

extern cb_tree build_assign (struct cb_list *vars, char op, cb_tree val);
extern cb_tree build_add (cb_tree v, cb_tree n, int round);
extern cb_tree build_sub (cb_tree v, cb_tree n, int round);
extern cb_tree build_move (cb_tree src, cb_tree dst);
extern cb_tree build_corresponding (cb_tree (*func)(), cb_tree x1, cb_tree x2, int opt);
extern cb_tree build_divide (cb_tree dividend, cb_tree divisor, cb_tree quotient, cb_tree remainder);
extern cb_tree build_cond (cb_tree x);
extern cb_tree build_evaluate (struct cb_list *subject_list, struct cb_list *case_list);
extern cb_tree build_search_all (cb_tree table, cb_tree when);

#endif /* _TREE_H_ */
