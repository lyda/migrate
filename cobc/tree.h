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

#ifndef CB_TREE_H
#define CB_TREE_H

#define CB_BEFORE		0
#define CB_AFTER		1

#define CB_CALL_BY_REFERENCE	1
#define CB_CALL_BY_CONTENT	2
#define CB_CALL_BY_LENGTH	3
#define CB_CALL_BY_VALUE	4

enum cb_tag {
  /* primitives */
  CB_TAG_CONST,			/* constant value */
  CB_TAG_ALPHABET_NAME,		/* alphabet-name identifier */
  CB_TAG_SYSTEM_NAME,		/* system-name identifier */
  CB_TAG_INTEGER,		/* integer constant */
  CB_TAG_STRING,		/* string constant */
  CB_TAG_LITERAL,		/* numeric/alphanumeric literal */
  CB_TAG_DECIMAL,		/* decimal number */
  CB_TAG_FIELD,			/* user-defined variable */
  CB_TAG_FILE,			/* file description */
  /* expressions */
  CB_TAG_REFERENCE,		/* reference to a field, file, or label */
  CB_TAG_BINARY_OP,		/* binary operation */
  CB_TAG_FUNCALL,		/* run-time function call */
  CB_TAG_CAST_INTEGER,		/* cast to integer */
  /* statements */
  CB_TAG_LABEL,			/* label statement */
  CB_TAG_IF,			/* IF statement */
  CB_TAG_PERFORM,		/* PERFORM statement */
  CB_TAG_SEQUENCE,		/* multiple statements */
  CB_TAG_STATEMENT,		/* general statement */
  /* miscellaneous */
  CB_TAG_PROPOSITION,		/* CLASS definition */
  CB_TAG_PARAMETER,
};

enum cb_alphabet_name_type {
  CB_ALPHABET_NATIVE,
  CB_ALPHABET_STANDARD_1,
  CB_ALPHABET_STANDARD_2,
  CB_ALPHABET_CUSTOM,
};

enum cb_system_name_category {
  CB_CALL_CONVENTION_NAME,
  CB_CODE_NAME,
  CB_COMPUTER_NAME,
  CB_DEVICE_NAME,
  CB_ENTRY_CONVENTION_NAME,
  CB_EXTERNAL_LOCALE_NAME,
  CB_FEATURE_NAME,
  CB_LIBRARY_NAME,
  CB_SWITCH_NAME,
  CB_TEXT_NAME,
};

enum cb_class {
  CB_CLASS_UNKNOWN,
  CB_CLASS_ALPHABETIC,
  CB_CLASS_ALPHANUMERIC,
  CB_CLASS_BOOLEAN,
  CB_CLASS_INDEX,
  CB_CLASS_NATIONAL,
  CB_CLASS_NUMERIC,
  CB_CLASS_OBJECT,
  CB_CLASS_POINTER,
};

enum cb_category {
  CB_CATEGORY_UNKNOWN,
  CB_CATEGORY_ALPHABETIC,
  CB_CATEGORY_ALPHANUMERIC,
  CB_CATEGORY_ALPHANUMERIC_EDITED,
  CB_CATEGORY_BOOLEAN,
  CB_CATEGORY_INDEX,
  CB_CATEGORY_NATIONAL,
  CB_CATEGORY_NATIONAL_EDITED,
  CB_CATEGORY_NUMERIC,
  CB_CATEGORY_NUMERIC_EDITED,
  CB_CATEGORY_OBJECT_REFERENCE,
  CB_CATEGORY_DATA_POINTER,
  CB_CATEGORY_PROGRAM_POINTER,
};

enum cb_storage {
  CB_STORAGE_CONSTANT,		/* Constants */
  CB_STORAGE_FILE,		/* FILE SECTION */
  CB_STORAGE_WORKING,		/* WORKING-STORAGE SECTION */
  CB_STORAGE_LOCAL,		/* LOCAL-STORAGE SECTION */
  CB_STORAGE_LINKAGE,		/* LINKAGE SECTION */
  CB_STORAGE_SCREEN		/* SCREEN SECTION */
};

enum cb_usage {
  CB_USAGE_BINARY,
  CB_USAGE_BIT,
  CB_USAGE_DISPLAY,
  CB_USAGE_FLOAT,
  CB_USAGE_INDEX,
  CB_USAGE_NATIONAL,
  CB_USAGE_OBJECT,
  CB_USAGE_PACKED,
  CB_USAGE_POINTER,
  CB_USAGE_PROGRAM,
};


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

struct cb_tree_common {
  enum cb_tag tag;
  enum cb_category category;
  unsigned char *source_file;
  int source_line;
};

typedef struct cb_tree_common *cb_tree;

#define CB_TREE(x)		((struct cb_tree_common *) (x))
#define CB_TREE_TAG(x)		(CB_TREE (x)->tag)
#define CB_TREE_CLASS(x)	cb_tree_class (CB_TREE (x))
#define CB_TREE_CATEGORY(x)	cb_tree_category (CB_TREE (x))

#ifdef COB_DEBUG
#define CB_TREE_CAST(tg,ty,x)						\
  ({									\
    cb_tree _x = (x);							\
    if (!_x || CB_TREE_TAG (_x) != tg)					\
      {									\
	fprintf (stderr,						\
		 "%s:%d: invalid type cast from `%s' at %d in %s\n",	\
		 __FILE__, __LINE__, _x ? cb_name (_x) : "null",	\
		 cb_source_line, cb_source_file);			\
	abort ();							\
      }									\
    ((ty *) (_x));							\
  })
#else
#define CB_TREE_CAST(tg,ty,x)	((ty *) (x))
#endif

extern char *cb_name (cb_tree x);
extern enum cb_class cb_tree_class (cb_tree x);
extern enum cb_category cb_tree_category (cb_tree x);
extern int cb_fits_int (cb_tree x);


/*
 * Constants
 */

extern cb_tree cb_any;
extern cb_tree cb_true;
extern cb_tree cb_false;
extern cb_tree cb_null;
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
  const char *val;
};

#define CB_CONST(x)	(CB_TREE_CAST (CB_TAG_CONST, struct cb_const, x))
#define CB_CONST_P(x)	(CB_TREE_TAG (x) == CB_TAG_CONST)

extern void cb_init_constants (void);


/*
 * Alphabet-name
 */

struct cb_alphabet_name {
  struct cb_tree_common common;
  enum cb_alphabet_name_type type;
  struct cb_list *custom_list;
};

#define CB_ALPHABET_NAME(x)	(CB_TREE_CAST (CB_TAG_ALPHABET_NAME, struct cb_alphabet_name, x))
#define CB_ALPHABET_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_ALPHABET_NAME)

extern cb_tree cb_build_alphabet_name (enum cb_alphabet_name_type type);


/*
 * System-name
 */

struct cb_system_name {
  struct cb_tree_common common;
  enum cb_system_name_category category;
  int token;
};

#define CB_SYSTEM_NAME(x)	(CB_TREE_CAST (CB_TAG_SYSTEM_NAME, struct cb_system_name, x))
#define CB_SYSTEM_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_SYSTEM_NAME)

extern cb_tree cb_build_system_name (enum cb_system_name_category category, int token);


/*
 * Integer
 */

struct cb_integer {
  struct cb_tree_common common;
  int val;
};

#define CB_INTEGER(x)	(CB_TREE_CAST (CB_TAG_INTEGER, struct cb_integer, x))
#define CB_INTEGER_P(x)	(CB_TREE_TAG (x) == CB_TAG_INTEGER)

extern cb_tree cb_build_integer (int val);


/*
 * String
 */

struct cb_string {
  struct cb_tree_common common;
  const unsigned char *str;
};

#define CB_STRING(x)	(CB_TREE_CAST (CB_TAG_STRING, struct cb_string, x))
#define CB_STRING_P(x)	(CB_TREE_TAG (x) == CB_TAG_STRING)

extern cb_tree cb_build_string (const unsigned char *str);


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

#define CB_LITERAL(x)	(CB_TREE_CAST (CB_TAG_LITERAL, struct cb_literal, x))
#define CB_LITERAL_P(x)	(CB_TREE_TAG (x) == CB_TAG_LITERAL)

extern cb_tree cb_build_numeric_literal (int sign, unsigned char *data, int expt);
extern cb_tree cb_build_alphanumeric_literal (size_t size, unsigned char *data);
extern int cb_literal_to_int (struct cb_literal *l);


/*
 * Decimal
 */

struct cb_decimal {
  struct cb_tree_common common;
  int id;
};

#define CB_DECIMAL(x)	(CB_TREE_CAST (CB_TAG_DECIMAL, struct cb_decimal, x))
#define CB_DECIMAL_P(x)	(CB_TREE_TAG (x) == CB_TAG_DECIMAL)

extern cb_tree cb_build_decimal (int id);


/*
 * Picture
 */

struct cb_picture {
  int size;			/* byte size */
  char *orig;			/* original picture string */
  char *str;			/* packed picture string */
  enum cb_category category;	/* field category */
  char digits;			/* the number of digit places */
  char expt;			/* 10 ^ expt */
  char have_sign;		/* have `S' */
};

extern struct cb_picture *cb_parse_picture (const char *str);


/*
 * Field
 */

struct cb_field {
  struct cb_tree_common common;
  const char *name;		/* the original name */
  char *cname;			/* the name used in C */
  int size;			/* field size */
  int memory_size;		/* memory size */
  int offset;			/* byte offset from the top (ie, 01 field) */
  int level;			/* level number */
  int occurs_min;		/* OCCURS <max> */
  int occurs_max;		/* or OCCURS <min> TO <max> */
  int indexes;			/* the number of parents who have OCCURS */
  cb_tree occurs_depending;	/* OCCURS ... DEPENDING ON */
  enum cb_storage storage;
  enum cb_usage usage;		/* USAGE */
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
  long flag_occurs        : 1;	/* OCCURS */
  long flag_local         : 1;	/* has local scope */
  long flag_base          : 1;
  long flag_field         : 1;
  /* screen parameters */
  cb_tree screen_line;
  cb_tree screen_column;
  struct cb_field *screen_from;
  struct cb_field *screen_to;
  long screen_flag;		/* flags used in SCREEN SECTION */
};

#define CB_FIELD(x)		(CB_TREE_CAST (CB_TAG_FIELD, struct cb_field, x))
#define CB_FIELD_P(x)		(CB_TREE_TAG (x) == CB_TAG_FIELD)

extern cb_tree cb_build_index (cb_tree name);
extern cb_tree cb_build_constant (cb_tree name, cb_tree value);
extern cb_tree cb_build_field (int level, cb_tree name, struct cb_field *last_field, enum cb_storage storage);
extern struct cb_field *cb_resolve_redefines (struct cb_field *field, cb_tree redefines);
extern void cb_validate_field (struct cb_field *p);
extern void cb_validate_88_item (struct cb_field *p);

extern struct cb_field *cb_field (cb_tree x);
extern int cb_field_size (cb_tree x);
extern struct cb_field *cb_field_founder (struct cb_field *p);

/* Index */

#define CB_INDEX_P(x)				\
  ((CB_FIELD_P (x) || CB_REFERENCE_P (x))	\
   && cb_field (x)->usage == CB_USAGE_INDEX)


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

#define CB_FILE(x)	(CB_TREE_CAST (CB_TAG_FILE, struct cb_file, x))
#define CB_FILE_P(x)	(CB_TREE_TAG (x) == CB_TAG_FILE)

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
  cb_tree chain;
};

#define CB_REFERENCE(x)		(CB_TREE_CAST (CB_TAG_REFERENCE, struct cb_reference, x))
#define CB_REFERENCE_P(x)	(CB_TREE_TAG (x) == CB_TAG_REFERENCE)

#define CB_NAME(x)		(CB_REFERENCE (x)->word->name)

extern cb_tree make_filler (void);
extern cb_tree make_reference (const char *name);
extern cb_tree copy_reference (cb_tree ref, cb_tree value);
extern const char *cb_define (cb_tree name, cb_tree val);
extern cb_tree cb_ref (cb_tree x);


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

#define CB_BINARY_OP(x)		(CB_TREE_CAST (CB_TAG_BINARY_OP, struct cb_binary_op, x))
#define CB_BINARY_OP_P(x)	(CB_TREE_TAG (x) == CB_TAG_BINARY_OP)

#define cb_build_parenthesize(x) cb_build_binary_op (x, '@', 0)
#define cb_build_negation(x)	cb_build_binary_op (x, '!', 0)

extern cb_tree cb_build_binary_op (cb_tree x, char op, cb_tree y);
extern cb_tree cb_build_connective_op (struct cb_list *l, char op);


/*
 * Function call
 */

struct cb_funcall {
  struct cb_tree_common common;
  const char *name;
  int argc;
  void *argv[4];
};

#define CB_FUNCALL(x)		(CB_TREE_CAST (CB_TAG_FUNCALL, struct cb_funcall, x))
#define CB_FUNCALL_P(x)		(CB_TREE_TAG (x) == CB_TAG_FUNCALL)

extern cb_tree cb_build_funcall (const char *name, int argc, void *a1, void *a2, void *a3, void *a4);

#define cb_build_funcall_0(f)		cb_build_funcall (f, 0, 0, 0, 0, 0)
#define cb_build_funcall_1(f,a1)	cb_build_funcall (f, 1, a1, 0, 0, 0)
#define cb_build_funcall_2(f,a1,a2)	cb_build_funcall (f, 2, a1, a2, 0, 0)
#define cb_build_funcall_3(f,a1,a2,a3)	cb_build_funcall (f, 3, a1, a2, a3, 0)
#define cb_build_funcall_4(f,a1,a2,a3,a4) cb_build_funcall (f, 4, a1, a2, a3, a4)


/*
 * Cast to integer
 */

struct cb_cast_integer {
  struct cb_tree_common common;
  cb_tree val;
};

#define CB_CAST_INTEGER(x)	(CB_TREE_CAST (CB_TAG_CAST_INTEGER, struct cb_cast_integer, x))
#define CB_CAST_INTEGER_P(x)	(CB_TREE_TAG (x) == CB_TAG_CAST_INTEGER)

extern cb_tree cb_build_cast_integer (cb_tree val);


/*
 * Label
 */

struct cb_label {
  struct cb_tree_common common;
  const char *name;
  const char *cname;
  struct cb_label *section;
  struct cb_list *children;
  char need_begin;
  char need_return;
};

#define CB_LABEL(x)		(CB_TREE_CAST (CB_TAG_LABEL, struct cb_label, x))
#define CB_LABEL_P(x)		(CB_TREE_TAG (x) == CB_TAG_LABEL)

extern cb_tree cb_build_label (cb_tree name, struct cb_label *section);


/*
 * IF
 */

struct cb_if {
  struct cb_tree_common common;
  cb_tree test;
  cb_tree stmt1;
  cb_tree stmt2;
};

#define CB_IF(x)		(CB_TREE_CAST (CB_TAG_IF, struct cb_if, x))
#define CB_IF_P(x)		(CB_TREE_TAG (x) == CB_TAG_IF)

extern cb_tree cb_build_if (cb_tree test, cb_tree stmt1, cb_tree stmt2);


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

#define CB_PERFORM(x)		(CB_TREE_CAST (CB_TAG_PERFORM, struct cb_perform, x))
#define CB_PERFORM_P(x)		(CB_TREE_TAG (x) == CB_TAG_PERFORM)

extern cb_tree cb_build_perform (int type);
extern cb_tree cb_build_perform_once (cb_tree body);
extern cb_tree cb_build_perform_exit (struct cb_label *label);
extern void cb_add_perform_varying (struct cb_perform *perf, cb_tree name, cb_tree from, cb_tree step, cb_tree until);


/*
 * Sequence
 */

struct cb_sequence {
  struct cb_tree_common common;
  struct cb_list *list;
  int save_status;
};

#define CB_SEQUENCE(x)		(CB_TREE_CAST (CB_TAG_SEQUENCE, struct cb_sequence, x))
#define CB_SEQUENCE_P(x)	(CB_TREE_TAG (x) == CB_TAG_SEQUENCE)

extern cb_tree make_sequence (struct cb_list *list);


/*
 * Statement
 */

struct cb_statement {
  struct cb_tree_common common;
  const char *name;
  int need_terminator;
};

#define CB_STATEMENT(x)		(CB_TREE_CAST (CB_TAG_STATEMENT, struct cb_statement, x))
#define CB_STATEMENT_P(x)	(CB_TREE_TAG (x) == CB_TAG_STATEMENT)

extern struct cb_statement *cb_build_statement (const char *name);


/*
 * Proposition
 */

struct cb_proposition {
  struct cb_tree_common common;
  const char *name;
  char *cname;
  struct cb_list *list;
};

#define CB_PROPOSITION(x)	(CB_TREE_CAST (CB_TAG_PROPOSITION, struct cb_proposition, x))
#define CB_PROPOSITION_P(x)	(CB_TREE_TAG (x) == CB_TAG_PROPOSITION)

extern cb_tree cb_build_proposition (cb_tree name, struct cb_list *list);


/*
 * Parameter
 */

struct cb_parameter {
  struct cb_tree_common common;
  int type;
  cb_tree x;
  cb_tree y;
};

#define CB_PARAMETER(x)		(CB_TREE_CAST (CB_TAG_PARAMETER, struct cb_parameter, x))
#define CB_PARAMETER_P(x)	(CB_TREE_TAG (x) == CB_TAG_PARAMETER)

extern cb_tree cb_build_parameter (int type, cb_tree x, cb_tree y);
#define cb_build_parameter_1(type,x) cb_build_parameter (type, x, 0)
#define cb_build_pair(x,y)	 cb_build_parameter (0, x, y)


/*
 * Program
 */

struct cb_program {
  /* program variables */
  const char *program_id;
  unsigned char decimal_point;		/* '.' or ',' */
  unsigned char currency_symbol;	/* '$' or user-specified */
  unsigned char numeric_separator;	/* ',' or '.' */
  struct cb_list *entry_list;
  struct cb_list *index_list;
  struct cb_list *file_list;
  struct cb_list *exec_list;
  struct cb_list *label_list;
  struct cb_list *reference_list;
  struct cb_list *proposition_list;
  struct cb_field *working_storage;
  struct cb_field *local_storage;
  struct cb_field *linkage_storage;
  struct cb_field *screen_storage;
  struct cb_label *file_handler[5];
  cb_tree collating_sequence;
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

extern struct cb_program *cb_build_program (void);

extern cb_tree cb_build_identifier (cb_tree x);
extern cb_tree cb_build_assign (struct cb_list *vars, char op, cb_tree val);
extern cb_tree cb_build_add (cb_tree v, cb_tree n, int round);
extern cb_tree cb_build_sub (cb_tree v, cb_tree n, int round);
extern cb_tree cb_build_move (cb_tree src, cb_tree dst);
extern cb_tree cb_build_corr (cb_tree (*func)(), cb_tree x1, cb_tree x2, int opt);
extern cb_tree cb_build_divide (cb_tree dividend, cb_tree divisor, cb_tree quotient, cb_tree remainder);
extern cb_tree cb_build_cond (cb_tree x);
extern cb_tree cb_build_evaluate (struct cb_list *subject_list, struct cb_list *case_list);
extern cb_tree cb_build_search_all (cb_tree table, cb_tree when);

#endif /* CB_TREE_H */
