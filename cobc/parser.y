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

%expect 126

%{
#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libcob.h>

#include "cobc.h"
#include "tree.h"
#include "scanner.h"
#include "codegen.h"
#include "reserved.h"

#define yydebug		yy_bison_debug
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#define IGNORE(x)	/* ignored */
#define OBSOLETE(x)	yywarn ("keyword `%s' is obsolete", x)

#define push_tree(x) \
  program_spec.exec_list = cons (x, program_spec.exec_list)

#define push_call_0(t)		 push_tree (make_call_0 (t))
#define push_call_1(t,a)	 push_tree (make_call_1 (t, a))
#define push_call_2(t,a,b)	 push_tree (make_call_2 (t, a, b))
#define push_call_3(t,a,b,c)	 push_tree (make_call_3 (t, a, b, c))
#define push_call_4(t,a,b,c,d)	 push_tree (make_call_4 (t, a, b, c, d))

#define push_move(x,y)		 push_call_2 (COB_MOVE, x, y)

#define push_label(x)				\
  do {						\
    struct cobc_label_name *p = x;		\
    finalize_label_name (p);			\
    push_tree (p);				\
  } while (0)

#define push_exit_section(x)				\
  do {							\
    cobc_tree p = make_perform (COBC_PERFORM_EXIT);	\
    COBC_PERFORM (p)->data = COBC_TREE (x);		\
    push_tree (p);					\
  } while (0)

#define push_assign(lst,op,val)				\
  do {							\
    cobc_tree v = (val);				\
    struct cobc_list *l;				\
    /* save temporary value for multiple targets */	\
    if (lst->next)					\
      {							\
	push_tree (make_assign (cobc_dt, v, 0));	\
	v = cobc_dt;					\
      }							\
    /* set value of the assignment */			\
    for (l = lst; l; l = l->next)			\
      {							\
	struct cobc_assign *p = l->item;		\
	if (op)						\
	  p->value = make_expr (p->field, op, v);	\
	else						\
	  p->value = v;					\
      }							\
    push_tree (make_status_sequence (lst));		\
  } while (0)

#define push_corr(func,g1,g2,opt) \
  push_tree (make_status_sequence (make_corr (func, g1, g2, opt, NULL)))

#define push_status_handler(val,st1,st2) \
  push_tree (make_if (make_cond (cobc_status, COBC_COND_EQ, val), st1, st2))

struct program_spec program_spec;

static struct cobc_field *current_field;
static struct cobc_file_name *current_file_name;
static struct cobc_label_name *current_section, *current_paragraph;
static int current_call_mode;
static cobc_tree inspect_name;

static struct cobc_list *last_exec_list;
static struct cobc_list *label_check_list;

static int warning_count = 0;
static int error_count = 0;

static void init_field (int level, cobc_tree field);
static void validate_field (struct cobc_field *p);
static void validate_field_tree (struct cobc_field *p);
static void validate_file_name (struct cobc_file_name *p);
static void validate_label_name (struct cobc_label_name *p);

static void field_set_used (struct cobc_field *p);
static int builtin_switch_id (cobc_tree x);

static cobc_tree make_add (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_sub (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_move (cobc_tree f1, cobc_tree f2, int round);
static struct cobc_list *make_corr (cobc_tree (*func)(), cobc_tree g1, cobc_tree g2, int opt, struct cobc_list *l);
static cobc_tree make_opt_cond (cobc_tree last, int type, cobc_tree this);
static cobc_tree make_cond_name (cobc_tree x);

static void redefinition_error (cobc_tree x);
static void undefined_error (struct cobc_word *w, cobc_tree parent);
static void ambiguous_error (struct cobc_word *p);
%}

%union {
  int inum;
  char *str;
  cobc_tree tree;
  struct cobc_word *word;
  struct cobc_list *list;
  struct cobc_picture *pict;
  struct cobc_generic *gene;
}

%left  '+', '-'
%left  '*', '/'
%left  '^'
%left  OR
%left  AND
%right NOT

%token <pict> PICTURE_TOK
%token <tree> INTEGER_LITERAL,NUMERIC_LITERAL,NONNUMERIC_LITERAL
%token <tree> CLASS_NAME,CONDITION_NAME,MNEMONIC_NAME
%token <word> WORD,LABEL_WORD

%token EQUAL,GREATER,LESS,GE,LE,COMMAND_LINE,ENVIRONMENT_VARIABLE,ALPHABET
%token DATE,DAY,DAY_OF_WEEK,TIME,READ,WRITE,OBJECT_COMPUTER,INPUT_OUTPUT
%token TO,FOR,IS,ARE,THRU,THAN,NO,CANCEL,ASCENDING,DESCENDING,ZERO
%token SOURCE_COMPUTER,BEFORE,AFTER,RESERVE,DECLARATIVES,USE
%token RIGHT,JUSTIFIED,SYNCHRONIZED,SEPARATE,BLOCK,CODE_SET
%token TOK_INITIAL,FIRST,ALL,LEADING,OF,IN,BY,STRING,UNSTRING,DEBUGGING
%token START,DELETE,PROGRAM,GLOBAL,EXTERNAL,SIZE,DELIMITED,COLLATING,SEQUENCE
%token GIVING,INSPECT,TALLYING,REPLACING,ON,OFF,POINTER,OVERFLOW,NATIVE
%token DELIMITER,COUNT,LEFT,TRAILING,CHARACTER,FILLER,OCCURS,TIMES,CLASS
%token ADD,SUBTRACT,MULTIPLY,DIVIDE,ROUNDED,REMAINDER,ERROR,SIZE,INDEX
%token REEL,UNIT,REMOVAL,REWIND,LOCK
%token FD,REDEFINES,TOK_FILE,USAGE,BLANK,SIGN,VALUE,MOVE
%token PROGRAM_ID,DIVISION,CONFIGURATION,SPECIAL_NAMES,MEMORY,ALTER
%token FILE_CONTROL,I_O_CONTROL,FROM,SAME,AREA,EXCEPTION,UNTIL
%token WORKING_STORAGE,LINKAGE,DECIMAL_POINT,COMMA,DUPLICATES,WITH,EXIT
%token LABEL,RECORD,RECORDS,STANDARD,STANDARD_1,STANDARD_2,VARYING,OMITTED
%token CONTAINS,CHARACTERS,COMPUTE,GO,STOP,RUN,ACCEPT,PERFORM,RENAMES
%token IF,ELSE,SENTENCE,LINE,PAGE,OPEN,CLOSE,REWRITE,SECTION,SYMBOLIC
%token ADVANCING,INTO,AT,END,NEGATIVE,POSITIVE,SPACE,NOT
%token CALL,USING,INVALID,CONTENT,QUOTE,LOW_VALUE,HIGH_VALUE
%token SELECT,ASSIGN,DISPLAY,UPON,SET,UP,DOWN,SEARCH
%token ORGANIZATION,ACCESS,MODE,KEY,STATUS,ALTERNATE,SORT,SORT_MERGE
%token SEQUENTIAL,INDEXED,DYNAMIC,RANDOM,RELATIVE,WHEN,TEST,PROCEED
%token END_ADD,END_CALL,END_COMPUTE,END_DELETE,END_DIVIDE,END_EVALUATE
%token END_IF,END_MULTIPLY,END_PERFORM,END_READ,END_REWRITE,END_SEARCH
%token END_START,END_STRING,END_SUBTRACT,END_UNSTRING,END_WRITE
%token THEN,EVALUATE,OTHER,ALSO,CONTINUE,CURRENCY,REFERENCE,INITIALIZE
%token NUMERIC,ALPHABETIC,ALPHABETIC_LOWER,ALPHABETIC_UPPER
%token DEPENDING,CORRESPONDING,CONVERTING,FUNCTION_NAME,OPTIONAL,RETURNING
%token IDENTIFICATION,ENVIRONMENT,DATA,PROCEDURE,TRUE,FALSE,ANY
%token AUTHOR,DATE_WRITTEN,DATE_COMPILED,INSTALLATION,SECURITY
%token COMMON,NEXT,PACKED_DECIMAL,INPUT,I_O,OUTPUT,EXTEND,BINARY
%token ALPHANUMERIC,ALPHANUMERIC_EDITED,NUMERIC_EDITED,NATIONAL,NATIONAL_EDITED

%type <str> class
%type <gene> tallying_item,replacing_item,inspect_before_after
%type <gene> call_item,write_option
%type <inum> flag_all,flag_duplicates,flag_optional,flag_global
%type <inum> flag_not,flag_next,flag_rounded,flag_separate
%type <inum> sign,integer,level_number,operator,display_upon,usage
%type <inum> before_or_after,perform_test,replacing_option,close_option
%type <inum> select_organization,select_access_mode,open_mode
%type <inum> ascending_or_descending
%type <list> occurs_key_list,occurs_index_list,value_item_list
%type <list> data_name_list,condition_name_list,opt_value_list
%type <list> evaluate_subject_list,evaluate_case,evaluate_case_list
%type <list> evaluate_when_list,evaluate_object_list
%type <list> inspect_tallying,inspect_replacing,inspect_converting
%type <list> label_list,subscript_list,number_list
%type <list> string_list,string_delimited_list,string_name_list
%type <list> tallying_list,replacing_list,inspect_before_after_list
%type <list> unstring_delimited,unstring_delimited_list,unstring_into
%type <list> unstring_delimited_item,unstring_into_item
%type <list> predefined_name_list,mnemonic_name_list
%type <list> file_name_list,math_name_list,math_edited_name_list
%type <list> call_item_list,call_using
%type <list> initialize_replacing,initialize_replacing_list
%type <list> special_name_class_item_list
%type <tree> special_name_class_item,special_name_class_literal
%type <tree> on_or_off,select_file_name
%type <tree> call_returning,add_to,field_description_list,value_item
%type <tree> field_description_list_1,field_description_list_2
%type <tree> condition,condition_2,comparative_condition,class_condition
%type <tree> sign_condition
%type <tree> imperative_statement,field_description
%type <tree> evaluate_object,evaluate_object_1
%type <tree> function,subscript,subref,refmod
%type <tree> opt_from_integer,opt_to_integer
%type <tree> search_varying,search_at_end,search_whens,search_when
%type <tree> perform_procedure,perform_sentence,perform_option
%type <tree> read_into,read_key,write_from,field_name,expr
%type <tree> file_name,opt_with_pointer,occurs_index,evaluate_subject
%type <tree> unstring_delimiter,unstring_count,unstring_tallying
%type <tree> opt_on_exception_sentence,at_end_sentence,not_at_end_sentence
%type <tree> invalid_key_sentence,not_invalid_key_sentence
%type <tree> opt_on_overflow_sentence,opt_not_on_overflow_sentence
%type <tree> opt_on_size_error_sentence,opt_not_on_size_error_sentence
%type <tree> numeric_name,numeric_edited_name,group_name,table_name,class_name
%type <tree> condition_name,qualified_cond_name,data_name,file_name
%type <tree> record_name,label_name,mnemonic_name,section_name,name
%type <tree> qualified_name,predefined_name
%type <list> qualified_predefined_word
%type <tree> integer_value,value,number
%type <tree> literal,basic_literal,figurative_constant
%type <word> qualified_word,label_word,undefined_word


%%
/*****************************************************************************
 * COBOL program sequence
 *****************************************************************************/

top:
  program_sequence		{ if (error_count) YYABORT; }
;
program_sequence:
  program
| program_sequence program
;
program:
  {
    /* init program spec */
    program_spec.program_id = NULL;
    program_spec.initial_program = 0;
    program_spec.class_list = NULL;
    program_spec.index_list = NULL;
    program_spec.file_name_list = NULL;
    program_spec.using_list = NULL;
    program_spec.exec_list = NULL;
    program_spec.input_handler = NULL;
    program_spec.output_handler = NULL;
    program_spec.i_o_handler = NULL;
    program_spec.extend_handler = NULL;
    label_check_list = NULL;
    /* init environment */
    cobc_in_procedure = 0;
    cob_decimal_point = '.';
    cob_currency_symbol = '$';
    /* init symbol table */
    init_word_table ();
    {
      cobc_tree rc = make_field_3 (lookup_user_word ("RETURN-CODE"),
				   "S9(9)", COBC_USAGE_BINARY);
      validate_field (COBC_FIELD (rc));
      finalize_field_tree (COBC_FIELD (rc));
    }
  }
  identification_division
  environment_division
  data_division
  {
    struct cobc_list *l;
    /* check if all required identifiers are defined in DATA DIVISION */
    program_spec.file_name_list = list_reverse (program_spec.file_name_list);
    for (l = program_spec.file_name_list; l; l = l->next)
      validate_file_name (l->item);
  }
  procedure_division
  _end_program
  {
    struct cobc_list *l;
    for (l = list_reverse (label_check_list); l; l = l->next)
      validate_label_name (l->item);
    program_spec.class_list = list_reverse (program_spec.class_list);
    program_spec.index_list = list_reverse (program_spec.index_list);
    program_spec.exec_list = list_reverse (program_spec.exec_list);
    if (error_count == 0)
      codegen (&program_spec);
  }
;
_end_program:
| END PROGRAM LABEL_WORD dot
;


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

identification_division:
  IDENTIFICATION DIVISION dot
  PROGRAM_ID '.' WORD opt_program_parameter dot
  identification_division_options
  {
    program_spec.program_id = $6->name;
  }
;
opt_program_parameter:
| _is TOK_INITIAL _program	{ program_spec.initial_program = 1; }
| _is COMMON _program		{ yywarn ("COMMON is not implemented yet"); }
;
identification_division_options:
| identification_division_options identification_division_option
;
identification_division_option:
  AUTHOR '.' comment		{ OBSOLETE ("AUTHOR"); }
| DATE_WRITTEN '.' comment	{ OBSOLETE ("DATE-WRITTEN"); }
| DATE_COMPILED '.' comment	{ OBSOLETE ("DATE-COMPILED"); }
| INSTALLATION '.' comment	{ OBSOLETE ("INSTALLATION"); }
| SECURITY '.' comment		{ OBSOLETE ("SECURITY"); }
;
comment: { cobc_skip_comment = 1; };


/*****************************************************************************
 * ENVIRONMENT DIVISION.
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION dot
  configuration_section
  input_output_section
;


/*******************
 * CONFICURATION SECTION
 *******************/

configuration_section:
| CONFIGURATION SECTION dot
  configuration_list
;
configuration_list:
| configuration_list configuration
;
configuration:
  source_computer
| object_computer
| special_names
;


/*
 * SOURCE COMPUTER
 */

source_computer:
  SOURCE_COMPUTER '.' WORD _with_debugging_mode dot
;
_with_debugging_mode:
| _with DEBUGGING MODE
  {
    yywarn ("DEBUGGING MODE is ignored");
    yywarn ("use compiler option `-debug' instead");
  }
;


/*
 * OBJECT COMPUTER
 */

object_computer:
  OBJECT_COMPUTER '.' WORD object_computer_options dot
;
object_computer_options:
| object_computer_options object_computer_option
;
object_computer_option:
  _program _collating SEQUENCE _is WORD	{ OBSOLETE ("COLLATING SEQUENCE"); }
| MEMORY SIZE _is integer CHARACTERS	{ OBSOLETE ("MEMORY SIZE"); }
;
_collating: | COLLATING ;


/*
 * SPECIAL-NAMES
 */

special_names:
  SPECIAL_NAMES '.' _special_names
;
_special_names:
| special_names dot
;
special_names:
  special_name
| special_names special_name
;
special_name:
  special_name_mnemonic
| special_name_alphabet
| special_name_symbolic
| special_name_class
| special_name_currency
| special_name_decimal_point
;


/* Buildin name */

special_name_mnemonic:
  WORD
  {
    int n = lookup_builtin_word ($1->name);
    if (n == 0)
      yyerror ("unknown name `%s'", $1->name);
    $<tree>$ = make_builtin (n);
  }
  special_name_mnemonic_define
  special_name_mnemonic_on_off
;
special_name_mnemonic_define:
| IS undefined_word
  {
    set_word_item ($2, $<tree>0);
  }
;
special_name_mnemonic_on_off:
| special_name_mnemonic_on_off
  on_or_off _status _is undefined_word
  {
    int id = builtin_switch_id ($<tree>-1);
    if (id != -1)
      {
	struct cobc_field *p = COBC_FIELD (make_field ($5));
	p->level = 88;
	p->parent = COBC_FIELD (cobc_switch[id]);
	p->value = $2;
	p->values = list (p->value);
	break;
      }
  }
;
on_or_off:
  ON				{ $$ = cobc_true; }
| OFF				{ $$ = cobc_false; }
;


/* ALPHABET */

special_name_alphabet:
  ALPHABET WORD _is alphabet_group
  {
    yywarn ("ALPHABET name is ignored");
  }
;
alphabet_group:
  STANDARD_1
| STANDARD_2
| NATIVE
| WORD { }
| alphabet_literal_list
;
alphabet_literal_list:
  alphabet_literal
| alphabet_literal_list alphabet_literal
;
alphabet_literal:
  literal alphabet_literal_option { }
;
alphabet_literal_option:
| THRU literal
| also_literal_list
;
also_literal_list:
  ALSO literal
| also_literal_list ALSO literal
;

/* SYMBOLIC CHARACTER */

special_name_symbolic:
  SYMBOLIC _characters symbolic_characters_list
  {
    yywarn ("SYMBOLIC CHARACTERS is ignored");
  }
;
symbolic_characters_list:
  symbolic_characters
| symbolic_characters_list symbolic_characters
;
symbolic_characters:
  char_list is_are integer_list
;
char_list:
  WORD { }
| char_list WORD { }
;
integer_list:
  integer { }
| integer_list integer { }
;
is_are: IS | ARE ;


/* CLASS */

special_name_class:
  CLASS undefined_word _is special_name_class_item_list
  {
    program_spec.class_list =
      cons (make_class ($2, $4), program_spec.class_list);
  }
;
special_name_class_item_list:
  special_name_class_item	{ $$ = list ($1); }
| special_name_class_item_list
  special_name_class_item	{ $$ = list_add ($1, $2); }
;
special_name_class_item:
  special_name_class_literal	{ $$ = $1; }
| special_name_class_literal THRU
  special_name_class_literal	{ $$ = make_pair ($1, $3); }
;
special_name_class_literal:
  literal
;


/* CURRENCY */

special_name_currency:
  CURRENCY _sign NONNUMERIC_LITERAL
  {
    unsigned char *s = COBC_LITERAL ($3)->str;
    if (strlen (s) != 1)
      yyerror ("invalid currency sign");
    cob_currency_symbol = s[0];
  }
;


/* DECIMAL_POINT */

special_name_decimal_point:
  DECIMAL_POINT _is COMMA	{ cob_decimal_point = ','; }
;


/*******************
 * INPUT-OUTPUT SECTION
 *******************/

input_output_section:
| INPUT_OUTPUT SECTION dot
  file_control
  i_o_control
;


/*
 * FILE-CONTROL
 */

file_control:
| FILE_CONTROL dot select_sequence
;
select_sequence:
| select_sequence
  SELECT flag_optional undefined_word
  {
    current_file_name = COBC_FILE_NAME (make_file_name ($4));
    current_file_name->organization = COB_ORG_LINE_SEQUENTIAL;
    current_file_name->access_mode = COB_ACCESS_SEQUENTIAL;
    current_file_name->optional = $3;
    program_spec.file_name_list =
      cons (current_file_name, program_spec.file_name_list);
  }
  select_options '.'
  {
    switch (current_file_name->organization)
      {
      case COB_ORG_INDEXED:
	if (!current_file_name->key)
	  yyerror ("RECORD KEY is required for file `%s'", $4->name);
	break;
      case COB_ORG_RELATIVE:
	if (current_file_name->access_mode != COB_ACCESS_SEQUENTIAL
	    && !current_file_name->key)
	  yyerror ("RELATIVE KEY is required for file `%s'", $4->name);
	break;
      }
  }
;
select_file_name:
  NONNUMERIC_LITERAL
| predefined_name
;
select_options:
| select_options select_option
;
select_option:
  ASSIGN _to select_file_name
  {
    current_file_name->assign = $3;
  }
| select_organization
  {
    current_file_name->organization = $1;
  }
| ORGANIZATION _is select_organization
  {
    current_file_name->organization = $3;
  }
| ACCESS _mode _is select_access_mode
  {
    current_file_name->access_mode = $4;
  }
| RELATIVE _key _is predefined_name
  {
    current_file_name->key = $4;
  }
| RECORD _key _is predefined_name
  {
    current_file_name->key = $4;
  }
| ALTERNATE RECORD _key _is predefined_name flag_duplicates
  {
    struct cobc_alt_key *p = malloc (sizeof (struct cobc_alt_key));
    p->key = $5;
    p->duplicates = $6;
    p->next = NULL;

    /* add to the end of list */
    if (current_file_name->alt_key_list == NULL)
      current_file_name->alt_key_list = p;
    else
      {
	struct cobc_alt_key *l = current_file_name->alt_key_list;
	for (; l->next; l = l->next);
	l->next = p;
      }
  }
| _file STATUS _is predefined_name
  {
    current_file_name->file_status = $4;
  }
| RESERVE integer _area		{ /* ignored */ }
;
select_organization:
  INDEXED			{ $$ = COB_ORG_INDEXED; }
| SEQUENTIAL			{ $$ = COB_ORG_SEQUENTIAL; }
| RELATIVE			{ $$ = COB_ORG_RELATIVE; }
| LINE SEQUENTIAL		{ $$ = COB_ORG_LINE_SEQUENTIAL; }
;
select_access_mode:
  SEQUENTIAL			{ $$ = COB_ACCESS_SEQUENTIAL; }
| DYNAMIC			{ $$ = COB_ACCESS_DYNAMIC; }
| RANDOM			{ $$ = COB_ACCESS_RANDOM; }
;
flag_optional:
  /* nothing */			{ $$ = 0; }
| OPTIONAL			{ $$ = 1; }
;
flag_duplicates:
  /* nothing */			{ $$ = 0; }
| _with DUPLICATES		{ $$ = 1; }
;


/*
 * I-O-CONTROL
 */

i_o_control:
| I_O_CONTROL '.'
  same_statement_list dot
  {
    yywarn ("I-O-CONTROL is not implemented yet");
  }
;
same_statement_list:
| same_statement_list same_statement
;
same_statement:
  SAME same_option _area _for file_name_list
;
same_option:
| RECORD
| SORT
| SORT_MERGE
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION dot
  file_section
  working_storage_section
  linkage_section
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| TOK_FILE SECTION dot
  file_description_sequence
;
file_description_sequence:
| file_description_sequence
  FD file_name			{ current_file_name = COBC_FILE_NAME ($3); }
  file_options '.'
  field_description_list
  {
    struct cobc_field *p = COBC_FIELD ($7);
    current_file_name->record = p;
    p->file = COBC_TREE (current_file_name);
    field_set_used (p);
    for (p = p->sister; p; p = p->sister)
      {
	p->redefines = current_file_name->record;
	p->file = COBC_TREE (current_file_name);
	field_set_used (p);
      }
  }
;
file_options:
| file_options file_option
;
file_option:
  _is GLOBAL			{ yyerror ("GLOBAL is not implemented"); }
| _is EXTERNAL			{ yyerror ("EXTERNAL is not implemented"); }
| block_clause
| record_clause
| label_clause
| data_clause
| codeset_clause
;


/*
 * BLOCK clause
 */

block_clause:
  BLOCK _contains integer opt_to_integer _records_or_characters
  {
    IGNORE ("BLOCK");
  }
;
_contains: | CONTAINS ;
_records_or_characters: | RECORDS | CHARACTERS ;


/*
 * RECORD clause
 */

record_clause:
  RECORD _contains integer _characters
  {
    current_file_name->record_max = $3;
  }
| RECORD _contains integer _to integer _characters
  {
    yywarn ("RECORD CONTAINS is not implemented");
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  DEPENDING _on data_name
  {
    yywarn ("RECORD VARYING is not implemented");
  }
;
opt_from_integer:
  /* nothing */			{ $$ = NULL; }
| _from INTEGER_LITERAL		{ $$ = $2; }
;
opt_to_integer:
  /* nothing */			{ $$ = NULL; }
| TO INTEGER_LITERAL		{ $$ = $2; }
;
_characters: | CHARACTERS ;


/*
 * LABEL clause
 */

label_clause:
  LABEL record_or_records label_option { IGNORE ("LABEL RECORD"); }
;
label_option:
  STANDARD
| OMITTED
;
record_or_records:
  RECORD _is
| RECORDS _are
;


/*
 * DATA clause
 */

data_clause:
  DATA record_or_records undefined_word_list { /* ignore */ }
;


/*
 * CODE-SET clause
 */

codeset_clause:
  CODE_SET _is WORD
  {
    yyerror ("CODE-SET is not implemented");
  }
;


/*******************
 * WORKING-STRAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION dot
  field_description_list
  {
    if ($4)
      program_spec.working_storage = COBC_FIELD ($4);
  }
;
field_description_list:
  /* nothing */			{ $$ = NULL; }
| field_description_list_1	{ $$ = $1; }
;
field_description_list_1:
  {
    current_field = NULL;
  }
  field_description_list_2
  {
    struct cobc_field *p;
    for (p = COBC_FIELD ($2); p; p = p->sister)
      {
	validate_field_tree (p);
	finalize_field_tree (p);
      }
    $$ = $2;
  }
;
field_description_list_2:
  field_description		{ $$ = $1; }
| field_description_list_2
  field_description		{ $$ = $1; }
;
field_description:
  level_number field_name
  {
    init_field ($1, $2);
  }
  field_options dot
  {
    validate_field (current_field);
    $$ = COBC_TREE (current_field);
  }
;
field_name:
  /* nothing */			{ $$ = make_filler (); }
| FILLER			{ $$ = make_filler (); }
| WORD				{ $$ = make_field ($1); }
;
field_options:
| field_options field_option
;
field_option:
  redefines_clause
| external_clause
| global_clause
| picture_clause
| usage_clause
| sign_clause
| occurs_clause
| justified_clause
| synchronized_clause
| blank_clause
| value_clause
| renames_clause
;


/* REDEFINES */

redefines_clause:
  REDEFINES WORD
  {
    switch ($2->count)
      {
      case 0:
	undefined_error ($2, 0);
	break;
      case 1:
	current_field->redefines = COBC_FIELD ($2->item);
	break;
      default:
	current_field->redefines =
	  COBC_FIELD (lookup_qualified_word ($2, current_field->parent)->item);
      }
  }
;


/* EXTERNAL */

external_clause:
  _is EXTERNAL			{ current_field->f.external = 1; }
;


/* GLOBAL */

global_clause:
  _is GLOBAL			{ yywarn ("GLOBAL is not implemented"); }
;


/* PICTURE */

picture_clause:
  PICTURE_TOK			{ current_field->pic = $1; }
;


/* USAGE */

usage_clause:
  _usage _is usage
;
usage:
  DISPLAY
  {
    current_field->usage = COBC_USAGE_DISPLAY;
  }
| BINARY /* or COMP */
  {
    current_field->usage = COBC_USAGE_BINARY;
  }
| INDEX
  {
    current_field->usage = COBC_USAGE_INDEX;
    current_field->pic = make_picture ();
    current_field->pic->digits = 9;
  }
| PACKED_DECIMAL /* or COMP-3 */
  {
    current_field->usage = COBC_USAGE_PACKED;
  }
;
_usage: | USAGE ;


/* SIGN */

sign_clause:
  _sign LEADING flag_separate
  {
    current_field->f.sign_separate = $3;
    current_field->f.sign_leading  = 1;
  }
| _sign TRAILING flag_separate
  {
    current_field->f.sign_separate = $3;
    current_field->f.sign_leading  = 0;
  }
;
flag_separate:
  /* nothing */			{ $$ = 0; }
| SEPARATE _character		{ $$ = 1; }
;
_character: | CHARACTER ;


/* OCCURS */

occurs_clause:
  OCCURS integer _times
  occurs_keys occurs_indexed
  {
    current_field->occurs = $2;
    current_field->occurs_min = 1;
    current_field->f.have_occurs = 1;
  }
| OCCURS integer TO integer _times DEPENDING _on data_name
  occurs_keys occurs_indexed
  {
    current_field->occurs = $4;
    current_field->occurs_min = $2;
    current_field->occurs_depending = $8;
    current_field->f.have_occurs = 1;
  }
;

occurs_keys:
  occurs_key_list
  {
    if ($1)
      {
	int i, nkeys = list_length ($1);
	struct cobc_key *keys = malloc (sizeof (struct cobc_key) * nkeys);
	struct cobc_list *l = $1;
	for (i = 0; i < nkeys; i++)
	  {
	    struct cobc_generic *p = l->item;
	    keys[i].dir = p->type;
	    keys[i].key = p->x;
	    l = l->next;
	  }
	current_field->keys = keys;
	current_field->nkeys = nkeys;
      }
  }
;
occurs_key_list:
  /* nothing */			{ $$ = NULL; }
| occurs_key_list
  ascending_or_descending _key _is predefined_name_list
  {
    struct cobc_list *l;
    for (l = $5; l; l = l->next)
      l->item = make_generic_1 ($2, l->item);
    $$ = list_append ($1, $5);
  }
;
ascending_or_descending:
  ASCENDING			{ $$ = COBC_ASCENDING; }
| DESCENDING			{ $$ = COBC_DESCENDING; }
;

occurs_indexed:
| INDEXED _by occurs_index_list
  {
    current_field->index_list = $3;
  }
;
occurs_index_list:
  occurs_index			{ $$ = list ($1); }
| occurs_index_list
  occurs_index			{ $$ = list_add ($1, $2); }
;
occurs_index:
  WORD
  {
    $$ = make_field_3 ($1, "S9(9)", COBC_USAGE_INDEX);
    validate_field (COBC_FIELD ($$));
    finalize_field_tree (COBC_FIELD ($$));
    program_spec.index_list = cons ($$, program_spec.index_list);
  }

_times: | TIMES ;


/* JUSTIFIED RIGHT */

justified_clause:
  JUSTIFIED _right		{ current_field->f.justified = 1; }
;
_right: | RIGHT ;


/* SYNCHRONIZED */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ current_field->f.synchronized = 1; }
;
left_or_right:
| LEFT
| RIGHT
;


/* BLANK */

blank_clause:
  BLANK _when ZERO		{ current_field->f.blank_zero = 1; }
;


/* VALUE */

value_clause:
  VALUE _is_are value_item_list
  {
    if (current_field->level == 88)
      {
	/* 88 condition */
	current_field->values = $3;
	if (COBC_PAIR_P ($3->item))
	  current_field->value = COBC_PAIR ($3->item)->x;
	else
	  current_field->value = $3->item;
      }
    else
      {
	/* single VALUE */
	if ($3->next != NULL || COBC_PAIR_P ($3->item))
	  yyerror ("only level 88 item may have multiple values");
	else
	  current_field->value = $3->item;
      }
  }
;
value_item_list:
  value_item			{ $$ = list ($1); }
| value_item_list value_item	{ $$ = list_add ($1, $2); }
;
value_item:
  literal			{ $$ = $1; }
| literal THRU literal		{ $$ = make_pair ($1, $3); }
;


/* RENAMES */

renames_clause:
  RENAMES qualified_name
  {
    current_field->redefines = COBC_FIELD ($2);
    current_field->pic = current_field->redefines->pic;
  }
| RENAMES qualified_name THRU qualified_name
  {
    current_field->redefines = COBC_FIELD ($2);
    current_field->rename_thru = COBC_FIELD ($4);
  }
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION dot
  field_description_list
  {
    if ($4)
      program_spec.linkage_storage = COBC_FIELD ($4);
  }
;


/*****************************************************************************
 * PROCEDURE DIVISION
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION procedure_using dot
  {
    current_section = NULL;
    current_paragraph = NULL;
    cobc_in_procedure = 1;
  }
  procedure_declaratives
  {
    push_label (COBC_LABEL_NAME (make_label_name (make_word ("$MAIN$"))));
  }
  procedure_list
  {
    if (current_paragraph)
      push_exit_section (current_paragraph);
    if (current_section)
      push_exit_section (current_section);
  }
;
procedure_using:
| USING data_name_list
  {
    if (!cobc_module_flag)
      {
	yywarn ("compiled as a module due to USING clause");
	yywarn ("use compiler option `-m' explicitly");
	cobc_module_flag = 1;
      }
    program_spec.using_list = $2;
  }
;

procedure_declaratives:
| DECLARATIVES dot
  procedure_list
  END DECLARATIVES
;


/*******************
 * Procedure list
 *******************/

procedure_list:
| procedure_list		{ last_exec_list = program_spec.exec_list; }
  procedure
  {
    struct cobc_list *l;
    for (l = program_spec.exec_list; l; l = l->next)
      if (l->next == last_exec_list)
	COBC_TREE (l->item)->loc = @3;
  }
;
procedure:
  section_header
| paragraph_header
| statement
| error '.'
| '.'
;


/*******************
 * Section/Paragraph
 *******************/

section_header:
  section_name SECTION dot
  {
    /* Exit the last section */
    if (current_paragraph)
      push_exit_section (current_paragraph);
    if (current_section)
      push_exit_section (current_section);

    /* Begin a new section */
    current_section = COBC_LABEL_NAME ($1);
    current_paragraph = NULL;
    push_label (current_section);
  }
  opt_use_statement
;

paragraph_header:
  section_name dot
  {
    /* Exit the last paragraph */
    if (current_paragraph)
      push_exit_section (current_paragraph);

    /* Begin a new paragraph */
    current_paragraph = COBC_LABEL_NAME ($1);
    current_paragraph->section = current_section;
    if (current_section)
      current_section->children =
	cons (current_paragraph, current_section->children);
    push_label (current_paragraph);
  }
;

/*
 * USE statement
 */

opt_use_statement:
| use_statement
;
use_statement:
  USE flag_global AFTER _standard exception_or_error PROCEDURE _on use_target
;
use_target:
  file_name_list
  {
    struct cobc_list *l;
    for (l = $1; l; l = l->next)
      COBC_FILE_NAME (l->item)->handler = current_section;
  }
| INPUT		{ program_spec.input_handler = current_section; }
| OUTPUT	{ program_spec.output_handler = current_section; }
| I_O		{ program_spec.i_o_handler = current_section; }
| EXTEND	{ program_spec.extend_handler = current_section; }
;
_standard: | STANDARD ;
exception_or_error: EXCEPTION | ERROR ;


/*******************
 * Statements
 *******************/

imperative_statement:
  {
    $<list>$ = program_spec.exec_list;
    program_spec.exec_list = NULL;
  }
  statement_list
  {
    $$ = make_sequence (list_reverse (program_spec.exec_list));
    program_spec.exec_list = $<list>1;
  }
;

statement_list:
  statement
| statement_list statement
;
statement:
  accept_statement
| add_statement
| alter_statement
| call_statement
| cancel_statement
| close_statement
| compute_statement
| delete_statement
| display_statement
| divide_statement
| evaluate_statement
| exit_statement
| goto_statement
| if_statement
| initialize_statement
| inspect_statement
| move_statement
| multiply_statement
| open_statement
| perform_statement
| read_statement
| rewrite_statement
| search_statement
| set_statement
| start_statement
| stoprun_statement
| string_statement
| subtract_statement
| unstring_statement
| write_statement
| CONTINUE
| NEXT SENTENCE
;


/*
 * ACCEPT statement
 */

accept_statement:
  ACCEPT data_name
  {
    push_call_1 (COB_ACCEPT, $2);
  }
| ACCEPT data_name FROM DATE
  {
    push_call_1 (COB_ACCEPT_DATE, $2);
  }
| ACCEPT data_name FROM DAY
  {
    push_call_1 (COB_ACCEPT_DAY, $2);
  }
| ACCEPT data_name FROM DAY_OF_WEEK
  {
    push_call_1 (COB_ACCEPT_DAY_OF_WEEK, $2);
  }
| ACCEPT data_name FROM TIME
  {
    push_call_1 (COB_ACCEPT_TIME, $2);
  }
| ACCEPT data_name FROM COMMAND_LINE
  {
    push_call_1 (COB_ACCEPT_COMMAND_LINE, $2);
  }
| ACCEPT data_name FROM ENVIRONMENT_VARIABLE value
  {
    push_call_2 (COB_ACCEPT_ENVIRONMENT, $2, $5);
  }
| ACCEPT data_name FROM mnemonic_name
  {
    if (COBC_BUILTIN ($4)->id == BUILTIN_STDIN)
      push_call_1 (COB_ACCEPT, $2);
    else
      yyerror ("ACCEPT FROM is allowed only from STDIN");
  }
;


/*
 * ADD statement
 */

add_statement:
  ADD add_body opt_on_size_error end_add
;
add_body:
  number_list TO math_name_list
  {
    /* ADD A B C TO X Y -->
       (let ((t (+ a b c))) (set! x (+ x t)) (set! y (+ y t))) */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    push_assign ($3, '+', e);
  }
| number_list add_to GIVING math_edited_name_list
  {
    /* ADD A B TO C GIVING X Y -->
       (let ((t (+ a b c))) (set! x t) (set! y t)) */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    if ($2)
      e = make_expr (e, '+', $2);
    push_assign ($4, 0, e);
  }
| CORRESPONDING group_name _to group_name flag_rounded
  {
    push_corr (make_add, $2, $4, $5);
  }
;
add_to:
  /* nothing */			{ $$ = NULL; }
| TO value			{ $$ = $2; }
;
end_add: | END_ADD ;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options		{  yywarn ("ALTER statement is obsolete"); }
;
alter_options:
| alter_options
  label_name TO _proceed_to label_name
;
_proceed_to: | PROCEED TO ;


/*
 * CALL statement
 */

call_statement:
  CALL value			{ current_call_mode = COBC_CALL_BY_REFERENCE; }
  call_using call_returning
  {
    push_call_3 (COB_CALL, $2, $4, $5);
  }
  opt_on_exception_or_overflow
  _end_call
;
call_using:
  /* nothing */			{ $$ = NULL; }
| USING call_item_list		{ $$ = $2; }
;
call_item_list:
  call_item			{ $$ = list ($1); }
| call_item_list
  call_item			{ $$ = list_add ($1, $2); }
;
call_item:
  value				{ $$ = make_generic_1 (current_call_mode, $1);}
| BY call_mode value		{ $$ = make_generic_1 (current_call_mode, $3);}
;
call_mode:
  REFERENCE			{ current_call_mode = COBC_CALL_BY_REFERENCE; }
| CONTENT			{ current_call_mode = COBC_CALL_BY_CONTENT; }
| VALUE				{ current_call_mode = COBC_CALL_BY_VALUE; }
;
call_returning:
  /* nothing */			{ $$ = NULL; }
| RETURNING data_name		{ $$ = $2; }
;

opt_on_exception_or_overflow:
  opt_on_exception_sentence
  opt_not_on_overflow_sentence
  {
    push_status_handler (cobc_int0, $2, $1);
  }
;
opt_on_exception_sentence:
  /* nothing */			{ $$ = make_call_0 (COB_CALL_ERROR); }
| _on exception_or_overflow
  imperative_statement		{ $$ = $3; }
;
exception_or_overflow:
  EXCEPTION
| OVERFLOW
;

_end_call: | END_CALL ;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL value
  {
    push_call_1 (COB_CANCEL, $2);
  }
;


/*
 * CLOSE statement
 */

close_statement:
  CLOSE close_file_list
;
close_file_list:
  close_file
| close_file_list close_file
;
close_file:
  file_name close_option
  {
    push_call_2 (COB_CLOSE, $1, make_integer ($2));
  }
;
close_option:
  /* nothing */			{ $$ = COB_CLOSE_NORMAL; }
| REEL				{ $$ = COB_CLOSE_REEL; }
| REEL _for REMOVAL		{ $$ = COB_CLOSE_REEL_REMOVAL; }
| UNIT				{ $$ = COB_CLOSE_UNIT; }
| UNIT _for REMOVAL		{ $$ = COB_CLOSE_UNIT_REMOVAL; }
| _with NO REWIND		{ $$ = COB_CLOSE_NO_REWIND; }
| _with LOCK			{ $$ = COB_CLOSE_LOCK; }
;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE compute_body opt_on_size_error _end_compute
;
compute_body:
  math_edited_name_list '=' expr
  {
    if (!is_numeric ($3))
      yyerror ("invalid expression");
    else
      {
	struct cobc_list *l;
	for (l = $1; l; l = l->next)
	  {
	    struct cobc_assign *p = l->item;
	    p->value = $3;
	  }
	push_tree (make_status_sequence ($1));
      }
  }
;
_end_compute: | END_COMPUTE ;


/*
 * DELETE statement
 */

delete_statement:
  DELETE file_name _record
  {
    current_file_name = COBC_FILE_NAME ($2);
    push_call_1 (COB_DELETE, $2);
  }
  opt_invalid_key
  _end_delete
;
_end_delete: | END_DELETE ;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY opt_value_list display_upon
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_call_2 (COB_DISPLAY, l->item, make_integer ($3));
  }
  display_with_no_advancing
  ;
display_upon:
  /* nothing */			{ $$ = COB_STDOUT; }
| _upon mnemonic_name
  {
    switch (COBC_BUILTIN ($2)->id)
      {
      case BUILTIN_STDOUT: $$ = COB_STDOUT; break;
      case BUILTIN_STDERR: $$ = COB_STDERR; break;
      default:
	yyerror ("invalid UPON item");
	$$ = COB_STDOUT;
	break;
      }
  }
;
display_with_no_advancing:
  /* nothing */ { push_call_1 (COB_NEWLINE, make_integer ($<inum>-1)); }
| _with NO ADVANCING { /* nothing */ }
;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE divide_body opt_on_size_error _end_divide
;
divide_body:
  number INTO math_name_list
  {
    push_assign ($3, '/', $1);
  }
| number INTO number GIVING math_edited_name_list
  {
    push_assign ($5, 0, make_expr ($3, '/', $1));
  }
| number BY number GIVING math_edited_name_list
  {
    push_assign ($5, 0, make_expr ($1, '/', $3));
  }
| number INTO number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    push_call_4 (COB_DIVIDE_QUOTIENT, $3, $1, $5, make_integer ($6));
    push_call_1 (COB_DIVIDE_REMAINDER, $8);
  }
| number BY number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    push_call_4 (COB_DIVIDE_QUOTIENT, $1, $3, $5, make_integer ($6));
    push_call_1 (COB_DIVIDE_REMAINDER, $8);
  }
;
_end_divide: | END_DIVIDE ;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE evaluate_subject_list evaluate_case_list _end_evaluate
  {
    push_tree (make_evaluate ($2, $3));
  }
;

evaluate_subject_list:
  evaluate_subject		{ $$ = list ($1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $$ = list_add ($1, $3); }
;
evaluate_subject:
  expr _is			{ $$ = $1; }
| condition			{ $$ = $1; }
| TRUE				{ $$ = cobc_true; }
| FALSE				{ $$ = cobc_false; }
;

evaluate_case_list:
  /* nothing */			{ $$ = NULL; }
| evaluate_case_list
  evaluate_case			{ $$ = list_add ($1, $2); }
;
evaluate_case:
  evaluate_when_list
  imperative_statement		{ $$ = cons ($2, $1); }
| WHEN OTHER
  imperative_statement		{ $$ = cons ($3, NULL); }
;
evaluate_when_list:
  WHEN evaluate_object_list	{ $$ = list ($2); }
| evaluate_when_list
  WHEN evaluate_object_list	{ $$ = list_add ($1, $3); }
;
evaluate_object_list:
  evaluate_object		{ $$ = list ($1); }
| evaluate_object_list ALSO
  evaluate_object		{ $$ = list_add ($1, $3); }
;
evaluate_object:
  flag_not evaluate_object_1
  {
    if ($1)
      {
	if ($2 == cobc_any || $2 == cobc_true || $2 == cobc_false)
	  {
	    yyerror ("cannot use NOT with TRUE, FALSE, or ANY");
	    $$ = $2;
	  }
	else
	  {
	    /* NOTE: $2 is not necessarily a condition, but
	     * we use COBC_COND_NOT here to store it, which
	     * is later expanded in output_evaluate_test. */
	    $$ = make_unary_cond ($2, COBC_COND_NOT);
	  }
      }
    else
      {
	$$ = $2;
	COBC_TREE ($$)->loc = @2;
      }
  }
;
evaluate_object_1:
  ANY				{ $$ = cobc_any; }
| TRUE				{ $$ = cobc_true; }
| FALSE				{ $$ = cobc_false; }
| condition			{ $$ = $1; }
| expr				{ $$ = $1; }
| expr THRU expr		{ $$ = make_pair ($1, $3); }
;
_end_evaluate: | END_EVALUATE ;


/*
 * EXIT statement
 */

exit_statement:
  EXIT				{ /* do nothing */ }
| EXIT PROGRAM			{ push_call_0 (COB_EXIT_PROGRAM); }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to label_list
  {
    if ($3->next)
      yyerror ("too many labels with GO TO");
    else
      push_call_1 (COB_GOTO, $3->item);
  }
| GO _to label_list DEPENDING _on numeric_name
  {
    push_call_2 (COB_GOTO_DEPENDING, $3, $6);
  }
| GO _to { yywarn ("GO TO without label is obsolete"); }
;


/*
 * IF statement
 */

if_statement:
  IF condition _then imperative_statement _end_if
  {
    push_tree (make_if ($2, $4, NULL));
  }
| IF condition _then imperative_statement ELSE imperative_statement _end_if
  {
    push_tree (make_if ($2, $4, $6));
  }
| IF error END_IF
;
_end_if: | END_IF ;


/*
 * INITIALIZE statement
 */

initialize_statement:
  INITIALIZE data_name_list initialize_replacing
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      if (!$3)
	push_call_1 (COB_INITIALIZE, l->item);
      else
	push_call_2 (COB_INITIALIZE_REPLACING, l->item, $3);
  }
;
initialize_replacing:
  /* nothing */			      { $$ = NULL; }
| REPLACING initialize_replacing_list { $$ = $2; }
;
initialize_replacing_list:
  /* nothing */			      { $$ = NULL; }
| initialize_replacing_list
  replacing_option _data BY value
  {
    $$ = list_add ($1, make_pair ((void *) $2, $5));
  }
;
replacing_option:
  ALPHABETIC			{ $$ = COB_ALPHABETIC; }
| ALPHANUMERIC			{ $$ = COB_ALPHANUMERIC; }
| NUMERIC			{ $$ = COB_NUMERIC; }
| ALPHANUMERIC_EDITED		{ $$ = COB_ALPHANUMERIC_EDITED; }
| NUMERIC_EDITED		{ $$ = COB_NUMERIC_EDITED; }
| NATIONAL			{ $$ = COB_NATIONAL; }
| NATIONAL_EDITED		{ $$ = COB_NATIONAL_EDITED; }
;
_data: | DATA ;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT data_name inspect_tallying
  {
    push_call_2 (COB_INSPECT_TALLYING, $2, $3);
  }
| INSPECT data_name inspect_replacing
  {
    push_call_2 (COB_INSPECT_REPLACING, $2, $3);
  }
| INSPECT data_name inspect_converting
  {
    push_call_2 (COB_INSPECT_CONVERTING, $2, $3);
  }
| INSPECT data_name inspect_tallying inspect_replacing
  {
    push_call_2 (COB_INSPECT_TALLYING, $2, $3);
    push_call_2 (COB_INSPECT_REPLACING, $2, $4);
  }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING tallying_list	{ $$ = $2; }
;
tallying_list:
  data_name FOR			{ inspect_name = $1; }
  tallying_item			{ $$ = list ($4); }
| tallying_list data_name FOR	{ inspect_name = $2; }
  tallying_item			{ $$ = list_add ($1, $5); }
| tallying_list tallying_item	{ $$ = list_add ($1, $2); }
;
tallying_item:
  CHARACTERS inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_CHARACTERS, inspect_name, 0, $2);
  }
| ALL value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_ALL, inspect_name, $2, $3);
  }
| LEADING value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_LEADING, inspect_name, $2, $3);
  }

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_list	{ $$ = $2; }
;
replacing_list:
  replacing_item		{ $$ = list ($1); }
| replacing_list replacing_item	{ $$ = list_add ($1, $2); }
;
replacing_item:
  CHARACTERS BY value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_CHARACTERS, NULL, $3, $4);
  }
| ALL value BY value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_ALL, $4, $2, $5);
  }
| LEADING value BY value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_LEADING, $4, $2, $5);
  }
| FIRST value BY value inspect_before_after_list
  {
    $$ = make_generic (COB_INSPECT_FIRST, $4, $2, $5);
  }

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING value TO value inspect_before_after_list
  {
    $$ = list (make_generic (COB_INSPECT_CONVERTING, $2, $4, $5));
  }

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* nothing */					 { $$ = NULL; }
| inspect_before_after_list inspect_before_after { $$ = list_add ($1, $2); }
;
inspect_before_after:
  BEFORE _initial value
  {
    $$ = make_generic (COB_INSPECT_BEFORE, $3, 0, 0);
  }
| AFTER _initial value
  {
    $$ = make_generic (COB_INSPECT_AFTER, $3, 0, 0);
  }
;
_initial: | TOK_INITIAL ;


/*
 * MOVE statement
 */

move_statement:
  MOVE value TO data_name_list
  {
    struct cobc_list *l;
    for (l = $4; l; l = l->next)
      push_move ($2, l->item);
  }
| MOVE CORRESPONDING group_name TO group_name
  {
    push_corr (make_move, $3, $5, 0);
  }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY multiply_body opt_on_size_error _end_multiply
;
multiply_body:
  number BY math_name_list
  {
    push_assign ($3, '*', $1);
  }
| number BY number GIVING math_edited_name_list
  {
    push_assign ($5, 0, make_expr ($1, '*', $3));
  }
;
_end_multiply: | END_MULTIPLY ;


/*
 * OPEN statement
 */

open_statement:
  OPEN open_options
;
open_options:
  open_option
| open_options open_option
;
open_option:
  open_mode file_name_list
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      {
	struct cobc_file_name *p = COBC_FILE_NAME (l->item);
	push_call_3 (COB_OPEN, p, p->assign, make_integer ($1));
      }
  }
;
open_mode:
  INPUT				{ $$ = COB_OPEN_INPUT; }
| OUTPUT			{ $$ = COB_OPEN_OUTPUT; }
| I_O				{ $$ = COB_OPEN_I_O; }
| EXTEND			{ $$ = COB_OPEN_EXTEND; }
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM perform_procedure perform_option
  {
    COBC_PERFORM ($3)->body = $2;
    push_tree ($3);
  }
| PERFORM perform_option perform_sentence
  {
    COBC_PERFORM ($2)->body = $3;
    push_tree ($2);
  }
;

perform_procedure:
  label_name			{ $$ = make_pair ($1, 0); }
| label_name THRU label_name	{ $$ = make_pair ($1, $3); }
;

perform_option:
  /* nothing */
  {
    $$ = make_perform (COBC_PERFORM_ONCE);
  }
| integer_value TIMES
  {
    $$ = make_perform (COBC_PERFORM_TIMES);
    COBC_PERFORM ($$)->data = $1;
  }
| perform_test UNTIL condition
  {
    $$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($$)->test = $1;
    add_perform_varying (COBC_PERFORM ($$), 0, 0, 0, $3);
  }
| perform_test VARYING numeric_name FROM value BY value UNTIL condition
  {
    $<tree>$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($<tree>$)->test = $1;
    add_perform_varying (COBC_PERFORM ($<tree>$), $3, $5, $7, $9);
  }
  perform_after_list
  {
    $$ = $<tree>10;
  }
;
perform_test:
  /* nothing */			{ $$ = COBC_BEFORE; }
| _with TEST before_or_after	{ $$ = $3; }
;
perform_after_list:
| perform_after_list
  AFTER numeric_name FROM value BY value UNTIL condition
  {
    add_perform_varying (COBC_PERFORM ($<tree>0), $3, $5, $7, $9);
  }
;

perform_sentence:
  imperative_statement END_PERFORM
;


/*
 * READ statements
 */

read_statement:
  READ file_name flag_next _record read_into read_key
  {
    current_file_name = COBC_FILE_NAME ($2);
    if ($3 || current_file_name->access_mode == COB_ACCESS_SEQUENTIAL)
      {
	/* READ NEXT */
	if ($6)
	  yywarn ("KEY ignored with sequential READ");
	push_call_1 (COB_READ_NEXT, $2);
      }
    else
      {
	/* READ */
	if (current_file_name->organization == COB_ORG_INDEXED)
	  {
	    if (!$6)
	      $6 = current_file_name->key;
	    push_call_3 (COB_START, $2, make_integer (COB_EQ), $6);
	  }
	push_call_1 (COB_READ, $2);
      }
    if ($5)
      push_move (COBC_TREE (current_file_name->record), $5);
  }
  read_option
  _end_read
;
read_into:
  /* nothing */			{ $$ = NULL; }
| INTO data_name		{ $$ = $2; }
;
read_key:
  /* nothing */			{ $$ = NULL; }
| KEY _is data_name		{ $$ = $3; }
;
read_option:
  /* nothing */
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, 0, 0, 0);
  }
| at_end
| invalid_key
;
_end_read: | END_READ ;


/*
 * REWRITE statement
 */

rewrite_statement:
  REWRITE record_name write_from
  {
    cobc_tree file = COBC_FIELD ($2)->file;
    current_file_name = COBC_FILE_NAME (file);
    if ($3)
      push_move ($3, $2);
    push_call_1 (COB_REWRITE, file);
  }
  opt_invalid_key
  _end_rewrite
;
_end_rewrite: | END_REWRITE ;


/*
 * SEARCH statement
 */

search_statement:
  SEARCH table_name search_varying search_at_end search_whens _end_search
  {
    push_call_4 (COB_SEARCH, $2, $3, $4, $5);
  }
| SEARCH ALL table_name search_at_end search_when _end_search
  {
    push_call_3 (COB_SEARCH_ALL, $3, $4, $5);
  }
;
search_varying:
  /* nothing */			{ $$ = NULL; }
| VARYING data_name		{ $$ = $2; }
;
search_at_end:
  /* nothing */			{ $$ = NULL; }
| _at END imperative_statement	{ $$ = $3; }
;
search_whens:
  search_when			{ $$ = $1; }
| search_when search_whens	{ $$ = $1; COBC_IF ($1)->stmt2 = $2; }
;
search_when:
  WHEN condition imperative_statement { $$ = make_if ($2, $3, 0); }
;
_end_search: | END_SEARCH ;


/*
 * SET statement
 */

set_statement:
  SET data_name_list TO number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_move ($4, l->item);
  }
| SET data_name_list UP BY number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_tree (make_op_assign (l->item, '+', $5));
  }
| SET data_name_list DOWN BY number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_tree (make_op_assign (l->item, '-', $5));
  }
| SET condition_name_list TO TRUE
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      {
	cobc_tree x = l->item;
	cobc_tree p = COBC_TREE (COBC_FIELD (x)->parent);
	if (COBC_SUBREF_P (x))
	  p = make_subref (p, COBC_SUBREF (x)->subs);
	push_move (COBC_TREE (COBC_FIELD (x)->value), p);
      }
  }
| SET set_on_off_list
;
set_on_off_list:
  set_on_off
| set_on_off_list set_on_off
;
set_on_off:
  mnemonic_name_list TO on_or_off
  {
    struct cobc_list *l;
    for (l = $1; l; l = l->next)
      {
	int id = builtin_switch_id (l->item);
	if (id != -1)
	  push_move ($3, cobc_switch[id]);
      }
  }
;


/*
 * START statement
 */

start_statement:
  START start_body opt_invalid_key _end_start
;
start_body:
  file_name
  {
    current_file_name = COBC_FILE_NAME ($1);
    push_call_3 (COB_START, $1, make_integer (COB_EQ), current_file_name->key);
  }
| file_name KEY _is operator data_name
  {
    int cond = 0;
    current_file_name = COBC_FILE_NAME ($1);
    switch ($4)
      {
      case COBC_COND_EQ: cond = COB_EQ; break;
      case COBC_COND_LT: cond = COB_LT; break;
      case COBC_COND_LE: cond = COB_LE; break;
      case COBC_COND_GT: cond = COB_GT; break;
      case COBC_COND_GE: cond = COB_GE; break;
      case COBC_COND_NE: cond = COB_NE; break;
      }
    push_call_3 (COB_START, $1, make_integer (cond), $5);
  }
;
_end_start: | END_START ;


/*
 * STOP RUN statement
 */

stoprun_statement:
  STOP RUN			{ push_call_0 (COB_STOP_RUN); }
| STOP NONNUMERIC_LITERAL	{ yywarn ("STOP literal is obsolete"); }
;


/*
 * STRING statement
 */

string_statement:
  STRING string_list INTO data_name opt_with_pointer
  {
    if ($5)
      $2 = cons (make_generic_1 (COB_STRING_WITH_POINTER, $5), $2);
    push_call_2 (COB_STRING, $4, $2);
  }
  opt_on_overflow
  _end_string
;
string_list:
  string_delimited_list			{ $$ = $1; }
| string_list string_delimited_list	{ $$ = list_append ($1, $2); }
;
string_delimited_list:
  string_name_list
  {
    $$ = $1;
  }
| string_name_list DELIMITED _by value
  {
    $$ = cons (make_generic_1 (COB_STRING_DELIMITED_NAME, $4), $1);
  }
| string_name_list DELIMITED _by SIZE
  {
    $$ = cons (make_generic_1 (COB_STRING_DELIMITED_SIZE, 0), $1);
  }
;
string_name_list:
  value
  {
    $$ = list (make_generic_1 (COB_STRING_CONCATENATE, $1));
  }
| string_name_list value
  {
    $$ = list_add ($1, make_generic_1 (COB_STRING_CONCATENATE, $2));
  }
;
opt_with_pointer:
  /* nothing */			{ $$ = NULL; }
| _with POINTER data_name	{ $$ = $3; }
;
_end_string: | END_STRING ;


/*
 * SUBTRACT statement
 */

subtract_statement:
  SUBTRACT subtract_body opt_on_size_error _end_subtract
;
subtract_body:
  number_list FROM math_name_list
  {
    /* SUBTRACT A B C FROM X Y -->
       (let ((t (+ a b c))) (set! x (- x t)) (set! y (- y t))) */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    push_assign ($3, '-', e);
  }
| number_list FROM number GIVING math_edited_name_list
  {
    /* SUBTRACT A B FROM C GIVING X Y -->
       (let ((t (- c (+ a b))) (set! x t) (set! y t)) */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    e = make_expr ($3, '-', e);
    push_assign ($5, 0, e);
  }
| CORRESPONDING group_name FROM group_name flag_rounded
  {
    push_corr (make_sub, $2, $4, $5);
  }
;
_end_subtract: | END_SUBTRACT ;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING data_name unstring_delimited
  INTO unstring_into opt_with_pointer unstring_tallying
  {
    if ($6)
      $3 = cons (make_generic_1 (COB_UNSTRING_WITH_POINTER, $6), $3);
    if ($7)
      $5 = list_add ($5, make_generic_1 (COB_UNSTRING_TALLYING, $7));
    push_call_2 (COB_UNSTRING, $2, list_append ($3, $5));
  }
  opt_on_overflow
  _end_unstring
;

unstring_delimited:
  /* nothing */			{ $$ = NULL; }
| DELIMITED _by
  unstring_delimited_list	{ $$ = $3; }
;
unstring_delimited_list:
  unstring_delimited_item	{ $$ = $1; }
| unstring_delimited_list OR
  unstring_delimited_item	{ $$ = list_append ($1, $3); }
;
unstring_delimited_item:
  flag_all value
  {
    int type = $1 ? COB_UNSTRING_DELIMITED_ALL : COB_UNSTRING_DELIMITED_BY;
    $$ = list (make_generic_1 (type, $2));
  }
;

unstring_into:
  unstring_into_item		{ $$ = $1; }
| unstring_into
  unstring_into_item		{ $$ = list_append ($1, $2); }
;
unstring_into_item:
  data_name unstring_delimiter unstring_count
  {
    $$ = list (make_generic_1 (COB_UNSTRING_INTO, $1));
    if ($2)
      $$ = list_add ($$, make_generic_1 (COB_UNSTRING_DELIMITER, $2));
    if ($3)
      $$ = list_add ($$, make_generic_1 (COB_UNSTRING_COUNT, $3));
  }
;
unstring_delimiter:
  /* nothing */			{ $$ = NULL; }
| DELIMITER _in data_name	{ $$ = $3; }
;
unstring_count:
  /* nothing */			{ $$ = NULL; }
| COUNT _in data_name		{ $$ = $3; }
;

unstring_tallying:
  /* nothing */			{ $$ = NULL; }
| TALLYING _in data_name	{ $$ = $3; }
;
_end_unstring: | END_UNSTRING ;


/*
 * WRITE statement
 */

write_statement:
  WRITE record_name write_from write_option
  {
    cobc_tree file = COBC_FIELD ($2)->file;
    current_file_name = COBC_FILE_NAME (file);
    /* AFTER ADVANCING */
    if ($4 && $4->type == COBC_AFTER)
      {
	if ($4->x)
	  push_call_2 (COB_WRITE_LINES, file, make_index ($4->x));
	else
	  push_call_1 (COB_WRITE_PAGE, file);
      }
    /* WRITE */
    if ($3)
      push_move ($3, $2);
    push_call_1 (COB_WRITE, file);
    /* BEFORE ADVANCING */
    if ($4 && $4->type == COBC_BEFORE)
      {
	if ($4->x)
	  push_call_2 (COB_WRITE_LINES, file, make_index ($4->x));
	else
	  push_call_1 (COB_WRITE_PAGE, file);
      }
  }
  opt_invalid_key
  _end_write
;
write_from:
  /* nothing */			{ $$ = NULL; }
| FROM value			{ $$ = $2; }
;
write_option:
  /* nothing */			{ $$ = NULL; }
| before_or_after _advancing integer_value _line
  {
    $$ = make_generic_1 ($1, $3);
  }
| before_or_after _advancing PAGE
  {
    $$ = make_generic_1 ($1, 0);
  }
;
before_or_after:
  BEFORE			{ $$ = COBC_BEFORE; }
| AFTER				{ $$ = COBC_AFTER; }
;
_advancing: | ADVANCING ;
_end_write: | END_WRITE ;


/*******************
 * Status handlers
 *******************/

/*
 * ON SIZE ERROR
 */

opt_on_size_error:
  opt_on_size_error_sentence
  opt_not_on_size_error_sentence
  {
    if ($1 || $2)
      push_status_handler (cobc_int0, $2, $1);
  }
;
opt_on_size_error_sentence:
  /* nothing */				  { $$ = NULL; }
| _on SIZE ERROR imperative_statement	  { $$ = $4; }
;
opt_not_on_size_error_sentence:
  /* nothing */				  { $$ = NULL; }
| NOT _on SIZE ERROR imperative_statement { $$ = $5; }
;


/*
 * ON OVERFLOW
 */

opt_on_overflow:
  opt_on_overflow_sentence
  opt_not_on_overflow_sentence
  {
    if ($1 || $2)
      push_status_handler (cobc_int0, $2, $1);
  }
;
opt_on_overflow_sentence:
  /* nothing */				{ $$ = NULL; }
| _on OVERFLOW imperative_statement	{ $$ = $3; }
;
opt_not_on_overflow_sentence:
  /* nothing */				{ $$ = NULL; }
| NOT _on OVERFLOW imperative_statement	{ $$ = $4; }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 1, $1, 0);
  }
| not_at_end_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 1, 0, $1);
  }
| at_end_sentence not_at_end_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 1, $1, $2);
  }
;
at_end_sentence:
  END imperative_statement		{ $$ = $2; }
| AT END imperative_statement		{ $$ = $3; }
;
not_at_end_sentence:
  NOT _at END imperative_statement	{ $$ = $4; }
;


/*
 * INVALID KEY
 */

opt_invalid_key:
  /* nothing */
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 2, 0, 0);
  }
| invalid_key
;
invalid_key:
  invalid_key_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 2, $1, 0);
  }
| not_invalid_key_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 2, 0, $1);
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    push_call_4 (COB_FILE_HANDLER, current_file_name, (void *) 2, $1, $2);
  }
;
invalid_key_sentence:
  INVALID _key imperative_statement	{ $$ = $3; }
;
not_invalid_key_sentence:
  NOT INVALID _key imperative_statement	{ $$ = $4; }
;


/*******************
 * Condition
 *******************/

condition:
  condition_name		{ $$ = make_cond_name ($1); }
| comparative_condition		{ $$ = $1; }
| class_condition		{ $$ = $1; }
| sign_condition		{ $$ = $1; }
| '(' condition ')'		{ $$ = $2; }
| NOT condition			{ $$ = make_unary_cond ($2, COBC_COND_NOT); }
| condition AND condition_2	{ $$ = make_cond ($1, COBC_COND_AND, $3); }
| condition OR condition_2	{ $$ = make_cond ($1, COBC_COND_OR, $3); }
;
condition_2:
  condition			{ $$ = $1; }
| expr _is			{ $$ = make_opt_cond ($<tree>-1, -1, $1); }
| NOT expr _is			{ $$ = make_opt_cond ($<tree>-1, -1, $2); }
| operator expr			{ $$ = make_opt_cond ($<tree>-1, $1, $2); }
;


/*
 * Comparative condition
 */

comparative_condition:
  expr _is operator expr
  {
    if (COBC_EXPR_P ($1) || COBC_EXPR_P ($4))
      if (COBC_TREE_CLASS ($1) != COBC_TREE_CLASS ($4))
	yyerror ("expression can only be compared with numeric value");
    $$ = make_cond ($1, $3, $4);
  }
;
operator:
  flag_not equal		{ $$ = $1 ? COBC_COND_NE : COBC_COND_EQ; }
| flag_not greater		{ $$ = $1 ? COBC_COND_LE : COBC_COND_GT; }
| flag_not less			{ $$ = $1 ? COBC_COND_GE : COBC_COND_LT; }
| flag_not greater_or_equal	{ $$ = $1 ? COBC_COND_LT : COBC_COND_GE; }
| flag_not less_or_equal	{ $$ = $1 ? COBC_COND_GT : COBC_COND_LE; }
;
equal: '=' | EQUAL _to ;
greater: '>' | GREATER _than ;
less: '<' | LESS _than ;
greater_or_equal: GE | GREATER _than OR EQUAL _to ;
less_or_equal: LE | LESS _than OR EQUAL _to ;


/*
 * Class condition
 */

class_condition:
  expr _is flag_not class
  {
    $$ = make_cond ($1, COBC_COND_CLASS, COBC_TREE ($4));
    if ($3)
      $$ = make_unary_cond ($$, COBC_COND_NOT);
  }
;
class:
  NUMERIC			{ $$ = "cob_is_numeric"; }
| ALPHABETIC			{ $$ = "cob_is_alpha"; }
| ALPHABETIC_LOWER		{ $$ = "cob_is_lower"; }
| ALPHABETIC_UPPER		{ $$ = "cob_is_upper"; }
| class_name			{ $$ = COBC_CLASS ($1)->cname; }
;


/*
 * Sign condition
 */

sign_condition:
  expr _is flag_not sign
  {
    $$ = make_cond ($1, $4, cobc_zero);
    if ($3)
      $$ = make_unary_cond ($$, COBC_COND_NOT);
  }
;
sign:
  ZERO				{ $$ = COBC_COND_EQ; }
| POSITIVE			{ $$ = COBC_COND_GT; }
| NEGATIVE			{ $$ = COBC_COND_LT; }
;


/*******************
 * Expression
 *******************/

expr:
  value				{ $$ = $1; }
| '(' expr ')'			{ $$ = $2; }
| '-' expr			{ $$ = make_expr (cobc_zero, '-', $2); }
| expr '+' expr			{ $$ = make_expr ($1, '+', $3); }
| expr '-' expr			{ $$ = make_expr ($1, '-', $3); }
| expr '*' expr			{ $$ = make_expr ($1, '*', $3); }
| expr '/' expr			{ $$ = make_expr ($1, '/', $3); }
| expr '^' expr			{ $$ = make_expr ($1, '^', $3); }
;


/*****************************************************************************
 * Basic structure
 *****************************************************************************/

/*******************
 * Names
 *******************/

/*
 * Various names
 */

/* Math name */

math_name_list:
  numeric_name flag_rounded	{ $$ = list (make_assign ($1, 0, $2)); }
| math_name_list
  numeric_name flag_rounded	{ $$ = list_add ($1, make_assign ($2, 0, $3));}
;

/* Math edited name */

math_edited_name_list:
  numeric_edited_name flag_rounded { $$ = list (make_assign ($1, 0, $2)); }
| math_edited_name_list
  numeric_edited_name flag_rounded { $$ = list_add ($1, make_assign ($2, 0, $3));}
;

/* Numeric name */

numeric_name:
  data_name
  {
    if (COBC_TREE_CLASS ($1) != COB_NUMERIC)
      yyerror ("`%s' not numeric", tree_to_string ($1));
    $$ = $1;
  }
;

/* Numeric edited name */

numeric_edited_name:
  data_name
  {
    int category = COBC_FIELD ($1)->category;
    if (category != COB_NUMERIC && category != COB_NUMERIC_EDITED)
      yyerror ("`%s' not numeric or numeric edited", tree_to_string ($1));
    $$ = $1;
  }
;

/* Group name */

group_name:
  data_name
  {
    if (!COBC_FIELD ($1)->children)
      yyerror ("`%s' not a group", tree_to_string ($1));
    $$ = $1;
  }
;

/* Table name */

table_name:
  name
  {
    if (!COBC_FIELD ($1)->index_list)
      yyerror ("`%s' must be indexed", tree_to_string ($1));
    $$ = $1;
  }
;


/*
 * Standard names
 */

/* Alphabet name

alphabet_name:
  name
; */

/* Class name */

class_name:
  CLASS_NAME
;

/* Condition name */

condition_name_list:
  condition_name		{ $$ = list ($1); }
| condition_name_list
  condition_name		{ $$ = list_add ($1, $2); }
;
condition_name:
  qualified_cond_name		{ $$ = $1; }
| qualified_cond_name subref	{ $$ = $2; }
;
qualified_cond_name:
  CONDITION_NAME
  {
    if (COBC_FIELD ($1)->word->count > 1)
      ambiguous_error (COBC_FIELD ($1)->word);
    $$ = $1;
    // field_set_used (COBC_FIELD ($$)->parent);
  }
| CONDITION_NAME in_of qualified_name
  {
    struct cobc_word *w = COBC_FIELD ($1)->word;
    struct cobc_word *qw = lookup_qualified_word (w, COBC_FIELD ($3));
    $$ = $1;
    if (!qw)
      undefined_error (w, $3);
    else
      {
	$$ = qw->item;
	// field_set_used (COBC_FIELD ($$)->parent);
      }
  }
;

/* Data name */

data_name_list:
  data_name			{ $$ = list ($1); }
| data_name_list data_name	{ $$ = list_add ($1, $2); }
;
data_name:
  name
  {
    struct cobc_field *p = COBC_FIELD ($1);
    $$ = $1;
    if (COBC_REFMOD_P ($1))
      $1 = COBC_REFMOD ($1)->field;
    if (COBC_FIELD_P ($1))
      {
	struct cobc_field *p = COBC_FIELD ($1);
	if (p->indexes > 0)
	  yyerror ("`%s' must be subscripted", p->word->name);
      }
    field_set_used (p);
  }
;

/* File name */

file_name_list:
  file_name			{ $$ = list ($1); }
| file_name_list file_name	{ $$ = list_add ($1, $2); }
;
file_name:
  name
  {
    if (!COBC_FILE_NAME_P ($1))
      yyerror ("`%s' not file name", tree_to_string ($1));
    $$ = $1;
  }
;

/* Record name */

record_name:
  name
  {
    if (!COBC_FIELD_P ($1) || !COBC_FIELD ($1)->file)
      yyerror ("`%s' not record name", tree_to_string ($1));
    $$ = $1;
  }
;

/* Level number */

level_number:
  integer
  {
    $$ = $1;
    if ($1 < 1 || ($1 > 49 && $1 != 66 && $1 != 77 && $1 != 88))
      {
	yyerror ("invalid level number `%02d'", $1);
	$$ = 1;
      }
  }
;

/* Mnemonic name */

mnemonic_name_list:
  mnemonic_name			{ $$ = list ($1); }
| mnemonic_name_list
  mnemonic_name			{ $$ = list_add ($1, $2); }
;
mnemonic_name:
  MNEMONIC_NAME
;

/* Section name */

section_name:
  label_word
  {
    if ($1->item
	&& (/* used as a non-label name */
	    !COBC_LABEL_NAME_P ($1->item)
	    /* used as a section name */
	    || COBC_LABEL_NAME ($1->item)->section == NULL
	    /* used as the same paragraph name in the same section */
	    || COBC_LABEL_NAME ($1->item)->section == current_section))
      {
	redefinition_error ($1->item);
	$$ = $1->item;
      }
    else
      $$ = make_label_name ($1);
  }
;


/*
 * Primitive name
 */

name:
  qualified_name		{ $$ = $1; }
| qualified_name subref		{ $$ = $2; }
| qualified_name refmod		{ $$ = $2; }
| qualified_name subref refmod	{ $$ = $3; }
;
qualified_name:
  qualified_word
  {
    $$ = $1->item;
    if (!$$)
      {
	undefined_error ($1, 0);
	$$ = make_filler ();
      }
  }
;
qualified_word:
  WORD
  {
    $$ = $1;
    if ($1->count > 1)
      ambiguous_error ($1);
  }
| WORD in_of qualified_name
  {
    $$ = lookup_qualified_word ($1, COBC_FIELD ($3));
    if (!$$)
      {
	undefined_error ($1, $3);
	$$ = $1;
      }
  }
;
subref:
 '(' subscript_list ')'
  {
    int required = COBC_FIELD ($<tree>0)->indexes;
    int given = list_length ($2);
    if (given != required)
      {
	const char *name = tree_to_string ($<tree>0);
	switch (required)
	  {
	  case 0:
	    yyerror ("`%s' cannot be subscripted", name);
	    break;
	  case 1:
	    yyerror ("`%s' requires one subscript", name);
	    break;
	  default:
	    yyerror ("`%s' requires %d subscripts", name, required);
	    break;
	  }
      }
    $$ = make_subref ($<tree>0, $2);
  }
;
refmod:
 '(' subscript ':' ')'
  {
    $$ = make_refmod ($<tree>0, $2, 0);
  }
| '(' subscript ':' subscript ')'
  {
    $$ = make_refmod ($<tree>0, $2, $4);
  }
;
subscript_list:
  subscript			{ $$ = list ($1); }
| subscript_list subscript	{ $$ = list_add ($1, $2); }
;
subscript:
  value				{ $$ = $1; }
| subscript '+' value		{ $$ = make_expr ($1, '+', $3); }
| subscript '-' value		{ $$ = make_expr ($1, '-', $3); }
;


/*
 * Label name
 */

label_list:
  label_name			{ $$ = list ($1); }
| label_list label_name		{ $$ = list_add ($1, $2); }
;
label_name:
  label_word
  {
    $$ = make_label_name_nodef ($1, 0);
    COBC_LABEL_NAME ($$)->section = current_section;
    label_check_list = cons ($$, label_check_list);
  }
| label_word in_of label_word
  {
    $$ = make_label_name_nodef ($1, $3);
    label_check_list = cons ($$, label_check_list);
  }
;
label_word:
  INTEGER_LITERAL	{ $$ = lookup_user_word (COBC_LITERAL ($1)->str); }
| LABEL_WORD		{ $$ = $1; }
;
in_of: IN | OF ;


/*
 * Predefined name
 */

predefined_name_list:
  predefined_name		{ $$ = list ($1); }
| predefined_name_list
  predefined_name		{ $$ = list_add ($1, $2); }
;
predefined_name:
  qualified_predefined_word	{ $$ = make_predefined ($1); }
;
qualified_predefined_word:
  WORD				{ $$ = cons ($1, NULL); }
| qualified_predefined_word in_of
  WORD				{ $$ = cons ($3, $1); }
;


/*
 * Undefined word
 */

undefined_word_list:
  undefined_word { }
| undefined_word_list undefined_word { }
;
undefined_word:
  WORD
  {
    if ($1->item)
      redefinition_error ($1->item);
    $$ = $1;
  }
;


/*******************
 * Values
 *******************/

/*
 * Special values
 */

/* Number */

number_list:
  number			{ $$ = list ($1); }
| number_list number		{ $$ = list_add ($1, $2); }
;
number:
  value
  {
    if (COBC_TREE_CLASS ($1) != COB_NUMERIC)
      yyerror ("numeric value is expected: %s", tree_to_string ($1));
    $$ = $1;
  }
;

/* Integer */

integer:
  INTEGER_LITERAL
  {
    $$ = literal_to_int (COBC_LITERAL ($1));
  }
;

integer_value:
  value
;


/*
 * Primitive value
 */

opt_value_list:
  /* nothing */			{ $$ = NULL; }
| opt_value_list value		{ $$ = list_add ($1, $2); }
;
value:
  data_name
| literal
| function
;
function:
  FUNCTION_NAME '(' opt_value_list ')'
  {
    yyerror ("FUNCTION is not implemented yet");
    YYABORT;
  }


/*
 * Literal
 */

literal:
  basic_literal			{ $$ = $1; }
| figurative_constant		{ $$ = $1; }
| ALL basic_literal		{ $$ = $2; COBC_LITERAL ($2)->all = 1; }
| ALL figurative_constant	{ $$ = $2; }
;
basic_literal:
  INTEGER_LITERAL
| NUMERIC_LITERAL
| NONNUMERIC_LITERAL
;
figurative_constant:
  SPACE				{ $$ = cobc_space; }
| ZERO				{ $$ = cobc_zero; }
| QUOTE				{ $$ = cobc_quote; }
| HIGH_VALUE			{ $$ = cobc_high; }
| LOW_VALUE			{ $$ = cobc_low; }
;


/*******************
 * Common rules
 *******************/

/*
 * dot
 */

dot:
  '.'
| error
| /* nothing */
  {
    yywarn ("`.' is expected after `%s'", cobc_last_text);
  }
;


/*
 * Common flags
 */

flag_all:
  /* nothing */			{ $$ = 0; }
| ALL				{ $$ = 1; }
;
flag_not:
  /* nothing */			{ $$ = 0; }
| NOT				{ $$ = 1; }
;
flag_next:
  /* nothing */			{ $$ = 0; }
| NEXT				{ $$ = 1; }
;
flag_global:
  /* nothing */			{ $$ = 0; }
| GLOBAL			{ $$ = 1; }
;
flag_rounded:
  /* nothing */			{ $$ = 0; }
| ROUNDED			{ $$ = 1; }
;


/*
 * Optional words
 */

_are: | ARE ;
_area: | AREA ;
_at: | AT ;
_by: | BY ;
_file: | TOK_FILE ;
_for: | FOR ;
_from: | FROM ;
_in: | IN ;
_is: | IS ;
_is_are: | IS | ARE ;
_key: | KEY ;
_line: | LINE ;
_mode: | MODE ;
_on: | ON ;
_program: | PROGRAM ;
_record: | RECORD ;
_sign: | SIGN _is ;
_size: | SIZE ;
_status: | STATUS ;
_than: | THAN ;
_then: | THEN ;
_to: | TO ;
_upon: | UPON ;
_when: | WHEN ;
_with: | WITH ;


%%

static cobc_tree
resolve_predefined_name (cobc_tree x)
{
  cobc_tree name;
  struct cobc_list *l = COBC_PREDEFINED (x)->words;
  struct cobc_word *p = l->item;
  if (p->count == 0)
    {
      yyerror ("`%s' is used, but not defined", p->name);
      return NULL;
    }
  else if (p->count > 1)
    ambiguous_error (p);

  name = p->item;
  for (l = l->next; l; l = l->next)
    {
      struct cobc_word *w = l->item;
      p = lookup_qualified_word (w, COBC_FIELD (name));
      if (!p)
	{
	  undefined_error (w, name);
	  return NULL;
	}
      name = p->item;
    }
  field_set_used (COBC_FIELD (name));
  return name;
}

static void
init_field (int level, cobc_tree field)
{
  struct cobc_field *last_field = current_field;
  if (last_field && last_field->level == 88)
    last_field = last_field->parent;

  current_field = COBC_FIELD (field);
  current_field->level = level;
  current_field->occurs = 1;
  current_field->usage = COBC_USAGE_DISPLAY;
  current_field->category = COB_ALPHANUMERIC;

  if (level == 1 || level == 77)
    {
      if (last_field)
	field_founder (last_field)->sister = current_field;
    }
  else if (!last_field || (last_field->level == 77 && level != 88))
    {
      yyerror ("level number must begin with 01, 66, or 77");
    }
  else if (level > last_field->level)
    {
      /* lower level */
      if (level != 88)
	last_field->children = current_field;
      current_field->parent = last_field;
      current_field->f.sign_separate = current_field->parent->f.sign_separate;
      current_field->f.sign_leading = current_field->parent->f.sign_leading;
    }
  else if (level == last_field->level)
    {
      /* same level */
    sister:
      /* ensure that there is no field with the same name
	 in the same level */
      if (current_field->word && current_field->word->count > 1)
	{
	  struct cobc_field *p = last_field->parent;
	  for (p = p->children; p; p = p->sister)
	    if (strcasecmp (current_field->word->name, p->word->name) == 0)
	      redefinition_error (COBC_TREE (p));
	}
      last_field->sister = current_field;
      current_field->parent = last_field->parent;
    }
  else
    {
      /* upper level */
      struct cobc_field *p;
      for (p = last_field->parent; p; p = p->parent)
	if (p->level == level)
	  {
	    last_field = p;
	    goto sister;
	  }
      yyerror ("field hierarchy broken");
    }

  /* inherit parent's properties */
  if (current_field->parent)
    {
      current_field->usage = current_field->parent->usage;
    }
}

static void
validate_field (struct cobc_field *p)
{
  if (p->level == 88)
    {
      /* conditional variable */
      COBC_TREE_CLASS (p) = COB_BOOLEAN;
      if (p->pic)
	yyerror ("level 88 field may not have PICTURE clause");
    }
  else
    {
      /* validate REDEFINES */

      /* validate PICTURE */
      if (p->pic)
	{
	  /* determine the class */
	  p->category = p->pic->category;
	  switch (p->category)
	    {
	    case COB_ALPHABETIC:
	      COBC_TREE_CLASS (p) = COB_ALPHABETIC;
	      break;
	    case COB_NUMERIC:
	      COBC_TREE_CLASS (p) = COB_NUMERIC;
	      break;
	    case COB_NUMERIC_EDITED:
	    case COB_ALPHANUMERIC:
	    case COB_ALPHANUMERIC_EDITED:
	      COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;
	      break;
	    case COB_NATIONAL:
	    case COB_NATIONAL_EDITED:
	      COBC_TREE_CLASS (p) = COB_NATIONAL;
	      break;
	    case COB_BOOLEAN:
	      COBC_TREE_CLASS (p) = COB_BOOLEAN;
	      break;
	    }
	}

      /* validate USAGE */

      /* validate SIGN */

      /* validate OCCURS */
      if (p->f.have_occurs)
	if (p->level < 2 || p->level > 49)
	  yyerror ("OCCURS cannot be used with level %02d field", p->level);

      /* validate JUSTIFIED RIGHT */
      if (p->f.justified)
	{
	  char c = p->category;
	  if (!(c == 'A' || c == 'X' || c == 'N'))
	    yyerror ("`%s' cannot have JUSTIFIED RIGHT",
		     tree_to_string (COBC_TREE (p)));
	}

      /* validate SYNCHRONIZED */
      if (p->f.synchronized)
	if (p->usage != COBC_USAGE_BINARY)
	  {
	    // yywarn ("SYNCHRONIZED here has no effect");
	    p->f.synchronized = 0;
	  }

      /* validate BLANK ZERO */

      /* validate VALUE */
      if (p->value)
	{
	  if (p->value == cobc_zero)
	    {
	      /* just accept */
	    }
	  else if (COBC_TREE_CLASS (p) == COB_NUMERIC
		   && COBC_TREE_CLASS (p->value) != COB_NUMERIC)
	    {
	    }
	  else if (COBC_TREE_CLASS (p) != COB_NUMERIC
		   && COBC_TREE_CLASS (p->value) == COB_NUMERIC)
	    {
	      yywarn ("VALUE should be non-numeric");
	    }
	  else
	    {
	    }
	}

    }

  /* count the number of indexes needed */
  if (p->parent)
    p->indexes = p->parent->indexes;
  if (p->f.have_occurs)
    p->indexes++;
}

static void
validate_field_tree (struct cobc_field *p)
{
  int i;
  int nkeys = p->nkeys;
  struct cobc_key *keys = p->keys;

  if (p->children)
    {
      /* group */
      COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;

      if (p->f.justified)
	yyerror ("group item cannot have JUSTFIED RIGHT");

      for (p = p->children; p; p = p->sister)
	validate_field_tree (p);
    }
  else if (p->level == 66)
    {
    }
  else
    {
      switch (p->usage)
	{
	case COBC_USAGE_DISPLAY:
	  break;
	case COBC_USAGE_BINARY:
	case COBC_USAGE_PACKED:
	  if (p->category != COB_NUMERIC)
	    yywarn ("field must be numeric");
	  break;
	case COBC_USAGE_INDEX:
	  COBC_TREE_CLASS (p) = COB_NUMERIC;
	  break;
	}

      if (!p->pic)
	{
	  if (p->usage != COBC_USAGE_INDEX)
	    yyerror ("`%s' must have PICTURE", tree_to_string (COBC_TREE (p)));
	  p->pic = make_picture ();
	}
    }

  for (i = 0; i < nkeys; i++)
    keys[i].key = resolve_predefined_name (keys[i].key);
}

static void
validate_file_name (struct cobc_file_name *p)
{
  struct cobc_alt_key *l;
  if (COBC_PREDEFINED_P (p->assign))
    p->assign = resolve_predefined_name (p->assign);
  if (p->key)
    p->key = resolve_predefined_name (p->key);
  if (p->file_status)
    p->file_status = resolve_predefined_name (p->file_status);
  for (l = p->alt_key_list; l; l = l->next)
    l->key = resolve_predefined_name (l->key);
}

static const char *
lookup_label (struct cobc_word *w, struct cobc_label_name *section)
{
  for (; w; w = w->link)
    if (w->item
	&& COBC_LABEL_NAME_P (w->item)
	&& section == COBC_LABEL_NAME (w->item)->section)
      return COBC_LABEL_NAME (w->item)->cname;

  yyerror ("`%s' undefined in section `%s'", w->name, section->word->name);
  return NULL;
}

static void
validate_label_name (struct cobc_label_name *p)
{
  if (p->in_word)
    {
      /* LABEL IN LABEL */
      if (p->in_word->count == 0)
	yyerror ("no such section `%s'", p->in_word->name);
      else if (!COBC_LABEL_NAME_P (p->in_word->item))
	yyerror ("invalid section name `%s'", p->in_word->name);
      else
	p->cname = lookup_label (p->word, COBC_LABEL_NAME (p->in_word->item));
    }
  else
    {
      /* LABEL */
      if (p->word->count == 1 && COBC_LABEL_NAME_P (p->word->item))
	p->cname = COBC_LABEL_NAME (p->word->item)->cname;
      else if (p->word->count > 0 && p->section)
	p->cname = lookup_label (p->word, p->section);
      else
	yyerror ("no such section `%s'", p->word->name);
    }
}


static void
field_set_used (struct cobc_field *p)
{
  p->f.used = 1;
  for (; p; p = p->parent)
    if (p->redefines)
      {
	p->redefines->f.used = 1;
	break;
      }
}

static int
builtin_switch_id (cobc_tree x)
{
  int id = COBC_BUILTIN (x)->id;
  switch (id)
    {
    case BUILTIN_SWITCH_1:
    case BUILTIN_SWITCH_2:
    case BUILTIN_SWITCH_3:
    case BUILTIN_SWITCH_4:
    case BUILTIN_SWITCH_5:
    case BUILTIN_SWITCH_6:
    case BUILTIN_SWITCH_7:
    case BUILTIN_SWITCH_8:
      return id - BUILTIN_SWITCH_1;
    default:
      yyerror ("not switch name");
      return -1;
    }
}


static cobc_tree
make_add (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 (COB_ADD, f2, f1, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_sub (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 (COB_SUB, f2, f1, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_move (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_2 (COB_MOVE, f1, f2);
}

static struct cobc_list *
make_corr (cobc_tree (*func)(), cobc_tree g1, cobc_tree g2, int opt,
	   struct cobc_list *l)
{
  struct cobc_field *p1, *p2;
  for (p1 = COBC_FIELD (g1)->children; p1; p1 = p1->sister)
    if (!p1->redefines && !p1->f.have_occurs)
      for (p2 = COBC_FIELD (g2)->children; p2; p2 = p2->sister)
	if (!p2->redefines && !p2->f.have_occurs)
	  if (strcmp (p1->word->name, p2->word->name) == 0)
	    {
	      cobc_tree t1 = COBC_TREE (p1);
	      cobc_tree t2 = COBC_TREE (p2);
	      if (COBC_SUBREF_P (g1))
		t1 = make_subref (t1, COBC_SUBREF (g1)->subs);
	      if (COBC_SUBREF_P (g2))
		t2 = make_subref (t2, COBC_SUBREF (g2)->subs);
	      if (p1->children && p2->children)
		l = make_corr (func, t1, t2, opt, l);
	      else
		{
		  COBC_FIELD (t1)->f.used = 1;
		  COBC_FIELD (t2)->f.used = 1;
		  l = cons (func (t1, t2, opt), l);
		}
	    }
  return l;
}

static cobc_tree
make_opt_cond (cobc_tree last, int type, cobc_tree this)
{
 again:
  if (COBC_COND (last)->type == COBC_COND_NOT)
    {
      COBC_COND (last)->left =
	make_opt_cond (COBC_COND (last)->left, type, this);
      return last;
    }

  if (!COBC_COND (last)->right)
    {
      yyerror ("broken condition");
      return last; /* error recovery */
    }

  if (COBC_COND (last)->type == COBC_COND_AND
      || COBC_COND (last)->type == COBC_COND_OR)
    {
      last = COBC_COND (last)->left;
      goto again;
    }

  if (type == -1)
    type = COBC_COND (last)->type;
  return make_cond (COBC_COND (last)->left, type, this);
}

static cobc_tree
make_cond_name (cobc_tree x)
{
  struct cobc_list *l;
  cobc_tree cond = NULL;
  cobc_tree parent = COBC_TREE (COBC_FIELD (x)->parent);
  if (COBC_SUBREF_P (x))
    parent = make_subref (parent, COBC_SUBREF (x)->subs);
  for (l = COBC_FIELD (x)->values; l; l = l->next)
    {
      cobc_tree c;
      if (COBC_PAIR_P (l->item))
	{
	  /* VALUE THRU VALUE */
	  struct cobc_pair *p = COBC_PAIR (l->item);
	  c = make_cond (make_cond (p->x, COBC_COND_LE, parent),
			 COBC_COND_AND,
			 make_cond (parent, COBC_COND_LE, p->y));
	}
      else
	{
	  /* VALUE */
	  c = make_cond (parent, COBC_COND_EQ, l->item);
	}
      if (!cond)
	cond = c;
      else
	cond = make_cond (cond, COBC_COND_OR, c);
    }
  if (!cond)
    cond = make_cond (cobc_int0, COBC_COND_EQ, cobc_int0);
  return cond;
}


static void
redefinition_error (cobc_tree x)
{
  struct cobc_field *p = COBC_FIELD (x);
  yyerror ("redefinition of `%s'", p->word->name);
  yyerror_loc (&x->loc, "`%s' previously defined here", p->word->name);
}

static void
undefined_error (struct cobc_word *w, cobc_tree parent)
{
  if (parent)
    yyerror ("`%s' undefined in `%s'", w->name, tree_to_string (parent));
  else
    yyerror ("`%s' undefined", w->name);
}

static void
ambiguous_error (struct cobc_word *p)
{
  yyerror ("`%s' ambiguous; need qualification", p->name);
}


static void
yyprintf (char *file, int line, char *prefix, char *fmt, va_list ap)
{
  fprintf (stderr, "%s:%d: %s",
	   file ? file : cobc_source_file,
	   line ? line : cobc_source_line,
	   prefix);
  vfprintf (stderr, fmt, ap);
  fputs ("\n", stderr);
}

void
yywarn (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "warning: ", fmt, ap);
  va_end (ap);

  warning_count++;
}

void
yyerror (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "", fmt, ap);
  va_end (ap);

  error_count++;
}

void
yywarn_loc (YYLTYPE *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->text, loc->first_line, "warning: ", fmt, ap);
  va_end (ap);

  warning_count++;
}

void
yyerror_loc (YYLTYPE *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->text, loc->first_line, "", fmt, ap);
  va_end (ap);

  error_count++;
}
