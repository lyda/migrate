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

%expect 124

%{
#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "tree.h"
#include "scanner.h"
#include "codegen.h"
#include "libcob.h"

#define yydebug		yy_bison_debug
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#define OBSOLETE(x)	yywarn ("keyword `%s' is obsolete", x)

#define push_tree(x)							\
  do {									\
    program_spec.exec_list = cons ((x), program_spec.exec_list);	\
  } while (0)

#define push_call_0(t)		push_tree (make_call_0 (t))
#define push_call_1(t,x)	push_tree (make_call_1 (t, x))
#define push_call_2(t,x,y)	push_tree (make_call_2 (t, x, y))
#define push_call_3(t,x,y,z)	push_tree (make_call_3 (t, x, y, z))

#define push_move(x,y)		push_call_2 (COB_MOVE, (x), (y))

#define push_label(x)							\
  do {									\
    finalize_label_name (x);						\
    push_tree (x);							\
    program_spec.label_list = cons (x, program_spec.label_list);	\
  } while (0)

#define push_exit_section(x)				\
  do {							\
    cobc_tree p = make_perform (COBC_PERFORM_EXIT);	\
    COBC_PERFORM (p)->cond = COBC_TREE (x);		\
    push_tree (p);					\
  } while (0)

#define push_assign(val,lst)			\
  do {						\
    struct cobc_list *l;			\
    for (l = (lst); l; l = l->next)		\
      {						\
	struct cobc_assign *p = l->item;	\
	p->value = (val);			\
      }						\
    push_tree (make_status_sequence (lst));	\
  } while (0)

#define push_corr(func,g1,g2,opt) \
  push_tree (make_status_sequence (make_corr (func, COBC_FIELD (g1), COBC_FIELD (g2), opt, NULL)))

#define push_status_handler(val,st1,st2) \
  push_tree (make_if (make_cond (cobc_status, COBC_COND_EQ, val), st1, st2))

struct write_option {
  int place;
  cobc_tree lines;
};

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

static cobc_tree make_add (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_sub (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_move (cobc_tree f1, cobc_tree f2, int round);
static struct cobc_list *make_corr (cobc_tree (*func)(), struct cobc_field *g1, struct cobc_field *g2, int opt, struct cobc_list *l);
static cobc_tree make_opt_cond (cobc_tree last, int type, cobc_tree this);
static void redefinition_error (cobc_tree x);
%}

%union {
  int inum;
  cobc_tree tree;
  struct cobc_word *word;
  struct cobc_list *list;
  struct cobc_picture *pict;
  struct inspect_item *insi;
  struct call_item *call;
  struct write_option *wrop;
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
%token SOURCE_COMPUTER,BEFORE,AFTER,RESERVE
%token RIGHT,JUSTIFIED,SYNCHRONIZED,SEPARATE,BLOCK
%token TOK_INITIAL,FIRST,ALL,LEADING,OF,IN,BY,STRING,UNSTRING,DEBUGGING
%token START,DELETE,PROGRAM,GLOBAL,EXTERNAL,SIZE,DELIMITED,COLLATING,SEQUENCE
%token GIVING,INSPECT,TALLYING,REPLACING,ON,OFF,POINTER,OVERFLOW,NATIVE
%token DELIMITER,COUNT,LEFT,TRAILING,CHARACTER,FILLER,OCCURS,TIMES,CLASS
%token ADD,SUBTRACT,MULTIPLY,DIVIDE,ROUNDED,REMAINDER,ERROR,SIZE,INDEX
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

%type <call> call_item
%type <insi> tallying_item,replacing_item,inspect_before_after
%type <inum> flag_all,flag_duplicates,flag_optional,flag_with_no_advancing
%type <inum> flag_not,flag_next,flag_rounded,flag_separate
%type <inum> class,integer,level_number,operator,on_or_off
%type <inum> usage,before_or_after,perform_test
%type <inum> select_organization,select_access_mode,open_mode
%type <list> occurs_key_list,occurs_index_list,value_item_list
%type <list> data_name_list,condition_name_list,name_list,opt_value_list
%type <list> evaluate_subject_list,evaluate_case,evaluate_case_list
%type <list> evaluate_when_list,evaluate_object_list
%type <list> inspect_tallying,inspect_replacing,inspect_converting
%type <list> label_list,subscript_list,number_list,name_list
%type <list> string_list,string_delimited_list,string_name_list
%type <list> tallying_list,replacing_list,inspect_before_after_list
%type <list> unstring_delimited,unstring_delimited_list,unstring_into
%type <list> unstring_delimited_item,unstring_into_item
%type <list> file_name_list,math_name_list,math_edited_name_list
%type <list> call_item_list,call_using
%type <tree> special_name_class_options,special_name_class_option
%type <tree> special_name_class_literal
%type <tree> call_returning,add_to,field_description_list,value_item
%type <tree> field_description_list_1,field_description_list_2
%type <tree> condition,condition_2,comparative_condition,class_condition
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
%type <tree> condition_name,data_name,file_name,record_name,label_name
%type <tree> mnemonic_name,section_name,name,qualified_name
%type <tree> integer_value,value,number
%type <tree> literal,basic_literal,figurative_constant
%type <word> qualified_word,label_word,undefined_word
%type <list> qualified_predefined_word
%type <wrop> write_option


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
    program_spec.program_id = NULL;
    program_spec.initial_program = 0;
    program_spec.class_list = NULL;
    program_spec.index_list = NULL;
    program_spec.file_name_list = NULL;
    program_spec.using_list = NULL;
    program_spec.label_list = NULL;
    program_spec.exec_list = NULL;
    label_check_list = NULL;
    cobc_in_procedure = 0;
    cob_decimal_point = '.';
    cob_currency_symbol = '$';
    init_word_table ();
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
    program_spec.label_list = list_reverse (program_spec.label_list);
    program_spec.exec_list = list_reverse (program_spec.exec_list);
    if (error_count == 0)
      codegen (&program_spec);
  }
;
_end_program:
| END PROGRAM WORD dot
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
  special_name_mnemonic_name
  special_name_mnemonic_on_off	{ }
;
special_name_mnemonic_name:
| IS undefined_word
  {
    set_word_item ($2, make_integer ($<inum>0));
  }
;
special_name_mnemonic_on_off:
| special_name_mnemonic_on_off
  on_or_off _status _is undefined_word
  {
    struct cobc_field *p = COBC_FIELD (make_field ($5));
    p->level = 88;
    p->cond = make_cond (cobc_int0, COBC_COND_EQ, cobc_int0);
  }
;
on_or_off:
  ON				{ $$ = 1; }
| OFF				{ $$ = 2; }
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
  CLASS undefined_word _is special_name_class_options
  {
    cobc_tree x = make_field ($2);
    COBC_FIELD (x)->level = 99; /* class */
    COBC_FIELD (x)->cond = $4;
    program_spec.class_list = cons (x, program_spec.class_list);
  }
;
special_name_class_options:
  special_name_class_option	{ $$ = $1; }
| special_name_class_options
  special_name_class_option	{ $$ = make_cond ($1, COBC_COND_OR, $2); }
;
special_name_class_option:
  special_name_class_literal
  {
    $$ = make_cond ($1, COBC_COND_EQ, 0);
  }
| special_name_class_literal THRU
  special_name_class_literal
  {
    $$ = make_cond (make_cond ($1, COBC_COND_LE, 0),
		    COBC_COND_AND,
		    make_cond (0, COBC_COND_LE, $1));
  }
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
  SELECT flag_optional undefined_word ASSIGN _to NONNUMERIC_LITERAL
  {
    current_file_name = COBC_FILE_NAME (make_file_name ($4));
    current_file_name->assign = COBC_LITERAL ($7)->str;
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
	  {
	    yyerror ("RECORD KEY is required for file `%s'", $4->name);
	    current_file_name->key = COBC_FIELD (make_filler ());
	  }
	break;
      case COB_ORG_RELATIVE:
	if (current_file_name->access_mode != COB_ACCESS_SEQUENTIAL
	    && !current_file_name->key)
	  {
	    yyerror ("RELATIVE KEY is required for file `%s'", $4->name);
	    current_file_name->key = COBC_FIELD (make_filler ());
	  }
	break;
      }
  }
;
select_options:
| select_options select_option
;
select_option:
  select_organization
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
| RELATIVE _key _is qualified_predefined_word
  {
    if (current_file_name->organization != COB_ORG_RELATIVE)
      yyerror ("only relative files may have RELATIVE KEY");
    else
      current_file_name->key = (struct cobc_field *) $4;
  }
| RECORD _key _is qualified_predefined_word
  {
    if (current_file_name->organization != COB_ORG_INDEXED)
      yyerror ("only indexed files may have RECORD KEY");
    else
      current_file_name->key = (struct cobc_field *) $4;
  }
| ALTERNATE RECORD _key _is qualified_predefined_word flag_duplicates
  {
    if (current_file_name->organization != COB_ORG_INDEXED)
      yyerror ("only indexed files may have ALTERNATE RECORD KEY");
    else
      {
	struct cobc_alt_key *p = malloc (sizeof (struct cobc_alt_key));
	p->key = (struct cobc_field *) $5;
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
  }
| _file STATUS _is qualified_predefined_word
  {
    current_file_name->status = (struct cobc_field *) $4;
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
| I_O_CONTROL dot
  same_statement_list
  {
    yywarn ("I-O-CONTROL is not implemented yet");
  }
;
same_statement_list:
| same_statement_list same_statement
;
same_statement:
  SAME same_option _area _for name_list '.'
;
same_option:
  RECORD
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
    for (p = p->sister; p; p = p->sister)
      {
	p->redefines = current_file_name->record;
	p->file = COBC_TREE (current_file_name);
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
;


/*
 * BLOCK clause
 */

block_clause:
  BLOCK _contains integer opt_to_integer _records_or_characters
  { /* ignored */ }
;
_contains: | CONTAINS ;
_records_or_characters: | RECORDS | CHARACTERS ;


/*
 * RECORD clause
 */

record_clause:
  RECORD _contains integer _characters
  {
    yywarn ("RECORD CONTAINS is not implemented");
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
| FROM INTEGER_LITERAL		{ $$ = $2; }
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
  LABEL record_or_records STANDARD { OBSOLETE ("LABEL RECORD"); }
| LABEL record_or_records OMITTED  { OBSOLETE ("LABEL RECORD"); }
;
record_or_records:
  RECORD _is
| RECORDS _are
;


/*
 * VALUE OF clause
 */


/*
 * DATA clause
 */

data_clause:
  DATA record_or_records undefined_word_list { /* ignore */ }
;


/*
 * LINAGE clause
 */


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
	yyerror ("`%s' undefined", $2->name);
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
  _is EXTERNAL			{ yywarn ("EXTERNAL is not implemented"); }
;


/* GLOBAL */

global_clause:
  _is GLOBAL			{ yywarn ("GLOBAL is not implemented"); }
;


/* PICTURE */

picture_clause:
  PICTURE_TOK
  {
    int level = current_field->level;
    if (level == 88)
      yyerror ("level %02d field may not have PICTURE clause", level);
    else
      current_field->pic = $1;
  }
;


/* USAGE */

usage_clause:
  _usage _is usage
;
usage:
  DISPLAY
  {
    current_field->usage = USAGE_DISPLAY;
  }
| BINARY /* or COMP */
  {
    current_field->usage = USAGE_BINARY;
  }
| INDEX
  {
    current_field->usage = USAGE_INDEX;
    current_field->pic = make_picture ();
  }
| PACKED_DECIMAL /* or COMP-3 */
  {
    current_field->usage = USAGE_PACKED;
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
  {
    current_field->occurs = $2;
    current_field->f.have_occurs = 1;
  }
  occurs_indexed
| OCCURS integer TO integer _times DEPENDING _on value
  {
    current_field->occurs = $4;
    current_field->f.have_occurs = 1;
    yywarn ("OCCURS DEPENDING is not implemented");
  }
  occurs_indexed
;
occurs_indexed:
| occurs_key_list INDEXED _by occurs_index_list
  {
    current_field->index_list = $4;
  }
;
occurs_key_list:
  /* nothing */			{ $$ = NULL; }
| occurs_key_list
  ASCENDING _key _is WORD	{ $$ = NULL; }
| occurs_key_list
  DESCENDING _key _is WORD	{ $$ = NULL; }
;
occurs_index_list:
  occurs_index			 { $$ = list ($1); }
| occurs_index_list occurs_index { $$ = list_add ($1, $2); }
;
occurs_index:
  WORD
  {
    $$ = make_field ($1);
    COBC_TREE_CLASS ($$) = COB_NUMERIC;
    COBC_FIELD ($$)->usage = USAGE_INDEX;
    COBC_FIELD ($$)->pic = make_picture ();
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
    if (current_field->level != 88)
      {
	if ($3->next != NULL || COBC_PAIR_P ($3))
	  yyerror ("only level 88 item may have multiple values");
	else
	  current_field->value = $3->item;
      }
    else
      {
	struct cobc_list *l;
	cobc_tree cond = NULL;
	cobc_tree parent = COBC_TREE (current_field->parent);
	for (l = $3; l; l = l->next)
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
	    if (cond)
	      cond = make_cond (cond, COBC_COND_OR, c);
	    else
	      cond = c;
	  }
	current_field->cond = cond;
	current_field->value = $3->item;
	if (COBC_PAIR_P (current_field->value))
	  current_field->value = COBC_PAIR (current_field->value)->x;
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
  RENAMES qualified_predefined_word
  renames_thru			{ yywarn ("RENAMES is not implemented"); }
;
renames_thru:
| THRU qualified_predefined_word
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
procedure_list:
| procedure_list		{ last_exec_list = program_spec.exec_list; }
  procedure
  {
    struct cobc_list *l;
    for (l = program_spec.exec_list; l; l = l->next)
      if (l->next == last_exec_list)
	{
	  COBC_TREE (l->item)->loc.file = @3.text;
	  COBC_TREE (l->item)->loc.line = @3.first_line;
	}
  }
;
procedure:
  procedure_section
| procedure_paragraph
| statement
| error '.'
| '.'
;


/*******************
 * Section/Paragraph
 *******************/

procedure_section:
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
;

procedure_paragraph:
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
    yywarn ("ACCEPT FROM name is not implemented");
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
    push_assign (make_expr (p->field, '+', e), $3);
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
    push_assign (e, $4);
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
    push_call_3 (COB_CALL, $2, COBC_TREE ($4), $5);
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
  value				{ $$ = make_call_item (current_call_mode, $1);}
| BY call_mode value		{ $$ = make_call_item (current_call_mode, $3);}
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
  CLOSE file_name_list
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_call_1 (COB_CLOSE, l->item);
  }
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
    struct cobc_file_name *p = COBC_FILE_NAME ($2);
    if (p->organization == COB_ORG_RELATIVE)
      push_call_2 (COB_DELETE, $2, make_index (COBC_TREE (p->key)));
    else
      push_call_2 (COB_DELETE, $2, cobc_int0);
  }
  opt_invalid_key
  _end_delete
;
_end_delete: | END_DELETE ;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY opt_value_list display_upon flag_with_no_advancing
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_call_1 (COB_DISPLAY, l->item);
    push_call_0 (COB_NEWLINE);
  }
  ;
display_upon:
| _upon mnemonic_name		{ /* ignored */ }
;
flag_with_no_advancing:
  /* nothing */			{ $$ = 0; }
| _with NO ADVANCING		{ $$ = 1; }
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
    push_assign (make_expr (p->field, '/', $1), $3);
  }
| number INTO number GIVING math_edited_name_list
  {
    push_assign (make_expr ($3, '/', $1), $5);
  }
| number BY number GIVING math_edited_name_list
  {
    push_assign (make_expr ($1, '/', $3), $5);
  }
| number INTO number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    struct cobc_list *l;
    l = list (make_call_3 (COB_DIVIDE, $5, $8, make_integer ($6)));
    l = cons (make_call_1 (COB_PUSH, $1), l);
    l = cons (make_call_1 (COB_PUSH, $3), l);
    push_tree (make_status_sequence (l));
  }
| number BY number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    struct cobc_list *l;
    l = list (make_call_3 (COB_DIVIDE, $5, $8, make_integer ($6)));
    l = cons (make_call_1 (COB_PUSH, $3), l);
    l = cons (make_call_1 (COB_PUSH, $1), l);
    push_tree (make_status_sequence (l));
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
	COBC_TREE ($$)->loc.file = @2.text;
	COBC_TREE ($$)->loc.line = @2.first_line;
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
    push_call_2 (COB_GOTO_DEPENDING, COBC_TREE ($3), $6);
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
  INITIALIZE name_list _initialize_replacing
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_call_1 (COB_INITIALIZE, l->item);
  }
;
_initialize_replacing:
| REPLACING initialize_replacing_list
  {
    yywarn ("INITIALIZE REPLACING is ignored");
  }
;
initialize_replacing_list:
  initialize_replacing
| initialize_replacing_list initialize_replacing
;
initialize_replacing:
  replacing_option _data BY value
;
replacing_option:
  ALPHABETIC
| ALPHANUMERIC
| NUMERIC
| ALPHANUMERIC_EDITED
| NUMERIC_EDITED
| NATIONAL
| NATIONAL_EDITED
;
_data: | DATA ;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT data_name inspect_tallying
  {
    push_call_2 (COB_INSPECT_TALLYING, $2, COBC_TREE ($3));
  }
| INSPECT data_name inspect_replacing
  {
    push_call_2 (COB_INSPECT_REPLACING, $2, COBC_TREE ($3));
  }
| INSPECT data_name inspect_converting
  {
    push_call_2 (COB_INSPECT_CONVERTING, $2, COBC_TREE ($3));
  }
| INSPECT data_name inspect_tallying inspect_replacing
  {
    push_call_2 (COB_INSPECT_TALLYING, $2, COBC_TREE ($3));
    push_call_2 (COB_INSPECT_REPLACING, $2, COBC_TREE ($4));
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
    $$ = make_inspect_item (INSPECT_CHARACTERS, inspect_name, 0, $2);
  }
| ALL value inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_ALL, inspect_name, $2, $3);
  }
| LEADING value inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_LEADING, inspect_name, $2, $3);
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
    $$ = make_inspect_item (INSPECT_CHARACTERS, NULL, $3, $4);
  }
| ALL value BY value inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_ALL, $4, $2, $5);
  }
| LEADING value BY value inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_LEADING, $4, $2, $5);
  }
| FIRST value BY value inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_FIRST, $4, $2, $5);
  }

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING value TO value inspect_before_after_list
  {
    $$ = list (make_inspect_item (INSPECT_CONVERTING, $2, $4, $5));
  }

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* nothing */					 { $$ = NULL; }
| inspect_before_after_list inspect_before_after { $$ = list_add ($1, $2); }
;
inspect_before_after:
  BEFORE _initial value
  {
    $$ = make_inspect_item (INSPECT_BEFORE, $3, 0, 0);
  }
| AFTER _initial value
  {
    $$ = make_inspect_item (INSPECT_AFTER, $3, 0, 0);
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
    push_assign (make_expr (p->field, '*', $1), $3);
  }
| number BY number GIVING math_edited_name_list
  {
    push_assign (make_expr ($1, '*', $3), $5);
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
      push_call_2 (COB_OPEN, l->item, make_integer ($1));
  }
;
open_mode:
  INPUT				{ $$ = 1; }
| I_O				{ $$ = 2; }
| OUTPUT			{ $$ = 3; }
| EXTEND			{ $$ = 4; }
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
    COBC_PERFORM ($$)->cond = $1;
  }
| perform_test UNTIL condition
  {
    $$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($$)->test = $1;
    COBC_PERFORM ($$)->cond = $3;
  }
| perform_test VARYING numeric_name FROM value BY value UNTIL condition
  perform_after_list
  {
    $$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($$)->test = $1;
    COBC_PERFORM ($$)->init = make_call_2 (COB_MOVE, $5, $3);
    COBC_PERFORM ($$)->step = make_assign ($3, make_expr ($3, '+', $7), 0);
    COBC_PERFORM ($$)->cond = $9;
  }
;
perform_test:
  /* nothing */			{ $$ = COBC_BEFORE; }
| _with TEST BEFORE		{ $$ = COBC_BEFORE; }
| _with TEST AFTER		{ $$ = COBC_AFTER; }
;
perform_after_list:
| perform_after_list
  AFTER numeric_name FROM value BY value UNTIL condition
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
    struct cobc_file_name *p = COBC_FILE_NAME ($2);
    if ($3 == 0)
      {
	/* READ */
	if ($6)
	  push_call_2 (COB_READ, $2, $6);
	else if (p->organization == COB_ORG_RELATIVE)
	  push_call_2 (COB_READ, $2, make_index (COBC_TREE (p->key)));
	else
	  push_call_2 (COB_READ, $2, cobc_int0);
      }
    else
      {
	/* READ NEXT */
	push_call_1 (COB_READ_NEXT, $2);
      }
    if ($5)
      push_move (COBC_TREE (p->record), $5);
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
    if ($2)
      {
	cobc_tree file = COBC_FIELD ($2)->file;
	struct cobc_file_name *p = COBC_FILE_NAME (file);
	if ($3)
	  push_move ($3, $2);
	if (p->organization == COB_ORG_RELATIVE)
	  push_call_2 (COB_REWRITE, file, make_index (COBC_TREE (p->key)));
	else
	  push_call_2 (COB_REWRITE, file, cobc_int0);
      }
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
    push_call_3 (COB_SEARCH, $2, $3, $5);
    if ($4)
      push_call_2 (COB_SEARCH_AT_END, $2, $4);
  }
| SEARCH ALL table_name search_at_end
  WHEN condition imperative_statement _end_search
  {
    // push_call_3 (COB_SEARCH, $2, $3, $5);
    if ($4)
      push_call_2 (COB_SEARCH_AT_END, $3, $4);
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
  SET name_list TO number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_move ($4, l->item);
  }
| SET name_list UP BY number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_tree (make_assign (l->item, make_expr (l->item, '+', $5), 0));
  }
| SET name_list DOWN BY number
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_tree (make_assign (l->item, make_expr (l->item, '-', $5), 0));
  }
| SET condition_name_list TO TRUE
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      push_call_1 (COB_SET_TRUE, l->item);
  }
| SET set_on_off_list
  {
    yywarn ("SET ON/OFF is not implemented");
  }
;
set_on_off_list:
  set_on_off
| set_on_off_list set_on_off
;
set_on_off:
  mnemonic_name_list TO ON
| mnemonic_name_list TO OFF
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
    struct cobc_file_name *p = COBC_FILE_NAME ($1);
    if (p->organization == COB_ORG_RELATIVE)
      push_call_3 (COB_START, $1, cobc_int0, make_index (COBC_TREE (p->key)));
    else
      push_call_3 (COB_START, $1, cobc_int0, cobc_int0);
  }
| file_name KEY _is operator data_name
  {
    push_call_3 (COB_START, $1, make_integer ($4), $5);
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
      $2 = cons (make_string_item (STRING_WITH_POINTER, $5), $2);
    push_call_2 (COB_STRING, $4, COBC_TREE ($2));
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
    $$ = cons (make_string_item (STRING_DELIMITED_NAME, $4), $1);
  }
| string_name_list DELIMITED _by SIZE
  {
    $$ = cons (make_string_item (STRING_DELIMITED_SIZE, 0), $1);
  }
;
string_name_list:
  value
  {
    $$ = list (make_string_item (STRING_CONCATENATE, $1));
  }
| string_name_list value
  {
    $$ = list_add ($1, make_string_item (STRING_CONCATENATE, $2));
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
    push_assign (make_expr (p->field, '-', e), $3);
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
    push_assign (e, $5);
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
      $3 = cons (make_string_item (UNSTRING_WITH_POINTER, $6), $3);
    if ($7)
      $5 = list_add ($5, make_string_item (UNSTRING_TALLYING, $7));
    push_call_2 (COB_UNSTRING, $2, COBC_TREE (list_append ($3, $5)));
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
    int type = $1 ? UNSTRING_DELIMITED_ALL : UNSTRING_DELIMITED_BY;
    $$ = list (make_string_item (type, $2));
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
    $$ = list (make_string_item (UNSTRING_INTO, $1));
    if ($2)
      $$ = list_add ($$, make_string_item (UNSTRING_DELIMITER, $2));
    if ($3)
      $$ = list_add ($$, make_string_item (UNSTRING_COUNT, $3));
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
    if ($2)
      {
	cobc_tree f = COBC_FIELD ($2)->file;
	struct cobc_file_name *p = COBC_FILE_NAME (f);
	/* AFTER ADVANCING */
	if ($4 && $4->place == COBC_AFTER)
	  {
	    if ($4->lines)
	      push_call_2 (COB_WRITE_LINES, f, make_index ($4->lines));
	    else
	      push_call_1 (COB_WRITE_PAGE, f);
	  }
	/* WRITE */
	if ($3)
	  push_move ($3, $2);
	if (p->organization == COB_ORG_RELATIVE)
	  push_call_2 (COB_WRITE, f, make_index (COBC_TREE (p->key)));
	else
	  push_call_2 (COB_WRITE, f, cobc_int0);
	/* BEFORE ADVANCING */
	if ($4 && $4->place == COBC_BEFORE)
	  {
	    if ($4->lines)
	      push_call_2 (COB_WRITE_LINES, f, make_index ($4->lines));
	    else
	      push_call_1 (COB_WRITE_PAGE, f);
	  }
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
    $$ = malloc (sizeof (struct write_option));
    $$->place = $1;
    $$->lines = $3;
  }
| before_or_after _advancing PAGE
  {
    $$ = malloc (sizeof (struct write_option));
    $$->place = $1;
    $$->lines = 0;
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
    push_status_handler (cobc_int0, 0, $1);
  }
| not_at_end_sentence
  {
    push_status_handler (cobc_int0, $1, 0);
  }
| at_end_sentence not_at_end_sentence
  {
    push_status_handler (cobc_int0, $2, $1);
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
| invalid_key
;
invalid_key:
  invalid_key_sentence
  {
    push_status_handler (cobc_int0, 0, $1);
  }
| not_invalid_key_sentence
  {
    push_status_handler (cobc_int0, $1, 0);
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    push_status_handler (cobc_int0, $2, $1);
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
  condition_name		{ $$ = COBC_FIELD ($1)->cond; }
| comparative_condition		{ $$ = $1; }
| class_condition		{ $$ = $1; }
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
    $$ = make_unary_cond ($1, $4);
    if ($3)
      $$ = make_unary_cond ($$, COBC_COND_NOT);
  }
| expr _is flag_not class_name
  {
    $$ = make_cond ($1, COBC_COND_CLASS, $4);
    if ($3)
      $$ = make_unary_cond ($$, COBC_COND_NOT);
  }
;
class:
  NUMERIC			{ $$ = COBC_COND_NUMERIC; }
| ALPHABETIC			{ $$ = COBC_COND_ALPHABETIC; }
| ALPHABETIC_LOWER		{ $$ = COBC_COND_LOWER; }
| ALPHABETIC_UPPER		{ $$ = COBC_COND_UPPER; }
| POSITIVE			{ $$ = COBC_COND_POSITIVE; }
| NEGATIVE			{ $$ = COBC_COND_NEGATIVE; }
| ZERO				{ $$ = COBC_COND_ZERO; }
;


/*******************
 * Expression
 *******************/

expr:
  value				{ $$ = $1; }
| '(' expr ')'			{ $$ = $2; }
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
  name
  {
    if (COBC_TREE_CLASS ($1) != COB_NUMERIC)
      yyerror ("`%s' not numeric", tree_to_string ($1));
    $$ = $1;
  }
;

/* Numeric edited name */

numeric_edited_name:
  name
  {
    int category = COBC_FIELD ($1)->category;
    if (category != COB_NUMERIC && category != COB_NUMERIC_EDITED)
      yyerror ("`%s' not numeric or numeric edited", tree_to_string ($1));
    $$ = $1;
  }
;

/* Group name */

group_name:
  name
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
  CONDITION_NAME		{ $$ = $1; }
;

/* Data name */

data_name_list:
  data_name			{ $$ = list ($1); }
| data_name_list data_name	{ $$ = list_add ($1, $2); }
;
data_name:
  name
  {
    if (COBC_FIELD_P ($1))
      {
	struct cobc_field *p = COBC_FIELD ($1);
	if (p->indexes > 0)
	  {
	    yyerror ("`%s' must be subscripted", p->word->name);
	    // $1 = make_subref ($1, $2);
	  }
      }
    $$ = $1;
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
    if (!COBC_FIELD ($1)->file)
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
  mnemonic_name { }
| mnemonic_name_list mnemonic_name { }
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

name_list:
  name				{ $$ = list ($1); }
| name_list name		{ $$ = list_add ($1, $2); }
;
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
	yyerror ("`%s' undefined", $1->name);
	$$ = make_filler ();
      }
  }
qualified_word:
  WORD
  {
    $$ = $1;
    if ($1->count > 1)
      yywarn ("`%s' ambiguous; need qualification", $1->name);
  }
| WORD in_of qualified_name
  {
    $$ = lookup_qualified_word ($1, COBC_FIELD ($3));
    if (!$$)
      {
	yyerror ("`%s' undefined in `%s'", $1->name, tree_to_string ($3));
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


/*
 * Predefined words
 */

qualified_predefined_word:
  WORD				{ $$ = cons ($1, NULL); }
| qualified_predefined_word in_of
  WORD				{ $$ = cons ($3, $1); }
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
    struct cobc_location loc = {cobc_source_file, cobc_last_line};
    yywarn_loc (&loc, "`.' is expected after `%s'", cobc_last_text);
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

static void
init_field (int level, cobc_tree field)
{
  struct cobc_field *last_field = current_field;

  current_field = COBC_FIELD (field);
  current_field->level = level;
  current_field->occurs = 1;
  current_field->usage = USAGE_DISPLAY;
  current_field->category = COB_ALPHANUMERIC;

  if (level == 1 || level == 77)
    {
      if (last_field)
	{
	  struct cobc_field *p = last_field;
	  while (p->parent)
	    p = p->parent;
	  p->sister = current_field;
	}
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
  if (p->level == 66)
    {
      /* REDEFINES */
    }
  else if (p->level == 88)
    {
      /* conditional variable */
      COBC_TREE_CLASS (p) = COB_BOOLEAN;
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
      switch (p->usage)
	{
	case USAGE_DISPLAY:
	  break;
	case USAGE_BINARY:
	case USAGE_PACKED:
	  if (p->category != COB_NUMERIC)
	    yywarn ("USAGE: should be numeric");
	  break;
	case USAGE_INDEX:
	  COBC_TREE_CLASS (p) = COB_NUMERIC;
	  break;
	}

      /* validate SIGN */

      /* validate OCCURS */
      if (p->f.have_occurs)
	if (p->level < 2 || p->level > 49)
	  yyerror ("OCCURS cannot be used with level %02d field", p->level);

      /* validate JUSTIFIED RIGHT */

      /* validate SYNCHRONIZED */
      if (p->f.synchronized)
	if (p->usage != USAGE_BINARY)
	  {
	    yywarn ("SYNCHRONIZED here has no effect");
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
	      yywarn ("VALUE should be numeric");
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

      if (p->f.justified)
	{
	  char c = p->category;
	  if (!(c == 'A' || c == 'X' || c == 'N'))
	    yyerror ("`%s' cannot have JUSTIFIED RIGHT",
		     tree_to_string (COBC_TREE (p)));
	}

      /* count the number of indexes needed */
      if (p->parent)
	p->indexes = p->parent->indexes;
      if (p->f.have_occurs)
	p->indexes++;
    }
}

static void
validate_field_tree (struct cobc_field *p)
{
  if (p->children)
    {
      /* group */
      COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;

      if (p->f.justified)
	yyerror ("group item cannot have JUSTFIED RIGHT");

      for (p = p->children; p; p = p->sister)
	validate_field_tree (p);
    }
  else {
    if (!p->pic)
      {
	if (p->usage != USAGE_INDEX)
	  yyerror ("`%s' must have PICTURE", tree_to_string (COBC_TREE (p)));
	p->pic = make_picture ();
      }
  }
}

static struct cobc_field *
lookup_predefined_word (struct cobc_list *l)
{
  cobc_tree x;
  struct cobc_word *p = l->item;
  if (p->count == 0)
    {
      yyerror ("`%s' is used, but not defined", p->name);
      return NULL;
    }
  else if (p->count > 1)
    yywarn ("`%s' ambiguous; need qualification", p->name);

  x = p->item;
  for (l = l->next; l; l = l->next)
    {
      struct cobc_word *w = l->item;
      p = lookup_qualified_word (w, COBC_FIELD (x));
      if (!p)
	{
	  yyerror ("`%s' undefined in `%s'", w->name, tree_to_string (x));
	  return NULL;
	}
      x = p->item;
    }
  return COBC_FIELD (x);
}

static void
validate_file_name (struct cobc_file_name *p)
{
  struct cobc_alt_key *l;
  if (p->key)
    p->key = lookup_predefined_word ((struct cobc_list *) p->key);
  if (p->status)
    p->status = lookup_predefined_word ((struct cobc_list *) p->status);
  for (l = p->alt_key_list; l; l = l->next)
    l->key = lookup_predefined_word ((struct cobc_list *) l->key);
}

static const char *
lookup_label (struct cobc_word *w, struct cobc_label_name *section)
{
  for (; w; w = w->link)
    if (w->item
	&& COBC_LABEL_NAME_P (w->item)
	&& section == COBC_LABEL_NAME (w->item)->section)
      {
	/* found */
	return COBC_LABEL_NAME (w->item)->cname;
      }
  yyerror ("`%s' not defined in section `%s'",
	   w->name, section->word->name);
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

static cobc_tree
make_add (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 (COB_ADD, f1, f2, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_sub (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 (COB_SUB, f1, f2, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_move (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_2 (COB_MOVE, f1, f2);
}

static struct cobc_list *
make_corr (cobc_tree (*func)(), struct cobc_field *g1, struct cobc_field *g2,
	   int opt, struct cobc_list *l)
{
  struct cobc_field *p1, *p2;
  for (p1 = g1->children; p1; p1 = p1->sister)
    if (!p1->redefines && !p1->f.have_occurs)
      for (p2 = g2->children; p2; p2 = p2->sister)
	if (!p2->redefines && !p2->f.have_occurs)
	  if (strcmp (p1->word->name, p2->word->name) == 0)
	    {
	      if (p1->children && p2->children)
		l = make_corr (func, p1, p2, opt, l);
	      else
		l = cons (func (COBC_TREE (p1), COBC_TREE (p2), opt), l);
	    }
  return l;
}

static cobc_tree
make_opt_cond (cobc_tree last, int type, cobc_tree this)
{
 again:
  if (COND_TYPE (last) == COBC_COND_NOT)
    {
      COND_LEFT (last) = make_opt_cond (COND_LEFT (last), type, this);
      return last;
    }

  if (COND_IS_UNARY (last))
    {
      yyerror ("broken condition");
      return last; /* error recovery */
    }

  if (COND_TYPE (last) == COBC_COND_AND || COND_TYPE (last) == COBC_COND_OR)
    {
      last = COND_LEFT (last);
      goto again;
    }

  if (type == -1)
    type = COND_TYPE (last);
  return make_cond (COND_LEFT (last), type, this);
}

static void
redefinition_error (cobc_tree x)
{
  struct cobc_field *p = COBC_FIELD (x);
  yyerror ("redefinition of `%s'", p->word->name);
  yyerror_loc (&x->loc, "`%s' previously defined here", p->word->name);
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
yywarn_loc (struct cobc_location *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->file, loc->line, "warning: ", fmt, ap);
  va_end (ap);

  warning_count++;
}

void
yyerror_loc (struct cobc_location *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->file, loc->line, "", fmt, ap);
  va_end (ap);

  error_count++;
}
