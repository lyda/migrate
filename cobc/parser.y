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

%expect 122

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
#include "lib/gettext.h"

#define yydebug		yy_bison_debug
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#define IGNORE(x)	/* ignored */
#define PENDING(x)	yywarn (_("`%s' not implemented"), x)
#define OBSOLETE(x)	yywarn (_("`%s' obsolete"), x)

#define push(x)	program_spec.exec_list = cons (x, program_spec.exec_list)

#define push_location(x)	   push (x)

#define push_funcall_0(f)	   push (make_funcall_0 (f))
#define push_funcall_1(f,a)	   push (make_funcall_1 (f, a))
#define push_funcall_2(f,a,b)	   push (make_funcall_2 (f, a, b))
#define push_funcall_3(f,a,b,c)	   push (make_funcall_3 (f, a, b, c))
#define push_funcall_4(f,a,b,c,d)  push (make_funcall_4 (f, a, b, c, d))

#define make_handler(v,a,b)	make_funcall_4 ("@handler", (void *) v, a, b, 0)

#define push_file_handler(f,h)					\
  {								\
    cobc_tree __f = (f);					\
    cobc_tree __h = (h);					\
    COBC_FUNCALL (__h)->argv[3] = COBC_FILE (__f)->handler;	\
    push (__h);							\
  }

#define push_sequence_with_handler(x,h)			\
  {							\
    cobc_tree __x = (x);				\
    cobc_tree __h = (h);				\
    if (__h) COBC_SEQUENCE (__x)->save_status = 1;	\
    push (__x);						\
    if (__h) push (__h);				\
  }

#define cobc_ref(x)	COBC_VALUE (resolve_name (x))

static struct cobc_program_spec program_spec;

struct cobc_program_spec *current_program = &program_spec;

static struct cobc_field *current_field;
static struct cobc_file *current_file;
static struct cobc_label *current_section, *current_paragraph;

static int current_call_mode;
static char *current_inspect_func;
static cobc_tree current_inspect_data;

static int warning_count = 0;
static int error_count = 0;

static int last_operator;
static cobc_tree last_lefthand;

static cobc_tree resolve_name (cobc_tree x);
static cobc_tree resolve_field (cobc_tree x);
static cobc_tree resolve_label (cobc_tree x);

static struct cobc_field *build_field (cobc_tree level, cobc_tree name, struct cobc_field *last_field);
static struct cobc_field *validate_redefines (struct cobc_field *field, cobc_tree redefines);
static void validate_field_tree (struct cobc_field *p);
static void finalize_file (struct cobc_file *f, struct cobc_field *records);

static void field_set_used (struct cobc_field *p);
static int builtin_switch_id (cobc_tree x);

static cobc_tree build_assign (struct cobc_list *vars, char op, cobc_tree val);
static cobc_tree build_add (cobc_tree v, cobc_tree n, int round);
static cobc_tree build_sub (cobc_tree v, cobc_tree n, int round);
static cobc_tree build_move (cobc_tree src, cobc_tree dst);
static cobc_tree build_corresponding (cobc_tree (*func)(), cobc_tree x1, cobc_tree x2, int opt);
static cobc_tree build_divide (cobc_tree dividend, cobc_tree divisor, cobc_tree quotient, int round, cobc_tree remainder);
static cobc_tree build_cond (cobc_tree x);
static cobc_tree build_evaluate (struct cobc_list *subject_list, struct cobc_list *case_list);
static cobc_tree build_search_all (cobc_tree table, cobc_tree when);

static void redefinition_error (cobc_tree x);
static void undefined_error (cobc_tree x, cobc_tree parent);
static void ambiguous_error (cobc_tree x);
%}

%union {
  int inum;
  char *str;
  cobc_tree tree;
  struct cobc_list *list;
  struct cobc_picture *pict;
}

%token <str>  FUNCTION_NAME
%token <pict> PICTURE_TOK
%token <tree> INTEGER_LITERAL NUMERIC_LITERAL NONNUMERIC_LITERAL
%token <tree> NAME CLASS_NAME MNEMONIC_NAME

%token <tree> ACCEPT ADD CALL CANCEL CLOSE COMPUTE DELETE DISPLAY DIVIDE
%token <tree> EVALUATE IF INITIALIZE INSPECT MERGE MOVE MULTIPLY OPEN PERFORM
%token <tree> READ RELEASE RETURN REWRITE SEARCH SET SORT START STRING
%token <tree> SUBTRACT UNSTRING WRITE

%token ACCESS ADVANCING AFTER ALL ALPHABET ALPHABETIC ALPHABETIC_LOWER
%token ALPHABETIC_UPPER ALPHANUMERIC ALPHANUMERIC_EDITED ALSO ALTER ALTERNATE
%token AND ANY ARE AREA ASCENDING ASSIGN AT AUTO BACKGROUND_COLOR BEFORE BELL
%token BINARY BLANK BLINK BLOCK BY CHARACTER CHARACTERS CLASS CODE_SET
%token COLLATING COLUMN COMMA COMMAND_LINE COMMON CONFIGURATION CONTAINS
%token CONTENT CONTINUE CONVERTING CORRESPONDING COUNT CRT CURRENCY CURSOR
%token DATA DATE DAY DAY_OF_WEEK DEBUGGING DECIMAL_POINT DECLARATIVES
%token DELIMITED DELIMITER DEPENDING DESCENDING DIVISION DOWN DUPLICATES
%token DYNAMIC ELSE END END_ACCEPT END_ADD END_CALL END_COMPUTE END_DELETE
%token END_DISPLAY END_DIVIDE END_EVALUATE END_IF END_MULTIPLY END_PERFORM
%token END_READ END_RETURN END_REWRITE END_SEARCH END_START END_STRING
%token END_SUBTRACT END_UNSTRING END_WRITE ENVIRONMENT ENVIRONMENT_VARIABLE
%token EOL EOS EQUAL ERASE ERROR EXCEPTION EXIT EXTEND EXTERNAL FALSE FD
%token FILE_CONTROL FILLER FIRST FOR FOREGROUND_COLOR FROM FULL GE GIVING
%token GLOBAL GO GREATER HIGHLIGHT HIGH_VALUE IDENTIFICATION IN INDEX INDEXED
%token INPUT INPUT_OUTPUT INTO INVALID IS I_O I_O_CONTROL JUSTIFIED KEY LABEL
%token LE LEADING LEFT LENGTH LESS LINE LINES LINKAGE LOCK LOWLIGHT LOW_VALUE
%token MEMORY MINUS MODE MULTIPLE NATIONAL NATIONAL_EDITED NATIVE NEGATIVE
%token NEXT NO NOT NUMBER NUMERIC NUMERIC_EDITED OBJECT_COMPUTER OCCURS OF OFF
%token OMITTED ON OPTIONAL OR ORDER ORGANIZATION OTHER OUTPUT OVERFLOW PADDING
%token PAGE PLUS POINTER POSITION POSITIVE PROCEDURE PROCEED PROGRAM
%token PROGRAM_ID QUOTE RANDOM RECORD RECORDS REDEFINES REEL REFERENCE
%token RELATIVE REMAINDER REMOVAL RENAMES REPLACING REQUIRED RESERVE RETURNING
%token REVERSE_VIDEO REWIND RIGHT ROUNDED RUN SAME SCREEN SD SECTION SECURE
%token SELECT SENTENCE SEPARATE SEQUENCE SEQUENTIAL SIGN SIZE SIZE SORT_MERGE
%token SOURCE_COMPUTER SPACE SPECIAL_NAMES STANDARD STANDARD_1 STANDARD_2
%token STATUS STOP SYMBOLIC SYNCHRONIZED TALLYING TAPE TEST THAN THEN THRU
%token TIME TIMES TO TOK_FILE TOK_INITIAL TRAILING TRUE UNDERLINE UNIT UNTIL
%token UP UPON USAGE USE USING VALUE VARYING WHEN WITH WORKING_STORAGE ZERO

%type <inum> flag_all flag_duplicates flag_optional flag_global
%type <inum> flag_not flag_next flag_rounded flag_separate
%type <inum> integer display_upon screen_plus_minus
%type <inum> before_or_after perform_test replacing_option usage
%type <inum> select_organization select_access_mode same_option
%type <inum> ascending_or_descending opt_from_integer opt_to_integer
%type <list> occurs_key_list occurs_index_list value_item_list data_name_list
%type <list> value_list opt_value_list evaluate_case
%type <list> evaluate_case_list evaluate_when_list evaluate_object_list
%type <list> label_list numeric_value_list string_list
%type <list> string_list_1 inspect_before_after_list
%type <list> reference_list mnemonic_name_list
%type <list> file_name_list math_name_list math_edited_name_list
%type <list> call_param_list call_using expr_item_list initialize_replacing
%type <list> initialize_replacing_list class_item_list
%type <tree> call_param evaluate_object
%type <tree> add_to at_line_column
%type <tree> call_not_on_exception call_on_exception call_returning class_item
%type <tree> column_number condition data_name expr expr_1
%type <tree> expr_item field_description field_description_list
%type <tree> field_description_list_1 field_description_list_2 field_name
%type <tree> file_name function group_name integer_label
%type <tree> integer_value label label_name qualified_name name name_1
%type <tree> line_number literal mnemonic_name opt_subscript subscript
%type <tree> numeric_value numeric_edited_name numeric_expr
%type <tree> numeric_name occurs_index on_or_off opt_screen_description_list
%type <tree> opt_with_pointer perform_option perform_procedure
%type <tree> program_name record_name reference_or_literal
%type <tree> reference screen_description screen_description_list
%type <tree> section_label section_name statement_list
%type <tree> table_name text_value undefined_name
%type <tree> value value_item write_from
%type <tree> on_size_error on_overflow at_end opt_invalid_key invalid_key


%%
/*****************************************************************************
 * COBOL program sequence
 *****************************************************************************/

start:
  program			{ if (error_count) YYABORT; }
;
program:
  {
    /* init program spec */
    memset (current_program, 0, sizeof (struct cobc_program_spec));
    current_program->env.decimal_point = '.';
    current_program->env.currency_symbol = '$';
    current_program->env.numeric_separator = ',';

    /* init environment */
    cobc_in_procedure = 0;
    init_word_table ();
    make_field_3 (make_reference (lookup_word ("RETURN-CODE")),
		  "S9(9)", COBC_USAGE_INDEX);
  }
  identification_division
  environment_division
  data_division
  {
    struct cobc_list *l;
    for (l = list_reverse (current_program->reference_list); l; l = l->next)
      field_set_used (COBC_FIELD (cobc_ref (l->item)));
  }
  procedure_division
  _end_program
  {
    struct cobc_list *l;
    for (l = list_reverse (current_program->label_list); l; l = l->next)
      resolve_label (l->item);
    current_program->file_list = list_reverse (current_program->file_list);
    current_program->exec_list = list_reverse (current_program->exec_list);
    if (error_count == 0)
      codegen (&program_spec);
  }
;
_end_program:
| END PROGRAM NAME '.'
;


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

identification_division:
  IDENTIFICATION DIVISION '.'
  PROGRAM_ID '.' NAME opt_program_parameter '.'
  {
    int converted = 0;
    char *s, *name = strdup (COBC_NAME ($6));
    for (s = name; *s; s++)
      if (*s == '-')
	{
	  converted = 1;
	  *s = '_';
	}
    if (converted)
      yywarn_x ($6, _("PROGRAM-ID is converted to `%s'"), name);
    current_program->program_id = name;
  }
;
opt_program_parameter:
| _is TOK_INITIAL _program	{ current_program->initial_program = 1; }
| _is COMMON _program		{ PENDING ("COMMON"); }
;


/*****************************************************************************
 * ENVIRONMENT DIVISION.
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION '.'
  configuration_section
  input_output_section
;


/*******************
 * CONFICURATION SECTION
 *******************/

configuration_section:
| CONFIGURATION SECTION '.'
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
  SOURCE_COMPUTER '.' NAME _with_debugging_mode '.'
;
_with_debugging_mode:
| _with DEBUGGING MODE
  {
    yywarn (_("DEBUGGING MODE is ignored"));
    yywarn (_("use compiler option `-debug' instead"));
  }
;


/*
 * OBJECT COMPUTER
 */

object_computer:
  OBJECT_COMPUTER '.' NAME object_computer_options '.'
;
object_computer_options:
| object_computer_options object_computer_option
;
object_computer_option:
  _program collating_sequence		{ PENDING ("COLLATING SEQUENCE"); }
| MEMORY SIZE _is integer CHARACTERS	{ OBSOLETE ("MEMORY SIZE"); }
;
collating_sequence:
  _collating SEQUENCE _is NAME
;
_collating: | COLLATING ;


/*
 * SPECIAL-NAMES
 */

special_names:
  SPECIAL_NAMES '.' opt_special_names
;
opt_special_names:
| special_name_list '.'
;
special_name_list:
  special_name
| special_name_list special_name
;
special_name:
  special_name_mnemonic
| special_name_alphabet
| special_name_symbolic
| special_name_class
| special_name_currency
| special_name_decimal_point
| CURSOR _is reference		{ PENDING ("CURSOR"); }
| CRT STATUS _is reference	{ PENDING ("CRT STATUS"); }
;


/* Buildin name */

special_name_mnemonic:
  NAME
  {
    int n = lookup_builtin_word (COBC_NAME ($1));
    if (n == 0)
      yyerror_x ($1, _("unknown name `%s'"), COBC_NAME ($1));
    $<tree>$ = make_builtin (n);
  }
  special_name_mnemonic_define
  special_name_mnemonic_on_off
;
special_name_mnemonic_define:
| IS undefined_name
  {
    associate ($2, $<tree>0);
  }
;
special_name_mnemonic_on_off:
| special_name_mnemonic_on_off
  on_or_off _status _is undefined_name
  {
    int id = builtin_switch_id ($<tree>-1);
    if (id != -1)
      {
	cobc_tree x = make_field ($5);
	COBC_FIELD (x)->level = 88;
	COBC_FIELD (x)->parent = COBC_FIELD (cobc_switch[id]);
	COBC_FIELD (x)->values = list ($2);
	COBC_TREE_CLASS (x) = COB_TYPE_BOOLEAN;
      }
  }
;
on_or_off:
  ON				{ $$ = cobc_true; }
| OFF				{ $$ = cobc_false; }
;


/* ALPHABET */

special_name_alphabet:
  ALPHABET NAME _is alphabet_group
  {
    PENDING ("ALPHABET");
  }
;
alphabet_group:
  STANDARD_1
| STANDARD_2
| NATIVE
| NAME { }
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
    PENDING ("SYMBOLIC CHARACTERS");
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
  NAME { }
| char_list NAME { }
;
integer_list:
  integer { }
| integer_list integer { }
;
is_are: IS | ARE ;


/* CLASS */

special_name_class:
  CLASS undefined_name _is class_item_list
  {
    current_program->class_list =
      list_add (current_program->class_list, make_class ($2, $4));
  }
;
class_item_list:
  class_item			{ $$ = list ($1); }
| class_item_list class_item	{ $$ = list_add ($1, $2); }
;
class_item:
  literal			{ $$ = $1; }
| literal THRU literal
  {
    if (COBC_LITERAL ($1)->data[0] < COBC_LITERAL ($3)->data[0])
      $$ = make_pair ($1, $3);
    else
      $$ = make_pair ($3, $1);
  }
;


/* CURRENCY */

special_name_currency:
  CURRENCY _sign NONNUMERIC_LITERAL
  {
    unsigned char *s = COBC_LITERAL ($3)->data;
    if (COBC_LITERAL ($3)->size != 1)
      yyerror_x ($3, _("invalid currency sign `%s'"), s);
    current_program->env.currency_symbol = s[0];
  }
;


/* DECIMAL_POINT */

special_name_decimal_point:
  DECIMAL_POINT _is COMMA
  {
    current_program->env.decimal_point = ',';
    current_program->env.numeric_separator = '.';
  }
;


/*******************
 * INPUT-OUTPUT SECTION
 *******************/

input_output_section:
| INPUT_OUTPUT SECTION '.'
  file_control
  i_o_control
;


/*
 * FILE-CONTROL
 */

file_control:
| FILE_CONTROL '.' select_sequence
;
select_sequence:
| select_sequence
  SELECT flag_optional undefined_name
  {
    current_file = COBC_FILE (make_file ($4));
    current_file->organization = COB_ORG_SEQUENTIAL;
    current_file->access_mode = COB_ACCESS_SEQUENTIAL;
    current_file->optional = $3;
    current_file->handler = cobc_standard_error_handler;
    current_program->file_list = cons (current_file, current_program->file_list);
  }
  select_options '.'
  {
    char *name = COBC_NAME ($4);

    /* check ASSIGN clause */
    if (current_file->assign == NULL)
      yyerror_x ($4, _("ASSIGN required for file `%s'"), name);

    /* check KEY clause */
    switch (current_file->organization)
      {
      case COB_ORG_INDEXED:
	if (current_file->key == NULL)
	  yyerror_x ($4, _("RECORD KEY required for file `%s'"), name);
	break;
      case COB_ORG_RELATIVE:
	if (current_file->key == NULL
	    && current_file->access_mode != COB_ACCESS_SEQUENTIAL)
	  yyerror_x ($4, _("RELATIVE KEY required for file `%s'"), name);
	break;
      }
  }
;
select_options:
| select_options select_option
;
select_option:
  ASSIGN _to reference_or_literal
  {
    current_file->assign = $3;
  }
| RESERVE integer _area
  {
    IGNORE ("RESERVE");
  }
| select_organization
  {
    current_file->organization = $1;
  }
| ORGANIZATION _is select_organization
  {
    current_file->organization = $3;
  }
| ACCESS _mode _is select_access_mode
  {
    current_file->access_mode = $4;
  }
| _file STATUS _is reference
  {
    current_file->file_status = $4;
  }
| PADDING _character _is reference_or_literal
  {
    PENDING ("PADDING");
  }
| RECORD DELIMITER _is STANDARD_1
  {
    PENDING ("RECORD DELIMITER");
  }
| RELATIVE _key _is reference
  {
    current_file->key = $4;
  }
| RECORD _key _is reference
  {
    current_file->key = $4;
  }
| ALTERNATE RECORD _key _is reference flag_duplicates
  {
    struct cobc_alt_key *p = malloc (sizeof (struct cobc_alt_key));
    p->key = $5;
    p->duplicates = $6;
    p->next = NULL;

    /* add to the end of list */
    if (current_file->alt_key_list == NULL)
      current_file->alt_key_list = p;
    else
      {
	struct cobc_alt_key *l = current_file->alt_key_list;
	for (; l->next; l = l->next);
	l->next = p;
      }
  }
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
reference_or_literal:
  reference
| literal
;
flag_optional:
  /* empty */			{ $$ = 0; }
| OPTIONAL			{ $$ = 1; }
;
flag_duplicates:
  /* empty */			{ $$ = 0; }
| _with DUPLICATES		{ $$ = 1; }
;


/*
 * I-O-CONTROL
 */

i_o_control:
| I_O_CONTROL '.'
  i_o_statements
;
i_o_statements:
| i_o_statement_list '.'
;
i_o_statement_list:
  i_o_statement
| i_o_statement_list i_o_statement
;
i_o_statement:
  same_statement
| multiple_statement
;

/* SAME statement */

same_statement:
  SAME same_option _area _for file_name_list
  {
    PENDING ("SAME");
  }
;
same_option:
  /* empty */			{ $$ = 0; }
| RECORD			{ $$ = 1; }
| SORT				{ $$ = 2; }
| SORT_MERGE			{ $$ = 3; }
;

/* MULTIPLE statment */

multiple_statement:
  MULTIPLE _file _tape _contains multiple_file_list
  {
    PENDING ("MULTIPLE");
  }
;
multiple_file_list:
  multiple_file
| multiple_file_list multiple_file
;
multiple_file:
  file_name POSITION integer { }
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION '.'
  file_section
  working_storage_section
  linkage_section
  screen_section
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| TOK_FILE SECTION '.'
  file_description_sequence
;
file_description_sequence:
| file_description_sequence
  file_type file_name
  {
    current_file = COBC_FILE (cobc_ref ($3));
    if ($<inum>2 != 0)
      current_file->organization = $<inum>2;
  }
  file_options '.'
  field_description_list
  {
    finalize_file (current_file, COBC_FIELD ($7));
  }
;
file_type:
  FD				{ $<inum>$ = 0; }
| SD				{ $<inum>$ = COB_ORG_SORT; }
;
file_options:
| file_options file_option
;
file_option:
  _is GLOBAL			{ PENDING ("GLOBAL"); }
| _is EXTERNAL			{ PENDING ("EXTERNAL"); }
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
    current_file->record_max = $3;
  }
| RECORD _contains integer _to integer _characters
  {
    current_file->record_min = $3;
    current_file->record_max = $5;
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  record_depending
  {
    current_file->record_min = $6;
    current_file->record_max = $7;
  }
;
record_depending:
| DEPENDING _on reference
  {
    current_file->record_depending = $3;
  }
;
opt_from_integer:
  /* empty */			{ $$ = 0; }
| _from integer			{ $$ = $2; }
;
opt_to_integer:
  /* empty */			{ $$ = 0; }
| TO integer			{ $$ = $2; }
;


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
  DATA record_or_records reference_list { IGNORE ("DATA RECORD"); }
;


/*
 * CODE-SET clause
 */

codeset_clause:
  CODE_SET _is NAME
  {
    PENDING ("CODE-SET");
  }
;


/*******************
 * WORKING-STRAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION '.'
  field_description_list
  {
    if ($4)
      current_program->working_storage = COBC_FIELD ($4);
  }
;
field_description_list:
  /* empty */			{ $$ = NULL; }
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
  INTEGER_LITERAL field_name
  {
    current_field = build_field ($1, $2, current_field);
    if (current_field == NULL)
      YYERROR;
  }
  field_options '.'
  {
    $$ = COBC_TREE (current_field);
  }
;
field_name:
  /* empty */			{ $$ = make_filler (); }
| FILLER			{ $$ = make_filler (); }
| NAME				{ $$ = $1; }
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
  REDEFINES qualified_name
  {
    current_field->redefines = validate_redefines (current_field, $2);
    current_field->f.in_redefines = 1;
    if (current_field->redefines == NULL)
      YYERROR;
  }
;


/* EXTERNAL */

external_clause:
  _is EXTERNAL			{ current_field->f.external = 1; }
;


/* GLOBAL */

global_clause:
  _is GLOBAL			{ PENDING ("GLOBAL"); }
;


/* PICTURE */

picture_clause:
  PICTURE_TOK			{ current_field->pic = $1; }
;


/* USAGE */

usage_clause:
  _usage usage
;
usage:
  DISPLAY			{ current_field->usage = COBC_USAGE_DISPLAY; }
| BINARY /* or COMP */		{ current_field->usage = COBC_USAGE_BINARY; }
| INDEX				{ current_field->usage = COBC_USAGE_INDEX; }
;
_usage: | USAGE _is ;


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
  /* empty */			{ $$ = 0; }
| SEPARATE _character		{ $$ = 1; }
;


/* OCCURS */

occurs_clause:
  OCCURS integer _times
  occurs_keys occurs_indexed
  {
    current_field->occurs = $2;
    current_field->occurs_min = 1;
    current_field->indexes++;
    current_field->f.have_occurs = 1;
  }
| OCCURS integer TO integer _times DEPENDING _on reference
  occurs_keys occurs_indexed
  {
    current_field->occurs = $4;
    current_field->occurs_min = $2;
    current_field->occurs_depending = $8;
    current_field->indexes++;
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
	    struct cobc_parameter *p = l->item;
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
  /* empty */			{ $$ = NULL; }
| occurs_key_list
  ascending_or_descending _key _is reference_list
  {
    struct cobc_list *l;
    for (l = $5; l; l = l->next)
      l->item = make_parameter_1 ($2, l->item);
    $$ = list_append ($1, $5);
  }
;
ascending_or_descending:
  ASCENDING			{ $$ = COB_ASCENDING; }
| DESCENDING			{ $$ = COB_DESCENDING; }
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
  undefined_name
  {
    $$ = make_field_3 ($1, "S9(9)", COBC_USAGE_INDEX);
    current_program->index_list = list_add (current_program->index_list, $$);
  }
;

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
      }
    else
      {
	/* single VALUE */
	if ($3->next != NULL || COBC_PARAMETER_P ($3->item))
	  yyerror (_("only level 88 item may have multiple values"));
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
    current_field->redefines = COBC_FIELD (cobc_ref ($2));
    current_field->pic = current_field->redefines->pic;
  }
| RENAMES qualified_name THRU qualified_name
  {
    current_field->redefines = COBC_FIELD (cobc_ref ($2));
    current_field->rename_thru = COBC_FIELD (cobc_ref ($4));
  }
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION '.'
  field_description_list
  {
    if ($4)
      current_program->linkage_storage = COBC_FIELD ($4);
  }
;


/*******************
 * SCREEN SECTION
 *******************/

screen_section:
| SCREEN SECTION '.'
  {
    current_field = NULL;
  }
  opt_screen_description_list
  {
    struct cobc_field *p;
    for (p = COBC_FIELD ($5); p; p = p->sister)
      finalize_field_tree (p);
    current_program->screen_storage = COBC_FIELD ($5);
    current_program->enable_screen = 1;
  }
;

opt_screen_description_list:
  /* empty */			{ $$ = NULL; }
| screen_description_list	{ $$ = $1; }
;
screen_description_list:
  screen_description		{ $$ = $1; }
| screen_description_list
  screen_description		{ $$ = $1; }
;
screen_description:
  INTEGER_LITERAL field_name
  {
    current_field = build_field ($1, $2, current_field);
    if (current_field == NULL)
      YYERROR;

    current_field->f.screen = 1;
    current_field->screen_flag |= COB_SCREEN_FG_NONE;
    current_field->screen_flag |= COB_SCREEN_BG_NONE;
    if (current_field->parent)
      current_field->screen_flag |= current_field->parent->screen_flag;
  }
  screen_options '.'
  {
    if (current_field->pic == NULL)
      current_field->pic = yylex_picture ("X(0)");
    if (!current_field->screen_line)
      {
	current_field->screen_line = cobc_int1;
	current_field->screen_flag |= COB_SCREEN_LINE_CONST;
      }
    if (!current_field->screen_column)
      {
	current_field->screen_column = cobc_int1;
	current_field->screen_flag |= COB_SCREEN_COLUMN_CONST;
      }
    $$ = COBC_TREE (current_field);
  }
;
screen_options:
| screen_options screen_option
;
screen_option:
  BLANK LINE	{ current_field->screen_flag |= COB_SCREEN_BLANK_LINE; }
| BLANK SCREEN	{ current_field->screen_flag |= COB_SCREEN_BLANK_SCREEN; }
| BELL		{ current_field->screen_flag |= COB_SCREEN_BELL; }
| BLINK		{ current_field->screen_flag |= COB_SCREEN_BLINK; }
| ERASE EOL	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOL; }
| ERASE EOS	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOS; }
| HIGHLIGHT	{ current_field->screen_flag |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ current_field->screen_flag |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ current_field->screen_flag |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ current_field->screen_flag |= COB_SCREEN_UNDERLINE; }
| AUTO		{ current_field->screen_flag |= COB_SCREEN_AUTO; }
| SECURE	{ current_field->screen_flag |= COB_SCREEN_SECURE; }
| REQUIRED	{ current_field->screen_flag |= COB_SCREEN_REQUIRED; }
| FULL		{ current_field->screen_flag |= COB_SCREEN_FULL; }
| LINE _number _is screen_plus_minus integer_value
  {
    current_field->screen_line = $5;
    if (COBC_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| COLUMN _number _is screen_plus_minus integer_value
  {
    current_field->screen_column = $5;
    if (COBC_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| FOREGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_FG_MASK;
    switch ($3)
      {
      case 0: current_field->screen_flag |= COB_SCREEN_FG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_FG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_FG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_FG_SKYBLUE; break;
      case 4: current_field->screen_flag |= COB_SCREEN_FG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_FG_PURPLE; break;
      case 6: current_field->screen_flag |= COB_SCREEN_FG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_FG_WHITE; break;
      default:
	yyerror (_("invalid color `%d'"), $3);
      }
  }
| BACKGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_BG_MASK;
    switch ($3)
      {
      case 0: current_field->screen_flag |= COB_SCREEN_BG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_BG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_BG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_BG_SKYBLUE; break;
      case 4: current_field->screen_flag |= COB_SCREEN_BG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_BG_PURPLE; break;
      case 6: current_field->screen_flag |= COB_SCREEN_BG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_BG_WHITE; break;
      default:
	yyerror (_("invalid color `%d'"), $3);
      }
  }
| usage_clause
| blank_clause
| justified_clause
| sign_clause
| value_clause
| picture_clause
  {
    field_set_used (current_field);
  }
| USING data_name
  {
    current_field->screen_from = COBC_FIELD ($2);
    current_field->screen_to = COBC_FIELD ($2);
  }
| FROM data_name
  {
    current_field->screen_from = COBC_FIELD ($2);
  }
| TO data_name
  {
    current_field->screen_to = COBC_FIELD ($2);
  }
;
screen_plus_minus:
  /* empty */
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_ABS;
  }
| PLUS
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_PLUS;
  }
| MINUS
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_MINUS;
  }
;


/*****************************************************************************
 * PROCEDURE DIVISION
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION procedure_using '.'
  {
    current_section = NULL;
    current_paragraph = NULL;
    cobc_in_procedure = 1;
  }
  procedure_declaratives
  {
    push (cobc_main_label);
  }
  procedure_list
  {
    if (current_paragraph)
      push (make_perform_exit (current_paragraph));
    if (current_section)
      push (make_perform_exit (current_section));
  }
;
procedure_using:
| USING data_name_list
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      {
	struct cobc_field *f = COBC_FIELD (cobc_ref (l->item));
	if (f->level != 01 && f->level != 77)
	  yyerror_x (l->item, _("`%s' not level 01 or 77"), f->name);
	l->item = cobc_ref (l->item);
      }
    current_program->using_list = $2;
  }
;

procedure_declaratives:
| DECLARATIVES '.'
  procedure_list
  END DECLARATIVES
;


/*******************
 * Procedure list
 *******************/

procedure_list:
| procedure_list procedure
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
  section_name SECTION '.'
  {
    /* Exit the last section */
    if (current_paragraph)
      push (make_perform_exit (current_paragraph));
    if (current_section)
      push (make_perform_exit (current_section));

    /* Begin a new section */
    current_section = COBC_LABEL (make_label ($1, NULL));
    current_paragraph = NULL;
    push (current_section);
  }
  opt_use_statement
;

paragraph_header:
  section_name '.'
  {
    /* Exit the last paragraph */
    if (current_paragraph)
      push (make_perform_exit (current_paragraph));

    /* Begin a new paragraph */
    current_paragraph = COBC_LABEL (make_label ($1, current_section));
    if (current_section)
      current_section->children =
	cons (current_paragraph, current_section->children);
    push (current_paragraph);
  }
;

section_name:
  section_label
  {
    struct cobc_word *w = COBC_REFERENCE ($1)->word;
    if (w->count > 0)
      {
	cobc_tree item = w->items->item;
	if (/* used as a non-label name */
	    !COBC_LABEL_P (item)
	    /* used as a section name */
	    || COBC_LABEL (item)->section == NULL
	    /* used as the same paragraph name in the same section */
	    || COBC_LABEL (item)->section == current_section)
	  {
	    redefinition_error ($1);
	    YYERROR;
	  }
      }
    $$ = $1;
  }
;
section_label:
  NAME
| integer_label
;
integer_label:
  INTEGER_LITERAL
  {
    $$ = make_reference (lookup_word (COBC_LITERAL ($1)->data));
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
  {
    current_section->need_begin = 1;
    current_section->need_return = 1;
  }
;
use_target:
  file_name_list
  {
    struct cobc_list *l;
    for (l = $1; l; l = l->next)
      COBC_FILE (cobc_ref (l->item))->handler = current_section;
  }
| INPUT	 { current_program->file_handler[COB_OPEN_INPUT]  = current_section; }
| OUTPUT { current_program->file_handler[COB_OPEN_OUTPUT] = current_section; }
| I_O	 { current_program->file_handler[COB_OPEN_I_O]    = current_section; }
| EXTEND { current_program->file_handler[COB_OPEN_EXTEND] = current_section; }
;
_standard: | STANDARD ;
exception_or_error: EXCEPTION | ERROR ;


/*******************
 * Statements
 *******************/

statement_list:
  {
    $<list>$ = current_program->exec_list;
    current_program->exec_list = NULL;
  }
  statement_list_1
  {
    $$ = make_sequence (list_reverse (current_program->exec_list));
    current_program->exec_list = $<list>1;
  }
;
statement_list_1:
  statement
| statement_list_1 statement
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
| merge_statement
| move_statement
| multiply_statement
| open_statement
| perform_statement
| read_statement
| release_statement
| return_statement
| rewrite_statement
| search_statement
| set_statement
| sort_statement
| start_statement
| stop_statement
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
  ACCEPT data_name at_line_column
  {
    push_location ($1);
    if (current_program->enable_screen)
      {
	if (COBC_FIELD ($2)->f.screen)
	  {
	    cobc_tree line = COBC_PARAMETER ($3)->x;
	    cobc_tree column = COBC_PARAMETER ($3)->y;
	    push_funcall_3 ("cob_screen_accept", $2, line, column);
	  }
	else
	  yyerror_x ($2, "`%s' not defined in SCREEN SECTION", tree_name ($2));
      }
    else
      {
	push_funcall_2 ("cob_accept", $2, make_integer (COB_SYSIN));
      }
  }
  _end_accept
| ACCEPT data_name FROM DATE
  {
    push_location ($1);
    push_funcall_1 ("cob_accept_date", $2);
  }
| ACCEPT data_name FROM DAY
  {
    push_location ($1);
    push_funcall_1 ("cob_accept_day", $2);
  }
| ACCEPT data_name FROM DAY_OF_WEEK
  {
    push_location ($1);
    push_funcall_1 ("cob_accept_day_of_week", $2);
  }
| ACCEPT data_name FROM TIME
  {
    push_location ($1);
    push_funcall_1 ("cob_accept_time", $2);
  }
| ACCEPT data_name FROM COMMAND_LINE
  {
    push_location ($1);
    push_funcall_1 ("cob_accept_command_line", $2);
  }
| ACCEPT data_name FROM ENVIRONMENT_VARIABLE value
  {
    push_location ($1);
    push_funcall_2 ("cob_accept_environment", $2, $5);
  }
| ACCEPT data_name FROM mnemonic_name
  {
    push_location ($1);
    switch (COBC_BUILTIN (cobc_ref ($4))->id)
      {
      case BUILTIN_CONSOLE:
      case BUILTIN_SYSIN:
	push_funcall_2 ("cob_accept", $2, make_integer (COB_SYSIN));
	break;
      default:
	yyerror_x ($4, _("invalid input stream `%s'"), tree_name ($4));
	break;
      }
  }
;
_end_accept: | END_ACCEPT ;

at_line_column:
  /* empty */			{ $$ = make_pair (cobc_int1, cobc_int1); }
| _at line_number column_number { $$ = make_pair ($2, $3); }
| _at column_number line_number { $$ = make_pair ($3, $2); }
;
line_number:
  LINE _number integer_value	{ $$ = make_cast_int32 ($3); }
;
column_number:
  COLUMN _number integer_value	{ $$ = make_cast_int32 ($3); }
;


/*
 * ADD statement
 */

add_statement:
  ADD add_body on_size_error _end_add
  {
    push_location ($1);
    push_sequence_with_handler ($<tree>2, $3);
  }
;
add_body:
  numeric_value_list TO math_name_list
  {
    /* ADD A B C TO X Y  -->  t = a + b + c; x += t; y += t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_binary_op (e, '+', l->item);
    $<tree>$ = build_assign ($3, '+', e);
  }
| numeric_value_list add_to GIVING math_edited_name_list
  {
    /* ADD A B TO C GIVING X Y  -->  t = a + b + c; x = t; y = t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_binary_op (e, '+', l->item);
    if ($2)
      e = make_binary_op (e, '+', $2);
    $<tree>$ = build_assign ($4, 0, e);
  }
| CORRESPONDING group_name _to group_name flag_rounded
  {
    $<tree>$ = build_corresponding (build_add, $4, $2, $5);
  }
;
add_to:
  /* empty */			{ $$ = NULL; }
| TO value			{ $$ = $2; }
;
_end_add: | END_ADD ;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options		{  OBSOLETE ("ALTER"); }
;
alter_options:
| alter_options
  label TO _proceed_to label
;
_proceed_to: | PROCEED TO ;


/*
 * CALL statement
 */

call_statement:
  CALL program_name		{ current_call_mode = COBC_CALL_BY_REFERENCE; }
  call_using call_returning call_on_exception call_not_on_exception _end_call
  {
    push_location ($1);
    push_funcall_4 ("@call", $2, $4, $6, $7);
    if ($5)
      push (build_move (cobc_return_code, $5));
  }
;
call_using:
  /* empty */			{ $$ = NULL; }
| USING call_param_list		{ $$ = $2; }
;
call_param_list:
  call_param			{ $$ = list ($1); }
| call_param_list
  call_param			{ $$ = list_add ($1, $2); }
;
call_param:
  value				{ $$ = make_parameter_1 (current_call_mode, $1);}
| _by call_mode value		{ $$ = make_parameter_1 (current_call_mode, $3);}
;
call_mode:
  REFERENCE			{ current_call_mode = COBC_CALL_BY_REFERENCE; }
| CONTENT			{ current_call_mode = COBC_CALL_BY_CONTENT; }
| CONTENT LENGTH _of		{ current_call_mode = COBC_CALL_BY_LENGTH; }
| VALUE				{ current_call_mode = COBC_CALL_BY_VALUE; }
;
call_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING data_name		{ $$ = $2; }
;
call_on_exception:
  /* empty */				{ $$ = NULL; }
| _on OVERFLOW statement_list		{ $$ = $3; }
| _on EXCEPTION statement_list		{ $$ = $3; }
;
call_not_on_exception:
  /* empty */				{ $$ = NULL; }
| NOT _on EXCEPTION statement_list	{ $$ = $4; }
;
_end_call: | END_CALL ;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL
  {
    push_location ($1);
  }
  cancel_list
;
cancel_list:
| cancel_list program_name
  {
    push_funcall_1 ("cob_cancel", $2);
  }
;
program_name:
  data_name
| NONNUMERIC_LITERAL
;


/*
 * CLOSE statement
 */

close_statement:
  CLOSE
  {
    push_location ($1);
  }
  close_list
;
close_list:
| close_list file_name close_option
  {
    cobc_tree file = cobc_ref ($2);
    push_funcall_2 ("cob_close", file, make_integer ($<inum>3));
    push_file_handler (file, make_handler (0, 0, 0));
  }
;
close_option:
  /* empty */			{ $<inum>$ = COB_CLOSE_NORMAL; }
| REEL				{ $<inum>$ = COB_CLOSE_REEL; }
| REEL _for REMOVAL		{ $<inum>$ = COB_CLOSE_REEL_REMOVAL; }
| UNIT				{ $<inum>$ = COB_CLOSE_UNIT; }
| UNIT _for REMOVAL		{ $<inum>$ = COB_CLOSE_UNIT_REMOVAL; }
| _with NO REWIND		{ $<inum>$ = COB_CLOSE_NO_REWIND; }
| _with LOCK			{ $<inum>$ = COB_CLOSE_LOCK; }
;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE compute_body on_size_error _end_compute
  {
    push_location ($1);
    push_sequence_with_handler ($<tree>2, $3);
  }
;
compute_body:
  math_edited_name_list '=' numeric_expr
  {
    $<tree>$ = build_assign ($1, 0, $3);
  }
;
_end_compute: | END_COMPUTE ;


/*
 * DELETE statement
 */

delete_statement:
  DELETE file_name _record opt_invalid_key _end_delete
  {
    cobc_tree file = cobc_ref ($2);
    push_location ($1);
    push_funcall_1 ("cob_delete", file);
    push_file_handler (file, $4);
  }
;
_end_delete: | END_DELETE ;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY opt_value_list display_upon at_line_column
  {
    struct cobc_list *l;
    push_location ($1);
    if (current_program->enable_screen)
      {
	for (l = $2; l; l = l->next)
	  if (COBC_FIELD (l->item)->f.screen)
	    {
	      cobc_tree line = COBC_PARAMETER ($4)->x;
	      cobc_tree column = COBC_PARAMETER ($4)->y;
	      push_funcall_3 ("cob_screen_display", l->item, line, column);
	    }
	  else
	    yyerror_x (l->item, "`%s' not defined in SCREEN SECTION",
			 tree_name (l->item));
      }
    else
      {
	cobc_tree fd = make_integer ($3);
	for (l = $2; l; l = l->next)
	  push_funcall_2 ("cob_display", l->item, fd);
      }
  }
  display_with_no_advancing
  _end_display
  ;
display_upon:
  /* empty */			{ $$ = COB_SYSOUT; }
| _upon mnemonic_name
  {
    switch (COBC_BUILTIN (cobc_ref ($2))->id)
      {
      case BUILTIN_CONSOLE: $$ = COB_SYSOUT; break;
      case BUILTIN_SYSOUT:  $$ = COB_SYSOUT; break;
      case BUILTIN_SYSERR:  $$ = COB_SYSERR; break;
      default:
	yyerror_x ($2, _("invalid UPON item"));
	$$ = COB_SYSOUT;
	break;
      }
  }
| UPON NAME
  {
    yywarn_x ($2, _("`%s' undefined in SPECIAL-NAMES"), COBC_NAME ($2));
    $$ = COB_SYSOUT;
  }
;
display_with_no_advancing:
  /* empty */
  {
    if (!current_program->enable_screen)
      push_funcall_1 ("cob_newline", make_integer ($<inum>-2));
  }
| _with NO ADVANCING { /* nothing */ }
;
_end_display: | END_DISPLAY ;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE divide_body on_size_error _end_divide
  {
    push_location ($1);
    push_sequence_with_handler ($<tree>2, $3);
  }
;
divide_body:
  numeric_value INTO math_name_list
  {
    $<tree>$ = build_assign ($3, '/', $1);
  }
| numeric_value INTO numeric_value GIVING math_edited_name_list
  {
    $<tree>$ = build_assign ($5, 0, make_binary_op ($3, '/', $1));
  }
| numeric_value BY numeric_value GIVING math_edited_name_list
  {
    $<tree>$ = build_assign ($5, 0, make_binary_op ($1, '/', $3));
  }
| numeric_value INTO numeric_value GIVING numeric_edited_name flag_rounded
  REMAINDER numeric_edited_name
  {
    $<tree>$ = build_divide ($3, $1, $5, $6, $8);
  }
| numeric_value BY numeric_value GIVING numeric_edited_name flag_rounded
  REMAINDER numeric_edited_name
  {
    $<tree>$ = build_divide ($1, $3, $5, $6, $8);
  }
;
_end_divide: | END_DIVIDE ;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE evaluate_subject_list evaluate_case_list _end_evaluate
  {
    push_location ($1);
    push (build_evaluate ($<list>2, $3));
  }
;

evaluate_subject_list:
  evaluate_subject		{ $<list>$ = list ($<tree>1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $<list>$ = list_add ($<list>1, $<tree>3); }
;
evaluate_subject:
  expr				{ $<tree>$ = $1; }
| TRUE				{ $<tree>$ = cobc_true; }
| FALSE				{ $<tree>$ = cobc_false; }
;

evaluate_case_list:
  /* empty */			{ $$ = NULL; }
| evaluate_case_list
  evaluate_case			{ $$ = list_add ($1, $2); }
;
evaluate_case:
  evaluate_when_list
  statement_list		{ $$ = cons ($2, $1); }
| WHEN OTHER
  statement_list		{ $$ = cons ($3, NULL); }
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
  flag_not expr			{ $$ = make_parameter ($1, $2, 0); }
| flag_not expr THRU expr	{ $$ = make_parameter ($1, $2, $4); }
| ANY				{ $$ = make_parameter (0, cobc_any, 0); }
| TRUE				{ $$ = make_parameter (0, cobc_true, 0); }
| FALSE				{ $$ = make_parameter (0, cobc_false, 0); }
;
_end_evaluate: | END_EVALUATE ;


/*
 * EXIT statement
 */

exit_statement:
  EXIT				{ /* nothing */ }
| EXIT PROGRAM
  {
    push_funcall_0 ("@exit-program");
  }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to label_list
  {
    if ($3 == NULL)
      OBSOLETE ("GO TO without label");
    else if ($3->next)
      yyerror_x ($3->next->item, _("too many labels with GO TO"));
    else
      push_funcall_1 ("@goto", $3->item);
  }
| GO _to label_list DEPENDING _on numeric_name
  {
    push_funcall_2 ("@goto-depending", $3, $6);
  }
;


/*
 * IF statement
 */

if_statement:
  IF condition _then statement_list _end_if
  {
    push_location ($1);
    push (make_if ($2, $4, NULL));
  }
| IF condition _then statement_list ELSE statement_list _end_if
  {
    push_location ($1);
    push (make_if ($2, $4, $6));
  }
| IF error END_IF { }
;
_end_if: | END_IF ;


/*
 * INITIALIZE statement
 */

initialize_statement:
  INITIALIZE data_name_list initialize_replacing
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $2; l; l = l->next)
      push_funcall_2 ("@initialize", l->item, $3);
  }
;
initialize_replacing:
  /* empty */			      { $$ = NULL; }
| REPLACING initialize_replacing_list { $$ = $2; }
;
initialize_replacing_list:
  /* empty */			      { $$ = NULL; }
| initialize_replacing_list
  replacing_option _data BY value
  {
    $$ = list_add ($1, make_parameter_1 ($2, $5));
  }
;
replacing_option:
  ALPHABETIC			{ $$ = COB_TYPE_ALPHABETIC; }
| ALPHANUMERIC			{ $$ = COB_TYPE_ALPHANUMERIC; }
| NUMERIC			{ $$ = COB_TYPE_NUMERIC; }
| ALPHANUMERIC_EDITED		{ $$ = COB_TYPE_ALPHANUMERIC_EDITED; }
| NUMERIC_EDITED		{ $$ = COB_TYPE_NUMERIC_EDITED; }
| NATIONAL			{ $$ = COB_TYPE_NATIONAL; }
| NATIONAL_EDITED		{ $$ = COB_TYPE_NATIONAL_EDITED; }
;
_data: | DATA ;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT
  {
    push_location ($1);
  }
  data_name inspect_list
;
inspect_list:
| inspect_list inspect_item
;
inspect_item:
  inspect_tallying
  {
    struct cobc_list *l = $<list>1;
    l = cons (make_funcall_2 ("cob_inspect_init", $<tree>-1, cobc_int0), l);
    l = list_add (l, make_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
| inspect_replacing
  {
    struct cobc_list *l = $<list>1;
    l = cons (make_funcall_2 ("cob_inspect_init", $<tree>-1, cobc_int1), l);
    l = list_add (l, make_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
| inspect_converting
  {
    struct cobc_list *l = $<list>1;
    l = cons (make_funcall_2 ("cob_inspect_init", $<tree>-1, cobc_int0), l);
    l = list_add (l, make_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING
  {
    current_inspect_func = NULL;
    current_inspect_data = NULL;
  }
  tallying_list
  {
    $<list>$ = $<list>3;
  }
;
tallying_list:
  tallying_item			{ $<list>$ = $<list>1; }
| tallying_list tallying_item	{ $<list>$ = list_append ($<list>1, $<list>2); }
;
tallying_item:
  data_name FOR
  {
    current_inspect_data = $1;
    $<list>$ = NULL;
  }
| CHARACTERS inspect_before_after_list
  {
    if (current_inspect_data == NULL)
      yyerror (_("data name expected before CHARACTERS"));
    current_inspect_func = NULL;
    $<list>$ = list_add ($2, make_funcall_1 ("cob_inspect_characters", current_inspect_data));
  }
| ALL
  {
    if (current_inspect_data == NULL)
      yyerror (_("data name expected before ALL"));
    current_inspect_func = "cob_inspect_all";
    $<list>$ = NULL;
  }
| LEADING
  {
    if (current_inspect_data == NULL)
      yyerror (_("data name expected before LEADING"));
    current_inspect_func = "cob_inspect_leading";
    $<list>$ = NULL;
  }
| text_value inspect_before_after_list
  {
    if (current_inspect_func == NULL)
      yyerror_x ($1, _("ALL or LEADING expected before `%s'"), tree_name ($1));
    $<list>$ = list_add ($2, make_funcall_2 (current_inspect_func, current_inspect_data, $1));
  }
;

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_item
  {
    $<list>$ = $<list>2;
  }
| inspect_replacing replacing_item
  {
    $<list>$ = list_append ($<list>1, $<list>2);
  }
;
replacing_item:
  CHARACTERS BY value inspect_before_after_list
  {
    $<list>$ = list_add ($4, make_funcall_1 ("cob_inspect_characters", $3));
  }
| ALL value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, make_funcall_2 ("cob_inspect_all", $4, $2));
  }
| LEADING value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, make_funcall_2 ("cob_inspect_leading", $4, $2));
  }
| FIRST value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, make_funcall_2 ("cob_inspect_first", $4, $2));
  }
;

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING value TO value inspect_before_after_list
  {
    $<list>$ = list_add ($5, make_funcall_2 ("cob_inspect_converting", $2, $4));
  }
;

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* empty */
  {
    $$ = list (make_funcall_0 ("cob_inspect_start"));
  }
| inspect_before_after_list before_or_after _initial value
  {
    if ($2 == COBC_BEFORE)
      $$ = list_add ($1, make_funcall_1 ("cob_inspect_before", $4));
    else
      $$ = list_add ($1, make_funcall_1 ("cob_inspect_after", $4));
  }
;
_initial: | TOK_INITIAL ;


/*
 * MERGE statement
 */

merge_statement:
  MERGE file_name sort_key_list sort_collating
  {
    push_location ($1);
    push_funcall_2 ("@sort-init", $2, $<list>3);
    $<tree>$ = $2; /* used in sort_input, sort_output */
  }
  sort_input sort_output
;


/*
 * MOVE statement
 */

move_statement:
  MOVE value TO data_name_list
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $4; l; l = l->next)
      push (build_move ($2, l->item));
  }
| MOVE CORRESPONDING group_name TO group_name
  {
    push_location ($1);
    push (build_corresponding (build_move, $3, $5, -1));
  }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY multiply_body on_size_error _end_multiply
  {
    push_location ($1);
    push_sequence_with_handler ($<tree>2, $3);
  }
;
multiply_body:
  numeric_value BY math_name_list
  {
    $<tree>$ = build_assign ($3, '*', $1);
  }
| numeric_value BY numeric_value GIVING math_edited_name_list
  {
    $<tree>$ = build_assign ($5, 0, make_binary_op ($1, '*', $3));
  }
;
_end_multiply: | END_MULTIPLY ;


/*
 * OPEN statement
 */

open_statement:
  OPEN
  {
    push_location ($1);
  }
  open_list
;
open_list:
| open_list open_mode file_name_list
  {
    struct cobc_list *l;
    for (l = $3; l; l = l->next)
      {
	cobc_tree file = cobc_ref (l->item);
	push_funcall_2 ("cob_open", file, make_integer ($<inum>2));
	push_file_handler (file, make_handler (0, 0, 0));
      }
  }
;
open_mode:
  INPUT				{ $<inum>$ = COB_OPEN_INPUT; }
| OUTPUT			{ $<inum>$ = COB_OPEN_OUTPUT; }
| I_O				{ $<inum>$ = COB_OPEN_I_O; }
| EXTEND			{ $<inum>$ = COB_OPEN_EXTEND; }
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM perform_procedure perform_option
  {
    push_location ($1);
    COBC_PERFORM ($3)->body = $2;
    push ($3);
  }
| PERFORM perform_option statement_list END_PERFORM
  {
    push_location ($1);
    COBC_PERFORM ($2)->body = $3;
    push ($2);
  }
;

perform_procedure:
  label
  {
    COBC_REFERENCE ($1)->length = cobc_true; /* return from $1 */
    $$ = make_pair ($1, $1);
  }
| label THRU label
  {
    COBC_REFERENCE ($3)->length = cobc_true; /* return from $3 */
    $$ = make_pair ($1, $3);
  }
;

perform_option:
  /* empty */
  {
    $$ = make_perform (COBC_PERFORM_ONCE);
  }
| integer_value TIMES
  {
    $$ = make_perform (COBC_PERFORM_TIMES);
    COBC_PERFORM ($$)->data = $1;
    current_program->loop_counter++;
  }
| perform_test UNTIL condition
  {
    $$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($$)->test = $1;
    add_perform_varying (COBC_PERFORM ($$), 0, 0, 0, $3);
  }
| perform_test VARYING
  {
    $<tree>$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($<tree>$)->test = $1;
  }
  perform_varying_list
  {
    $$ = $<tree>3;
  }
;
perform_test:
  /* empty */			{ $$ = COBC_BEFORE; }
| _with TEST before_or_after	{ $$ = $3; }
;
perform_varying_list:
  perform_varying
| perform_varying_list AFTER { $<tree>$ = $<tree>0; }
  perform_varying
;
perform_varying:
  numeric_name FROM value BY value UNTIL condition
  {
    cobc_tree step = build_add ($1, $5, 0);
    add_perform_varying (COBC_PERFORM ($<tree>0), $1, $3, step, $7);
  }
;


/*
 * READ statements
 */

read_statement:
  READ file_name flag_next _record read_into read_key read_handler _end_read
  {
    cobc_tree file = cobc_ref ($2);
    cobc_tree key = $<tree>6;
    push_location ($1);
    if ($3 || COBC_FILE (file)->access_mode == COB_ACCESS_SEQUENTIAL)
      {
	/* READ NEXT */
	if (key)
	  yywarn (_("KEY ignored with sequential READ"));
	push_funcall_2 ("cob_read", file, cobc_int0);
      }
    else
      {
	/* READ */
	push_funcall_2 ("cob_read", file, key ? key : COBC_FILE (file)->key);
      }
    if ($<tree>5)
      push (build_move (COBC_TREE (COBC_FILE (file)->record), $<tree>5));
    push_file_handler (file, $<tree>7);
  }
;
read_into:
  /* empty */			{ $<tree>$ = NULL; }
| INTO data_name		{ $<tree>$ = $2; }
;
read_key:
  /* empty */			{ $<tree>$ = NULL; }
| KEY _is data_name		{ $<tree>$ = $3; }
;
read_handler:
  /* empty */			{ $<tree>$ = make_handler (0, 0, 0); }
| at_end			{ $<tree>$ = $1; }
| invalid_key			{ $<tree>$ = $1; }
;
_end_read: | END_READ ;


/*
 * RELEASE statement
 */

release_statement:
  RELEASE record_name write_from
  {
    cobc_tree file = COBC_TREE (COBC_FIELD (cobc_ref ($2))->file);
    push_location ($1);
    if ($3)
      push (build_move ($3, $2));
    push_funcall_2 ("cob_write", file, $2);
  }
;


/*
 * RETURN statement
 */

return_statement:
  RETURN file_name _record read_into at_end _end_return
  {
    cobc_tree file = cobc_ref ($2);
    push_location ($1);
    push_funcall_2 ("cob_read", file, cobc_int0);
    if ($<tree>4)
      push (build_move (COBC_TREE (COBC_FILE (file)->record), $<tree>4));
    push_file_handler (file, $5);
  }
;
_end_return: | END_RETURN ;


/*
 * REWRITE statement
 */

rewrite_statement:
  REWRITE record_name write_from opt_invalid_key _end_rewrite
  {
    cobc_tree file = COBC_TREE (COBC_FIELD (cobc_ref ($2))->file);
    push_location ($1);
    if ($3)
      push (build_move ($3, $2));
    push_funcall_2 ("cob_rewrite", file, $2);
    push_file_handler (file, $4);
  }
;
_end_rewrite: | END_REWRITE ;


/*
 * SEARCH statement
 */

search_statement:
  SEARCH table_name search_varying search_at_end search_whens _end_search
  {
    push_location ($1);
    push_funcall_4 ("@search", $2, $<tree>3, $<tree>4, $<tree>5);
  }
| SEARCH ALL table_name search_at_end search_all_when _end_search
  {
    push_location ($1);
    push_funcall_3 ("@search-all", $3, $<tree>4, $<tree>5);
  }
;
search_varying:
  /* empty */			{ $<tree>$ = NULL; }
| VARYING data_name		{ $<tree>$ = $2; }
;
search_at_end:
  /* empty */			{ $<tree>$ = NULL; }
| _at END statement_list	{ $<tree>$ = $3; }
;
search_whens:
  search_when			{ $<tree>$ = $<tree>1; }
| search_when search_whens
  {
    $<tree>$ = $<tree>1;
    COBC_IF ($<tree>1)->stmt2 = $<tree>2;
  }
;
search_when:
  WHEN condition statement_list	{ $<tree>$ = make_if ($2, $3, 0); }
;
search_all_when:
  WHEN expr statement_list
  {
    $<tree>$ = make_if (build_search_all ($<tree>-1, $2), $3, 0);
  }
;
_end_search: | END_SEARCH ;


/*
 * SET statement
 */

set_statement:
  SET data_name_list TO numeric_value
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $2; l; l = l->next)
      push (build_move ($4, l->item));
  }
| SET data_name_list UP BY numeric_value
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $2; l; l = l->next)
      push (build_add (l->item, $5, 0));
  }
| SET data_name_list DOWN BY numeric_value
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $2; l; l = l->next)
      push (build_sub (l->item, $5, 0));
  }
| SET data_name_list TO TRUE
  {
    struct cobc_list *l;
    push_location ($1);
    for (l = $2; l; l = l->next)
      {
	struct cobc_field *f = field (l->item);
	set_value (l->item, COBC_TREE (f->parent));
	if (COBC_PARAMETER_P (f->values->item))
	  push (build_move (COBC_PARAMETER (f->values->item)->x, l->item));
	else
	  push (build_move (f->values->item, l->item));
      }
  }
| SET
  {
    push_location ($1);
  }
  set_on_off_list
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
	int id = builtin_switch_id (cobc_ref (l->item));
	if (id != -1)
	  push (build_move ($3, cobc_switch[id]));
      }
  }
;


/*
 * SORT statement
 */

sort_statement:
  SORT file_name sort_key_list sort_duplicates sort_collating
  {
    push_location ($1);
    push_funcall_2 ("@sort-init", $2, $<list>3);
    $<tree>$ = $2; /* used in sort_input, sort_output */
  }
  sort_input sort_output
;
sort_key_list:
  /* empty */			{ $<list>$ = NULL; }
| sort_key_list
  _on ascending_or_descending _key data_name_list
  {
    struct cobc_list *l;
    for (l = $5; l; l = l->next)
      l->item = make_parameter_1 ($3, l->item);
    $<list>$ = list_append ($<list>1, $5);
  }
;
sort_duplicates:
| _with DUPLICATES _in _order	{ IGNORE ("DUPLICATES"); }
;
sort_collating:
| collating_sequence		{ PENDING ("COLLATING SEQUENCE"); }
;

sort_input:
  USING file_name_list
  {
    struct cobc_list *l;
    push_funcall_2 ("cob_open", $<tree>0, make_integer (COB_OPEN_OUTPUT));
    for (l = $2; l; l = l->next)
      push_funcall_2 ("cob_sort_using", $<tree>0, l->item);
    push_funcall_2 ("cob_close", $<tree>0, make_integer (COB_CLOSE_NORMAL));
  }
| INPUT PROCEDURE _is perform_procedure
  {
    push_funcall_2 ("cob_open", $<tree>0, make_integer (COB_OPEN_OUTPUT));
    push (make_perform_once ($4));
    push_funcall_2 ("cob_close", $<tree>0, make_integer (COB_CLOSE_NORMAL));
  }
;

sort_output:
  GIVING file_name_list
  {
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      {
	push_funcall_2 ("cob_open", $<tree>-1, make_integer (COB_OPEN_INPUT));
	push_funcall_2 ("cob_sort_giving", $<tree>-1, l->item);
	push_funcall_2 ("cob_close", $<tree>-1, make_integer (COB_CLOSE_NORMAL));
      }
  }
| OUTPUT PROCEDURE _is perform_procedure
  {
    push_funcall_2 ("cob_open", $<tree>-1, make_integer (COB_OPEN_INPUT));
    push (make_perform_once ($4));
    push_funcall_2 ("cob_close", $<tree>-1, make_integer (COB_CLOSE_NORMAL));
  }
;


/*
 * START statement
 */

start_statement:
  START file_name		{ $<inum>$ = COB_EQ; }
  start_key opt_invalid_key _end_start
  {
    cobc_tree file = cobc_ref ($2);
    if ($<tree>4 == NULL)
      $<tree>4 = COBC_FILE (file)->key;
    push_location ($1);
    push_funcall_3 ("cob_start", file, make_integer ($<inum>3), $<tree>4);
    push_file_handler (file, $5);
  }
;
start_key:
  /* empty */			{ $<tree>$ = NULL; }
| KEY _is start_op data_name	{ $<inum>0 = $<inum>3; $<tree>$ = $4; }
;
start_op:
  flag_not equal		{ $<inum>$ = $1 ? COB_NE : COB_EQ; }
| flag_not greater		{ $<inum>$ = $1 ? COB_LE : COB_GT; }
| flag_not less			{ $<inum>$ = $1 ? COB_GE : COB_LT; }
| flag_not greater_or_equal	{ $<inum>$ = $1 ? COB_LT : COB_GE; }
| flag_not less_or_equal	{ $<inum>$ = $1 ? COB_GT : COB_LE; }
;
_end_start: | END_START ;


/*
 * STOP statement
 */

stop_statement:
  STOP RUN
  {
    push_funcall_0 ("cob_stop_run");
  }
| STOP program_name
  {
    OBSOLETE ("STOP literal");
  }
;


/*
 * STRING statement
 */

string_statement:
  STRING string_list INTO data_name opt_with_pointer on_overflow _end_string
  {
    struct cobc_list *l = $2;
    l = cons (make_funcall_2 ("cob_string_init", $4, $5), l);
    l = list_add (l, make_funcall_0 ("cob_string_finish"));
    push_location ($1);
    push_sequence_with_handler (make_sequence (l), $6);
  }
;
string_list:
  string_list_1			{ $$ = $1; }
| string_list string_list_1	{ $$ = list_append ($1, $2); }
;
string_list_1:
  value_list string_delimited
  {
    struct cobc_list *l;
    for (l = $1; l; l = l->next)
      l->item = make_funcall_2 ("cob_string_append", l->item, $<tree>2);
    $$ = $1;
  }
;
string_delimited:
  /* empty */			{ $<tree>$ = cobc_int0; }
| DELIMITED _by SIZE		{ $<tree>$ = cobc_int0; }
| DELIMITED _by value		{ $<tree>$ = $3; }
;

opt_with_pointer:
  /* empty */			{ $$ = cobc_int0; }
| _with POINTER data_name	{ $$ = $3; }
;

_end_string: | END_STRING ;


/*
 * SUBTRACT statement
 */

subtract_statement:
  SUBTRACT subtract_body on_size_error _end_subtract
  {
    push_location ($1);
    push_sequence_with_handler ($<tree>2, $3);
  }
;
subtract_body:
  numeric_value_list FROM math_name_list
  {
    /* SUBTRACT A B C FROM X Y  -->  t = a + b + c; x -= t; y -= t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    for (l = $1->next; l; l = l->next)
      e = make_binary_op (e, '+', l->item);
    $<tree>$ = build_assign ($3, '-', e);
  }
| numeric_value_list FROM numeric_value GIVING math_edited_name_list
  {
    /* SUBTRACT A B FROM C GIVING X Y  -->  t = c - a - b; x = t; y = t */
    struct cobc_list *l;
    cobc_tree e = $3;
    for (l = $1; l; l = l->next)
      e = make_binary_op (e, '-', l->item);
    $<tree>$ = build_assign ($5, 0, e);
  }
| CORRESPONDING group_name FROM group_name flag_rounded
  {
    $<tree>$ = build_corresponding (build_sub, $4, $2, $5);
  }
;
_end_subtract: | END_SUBTRACT ;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING data_name unstring_delimited INTO unstring_into
  opt_with_pointer unstring_tallying on_overflow _end_unstring
  {
    struct cobc_list *l = $<list>3;
    l = cons (make_funcall_2 ("cob_unstring_init", $2, $6), l);
    l = list_append (l, $<list>5);
    if ($<tree>7)
      l = list_add (l, make_funcall_1 ("cob_unstring_tallying", $<tree>7));
    l = list_add (l, make_funcall_0 ("cob_unstring_finish"));
    push_location ($1);
    push_sequence_with_handler (make_sequence (l), $8);
  }
;

unstring_delimited:
  /* empty */			{ $<list>$ = NULL; }
| DELIMITED _by
  unstring_delimited_list	{ $<list>$ = $<list>3; }
;
unstring_delimited_list:
  unstring_delimited_item	{ $<list>$ = list ($<tree>1); }
| unstring_delimited_list OR
  unstring_delimited_item	{ $<list>$ = list_add ($<list>1, $<tree>3); }
;
unstring_delimited_item:
  flag_all value
  {
    cobc_tree flag = $1 ? cobc_int1 : cobc_int0;
    $<tree>$ = make_funcall_2 ("cob_unstring_delimited", $2, flag);
  }
;

unstring_into:
  unstring_into_item		{ $<list>$ = list ($<tree>1); }
| unstring_into
  unstring_into_item		{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
unstring_into_item:
  data_name unstring_delimiter unstring_count
  {
    $<tree>$ = make_funcall_3 ("cob_unstring_into", $1, $<tree>2, $<tree>3);
  }
;
unstring_delimiter:
  /* empty */			{ $<tree>$ = cobc_int0; }
| DELIMITER _in data_name	{ $<tree>$ = $3; }
;
unstring_count:
  /* empty */			{ $<tree>$ = cobc_int0; }
| COUNT _in data_name		{ $<tree>$ = $3; }
;

unstring_tallying:
  /* empty */			{ $<tree>$ = NULL; }
| TALLYING _in data_name	{ $<tree>$ = $3; }
;

_end_unstring: | END_UNSTRING ;


/*
 * WRITE statement
 */

write_statement:
  WRITE record_name write_from write_option opt_invalid_key _end_write
  {
    struct cobc_field *f = COBC_FIELD (cobc_ref ($2));
    struct cobc_parameter *p = $<tree>4 ? COBC_PARAMETER ($<tree>4) : 0;
    cobc_tree file = COBC_TREE (f->file);
    push_location ($1);

    /* AFTER ADVANCING */
    if (p && p->type == COBC_AFTER)
      {
	if (p->x)
	  push_funcall_2 ("cob_write_lines", file, p->x);
	else
	  push_funcall_1 ("cob_write_page", file);
      }

    /* WRITE */
    if ($3)
      push (build_move ($3, $2));
    push_funcall_2 ("cob_write", file, $2);
    push_file_handler (file, $5);

    /* BEFORE ADVANCING */
    if (p && p->type == COBC_BEFORE)
      {
	if (p->x)
	  push_funcall_2 ("cob_write_lines", file, p->x);
	else
	  push_funcall_1 ("cob_write_page", file);
      }
  }
;
write_from:
  /* empty */			{ $$ = NULL; }
| FROM value			{ $$ = $2; }
;
write_option:
  /* empty */
  {
    $<tree>$ = NULL;
  }
| before_or_after _advancing integer_value _line_or_lines
  {
    $<tree>$ = make_parameter_1 ($1, make_cast_int32 ($3));
  }
| before_or_after _advancing PAGE
  {
    $<tree>$ = make_parameter_1 ($1, 0);
  }
;
before_or_after:
  BEFORE			{ $$ = COBC_BEFORE; }
| AFTER				{ $$ = COBC_AFTER; }
;
_line_or_lines: | LINE | LINES ;
_advancing: | ADVANCING ;
_end_write: | END_WRITE ;


/*******************
 * Status handlers
 *******************/

/*
 * ON SIZE ERROR
 */

on_size_error:
  opt_on_size_error
  opt_not_on_size_error
  {
    if ($<tree>1 || $<tree>2)
      $$ = make_handler (COB_EC_SIZE, $<tree>1, $<tree>2);
    else
      $$ = NULL;
  }
;
opt_on_size_error:
  /* empty */				{ $<tree>$ = NULL; }
| _on SIZE ERROR statement_list		{ $<tree>$ = $4; }
;
opt_not_on_size_error:
  /* empty */				{ $<tree>$ = NULL; }
| NOT _on SIZE ERROR statement_list	{ $<tree>$ = $5; }
;


/*
 * ON OVERFLOW
 */

on_overflow:
  opt_on_overflow
  opt_not_on_overflow
  {
    if ($<tree>1 || $<tree>2)
      $$ = make_handler (COB_EC_OVERFLOW, $<tree>1, $<tree>2);
    else
      $$ = NULL;
  }
;
opt_on_overflow:
  /* empty */				{ $<tree>$ = NULL; }
| _on OVERFLOW statement_list		{ $<tree>$ = $3; }
;
opt_not_on_overflow:
  /* empty */				{ $<tree>$ = NULL; }
| NOT _on OVERFLOW statement_list	{ $<tree>$ = $4; }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, $<tree>1, 0);
  }
| not_at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, 0, $<tree>1);
  }
| at_end_sentence not_at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, $<tree>1, $<tree>2);
  }
;
at_end_sentence:
  END statement_list		{ $<tree>$ = $2; }
| AT END statement_list		{ $<tree>$ = $3; }
;
not_at_end_sentence:
  NOT _at END statement_list	{ $<tree>$ = $4; }
;


/*
 * INVALID KEY
 */

opt_invalid_key:
  /* empty */
  {
    $$ = make_handler (0, 0, 0);
  }
| invalid_key
;
invalid_key:
  invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, $<tree>1, 0);
  }
| not_invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, 0, $<tree>1);
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, $<tree>1, $<tree>2);
  }
;
invalid_key_sentence:
  INVALID _key statement_list		{ $<tree>$ = $3; }
;
not_invalid_key_sentence:
  NOT INVALID _key statement_list	{ $<tree>$ = $4; }
;


/*******************
 * Expressions
 *******************/

condition:
  expr
  {
    $$ = build_cond ($1);
  }
;

numeric_expr:
  expr
  {
    if (COBC_TREE_CLASS ($1) != COB_TYPE_NUMERIC)
      {
	yyerror_x ($1, _("invalid expression `%s'"), tree_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

expr:
  {
    last_operator = 0;
    last_lefthand = NULL;
  }
  expr_1			{ $$ = $2; }
;
expr_1:
  expr_item_list
  {
    int i;
    char *class_func = NULL;
    struct cobc_list *l;
    struct stack_item {
      int prio;
      int token;
      cobc_tree value;
    } stack[list_length ($1)];

    static int reduce (int prio)
      {
	while (i >= 2 && stack[i-2].token != VALUE && stack[i-2].prio <= prio)
	  {
	    int token = stack[i-2].token;
	    if (stack[i-1].token != VALUE
		&& stack[i-1].token != '&'
		&& stack[i-1].token != '|')
	      return -1;
	    switch (token)
	      {
	      case '+': case '-': case '*': case '/': case '^':
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  make_binary_op (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      case '!':
		if (COBC_TREE_CLASS (stack[i-1].value) != COB_TYPE_BOOLEAN)
		  stack[i-1].value =
		    make_binary_op (last_lefthand, last_operator, stack[i-1].value);
		stack[i-2].token = VALUE;
		stack[i-2].value = make_negative (stack[i-1].value);
		i -= 1;
		break;
	      case '&':
	      case '|':
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		if (COBC_TREE_CLASS (stack[i-1].value) != COB_TYPE_BOOLEAN)
		  stack[i-1].value =
		    make_binary_op (last_lefthand, last_operator, stack[i-1].value);
		if (token == '|'
		    && ((COBC_BINARY_OP_P (stack[i-3].value)
			 && COBC_BINARY_OP (stack[i-3].value)->op == '&')
			|| (COBC_BINARY_OP_P (stack[i-1].value)
			    && COBC_BINARY_OP (stack[i-1].value)->op == '&')))
		  yywarn ("suggest parentheses around AND within OR");
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  make_binary_op (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      default:
		if (stack[i-3].token == '&' || stack[i-3].token == '|')
		  {
		    last_operator = token;
		    stack[i-2].token = VALUE;
		    stack[i-2].value =
		      make_binary_op (last_lefthand, token, stack[i-1].value);
		    i -= 1;
		  }
		else
		  {
		    last_lefthand = stack[i-3].value;
		    last_operator = token;
		    stack[i-3].token = VALUE;
		    stack[i-3].value =
		      make_binary_op (last_lefthand, token, stack[i-1].value);
		    i -= 2;
		  }
		break;
	      }
	  }

	/* handle special case "cmp OR x AND" */
	if (i >= 2
	    && prio == 7
	    && stack[i-2].token == '|'
	    && COBC_TREE_CLASS (stack[i-1].value) != COB_TYPE_BOOLEAN)
	  {
	    stack[i-1].token = VALUE;
	    stack[i-1].value =
	      make_binary_op (last_lefthand, last_operator, stack[i-1].value);
	  }
	return 0;
      }

    static int shift (int prio, int token, cobc_tree value)
      {
	if (prio > 0)
	  if (reduce (prio) == -1)
	    return -1;
	stack[i].prio  = prio;
	stack[i].token = token;
	stack[i].value = value;
	i++;
	return 0;
      }

    i = 0;
    for (l = $1; l; l = l->next)
      {
#define SHIFT(prio,token,value) \
        if (shift (prio, token, value) == -1) goto error
#define look_ahead(l) \
        ((l && COBC_INTEGER_P (l->item)) ? COBC_INTEGER (l->item)->val : 0)

	int token = 0;
	cobc_tree x = l->item;
	switch (COBC_TREE_TAG (x))
	  {
	  case cobc_tag_class:
	    class_func = COBC_CLASS (x)->cname;
	    goto unary_cond;
	  case cobc_tag_integer:
	    {
	      token = COBC_INTEGER (x)->val;
	      switch (token)
		{
		  /* arithmetic operator */
		case '^':
		  SHIFT (2, token, 0);
		  break;
		case '*':
		case '/':
		  SHIFT (3, token, 0);
		  break;
		case '-':
		  if (i == 0 || stack[i-1].token != VALUE)
		    {
		      /* unary negative */
		      l->next->item =
			make_binary_op (cobc_zero, '-', l->next->item);
		      break;
		    }
		  /* fall through */
		case '+':
		  SHIFT (4, token, 0);
		  break;

		  /* conditional operator */
		case '=':
		  SHIFT (5, '=', 0);
		  break;
		case '<':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, '[', 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, '<', 0);
		  break;
		case '>':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, ']', 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, '>', 0);
		  break;
		case LE:
		  SHIFT (5, '[', 0);
		  break;
		case GE:
		  SHIFT (5, ']', 0);
		  break;

		  /* class condition */
		case NUMERIC:
		  class_func = "cob_is_numeric";
		  goto unary_cond;
		case ALPHABETIC:
		  class_func = "cob_is_alpha";
		  goto unary_cond;
		case ALPHABETIC_LOWER:
		  class_func = "cob_is_lower";
		  goto unary_cond;
		case ALPHABETIC_UPPER:
		  class_func = "cob_is_upper";
		  goto unary_cond;

		  /* sign condition */
		case POSITIVE:
		case NEGATIVE:
		  goto unary_cond;

		unary_cond:
		  {
		    int not_flag = 0;
		    if (i > 0 && stack[i-1].token == '!')
		      {
			not_flag = 1;
			i--;
		      }
		    reduce (5);
		    if (i > 0 && stack[i-1].token == VALUE)
		      {
			int op;
			switch (token)
			  {
			  case ZERO:
			  case POSITIVE:
			  case NEGATIVE:
			    op = ((token == POSITIVE) ? '>' :
				  (token == NEGATIVE) ? '<' : '=');
			    stack[i-1].value =
			      make_binary_op (stack[i-1].value, op, cobc_zero);
			    break;
			  default:
			    stack[i-1].value =
			      make_funcall_1 (class_func, stack[i-1].value);
			    COBC_TREE_CLASS (stack[i-1].value) = COB_TYPE_BOOLEAN;
			    break;
			  }
			if (not_flag)
			  stack[i-1].value = make_negative (stack[i-1].value);
			break;
		      }
		    goto error;
		  }

		  /* logical operator */
		case NOT:
		  switch (look_ahead (l->next))
		    {
		    case '=': SHIFT (5, '~', 0); l = l->next; break;
		    case '<': SHIFT (5, ']', 0); l = l->next; break;
		    case '>': SHIFT (5, '[', 0); l = l->next; break;
		    case LE:  SHIFT (5, '>', 0); l = l->next; break;
		    case GE:  SHIFT (5, '<', 0); l = l->next; break;
		    default:  SHIFT (6, '!', 0); break;
		    }
		  break;
		case AND: SHIFT (7, '&', 0); break;
		case OR:  SHIFT (8, '|', 0); break;
		}
	      break;
	    }
	  default:
	    if (x == cobc_zero)
	      if (stack[i-1].token == VALUE
		  || stack[i-1].token == '!')
	      {
		token = ZERO;
		goto unary_cond;
	      }
	    SHIFT (0, VALUE, x);
	  }
      }
    reduce (9); /* reduce all */

    /*
     * At end
     */
    if (i != 1)
      {
      error:
	yyerror_x ($1->item, _("invalid expression `%s'"),
		   tree_name ($1->item));
	YYERROR;
      }

    $$ = stack[0].value;
  }
;

expr_item_list:
  expr_item			{ $$ = list ($1); }
| expr_item_list IS		{ $$ = $1; }
| expr_item_list expr_item	{ $$ = list_add ($1, $2); }
;
expr_item:
  value				{ $$ = $1; }
| '(' expr_1 ')'		{ $$ = make_parenthesize ($2); }
/* arithmetic operator */
| '+'				{ $$ = make_integer ('+'); }
| '-'				{ $$ = make_integer ('-'); }
| '*'				{ $$ = make_integer ('*'); }
| '/'				{ $$ = make_integer ('/'); }
| '^'				{ $$ = make_integer ('^'); }
/* conditional operator */
| equal				{ $$ = make_integer ('='); }
| greater			{ $$ = make_integer ('>'); }
| less				{ $$ = make_integer ('<'); }
| GE				{ $$ = make_integer (GE); }
| LE				{ $$ = make_integer (LE); }
/* class condition */
| NUMERIC			{ $$ = make_integer (NUMERIC); }
| ALPHABETIC			{ $$ = make_integer (ALPHABETIC); }
| ALPHABETIC_LOWER		{ $$ = make_integer (ALPHABETIC_LOWER); }
| ALPHABETIC_UPPER		{ $$ = make_integer (ALPHABETIC_UPPER); }
| CLASS_NAME			{ $$ = cobc_ref ($1); }
/* sign condition */
  /* ZERO is defined in `value' */
| POSITIVE			{ $$ = make_integer (POSITIVE); }
| NEGATIVE			{ $$ = make_integer (NEGATIVE); }
/* logical operator */
| NOT				{ $$ = make_integer (NOT); }
| AND				{ $$ = make_integer (AND); }
| OR				{ $$ = make_integer (OR); }
;

equal: '=' | EQUAL _to ;
greater: '>' | GREATER _than ;
less: '<' | LESS _than ;
greater_or_equal: GE | GREATER _than OR EQUAL _to ;
less_or_equal: LE | LESS _than OR EQUAL _to ;


/*****************************************************************************
 * Basic structure
 *****************************************************************************/


/*******************
 * Names
 *******************/

/* Math name */

math_name_list:
  numeric_name flag_rounded { $$ = list (make_parameter_1 ($2, $1)); }
| math_name_list
  numeric_name flag_rounded { $$ = list_add ($1, make_parameter_1 ($3, $2));}
;

/* Math edited name */

math_edited_name_list:
  numeric_edited_name flag_rounded { $$ = list (make_parameter_1 ($2, $1)); }
| math_edited_name_list
  numeric_edited_name flag_rounded { $$ = list_add ($1, make_parameter_1 ($3, $2));}
;

/* Numeric name */

numeric_name:
  data_name
  {
    if (COBC_TREE_CLASS ($1) != COB_TYPE_NUMERIC)
      {
	yyerror_x ($1, _("`%s' not numeric"), tree_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

/* Numeric edited name */

numeric_edited_name:
  data_name
  {
    int category = COBC_FIELD (cobc_ref ($1))->pic->category;
    if (category != COB_TYPE_NUMERIC && category != COB_TYPE_NUMERIC_EDITED)
      {
	yyerror_x ($1, _("`%s' not numeric or numeric edited"),
		     tree_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

/* Group name */

group_name:
  data_name
  {
    if (field ($1)->children == NULL)
      {
	yyerror_x ($1, _("`%s' not a group"), tree_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

/* Table name */

table_name:
  qualified_name
  {
    cobc_tree x = cobc_ref ($1);
    if (!COBC_FIELD (x)->index_list)
      {
	yyerror_x ($1, _("`%s' not indexed"), tree_name ($1));
	yyerror_x (x, _("`%s' defined here"), tree_name (x));
	YYERROR;
      }
    $$ = $1;
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
    struct cobc_field *f = COBC_FIELD (cobc_ref ($1));
    if (f->level == 88)
      field_set_used (f->parent);
    else
      field_set_used (f);
    $$ = $1;
  }
;

/* File name */

file_name_list:
  file_name			{ $$ = list ($1); }
| file_name_list file_name	{ $$ = list_add ($1, $2); }
;
file_name:
  NAME
  {
    struct cobc_reference *r = COBC_REFERENCE ($1);
    switch (r->word->count)
      {
      case 0:
	undefined_error ($1, 0);
	YYERROR;
      default:
	if (COBC_FILE_P (r->word->items->item))
	  {
	    set_value ($1, r->word->items->item);
	    $$ = $1;
	    break;
	  }
	yyerror_x ($1, _("`%s' not file name"), r->word->name);
	YYERROR;
      }
  }
;

/* Record name */

record_name:
  name
  {
    if (COBC_FIELD (cobc_ref ($1))->file == NULL)
      {
	yyerror_x ($1, _("`%s' not record name"), tree_name ($1));
	YYERROR;
      }
    $$ = $1;
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


/*
 * Primitive name
 */

name:
  name_1
  {
    $$ = resolve_field ($1);
    if (!$$)
      YYERROR;
  }
;
name_1:
  qualified_name		{ $$ = $1; }
| qualified_name subref		{ $$ = $1; }
| qualified_name refmod		{ $$ = $1; }
| qualified_name subref refmod	{ $$ = $1; }
;
subref:
  '(' subscript_list ')'
  {
    COBC_REFERENCE ($<tree>0)->subs = $<list>2;
    $<tree>$ = $<tree>0;
  }
;
refmod:
  '(' subscript ':' opt_subscript ')'
  {
    COBC_REFERENCE ($<tree>0)->offset = $2;
    COBC_REFERENCE ($<tree>0)->length = $4;
  }
;
subscript_list:
  subscript			{ $<list>$ = list ($1); }
| subscript_list subscript	{ $<list>$ = list_add ($<list>1, $2); }
;
opt_subscript:
  /* empty */			{ $$ = NULL; }
| subscript			{ $$ = $1; }
;
subscript:
  integer_value			{ $$ = $1; }
| subscript '+' integer_value	{ $$ = make_binary_op ($1, '+', $3); }
| subscript '-' integer_value	{ $$ = make_binary_op ($1, '-', $3); }
;

/* Label name */

label_list:
  label			{ $$ = list ($1); }
| label_list label	{ $$ = list_add ($1, $2); }
;
label:
  label_name
  {
    $$ = $1;
    COBC_REFERENCE ($$)->offset = COBC_TREE (current_section);
    current_program->label_list = cons ($$, current_program->label_list);
  }
;
label_name:
  qualified_name
| integer_label
| integer_label in_of integer_label
;

/* Reference */

reference_list:
  reference			{ $$ = list ($1); }
| reference_list reference	{ $$ = list_add ($1, $2); }
;
reference:
  qualified_name
  {
    $$ = $1;
    current_program->reference_list = cons ($$, current_program->reference_list);
  }
;
qualified_name:
  NAME				{ $$ = $1; }
| NAME in_of qualified_name
  {
    $$ = $1;
    COBC_REFERENCE ($1)->next = COBC_REFERENCE ($3);
  }
;
in_of: IN | OF ;

/* Undefined name */

undefined_name:
  NAME
  {
    if (COBC_REFERENCE ($1)->word->count > 0)
      redefinition_error ($1);
    $$ = $1;
  }
;


/*******************
 * Values
 *******************/

/*
 * Special values
 */

/* Numeric value */

numeric_value_list:
  numeric_value				{ $$ = list ($1); }
| numeric_value_list numeric_value	{ $$ = list_add ($1, $2); }
;
numeric_value:
  value
  {
    if (COBC_TREE_CLASS ($1) != COB_TYPE_NUMERIC)
      yyerror_x ($1, _("numeric value is expected `%s'"),
		   tree_name ($1));
    $$ = $1;
  }
;

/* Integer value */

integer:
  INTEGER_LITERAL
  {
    $$ = literal_to_int (COBC_LITERAL ($1));
  }
;

integer_value:
  value
  {
    if (COBC_TREE_CLASS ($1) != COB_TYPE_NUMERIC)
      goto invalid;

    switch (COBC_TREE_TAG ($1))
      {
      case cobc_tag_const:
	{
	  if ($1 != cobc_zero)
	    goto invalid;
	  break;
	}
      case cobc_tag_literal:
	{
	  struct cobc_literal *l = COBC_LITERAL ($1);
	  if (l->sign < 0 || l->decimals > 0)
	    goto invalid;
	  break;
	}
      case cobc_tag_reference:
	{
	  struct cobc_field *f = COBC_FIELD (cobc_ref ($1));
	  if (f->pic->decimals > 0)
	    goto invalid;
	  break;
	}
      default:
      invalid:
	yyerror_x ($1, _("`%s' must be an integer value"),
		     tree_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

/* Text */

text_value:
  data_name
| NONNUMERIC_LITERAL
| figurative_constant		{ $$ = $<tree>1; }
;


/*
 * Primitive value
 */

opt_value_list:
  /* empty */			{ $$ = NULL; }
| value_list			{ $$ = $1; }
;
value_list:
  value				{ $$ = list ($1); }
| value_list value		{ $$ = list_add ($1, $2); }
;
value:
  data_name
| literal
| function
;
function:
  FUNCTION_NAME '(' opt_value_list ')'
  {
    PENDING ("FUNCTION");
    YYABORT;
  }
;


/*
 * Literal
 */

literal:
  basic_literal			{ $$ = $<tree>1; }
| figurative_constant		{ $$ = $<tree>1; }
| ALL basic_literal		{ $$ = $<tree>2;
				  COBC_LITERAL ($<tree>2)->all = 1; }
| ALL figurative_constant	{ $$ = $<tree>2; }
;
basic_literal:
  INTEGER_LITERAL		{ $<tree>$ = $1; }
| NUMERIC_LITERAL		{ $<tree>$ = $1; }
| NONNUMERIC_LITERAL		{ $<tree>$ = $1; }
;
figurative_constant:
  SPACE				{ $<tree>$ = cobc_space; }
| ZERO				{ $<tree>$ = cobc_zero; }
| QUOTE				{ $<tree>$ = cobc_quote; }
| HIGH_VALUE			{ $<tree>$ = cobc_high; }
| LOW_VALUE			{ $<tree>$ = cobc_low; }
;


/*******************
 * Common rules
 *******************/

/*
 * Common flags
 */

flag_all:
  /* empty */			{ $$ = 0; }
| ALL				{ $$ = 1; }
;
flag_not:
  /* empty */			{ $$ = 0; }
| NOT				{ $$ = 1; }
;
flag_next:
  /* empty */			{ $$ = 0; }
| NEXT				{ $$ = 1; }
;
flag_global:
  /* empty */			{ $$ = 0; }
| GLOBAL			{ $$ = 1; }
;
flag_rounded:
  /* empty */			{ $$ = 0; }
| ROUNDED			{ $$ = 1; }
;


/*
 * Optional words
 */

_are: | ARE ;
_area: | AREA ;
_at: | AT ;
_by: | BY ;
_character: | CHARACTER ;
_characters: | CHARACTERS ;
_file: | TOK_FILE ;
_for: | FOR ;
_from: | FROM ;
_in: | IN ;
_is: | IS ;
_is_are: | IS | ARE ;
_key: | KEY ;
_mode: | MODE ;
_number: | NUMBER ;
_on: | ON ;
_of: | OF ;
_order: | ORDER ;
_program: | PROGRAM ;
_record: | RECORD ;
_sign: | SIGN _is ;
_size: | SIZE ;
_status: | STATUS ;
_tape: | TAPE ;
_than: | THAN ;
_then: | THEN ;
_to: | TO ;
_upon: | UPON ;
_when: | WHEN ;
_with: | WITH ;


%%

static cobc_tree
resolve_name (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);

  if (r->value)
    return x;

  if (r->next)
    {
      /* NAME IN <parent> */
      struct cobc_list *l;
      struct cobc_field *p;
      struct cobc_field *parent = field (resolve_name (COBC_TREE (r->next)));
      if (parent == NULL)
	return NULL;
      r->value = NULL;
      for (l = r->word->items; l; l = l->next)
	if (l->item && COBC_FIELD_P (l->item))
	  for (p = COBC_FIELD (l->item)->parent; p; p = p->parent)
	    if (p == parent)
	      {
		if (r->value)
		  {
		    ambiguous_error (x);
		    return NULL;
		  }
		set_value (x, l->item);
	      }
      if (r->value == NULL)
	{
	  undefined_error (x, COBC_TREE (parent));
	  return NULL;
	}
    }
  else
    {
      /* NAME */
      switch (r->word->count)
	{
	case 0:
	  undefined_error (x, 0);
	  return NULL;
	case 1:
	  set_value (x, r->word->items->item);
	  break;
	default:
	  ambiguous_error (x);
	  return NULL;
	}
    }

  return x;
}

static cobc_tree
resolve_field (cobc_tree x)
{
  if (resolve_name (x) == NULL)
    return NULL;
  else
    {
      struct cobc_reference *r = COBC_REFERENCE (x);
      struct cobc_field *f = COBC_FIELD (r->value);
      const char *name = r->word->name;

      /* check the number of subscripts */
      if (list_length (r->subs) != f->indexes)
	{
	  switch (f->indexes)
	    {
	    case 0:
	      yyerror_x (x, _("`%s' cannot be subscripted"), name);
	      break;
	    case 1:
	      yyerror_x (x, _("`%s' requires 1 subscript"), name);
	      break;
	    default:
	      yyerror_x (x, _("`%s' requires %d subscripts"), name, f->indexes);
	      break;
	    }
	  return x;
	}

      /* check the range of constant subscripts */
      if (r->subs)
	{
	  struct cobc_field *p;
	  struct cobc_list *l = r->subs = list_reverse (r->subs);

	  for (p = f; p; p = p->parent)
	    if (p->f.have_occurs)
	      {
		cobc_tree sub = l->item;
		if (COBC_LITERAL_P (sub))
		  {
		    int n = literal_to_int (COBC_LITERAL (sub));
		    if (n < 1 || n > p->occurs)
		      yyerror_x (x, _("index of `%s' out of range: %d"),
				 name, n);
		  }
		l = l->next;
	      }

	  r->subs = list_reverse (r->subs);
	}

      /* check the range of constant reference modification */
      if (r->offset && COBC_LITERAL_P (r->offset))
	{
	  int offset = literal_to_int (COBC_LITERAL (r->offset));
	  if (offset < 1 || offset > f->size)
	    yyerror_x (x, _("offset of `%s' out of range: %d"), name, offset);
	  else if (r->length && COBC_LITERAL_P (r->length))
	    {
	      int length = literal_to_int (COBC_LITERAL (r->length));
	      if (length < 1 || length > f->size - offset + 1)
		yyerror_x (x, _("length of `%s' out of range: %d"),
			   name, length);
	    }
	}

      return x;
    }
}

static cobc_tree
resolve_label (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  struct cobc_label *section = NULL;

  if (r->next)
    {
      /* LABEL IN LABEL */
      struct cobc_reference *r2 = r->next;
      cobc_tree x2 = COBC_TREE (r2);

      /* resolve section name */
      switch (r2->word->count)
	{
	case 0:
	  undefined_error (x2, 0);
	  return NULL;
	case 1:
	  if (COBC_LABEL_P (r2->word->items->item))
	    {
	      section = r2->word->items->item;
	      break;
	    }
	  /* fall through */
	default:
	  yyerror_x (x2, _("invalid section name `%s'"), r2->word->name);
	  return NULL;
	}
    }
  else
    {
      /* LABEL */
      switch (r->word->count)
	{
	case 0:
	  undefined_error (x, 0);
	  return NULL;
	case 1:
	  if (COBC_LABEL_P (r->word->items->item))
	    {
	      set_value (x, r->word->items->item);
	      break;
	    }
	  yyerror_x (x, _("invalid label `%s'"), r->word->name);
	  return NULL;
	default:
	  section = COBC_LABEL (r->offset);
	}
    }

  /* resolve paragraph name */
  if (r->value == NULL)
    {
      struct cobc_list *l;
      for (l = section->children; l; l = l->next)
	if (strcasecmp (r->word->name, COBC_LABEL (l->item)->name) == 0)
	  {
	    set_value (x, l->item);
	    break;
	  }
      if (r->value == NULL)
	{
	  yyerror_x (x, _("no such label `%s' IN `%s'"),
		       r->word->name, section->name);
	  return NULL;
	}
    }

  COBC_LABEL (r->value)->need_begin = 1;
  if (r->length)
    COBC_LABEL (r->value)->need_return = 1;

  return x;
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
      yyerror (_("not switch name"));
      return -1;
    }
}


static struct cobc_field *
build_field (cobc_tree level, cobc_tree name, struct cobc_field *last_field)
{
  struct cobc_field *f;
  struct cobc_reference *r = COBC_REFERENCE (name);
  int lv = literal_to_int (COBC_LITERAL (level));

  /* checks for redefinition */
  if (lv == 01 || lv == 77)
    {
      if (r->word->count > 0)
	{
	  redefinition_error (name);
	  return NULL;
	}
    }
  else
    {
      struct cobc_list *l;
      for (l = r->word->items; l; l = l->next)
	if (!COBC_FIELD_P (l->item)
	    || COBC_FIELD (l->item)->level == 01
	    || COBC_FIELD (l->item)->level == 77)
	  {
	    redefinition_error (name);
	    return NULL;
	  }
    }

  /* build the field */
  f = COBC_FIELD (make_field (name));
  f->level = lv;
  f->usage = COBC_USAGE_DISPLAY;
  f->occurs = 1;

  if (last_field && last_field->level == 88)
    last_field = last_field->parent;

  if (f->level == 01 || f->level == 77)
    {
      if (last_field)
	field_founder (last_field)->sister = f;
    }
  else if (!last_field)
    {
      yyerror_x (level, _("level number must begin with 01 or 77"));
      return NULL;
    }
  else if (f->level == 66)
    {
      struct cobc_field *p;
      f->parent = field_founder (last_field);
      for (p = f->parent->children; p->sister; p = p->sister);
      p->sister = f;
    }
  else if (f->level == 88)
    {
      if (last_field->level == 88)
	f->parent = last_field->parent;
      else
	f->parent = last_field;
      COBC_TREE_CLASS (f) = COB_TYPE_BOOLEAN;
    }
  else if (f->level > 49)
    {
      yyerror_x (level, _("invalid level number `%s'"),
		   COBC_LITERAL (level)->data);
      return NULL;
    }
  else if (f->level > last_field->level)
    {
      /* lower level */
      last_field->children = f;
      f->parent = last_field;
    }
  else if (f->level == last_field->level)
    {
      /* same level */
      struct cobc_field *p;
    sister:
      /* ensure that there is no field with the same name
	 in the same level */
      for (p = last_field->parent->children; p; p = p->sister)
	if (strcasecmp (f->name, p->name) == 0)
	  redefinition_error (name);
      last_field->sister = f;
      f->parent = last_field->parent;
    }
  else
    {
      /* upper level */
      struct cobc_field *p;
      for (p = last_field->parent; p; p = p->parent)
	if (p->level == f->level)
	  {
	    last_field = p;
	    goto sister;
	  }
      yyerror_x (level, _("field hierarchy broken"));
      return NULL;
    }

  /* inherit parent's properties */
  if (f->parent)
    {
      f->usage = f->parent->usage;
      f->indexes = f->parent->indexes;
      f->f.in_redefines = f->parent->f.in_redefines;
      f->f.sign_leading = f->parent->f.sign_leading;
      f->f.sign_separate = f->parent->f.sign_separate;
    }

  return f;
}

static struct cobc_field *
validate_redefines (struct cobc_field *field, cobc_tree redefines)
{
  struct cobc_field *f, *p;
  struct cobc_reference *r = COBC_REFERENCE (redefines);
  cobc_tree x = COBC_TREE (field);

  /* ISO+IEC+1989-2002: 13.16.42.2-7 */
  if (r->next)
    {
      yyerror_x (x, _("`%s' cannot be qualified"), COBC_NAME (redefines));
      return NULL;
    }

  /* resolve the name in the current group (if any) */
  if (field->parent)
    {
      cobc_tree parent = COBC_TREE (field->parent);
      r->next = COBC_REFERENCE (copy_reference (redefines, parent));
    }
  redefines = resolve_name (redefines);
  if (redefines == NULL)
    return NULL;

  f = COBC_FIELD (cobc_ref (redefines));

  /* ISO+IEC+1989-2002: 13.16.42.2-2 */
  if (f->level != field->level)
    {
      yyerror_x (x, _("level number of REDEFINES entries must be identical"));
      return NULL;
    }
  if (f->level == 66 || f->level == 88)
    {
      yyerror_x (x, _("level number of REDEFINES entry cannot be 66 or 88"));
      return NULL;
    }

  /* ISO+IEC+1989-2002: 13.16.42.2-11 */
  for (p = f->sister; p && p->redefines; p = p->sister);
  if (p != field)
    {
      yyerror_x (x, _("REDEFINES must follow the original definition"));
      return NULL;
    }

  return f;
}

static void
validate_field_tree (struct cobc_field *p)
{
  cobc_tree x = COBC_TREE (p);

  if (p->children)
    {
      /* group */
      if (p->pic)
	yyerror_x (x, _("group name `%s' may not have PICTURE"),
		   tree_name (x));

      if (p->f.justified)
	yyerror_x (x, _("group name `%s' may not have JUSTIFIED RIGHT"),
		   tree_name (x));

      for (p = p->children; p; p = p->sister)
	validate_field_tree (p);
    }
  else if (p->level == 66)
    {
    }
  else if (p->level == 88)
    {
      /* conditional variable */
      if (p->pic)
	yyerror_x (x, _("level 88 field `%s' may not have PICTURE"),
		   tree_name (x));
    }
  else
    {
      /* validate PICTURE */
      if (!p->pic)
	if (p->usage != COBC_USAGE_INDEX)
	  yyerror_x (x, _("PICTURE required for `%s'"), tree_name (x));

      /* validate USAGE */
      switch (p->usage)
	{
	case COBC_USAGE_DISPLAY:
	  break;
	case COBC_USAGE_BINARY:
	  if (p->pic->category != COB_TYPE_NUMERIC)
	    yywarn (_("field must be numeric"));
	  break;
	case COBC_USAGE_INDEX:
	  break;
	}

      /* validate SIGN */

      /* validate OCCURS */
      if (p->f.have_occurs)
	if (p->level < 2 || p->level > 49)
	  yyerror_x (x, _("level %02d field `%s' cannot have OCCURS"),
		     p->level, tree_name (x));

      /* validate JUSTIFIED RIGHT */
      if (p->f.justified)
	switch (p->pic->category)
	  {
	  case COB_TYPE_ALPHABETIC:
	  case COB_TYPE_ALPHANUMERIC:
	  case COB_TYPE_NATIONAL:
	    break;
	  default:
	    yyerror_x (x, _("`%s' cannot have JUSTIFIED RIGHT"),
		       tree_name (x));
	    break;
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
	  if (p->f.in_redefines)
	    {
	      /* ISO+IEC+1989-2002: 13.16.42.2-10 */
	      yyerror_x (x, _("entries under REDEFINES cannot have VALUE clause"));
	    }
	  else if (p->value == cobc_zero)
	    {
	      /* just accept */
	    }
	  else if (p->pic->category != COB_TYPE_NUMERIC)
	    {
	      if (COBC_TREE_CLASS (p->value) == COB_TYPE_NUMERIC)
		yyerror_x (x, _("VALUE should be alphanumeric"));
	    }
	}
    }
}

static void
finalize_file (struct cobc_file *f, struct cobc_field *records)
{
  char pic[BUFSIZ];
  struct cobc_field *p;

  for (p = records; p; p = p->sister)
    {
      /* check the record size */
      if (f->record_min > 0)
	if (p->size < f->record_min)
	  yyerror (_("record size too small `%s'"), p->name);
      if (f->record_max > 0)
	if (p->size > f->record_max)
	  yyerror (_("record size too large `%s'"), p->name);
    }

  /* compute the record size */
  if (f->record_min == 0)
    f->record_min = records->size;
  for (p = records; p; p = p->sister)
    {
      if (p->size < f->record_min)
	f->record_min = p->size;
      if (p->size > f->record_max)
	f->record_max = p->size;
    }

  /* create record */
  sprintf (pic, "X(%d)", f->record_max);
  f->record = COBC_FIELD (make_field_x (f->name, pic, COBC_USAGE_DISPLAY));
  field_set_used (f->record);
  f->record->sister = records;

  for (p = records; p; p = p->sister)
    {
      p->file = f;
      p->redefines = f->record;
      field_set_used (p);
    }
}


/*
 * Numerical operation
 */

#define add_stmt(s,x) \
  COBC_SEQUENCE (s)->list = list_add (COBC_SEQUENCE (s)->list, x)

static cobc_tree
decimal_alloc (void)
{
  cobc_tree x = make_decimal (current_program->decimal_index++);
  if (current_program->decimal_index > current_program->decimal_index_max)
    current_program->decimal_index_max = current_program->decimal_index;
  return x;
}

static void
decimal_free (void)
{
  current_program->decimal_index--;
}

static void
decimal_compute (cobc_tree s, char op, cobc_tree x, cobc_tree y)
{
  char *func;
  switch (op)
    {
    case '+': func = "cob_decimal_add"; break;
    case '-': func = "cob_decimal_sub"; break;
    case '*': func = "cob_decimal_mul"; break;
    case '/': func = "cob_decimal_div"; break;
    case '^': func = "cob_decimal_pow"; break;
    default: abort ();
    }
  add_stmt (s, make_funcall_2 (func, x, y));
}

static void
decimal_expand (cobc_tree s, cobc_tree d, cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
      {
	cobc_tree e;
	if (x == cobc_zero)
	  e = make_funcall_3 ("cob_decimal_set_int", d, cobc_int0, cobc_int0);
	else
	  abort ();
	add_stmt (s, e);
	break;
      }
    case cobc_tag_literal:
      {
	/* set d, N */
	struct cobc_literal *p = COBC_LITERAL (x);
	if (p->size < 10)
	  add_stmt (s, make_funcall_3 ("cob_decimal_set_int",
				       d, make_cast_int32 (x),
				       make_integer (p->decimals)));
	else
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case cobc_tag_reference:
      {
	/* set d, X */
	struct cobc_field *f = field (x);
	if (cobc_flags.failsafe && f->usage == COBC_USAGE_DISPLAY)
	  add_stmt (s, make_funcall_2 ("cob_check_numeric",
					x, make_string (f->name)));
	if (f->usage == COBC_USAGE_INDEX)
	  add_stmt (s, make_funcall_3 ("cob_decimal_set_int",
				       d, make_cast_int32 (x), cobc_int0));
	else
	  add_stmt (s, make_funcall_2 ("cob_decimal_set_field", d, x));
	break;
      }
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	if (p->op == '@')
	  {
	    decimal_expand (s, d, p->x);
	  }
	else
	  {
	    /* set d, X
	     * set t, Y
	     * OP d, t */
	    cobc_tree t = decimal_alloc ();
	    decimal_expand (s, d, p->x);
	    decimal_expand (s, t, p->y);
	    decimal_compute (s, p->op, d, t);
	    decimal_free ();
	  }
	break;
      }
    default:
      abort ();
    }
}

static void
decimal_assign (cobc_tree s, cobc_tree x, cobc_tree d, int round)
{
  char *func = round ? "cob_decimal_get_r" : "cob_decimal_get";
  add_stmt (s, make_funcall_2 (func, d, x));
}

static cobc_tree
build_decimal_assign (struct cobc_list *vars, char op, cobc_tree val)
{
  struct cobc_list *l;
  cobc_tree s1 = make_sequence (NULL);
  cobc_tree s2 = make_sequence (NULL);
  cobc_tree d = decimal_alloc ();

  /* set d, VAL */
  decimal_expand (s2, d, val);

  if (op == 0)
    {
      for (l = vars; l; l = l->next)
	{
	  /* set VAR, d */
	  struct cobc_parameter *p = l->item;
	  decimal_assign (s2, p->x, d, p->type);
	  add_stmt (s1, s2);
	  if (l->next)
	    s2 = make_sequence (NULL);
	}
    }
  else
    {
      cobc_tree t = decimal_alloc ();
      for (l = vars; l; l = l->next)
	{
	  /* set t, VAR
	   * OP t, d
	   * set VAR, t
	   */
	  struct cobc_parameter *p = l->item;
	  decimal_expand (s2, t, p->x);
	  decimal_compute (s2, op, t, d);
	  decimal_assign (s2, p->x, t, p->type);
	  add_stmt (s1, s2);
	  if (l->next)
	    s2 = make_sequence (NULL);
	}
      decimal_free ();
    }

  decimal_free ();
  return s1;
}

static cobc_tree
build_assign (struct cobc_list *vars, char op, cobc_tree val)
{
  if (!COBC_BINARY_OP_P (val))
    if (op == '+' || op == '-')
      {
	struct cobc_list *l;
	for (l = vars; l; l = l->next)
	  {
	    struct cobc_parameter *p = l->item;
	    if (op == '+')
	      l->item = build_add (p->x, val, p->type);
	    else
	      l->item = build_sub (p->x, val, p->type);
	  }
	return make_sequence (vars);
      }

  return build_decimal_assign (vars, op, val);
}


/*
 * ADD/SUBTRACT/MOVE CORRESPONDING
 */

static cobc_tree
build_add (cobc_tree v, cobc_tree n, int round)
{
  struct cobc_field *f = field (v);

  if (f->usage == COBC_USAGE_INDEX)
    return build_move (make_binary_op (v, '+', n), v);

  switch (COBC_TREE_TAG (n))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (n);
	if (l->size < 10 && l->decimals == 0 && round == 0)
	  return make_funcall_2 ("cob_add_int", v, make_cast_int32 (n));
	/* fall through */
      }
    default:
      {
	if (round)
	  return make_funcall_2 ("cob_add_r", v, n);
	else
	  return make_funcall_2 ("cob_add", v, n);
      }
    }
}

static cobc_tree
build_sub (cobc_tree v, cobc_tree n, int round)
{
  struct cobc_field *f = field (v);

  if (f->usage == COBC_USAGE_INDEX)
    return build_move (make_binary_op (v, '-', n), v);

  switch (COBC_TREE_TAG (n))
    {
    case cobc_tag_literal:
      {
	struct cobc_literal *l = COBC_LITERAL (n);
	if (l->size < 10 && l->decimals == 0 && round == 0)
	  return make_funcall_2 ("cob_sub_int", v, make_cast_int32 (n));
	/* fall through */
      }
    default:
      {
	if (round)
	  return make_funcall_2 ("cob_sub_r", v, n);
	else
	  return make_funcall_2 ("cob_sub", v, n);
      }
    }
}

static cobc_tree
build_move (cobc_tree src, cobc_tree dst)
{
  return make_funcall_2 ("@move", src, dst);
}

static struct cobc_list *
build_corresponding_1 (cobc_tree (*func)(), cobc_tree x1, cobc_tree x2,
		       int opt, struct cobc_list *l)
{
  struct cobc_field *f1, *f2;
  for (f1 = field (x1)->children; f1; f1 = f1->sister)
    if (!f1->redefines && !f1->f.have_occurs)
      for (f2 = field (x2)->children; f2; f2 = f2->sister)
	if (!f2->redefines && !f2->f.have_occurs)
	  if (strcmp (f1->name, f2->name) == 0)
	    {
	      cobc_tree t1 = copy_reference (x1, COBC_TREE (f1));
	      cobc_tree t2 = copy_reference (x2, COBC_TREE (f2));
	      if (f1->children && f2->children)
		l = build_corresponding_1 (func, t1, t2, opt, l);
	      else
		{
		  field (t1)->f.used = 1;
		  field (t2)->f.used = 1;
		  if (opt < 0)
		    l = cons (func (t1, t2), l);
		  else
		    l = cons (func (t1, t2, opt), l);
		}
	    }
  return l;
}

static cobc_tree
build_corresponding (cobc_tree (*func)(), cobc_tree x1, cobc_tree x2, int opt)
{
  return make_sequence (build_corresponding_1 (func, x1, x2, opt, NULL));
}


/*
 * DIVIDE
 */

static cobc_tree
build_divide (cobc_tree dividend, cobc_tree divisor,
	      cobc_tree quotient, int round, cobc_tree remainder)
{
  struct cobc_list *l = NULL;
  l = list_add (l, make_funcall_4 ("cob_div_quotient",
				   dividend, divisor, quotient,
				   round ? cobc_int1 : cobc_int0));
  l = list_add (l, make_funcall_1 ("cob_div_remainder", remainder));
  return make_sequence (l);
}


/*
 * Condition
 */

static cobc_tree
build_cond_88 (cobc_tree x)
{
  struct cobc_field *f = field (x);
  struct cobc_list *l;
  cobc_tree c1 = NULL;

  /* refer to parent's data storage */
  x = copy_reference (x, COBC_TREE (f->parent));

  /* build condition */
  for (l = f->values; l; l = l->next)
    {
      cobc_tree c2;
      if (COBC_PARAMETER_P (l->item))
	{
	  /* VALUE THRU VALUE */
	  struct cobc_parameter *p = COBC_PARAMETER (l->item);
	  c2 = make_binary_op (make_binary_op (p->x, '[', x),
			       '&',
			       make_binary_op (x, '[', p->y));
	}
      else
	{
	  /* VALUE */
	  c2 = make_binary_op (x, '=', l->item);
	}
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = make_binary_op (c1, '|', c2);
    }
  return c1;
}

static cobc_tree
build_cond (cobc_tree x)
{
  switch (COBC_TREE_TAG (x))
    {
    case cobc_tag_const:
    case cobc_tag_funcall:
      return x;
    case cobc_tag_reference:
      {
	/* level 88 condition */
	if (field (x)->level == 88)
	  {
	    /* We need to build a 88 condition at every occurrence
	       instead of once at the beginning, because a 88 item
	       may be subscripted (i.e., not a constant tree). */
	    return build_cond (build_cond_88 (x));
	  }

	abort ();
      }
    case cobc_tag_binary_op:
      {
	struct cobc_binary_op *p = COBC_BINARY_OP (x);
	switch (p->op)
	  {
	  case '@':
	    return build_cond (p->x);
	  case '!':
	    p->x = build_cond (p->x);
	    break;
	  case '&': case '|':
	    p->x = build_cond (p->x);
	    p->y = build_cond (p->y);
	    break;
	  default:
	    if ((COBC_REFERENCE_P (p->x)
		 && field (p->x)->usage == COBC_USAGE_INDEX)
		|| (COBC_REFERENCE_P (p->y)
		    && field (p->y)->usage == COBC_USAGE_INDEX))
	      {
		return x;
	      }
	    else if (COBC_BINARY_OP_P (p->x) || COBC_BINARY_OP_P (p->y))
	      {
		/* decimal comparison */
		cobc_tree s = make_sequence (NULL);
		cobc_tree d1 = decimal_alloc ();
		cobc_tree d2 = decimal_alloc ();
		decimal_expand (s, d1, p->x);
		decimal_expand (s, d2, p->y);
		add_stmt (s, make_funcall_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		p->x = s;
	      }
	    else if (COBC_LITERAL_P (p->y))
	      {
		struct cobc_literal *l = COBC_LITERAL (p->y);
		int size = field_size (p->x);

		if (COBC_TREE_CLASS (p->x) == COB_TYPE_NUMERIC
		    && COBC_TREE_CLASS (p->y) == COB_TYPE_NUMERIC)
		  {
		    if (l->size < 10 && l->decimals == 0)
		      p->x = make_funcall_2 ("cob_cmp_int",
					     p->x, make_cast_int32 (p->y));
		    else
		      p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
		  }
		else if (size > 0 && size >= l->size && !l->all)
		  p->x = make_funcall_2 ("@memcmp", p->x, p->y);
		else
		  p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    else
	      {
		/* field comparison */
		p->x = make_funcall_2 ("cob_cmp", p->x, p->y);
	      }
	    break;
	  }
	return x;
      }
    default:
      abort ();
    }
}


/*
 * EVALUATE
 */

static cobc_tree
build_evaluate_test (cobc_tree s, struct cobc_parameter *p)
{
  /* ANY is always true */
  if (p->x == cobc_any)
    return cobc_true;

  /* x THRU y */
  if (p->y)
    {
      cobc_tree x = make_binary_op (make_binary_op (p->x, '[', s),
				    '&',
				    make_binary_op (s, '[', p->y));
      return p->type ? make_negative (x) : x;
    }

  /* TRUE or FALSE */
  if (s == cobc_true)
    return p->type ? make_negative (p->x) : p->x;
  if (s == cobc_false)
    return p->type ? p->x : make_negative (p->x);
  if (p->x == cobc_true)
    return p->type ? make_negative (s) : s;
  if (p->x == cobc_false)
    return p->type ? s : make_negative (s);

  /* regular comparison */
  if (p->type)
    return make_binary_op (s, '~', p->x);
  else
    return make_binary_op (s, '=', p->x);
}

static cobc_tree
build_evaluate_internal (struct cobc_list *subject_list, struct cobc_list *case_list)
{
  cobc_tree stmt;
  cobc_tree c1 = NULL;
  struct cobc_list *subjs, *whens, *objs;

  if (case_list == NULL)
    return NULL;

  whens = case_list->item;
  stmt = whens->item;
  whens = whens->next;

  /* for each WHEN sequence */
  for (; whens; whens = whens->next)
    {
      cobc_tree c2 = NULL;
      /* single WHEN test */
      for (subjs = subject_list, objs = whens->item;
	   subjs && objs;
	   subjs = subjs->next, objs = objs->next)
	{
	  cobc_tree c3 = build_evaluate_test (subjs->item, objs->item);
	  if (c2 == NULL)
	    c2 = c3;
	  else
	    c2 = make_binary_op (c2, '&', c3);
	}
      if (subjs || objs)
	yyerror (_("wrong number of WHEN parameters"));
      /* connect multiple WHEN's */
      if (c1 == NULL)
	c1 = c2;
      else
	c1 = make_binary_op (c1, '|', c2);
    }

  if (c1 == NULL)
    return stmt;
  else
    return make_if (build_cond (c1), stmt,
		    build_evaluate_internal (subject_list, case_list->next));
}

static cobc_tree
build_evaluate (struct cobc_list *subject_list, struct cobc_list *case_list)
{
  return build_evaluate_internal (subject_list, case_list);
}


/*
 * SEARCH ALL
 */

static void
search_set_keys (struct cobc_field *f, cobc_tree x)
{
  struct cobc_binary_op *p;

  if (COBC_REFERENCE_P (x))
    x = build_cond_88 (x);
  
  p = COBC_BINARY_OP (x);
  switch (p->op)
    {
    case '&':
      search_set_keys (f, p->x);
      search_set_keys (f, p->y);
      break;
    case '=':
      {
	int i;
	for (i = 0; i < f->nkeys; i++)
	  if (field (p->x) == field (f->keys[i].key))
	    {
	      f->keys[i].ref = p->x;
	      f->keys[i].val = p->y;
	      break;
	    }
	if (i == f->nkeys)
	  yyerror_x (x, _("undeclared key `%s'"), field (p->x)->name);
	break;
      }
    default:
      yyerror_x (x, _("invalid SEARCH ALL condition"));
      break;
    }
}

static cobc_tree
build_search_all (cobc_tree table, cobc_tree cond)
{
  int i;
  struct cobc_field *f = field (table);
  cobc_tree c1 = NULL;

  /* set keys */
  for (i = 0; i < f->nkeys; i++)
    f->keys[i].ref = 0;
  search_set_keys (f, cond);

  /* build condition */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].ref)
      {
	cobc_tree c2;
	if (f->keys[i].dir == COB_ASCENDING)
	  c2 = make_binary_op (f->keys[i].ref, '=', f->keys[i].val);
	else
	  c2 = make_binary_op (f->keys[i].val, '=', f->keys[i].ref);
	if (c1 == NULL)
	  c1 = c2;
	else
	  c1 = make_binary_op (c1, '&', c2);
      }

  return build_cond (c1);
}


static void
redefinition_error (cobc_tree x)
{
  struct cobc_word *w = COBC_REFERENCE (x)->word;
  yyerror_x (x, _("redefinition of `%s'"), w->name);
  yyerror_x (w->items->item, _("`%s' previously defined here"),
	     tree_name (w->items->item));
}

static void
undefined_error (cobc_tree x, cobc_tree parent)
{
  struct cobc_word *w = COBC_REFERENCE (x)->word;
  if (parent)
    yyerror_x (x, _("`%s' undefined in `%s'"),
		 w->name, tree_name (parent));
  else
    yyerror_x (x, _("`%s' undefined"), w->name);
}

static void
ambiguous_error (cobc_tree x)
{
  struct cobc_word *w = COBC_REFERENCE (x)->word;
  if (w->error == 0)
    {
      struct cobc_list *l;

      /* display error on the first time */
      yyerror_x (x, _("`%s' ambiguous; need qualification"), w->name);
      w->error = 1;

      /* display all fields with the same name */
      for (l = w->items; l; l = l->next)
	{
	  char buff[BUFSIZ];
	  struct cobc_field *p;
	  cobc_tree x = l->item;
	  sprintf (buff, "`%s' ", w->name);
	  if (COBC_FIELD_P (x))
	    for (p = COBC_FIELD (x)->parent; p; p = p->parent)
	      {
		strcat (buff, "in `");
		strcat (buff, p->name);
		strcat (buff, "' ");
	      }
	  strcat (buff, _("defined here"));
	  yyerror_x (x, buff);
	}
    }
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
  yyprintf (0, 0, "error: ", fmt, ap);
  va_end (ap);

  error_count++;
}

void
yywarn_x (cobc_tree x, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->source_file, x->source_line, "warning: ", fmt, ap);
  va_end (ap);

  warning_count++;
}

void
yyerror_x (cobc_tree x, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->source_file, x->source_line, "error: ", fmt, ap);
  va_end (ap);

  error_count++;
}
