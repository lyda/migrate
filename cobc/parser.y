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

%expect 104

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

#define YYLTYPE		struct cobc_location

#define IGNORE(x)	/* ignored */
#define PENDING(x)	yywarn (_("`%s' not implemented"), x)
#define OBSOLETE(x)	yywarn (_("`%s' obsolete"), x)

#define push_tree(x) \
  program_spec.exec_list = cons (x, program_spec.exec_list)

#define push_call_0(f)		 push_tree (make_call_0 (f))
#define push_call_1(f,a)	 push_tree (make_call_1 (f, a))
#define push_call_1_list(f,a,l)	 push_tree (make_call_1_list (f, a, l))
#define push_call_2(f,a,b)	 push_tree (make_call_2 (f, a, b))
#define push_call_3(f,a,b,c)	 push_tree (make_call_3 (f, a, b, c))
#define push_call_4(f,a,b,c,d)	 push_tree (make_call_4 (f, a, b, c, d))

#define push_inline_0(f)	 push_tree (make_inline_0 (f))
#define push_inline_1(f,a)	 push_tree (make_inline_1 (f, a))
#define push_inline_2(f,a,b)	 push_tree (make_inline_2 (f, a, b))
#define push_inline_3(f,a,b,c)	 push_tree (make_inline_3 (f, a, b, c))
#define push_inline_4(f,a,b,c,d) push_tree (make_inline_4 (f, a, b, c, d))

#define push_move(x,y)		 push_inline_2 (output_move, x, y)
#define push_handler(f,v,a,b)	 push_inline_4 (output_file_handler, f, (void *) v, a, b)

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

#define inspect_push(tag,a1,a2) \
  inspect_list = list_add (inspect_list, make_generic (tag, a1, a2))

struct cobc_program_spec program_spec;

static struct cobc_field *current_field;
static struct cobc_file_name *current_file_name;
static struct cobc_label_name *current_section, *current_paragraph;

static int call_mode;
static int inspect_mode;
static cobc_tree inspect_name;
static struct cobc_list *inspect_list;

static struct cobc_list *label_check_list;

static int warning_count = 0;
static int error_count = 0;

static int last_operator;
static cobc_tree last_lefthand;

static void register_predefined_name (cobc_tree *ptr, cobc_tree name);
static void resolve_predefined_names (void);

static void init_field (int level, cobc_tree field);
static void validate_field (struct cobc_field *p);
static void validate_field_tree (struct cobc_field *p);
static void finalize_file_name (struct cobc_file_name *f, struct cobc_field *records);
static void validate_label_name (struct cobc_label_name *p);

static void field_set_used (struct cobc_field *p);
static int builtin_switch_id (cobc_tree x);

static cobc_tree make_add (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_sub (cobc_tree f1, cobc_tree f2, int round);
static cobc_tree make_move (cobc_tree f1, cobc_tree f2, int round);
static struct cobc_list *make_corr (cobc_tree (*func)(), cobc_tree g1, cobc_tree g2, int opt, struct cobc_list *l);
static cobc_tree make_opt_cond (cobc_tree last, int type, cobc_tree this);
static cobc_tree make_cond_name (cobc_tree x);

static void redefinition_error (struct cobc_location *loc, cobc_tree x);
static void undefined_error (struct cobc_location *loc, struct cobc_word *w, cobc_tree parent);
static void ambiguous_error (struct cobc_location *loc, struct cobc_word *w);
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

%token <str>  FUNCTION_NAME
%token <pict> PICTURE_TOK
%token <tree> INTEGER_LITERAL NUMERIC_LITERAL NONNUMERIC_LITERAL
%token <tree> CLASS_NAME CONDITION_NAME MNEMONIC_NAME
%token <word> WORD LABEL_WORD

%token EQUAL GREATER LESS GE LE COMMAND_LINE ENVIRONMENT_VARIABLE ALPHABET
%token DATE DAY DAY_OF_WEEK TIME READ WRITE OBJECT_COMPUTER INPUT_OUTPUT
%token TO FOR IS ARE THRU THAN NO CANCEL ASCENDING DESCENDING ZERO
%token SOURCE_COMPUTER BEFORE AFTER RESERVE DECLARATIVES USE AND OR NOT
%token RIGHT JUSTIFIED SYNCHRONIZED SEPARATE BLOCK CODE_SET
%token TOK_INITIAL FIRST ALL LEADING OF IN BY STRING UNSTRING DEBUGGING
%token START DELETE PROGRAM GLOBAL EXTERNAL SIZE DELIMITED COLLATING SEQUENCE
%token GIVING INSPECT TALLYING REPLACING ON OFF POINTER OVERFLOW NATIVE
%token DELIMITER COUNT LEFT TRAILING CHARACTER FILLER OCCURS TIMES CLASS
%token ADD SUBTRACT MULTIPLY DIVIDE ROUNDED REMAINDER ERROR SIZE INDEX
%token REEL UNIT REMOVAL REWIND LOCK PADDING CRT CURSOR
%token FD REDEFINES TOK_FILE USAGE BLANK SIGN VALUE MOVE
%token PROGRAM_ID DIVISION CONFIGURATION SPECIAL_NAMES MEMORY ALTER
%token FILE_CONTROL I_O_CONTROL FROM SAME AREA EXCEPTION UNTIL
%token WORKING_STORAGE LINKAGE DECIMAL_POINT COMMA DUPLICATES WITH EXIT
%token LABEL RECORD RECORDS STANDARD STANDARD_1 STANDARD_2 VARYING OMITTED
%token CONTAINS CHARACTERS COMPUTE GO STOP RUN ACCEPT PERFORM RENAMES
%token IF ELSE SENTENCE LINE LINES PAGE OPEN CLOSE REWRITE SECTION SYMBOLIC
%token ADVANCING INTO AT END NEGATIVE POSITIVE SPACE NOT
%token CALL USING INVALID CONTENT QUOTE LOW_VALUE HIGH_VALUE
%token SELECT ASSIGN DISPLAY UPON SET UP DOWN SEARCH
%token ORGANIZATION ACCESS MODE KEY STATUS ALTERNATE SORT SORT_MERGE
%token SEQUENTIAL INDEXED DYNAMIC RANDOM RELATIVE WHEN TEST PROCEED
%token END_ACCEPT END_ADD END_CALL END_COMPUTE END_DELETE END_DISPLAY
%token END_DIVIDE END_EVALUATE END_IF END_MULTIPLY END_PERFORM END_READ
%token END_REWRITE END_SEARCH END_START END_STRING END_SUBTRACT END_UNSTRING
%token END_WRITE
%token THEN EVALUATE OTHER ALSO CONTINUE CURRENCY REFERENCE INITIALIZE
%token NUMERIC ALPHABETIC ALPHABETIC_LOWER ALPHABETIC_UPPER
%token DEPENDING CORRESPONDING CONVERTING OPTIONAL RETURNING
%token IDENTIFICATION ENVIRONMENT DATA PROCEDURE TRUE FALSE ANY
%token AUTHOR DATE_WRITTEN DATE_COMPILED INSTALLATION SECURITY
%token COMMON NEXT INPUT I_O OUTPUT EXTEND BINARY
%token ALPHANUMERIC ALPHANUMERIC_EDITED NUMERIC_EDITED NATIONAL NATIONAL_EDITED
%token SCREEN BELL BLINK ERASE EOL EOS HIGHLIGHT LOWLIGHT
%token REVERSE_VIDEO UNDERLINE COLUMN FOREGROUND_COLOR BACKGROUND_COLOR AUTO
%token SECURE REQUIRED FULL NUMBER PLUS MINUS

%type <gene> call_param write_option
%type <inum> flag_all flag_duplicates flag_optional flag_global
%type <inum> flag_not flag_next flag_rounded flag_separate
%type <inum> integer level_number start_operator display_upon screen_plus_minus
%type <inum> before_or_after perform_test replacing_option close_option
%type <inum> select_organization select_access_mode open_mode same_option
%type <inum> ascending_or_descending opt_from_integer opt_to_integer usage
%type <list> occurs_key_list occurs_index_list value_item_list
%type <list> data_name_list condition_name_list opt_value_list
%type <list> evaluate_subject_list evaluate_case evaluate_case_list
%type <list> evaluate_when_list evaluate_object_list
%type <list> label_name_list subscript_list number_list
%type <list> string_list string_delimited_list string_name_list
%type <list> unstring_delimited unstring_delimited_list unstring_into
%type <list> unstring_delimited_item unstring_into_item
%type <list> predefined_name_list qualified_predefined_word mnemonic_name_list
%type <list> file_name_list math_name_list math_edited_name_list
%type <list> call_param_list call_using expr_item_list
%type <list> initialize_replacing initialize_replacing_list
%type <list> special_name_class_item_list
%type <tree> special_name_class_item special_name_class_literal
%type <tree> add_to value_item field_description field_description_list
%type <tree> field_description_list_1 field_description_list_2
%type <tree> opt_screen_description_list screen_description_list
%type <tree> screen_description condition imperative_statement
%type <tree> evaluate_object evaluate_object_1
%type <tree> function subscript subref refmod on_or_off
%type <tree> search_varying search_at_end search_whens search_when
%type <tree> perform_procedure perform_sentence perform_option start_key
%type <tree> read_into read_key write_from field_name expr expr_1 expr_item
%type <tree> file_name opt_with_pointer occurs_index evaluate_subject
%type <tree> unstring_delimiter unstring_count unstring_tallying
%type <tree> at_end_sentence not_at_end_sentence
%type <tree> invalid_key_sentence not_invalid_key_sentence
%type <tree> opt_on_overflow_sentence opt_not_on_overflow_sentence
%type <tree> call_returning call_on_exception call_not_on_exception
%type <tree> opt_on_size_error_sentence opt_not_on_size_error_sentence
%type <tree> numeric_name numeric_edited_name group_name table_name class_name
%type <tree> program_name condition_name qualified_cond_name data_name
%type <tree> file_name record_name label_name mnemonic_name section_name name
%type <tree> qualified_name predefined_name
%type <tree> integer_value text_value value number
%type <tree> literal_or_predefined literal basic_literal figurative_constant
%type <tree> at_line_column line_number column_number
%type <word> qualified_word label_word undefined_word


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
    memset (&program_spec, 0, sizeof (struct cobc_program_spec));
    program_spec.input_handler = cobc_default_error_handler;
    program_spec.output_handler = cobc_default_error_handler;
    program_spec.i_o_handler = cobc_default_error_handler;
    program_spec.extend_handler = cobc_default_error_handler;
    label_check_list = NULL;
    /* init environment */
    cobc_in_procedure = 0;
    cob_decimal_point = '.';
    cob_currency_symbol = '$';
    /* init symbol table */
    init_word_table ();
    {
      cobc_tree rc = make_field_3 (lookup_user_word ("RETURN-CODE"),
				   "S9(9)", COBC_USAGE_INDEX);
      validate_field (COBC_FIELD (rc));
    }
  }
  identification_division
  environment_division
  data_division
  {
    /* check if all required identifiers are defined in DATA DIVISION */
    resolve_predefined_names ();
  }
  procedure_division
  _end_program
  {
    struct cobc_list *l;
    for (l = list_reverse (label_check_list); l; l = l->next)
      validate_label_name (l->item);
    program_spec.file_name_list = list_reverse (program_spec.file_name_list);
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
  {
    char *s;
    int converted = 0;
    for (s = $6->name; *s; s++)
      if (*s == '-')
	{
	  converted = 1;
	  *s = '_';
	}
    if (converted)
      yywarn (_("PROGRAM-ID is converted to `%s'"), $6->name);
    program_spec.program_id = $6->name;
  }
  identification_division_options
;
opt_program_parameter:
| _is TOK_INITIAL _program	{ program_spec.initial_program = 1; }
| _is COMMON _program		{ PENDING ("COMMON"); }
;
identification_division_options:
| identification_division_options identification_division_option
;
identification_division_option:
  AUTHOR '.' comment		{ IGNORE ("AUTHOR"); }
| DATE_WRITTEN '.' comment	{ IGNORE ("DATE-WRITTEN"); }
| DATE_COMPILED '.' comment	{ IGNORE ("DATE-COMPILED"); }
| INSTALLATION '.' comment	{ IGNORE ("INSTALLATION"); }
| SECURITY '.' comment		{ IGNORE ("SECURITY"); }
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
    yywarn (_("DEBUGGING MODE is ignored"));
    yywarn (_("use compiler option `-debug' instead"));
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
  SPECIAL_NAMES '.' opt_special_names
;
opt_special_names:
| special_name_list dot
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
| CURSOR _is predefined_name { PENDING ("CURSOR"); }
| CRT STATUS _is predefined_name { PENDING ("CRT STATUS"); }
;


/* Buildin name */

special_name_mnemonic:
  WORD
  {
    int n = lookup_builtin_word ($1->name);
    if (n == 0)
      yyerror_loc (&@1, _("unknown name `%s'"), $1->name);
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
    PENDING ("ALPHABET");
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
      list_add (program_spec.class_list, make_class ($2, $4));
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
      yyerror_loc (&@3, _("invalid currency sign `%s'"), s);
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
    current_file_name->organization = COB_ORG_SEQUENTIAL;
    current_file_name->access_mode = COB_ACCESS_SEQUENTIAL;
    current_file_name->optional = $3;
    current_file_name->handler = cobc_standard_error_handler;
    COBC_TREE_LOC (current_file_name) = @4;
    program_spec.file_name_list =
      cons (current_file_name, program_spec.file_name_list);
  }
  select_options '.'
  {
    /* check ASSIGN clause */
    if (current_file_name->assign == NULL)
      yyerror_loc (&@2, _("ASSIGN required for file `%s'"), $4->name);

    /* check KEY clause */
    switch (current_file_name->organization)
      {
      case COB_ORG_INDEXED:
	if (current_file_name->key == NULL)
	  yyerror_loc (&@2, _("RECORD KEY required for file `%s'"), $4->name);
	break;
      case COB_ORG_RELATIVE:
	if (current_file_name->access_mode != COB_ACCESS_SEQUENTIAL
	    && current_file_name->key == NULL)
	  yyerror_loc (&@2, _("RELATIVE KEY required for file `%s'"), $4->name);
	break;
      }
  }
;
select_options:
| select_options select_option
;
select_option:
  ASSIGN _to literal_or_predefined
  {
    if (COBC_PREDEFINED_P ($3))
      register_predefined_name (&current_file_name->assign, $3);
    else
      current_file_name->assign = $3;
  }
| RESERVE integer _area
  {
    IGNORE ("RESERVE");
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
| _file STATUS _is predefined_name
  {
    register_predefined_name (&current_file_name->file_status, $4);
  }
| PADDING _character _is literal_or_predefined
  {
    PENDING ("PADDING");
  }
| RECORD DELIMITER _is STANDARD_1
  {
    PENDING ("RECORD DELIMITER");
  }
| RELATIVE _key _is predefined_name
  {
    register_predefined_name (&current_file_name->key, $4);
  }
| RECORD _key _is predefined_name
  {
    register_predefined_name (&current_file_name->key, $4);
  }
| ALTERNATE RECORD _key _is predefined_name flag_duplicates
  {
    struct cobc_alt_key *p = malloc (sizeof (struct cobc_alt_key));
    register_predefined_name (&p->key, $5);
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
literal_or_predefined:
  literal
| predefined_name
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
  same_statement_list dot
;
same_statement_list:
| same_statement_list same_statement
;
same_statement:
  SAME same_option _area _for file_name_list
  {
    switch ($2)
      {
      case 0:
	PENDING ("SAME");
	break;
      case 1:
	PENDING ("SAME RECORD");
      }
  }
;
same_option:
  /* empty */			{ $$ = 0; }
| RECORD			{ $$ = 1; }
| SORT				{ $$ = 2; }
| SORT_MERGE			{ $$ = 3; }
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION dot
  file_section
  working_storage_section
  linkage_section
  screen_section
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
    finalize_file_name (current_file_name, COBC_FIELD ($7));
  }
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
    current_file_name->record_max = $3;
  }
| RECORD _contains integer _to integer _characters
  {
    current_file_name->record_min = $3;
    current_file_name->record_max = $5;
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  record_depending
  {
    current_file_name->record_min = $6;
    current_file_name->record_max = $7;
  }
;
record_depending:
| DEPENDING _on predefined_name
  {
    register_predefined_name (&current_file_name->record_depending, $3);
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
  DATA record_or_records undefined_word_list { IGNORE ("DATA RECORD"); }
;


/*
 * CODE-SET clause
 */

codeset_clause:
  CODE_SET _is WORD
  {
    PENDING ("CODE-SET");
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
  level_number field_name
  {
    $2->loc = @2;
    init_field ($1, $2);
  }
  field_options dot
  {
    validate_field (current_field);
    $$ = COBC_TREE (current_field);
  }
;
field_name:
  /* empty */			{ $$ = make_filler (); }
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
	undefined_error (&@2, $2, 0);
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
  _is GLOBAL			{ PENDING ("GLOBAL"); }
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
    current_field->pic = yylex_picture ("9(9)");
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
    current_field->f.have_occurs = 1;
  }
| OCCURS integer TO integer _times DEPENDING _on predefined_name
  occurs_keys occurs_indexed
  {
    current_field->occurs = $4;
    current_field->occurs_min = $2;
    register_predefined_name (&current_field->occurs_depending, $8);
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
	    register_predefined_name (&keys[i].key, p->x);
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
    program_spec.index_list = list_add (program_spec.index_list, $$);
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
	if (COBC_PAIR_P ($3->item))
	  current_field->value = COBC_PAIR ($3->item)->x;
	else
	  current_field->value = $3->item;
      }
    else
      {
	/* single VALUE */
	if ($3->next != NULL || COBC_PAIR_P ($3->item))
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


/*******************
 * SCREEN SECTION
 *******************/

screen_section:
| SCREEN SECTION dot
  {
    current_field = NULL;
  }
  opt_screen_description_list
  {
    struct cobc_field *p;
    for (p = COBC_FIELD ($5); p; p = p->sister)
      finalize_field_tree (p);
    program_spec.screen_storage = COBC_FIELD ($5);
    program_spec.enable_screen = 1;
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
  level_number field_name
  {
    $2->loc = @2;
    init_field ($1, $2);
    current_field->f.screen = 1;
    current_field->screen_flag |= COB_SCREEN_FG_NONE;
    current_field->screen_flag |= COB_SCREEN_BG_NONE;
  }
  screen_options dot
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
	yyerror_loc (&@3, _("invalid color `%d'"), $3);
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
	yyerror_loc (&@3, _("invalid color `%d'"), $3);
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
| PROCEDURE DIVISION procedure_using dot
  {
    current_section = NULL;
    current_paragraph = NULL;
    cobc_in_procedure = 1;
  }
  procedure_declaratives
  {
    struct cobc_label_name *label =
      COBC_LABEL_NAME (make_label_name (make_word ("$MAIN$")));
    label->need_begin = 1;
    push_label (label);
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
    struct cobc_list *l;
    for (l = $2; l; l = l->next)
      {
	struct cobc_field *p = COBC_FIELD (l->item);
	if (p->level != 01 && p->level != 77)
	  yyerror (_("`%s' not level 01 or 77"), p->word->name);
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
    cobc_location = @1;
    if (program_spec.enable_screen)
      {
	if (COBC_FIELD ($2)->f.screen)
	  {
	    cobc_tree line = $3 ? make_index (COBC_PAIR ($3)->x) : cobc_int1;
	    cobc_tree column = $3 ? make_index (COBC_PAIR ($3)->y) : cobc_int1;
	    push_call_3 ("cob_screen_accept", $2, line, column);
	  }
	else
	  yyerror_loc (&@2, "`%s' not defined in SCREEN SECTION",
		       tree_to_string ($2));
      }
    else
      {
	push_call_2 ("cob_accept", $2, make_integer (COB_SYSIN));
      }
  }
  _end_accept
| ACCEPT data_name FROM DATE
  {
    cobc_location = @1;
    push_call_1 ("cob_accept_date", $2);
  }
| ACCEPT data_name FROM DAY
  {
    cobc_location = @1;
    push_call_1 ("cob_accept_day", $2);
  }
| ACCEPT data_name FROM DAY_OF_WEEK
  {
    cobc_location = @1;
    push_call_1 ("cob_accept_day_of_week", $2);
  }
| ACCEPT data_name FROM TIME
  {
    cobc_location = @1;
    push_call_1 ("cob_accept_time", $2);
  }
| ACCEPT data_name FROM COMMAND_LINE
  {
    cobc_location = @1;
    push_call_1 ("cob_accept_command_line", $2);
  }
| ACCEPT data_name FROM ENVIRONMENT_VARIABLE value
  {
    cobc_location = @1;
    push_call_2 ("cob_accept_environment", $2, $5);
  }
| ACCEPT data_name FROM mnemonic_name
  {
    cobc_location = @1;
    if (COBC_BUILTIN ($4)->id == BUILTIN_CONSOLE
	|| COBC_BUILTIN ($4)->id == BUILTIN_SYSIN)
      push_call_2 ("cob_accept", $2, make_integer (COB_SYSIN));
    else
      yyerror_tree ($4, _("invalid input stream"));
  }
;
_end_accept: | END_ACCEPT ;

at_line_column:
  /* empty */			{ $$ = NULL; }
| _at line_number column_number { $$ = make_pair ($2, $3); }
| _at column_number line_number { $$ = make_pair ($3, $2); }
;
line_number:
  LINE _number integer_value	{ $$ = $3; }
;
column_number:
  COLUMN _number integer_value	{ $$ = $3; }
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
    /* ADD A B C TO X Y  -->  t = a + b + c; x += t; y += t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    cobc_location = @1;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    push_assign ($3, '+', e);
  }
| number_list add_to GIVING math_edited_name_list
  {
    /* ADD A B TO C GIVING X Y  -->  t = a + b + c; x = t; y = t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    cobc_location = @1;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    if ($2)
      e = make_expr (e, '+', $2);
    push_assign ($4, 0, e);
  }
| CORRESPONDING group_name _to group_name flag_rounded
  {
    cobc_location = @1;
    push_corr (make_add, $2, $4, $5);
  }
;
add_to:
  /* empty */			{ $$ = NULL; }
| TO value			{ $$ = $2; }
;
end_add: | END_ADD ;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options		{  OBSOLETE ("ALTER"); }
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
  CALL program_name		{ call_mode = COBC_CALL_BY_REFERENCE; }
  call_using call_returning call_on_exception call_not_on_exception
  {
    cobc_location = @1;
    push_inline_4 (output_call_statement, $2, $4, $6, $7);
    if ($5)
      push_move (cobc_return_code, $5);
  }
  _end_call
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
  value				{ $$ = make_generic_1 (call_mode, $1);}
| _by call_mode value		{ $$ = make_generic_1 (call_mode, $3);}
;
call_mode:
  REFERENCE			{ call_mode = COBC_CALL_BY_REFERENCE; }
| CONTENT			{ call_mode = COBC_CALL_BY_CONTENT; }
| VALUE				{ call_mode = COBC_CALL_BY_VALUE; }
;
call_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING data_name		{ $$ = $2; }
;
call_on_exception:
  /* empty */				{ $$ = NULL; }
| _on OVERFLOW imperative_statement	{ $$ = $3; }
| _on EXCEPTION imperative_statement	{ $$ = $3; }
;
call_not_on_exception:
  /* empty */				 { $$ = NULL; }
| NOT _on EXCEPTION imperative_statement { $$ = $4; }
;
_end_call: | END_CALL ;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL cancel_list
;
cancel_list:
| cancel_list program_name
  {
    cobc_location = @2;
    push_call_1 ("cob_cancel", $2);
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
  CLOSE close_list
;
close_list:
| close_list file_name close_option
  {
    cobc_location = @2;
    push_call_2 ("cob_close", $2, make_integer ($3));
    push_handler ($2, 0, 0, 0);
  }
;
close_option:
  /* empty */			{ $$ = COB_CLOSE_NORMAL; }
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
      yyerror (_("invalid expression"));
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
    cobc_location = @1;
    push_call_1 ("cob_delete", $2);
    $<tree>$ = $2;
  }
  opt_invalid_key
  _end_delete
;
_end_delete: | END_DELETE ;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY opt_value_list display_upon at_line_column
  {
    struct cobc_list *l;
    cobc_location = @1;
    if (program_spec.enable_screen)
      {
	for (l = $2; l; l = l->next)
	  if (COBC_FIELD (l->item)->f.screen)
	    {
	      cobc_tree line = $4 ? make_index (COBC_PAIR ($4)->x) : cobc_int1;
	      cobc_tree column = $4 ? make_index (COBC_PAIR ($4)->y) : cobc_int1;
	      push_call_3 ("cob_screen_display", l->item, line, column);
	    }
	  else
	    yyerror_loc (&@2, "`%s' not defined in SCREEN SECTION",
			 tree_to_string (l->item));
      }
    else
      {
	cobc_tree fd = make_integer ($3);
	for (l = $2; l; l = l->next)
	  push_inline_2 (output_display, l->item, fd);
      }
  }
  display_with_no_advancing
  _end_display
  ;
display_upon:
  /* empty */			{ $$ = COB_SYSOUT; }
| _upon mnemonic_name
  {
    switch (COBC_BUILTIN ($2)->id)
      {
      case BUILTIN_CONSOLE: $$ = COB_SYSOUT; break;
      case BUILTIN_SYSOUT:  $$ = COB_SYSOUT; break;
      case BUILTIN_SYSERR:  $$ = COB_SYSERR; break;
      default:
	yyerror (_("invalid UPON item"));
	$$ = COB_SYSOUT;
	break;
      }
  }
| UPON LABEL_WORD
  {
    yywarn (_("`%s' undefined in SPECIAL-NAMES"), $2->name);
    $$ = COB_SYSOUT;
  }
;
display_with_no_advancing:
  /* empty */
  {
    if (!program_spec.enable_screen)
      push_call_1 ("cob_newline", make_integer ($<inum>-2));
  }
| _with NO ADVANCING { /* nothing */ }
;
_end_display: | END_DISPLAY ;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE divide_body opt_on_size_error _end_divide
;
divide_body:
  number INTO math_name_list
  {
    cobc_location = @1;
    push_assign ($3, '/', $1);
  }
| number INTO number GIVING math_edited_name_list
  {
    cobc_location = @1;
    push_assign ($5, 0, make_expr ($3, '/', $1));
  }
| number BY number GIVING math_edited_name_list
  {
    cobc_location = @1;
    push_assign ($5, 0, make_expr ($1, '/', $3));
  }
| number INTO number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    cobc_location = @1;
    push_call_4 ("cob_div_quotient", $3, $1, $5, make_integer ($6));
    push_call_1 ("cob_div_remainder", $8);
  }
| number BY number GIVING numeric_edited_name flag_rounded REMAINDER numeric_edited_name
  {
    cobc_location = @1;
    push_call_4 ("cob_div_quotient", $1, $3, $5, make_integer ($6));
    push_call_1 ("cob_div_remainder", $8);
  }
;
_end_divide: | END_DIVIDE ;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE evaluate_subject_list evaluate_case_list _end_evaluate
  {
    cobc_location = @1;
    push_tree (make_evaluate ($2, $3));
  }
;

evaluate_subject_list:
  evaluate_subject		{ $$ = list ($1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $$ = list_add ($1, $3); }
;
evaluate_subject:
  expr				{ $$ = $1; }
| TRUE				{ $$ = cobc_true; }
| FALSE				{ $$ = cobc_false; }
;

evaluate_case_list:
  /* empty */			{ $$ = NULL; }
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
    $$ = $2;
    if ($1)
      {
	if ($2 == cobc_any || $2 == cobc_true || $2 == cobc_false)
	  yyerror (_("cannot use NOT with TRUE, FALSE, or ANY"));
	else
	  /* NOTE: $2 is not necessarily a condition, but
	   * we use COBC_COND_NOT here to store it, which
	   * is later expanded in output_evaluate_test. */
	  $$ = make_negative ($2);
      }
  }
;
evaluate_object_1:
  expr				{ $$ = $1; }
| expr THRU expr		{ $$ = make_pair ($1, $3); }
| ANY				{ $$ = cobc_any; }
| TRUE				{ $$ = cobc_true; }
| FALSE				{ $$ = cobc_false; }
;
_end_evaluate: | END_EVALUATE ;


/*
 * EXIT statement
 */

exit_statement:
  EXIT				{ /* nothing */ }
| EXIT PROGRAM
  {
    cobc_location = @1;
    push_call_0 ("cob_exit_program");
  }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to label_name_list
  {
    if ($3 == NULL)
      OBSOLETE ("GO TO without label");
    else if ($3->next)
      yyerror_loc (&@1, _("too many labels with GO TO"));
    else
      {
	cobc_location = @1;
	COBC_LABEL_NAME ($3->item)->need_begin = 1;
	push_inline_1 (output_goto, $3->item);
      }
  }
| GO _to label_name_list DEPENDING _on numeric_name
  {
    struct cobc_list *l;
    cobc_location = @1;
    for (l = $3; l; l = l->next)
      COBC_LABEL_NAME (l->item)->need_begin = 1;
    push_inline_2 (output_goto_depending, $3, $6);
  }
;


/*
 * IF statement
 */

if_statement:
  IF condition _then imperative_statement _end_if
  {
    cobc_location = @1;
    push_tree (make_if ($2, $4, NULL));
  }
| IF condition _then imperative_statement ELSE imperative_statement _end_if
  {
    cobc_location = @1;
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
    cobc_location = @1;
    for (l = $2; l; l = l->next)
      if (!$3)
	push_inline_1 (output_initialize, l->item);
      else
	push_inline_2 (output_initialize_replacing, l->item, $3);
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
| INSPECT data_name inspect_replacing
| INSPECT data_name inspect_converting
| INSPECT data_name inspect_tallying { $<tree>$ = $2; } inspect_replacing
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING
  {
    inspect_list = NULL;
    inspect_name = 0;
    inspect_mode = 0;
    inspect_push (COB_INSPECT_TALLYING, 0, 0);
  }
  tallying_list
  {
    cobc_location = @1;
    push_call_1_list ("cob_inspect", $<tree>0, inspect_list);
  }
;
tallying_list:
  tallying_item
| tallying_list tallying_item
;
tallying_item:
  data_name FOR
  {
    inspect_name = $1;
  }
| CHARACTERS inspect_before_after_list
  {
    inspect_mode = 0;
    if (inspect_name == 0)
      yyerror_loc (&@1, _("data name expected before CHARACTERS"));
    else
      inspect_push (COB_INSPECT_CHARACTERS, inspect_name, 0);
  }
| ALL
  {
    if (inspect_name == 0)
      yyerror_loc (&@1, _("data name expected before ALL"));
    inspect_mode = COB_INSPECT_ALL;
  }
| LEADING
  {
    if (inspect_name == 0)
      yyerror_loc (&@1, _("data name expected before LEADING"));
    inspect_mode = COB_INSPECT_LEADING;
  }
| text_value inspect_before_after_list
  {
    if (inspect_mode == 0)
      yyerror_loc (&@1, _("ALL or LEADING expected before `%s'"),
		   tree_to_string ($1));
    else
      inspect_push (inspect_mode, inspect_name, $1);
  }
;

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING
  {
    inspect_list = NULL;
    inspect_push (COB_INSPECT_REPLACING, 0, 0);
  }
  replacing_list
  {
    cobc_location = @1;
    push_call_1_list ("cob_inspect", $<tree>0, inspect_list);
  }
;
replacing_list:
  replacing_item
| replacing_list replacing_item
;
replacing_item:
  CHARACTERS BY value inspect_before_after_list
  {
    inspect_push (COB_INSPECT_CHARACTERS, $3, 0);
  }
| ALL value BY value inspect_before_after_list
  {
    inspect_push (COB_INSPECT_ALL, $4, $2);
  }
| LEADING value BY value inspect_before_after_list
  {
    inspect_push (COB_INSPECT_LEADING, $4, $2);
  }
| FIRST value BY value inspect_before_after_list
  {
    inspect_push (COB_INSPECT_FIRST, $4, $2);
  }
;

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING
  {
    inspect_list = NULL;
  }
  value TO value inspect_before_after_list
  {
    cobc_location = @1;
    inspect_push (COB_INSPECT_CONVERTING, $3, $5);
    push_call_1_list ("cob_inspect", $<tree>0, inspect_list);
  }
;

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* empty */
  {
    inspect_push (COB_INSPECT_INIT, 0, 0);
  }
| inspect_before_after_list
  BEFORE _initial value
  {
    inspect_push (COB_INSPECT_BEFORE, $4, 0);
  }
| inspect_before_after_list
  AFTER _initial value
  {
    inspect_push (COB_INSPECT_AFTER, $4, 0);
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
    cobc_location = @1;
    for (l = $4; l; l = l->next)
      push_move ($2, l->item);
  }
| MOVE CORRESPONDING group_name TO group_name
  {
    cobc_location = @1;
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
    cobc_location = @1;
    push_assign ($3, '*', $1);
  }
| number BY number GIVING math_edited_name_list
  {
    cobc_location = @1;
    push_assign ($5, 0, make_expr ($1, '*', $3));
  }
;
_end_multiply: | END_MULTIPLY ;


/*
 * OPEN statement
 */

open_statement:
  OPEN open_list
;
open_list:
| open_list open_mode file_name_list
  {
    struct cobc_list *l;
    cobc_location = @2;
    for (l = $3; l; l = l->next)
      {
	struct cobc_file_name *p = COBC_FILE_NAME (l->item);
	push_call_2 ("cob_open", p, make_integer ($2));
	push_handler (p, 0, 0, 0);
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
    COBC_LABEL_NAME (COBC_PAIR ($2)->x)->need_begin = 1;
    COBC_LABEL_NAME (COBC_PAIR ($2)->y)->need_return = 1;
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
  label_name			{ $$ = make_pair ($1, $1); }
| label_name THRU label_name	{ $$ = make_pair ($1, $3); }
;

perform_option:
  /* empty */
  {
    cobc_location = @0;
    $$ = make_perform (COBC_PERFORM_ONCE);
  }
| integer_value TIMES
  {
    cobc_location = @1;
    $$ = make_perform (COBC_PERFORM_TIMES);
    COBC_PERFORM ($$)->data = $1;
  }
| perform_test UNTIL condition
  {
    cobc_location = @1;
    $$ = make_perform (COBC_PERFORM_UNTIL);
    COBC_PERFORM ($$)->test = $1;
    add_perform_varying (COBC_PERFORM ($$), 0, 0, 0, $3);
  }
| perform_test VARYING numeric_name FROM value BY value UNTIL condition
  {
    cobc_location = @1;
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
  /* empty */			{ $$ = COBC_BEFORE; }
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
    struct cobc_file_name *f = COBC_FILE_NAME ($2);
    cobc_location = @1;
    if ($3 || f->access_mode == COB_ACCESS_SEQUENTIAL)
      {
	/* READ NEXT */
	if ($6)
	  yywarn (_("KEY ignored with sequential READ"));
	push_call_1 ("cob_read_next", $2);
      }
    else
      {
	/* READ */
	push_call_2 ("cob_read", $2, $6 ? $6 : f->key);
      }
    if ($5)
      push_move (COBC_TREE (f->record), $5);
    $<tree>$ = $2;
  }
  read_handler
  _end_read
;
read_into:
  /* empty */			{ $$ = NULL; }
| INTO data_name		{ $$ = $2; }
;
read_key:
  /* empty */			{ $$ = NULL; }
| KEY _is data_name		{ $$ = $3; }
;
read_handler:
  /* empty */
  {
    push_handler ($<tree>0, 0, 0, 0);
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
    cobc_location = @1;
    $<tree>$ = COBC_TREE (COBC_FIELD ($2)->file);
    if ($3)
      push_move ($3, $2);
    push_call_2 ("cob_rewrite", $<tree>$, $2);
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
    cobc_location = @1;
    push_inline_4 (output_search, $2, $3, $4, $5);
  }
| SEARCH ALL table_name search_at_end search_when _end_search
  {
    cobc_tree this, next = COBC_IF ($5)->test;
    struct cobc_field *p = COBC_FIELD ($3);
    cobc_location = @1;
    while (next != NULL)
      {
	if (COBC_COND (next)->type == COBC_COND_AND)
	  {
	    this = COBC_COND (next)->right;
	    next = COBC_COND (next)->left;
	  }
	else
	  {
	    this = next;
	    next = NULL;
	  }
	if (COBC_COND (this)->type == COBC_COND_EQ)
	  {
	    int i;
	    struct cobc_field *f = COBC_FIELD (COBC_COND (this)->left);
	    for (i = 0; i < p->nkeys; i++)
	      if (f == COBC_FIELD (p->keys[i].key))
		break;
	    if (i == p->nkeys)
	      yyerror_loc (&this->loc, _("undeclared key `%s'"),
			   tree_to_string (COBC_TREE (f)));
	  }
	else
	  {
	    yyerror_loc (&this->loc, _("condition not allowed in SEARCH ALL"));
	    break;
	  }
      }
    push_inline_3 (output_search_all, $3, $4, $5);
  }
;
search_varying:
  /* empty */			{ $$ = NULL; }
| VARYING data_name		{ $$ = $2; }
;
search_at_end:
  /* empty */			{ $$ = NULL; }
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
    cobc_location = @1;
    for (l = $2; l; l = l->next)
      push_move ($4, l->item);
  }
| SET data_name_list UP BY number
  {
    struct cobc_list *l;
    cobc_location = @1;
    for (l = $2; l; l = l->next)
      push_tree (make_op_assign (l->item, '+', $5));
  }
| SET data_name_list DOWN BY number
  {
    struct cobc_list *l;
    cobc_location = @1;
    for (l = $2; l; l = l->next)
      push_tree (make_op_assign (l->item, '-', $5));
  }
| SET condition_name_list TO TRUE
  {
    struct cobc_list *l;
    cobc_location = @1;
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
    cobc_location = @1;
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
  START file_name { $<inum>$ = COB_EQ; } start_key
  {
    cobc_location = @1;
    if ($4 == NULL)
      $4 = COBC_FILE_NAME ($2)->key;
    push_call_3 ("cob_start", $2, make_integer ($<inum>3), $4);
    $<tree>$ = $2;
  }
  opt_invalid_key
  _end_start
;
start_key:
  /* empty */			{ $$ = NULL; }
| KEY _is start_operator data_name
  {
    switch ($3)
      {
      case COBC_COND_EQ: $<inum>0 = COB_EQ; break;
      case COBC_COND_LT: $<inum>0 = COB_LT; break;
      case COBC_COND_LE: $<inum>0 = COB_LE; break;
      case COBC_COND_GT: $<inum>0 = COB_GT; break;
      case COBC_COND_GE: $<inum>0 = COB_GE; break;
      case COBC_COND_NE: $<inum>0 = COB_NE; break;
      }
    $$ = $4;
  }
;
start_operator:
  flag_not equal		{ $$ = $1 ? COBC_COND_NE : COBC_COND_EQ; }
| flag_not greater		{ $$ = $1 ? COBC_COND_LE : COBC_COND_GT; }
| flag_not less			{ $$ = $1 ? COBC_COND_GE : COBC_COND_LT; }
| flag_not greater_or_equal	{ $$ = $1 ? COBC_COND_LT : COBC_COND_GE; }
| flag_not less_or_equal	{ $$ = $1 ? COBC_COND_GT : COBC_COND_LE; }
;
_end_start: | END_START ;


/*
 * STOP statement
 */

stop_statement:
  STOP RUN
  {
    cobc_location = @1;
    push_call_0 ("cob_stop_run");
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
  STRING string_list INTO data_name opt_with_pointer
  {
    cobc_location = @1;
    if ($5)
      $2 = cons (make_generic_1 (COB_STRING_WITH_POINTER, $5), $2);
    push_call_1_list ("cob_string", $4, $2);
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
  /* empty */			{ $$ = NULL; }
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
    /* SUBTRACT A B C FROM X Y  -->  t = a + b + c; x -= t; y -= t; */
    struct cobc_list *l;
    cobc_tree e = $1->item;
    cobc_location = @1;
    for (l = $1->next; l; l = l->next)
      e = make_expr (e, '+', l->item);
    push_assign ($3, '-', e);
  }
| number_list FROM number GIVING math_edited_name_list
  {
    /* SUBTRACT A B FROM C GIVING X Y  -->  t = c - a - b; x = t; y = t */
    struct cobc_list *l;
    cobc_tree e = $3;
    cobc_location = @1;
    for (l = $1; l; l = l->next)
      e = make_expr (e, '-', l->item);
    push_assign ($5, 0, e);
  }
| CORRESPONDING group_name FROM group_name flag_rounded
  {
    cobc_location = @1;
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
    cobc_location = @1;
    if ($6)
      $3 = cons (make_generic_1 (COB_UNSTRING_WITH_POINTER, $6), $3);
    if ($7)
      $5 = list_add ($5, make_generic_1 (COB_UNSTRING_TALLYING, $7));
    push_call_1_list ("cob_unstring", $2, list_append ($3, $5));
  }
  opt_on_overflow
  _end_unstring
;

unstring_delimited:
  /* empty */			{ $$ = NULL; }
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
  /* empty */			{ $$ = NULL; }
| DELIMITER _in data_name	{ $$ = $3; }
;
unstring_count:
  /* empty */			{ $$ = NULL; }
| COUNT _in data_name		{ $$ = $3; }
;

unstring_tallying:
  /* empty */			{ $$ = NULL; }
| TALLYING _in data_name	{ $$ = $3; }
;
_end_unstring: | END_UNSTRING ;


/*
 * WRITE statement
 */

write_statement:
  WRITE record_name write_from write_option
  {
    cobc_location = @1;
    $<tree>$ = COBC_TREE (COBC_FIELD ($2)->file);
    /* AFTER ADVANCING */
    if ($4 && $4->type == COBC_AFTER)
      {
	if ($4->x)
	  push_call_2 ("cob_write_lines", $<tree>$, make_index ($4->x));
	else
	  push_call_1 ("cob_write_page", $<tree>$);
      }
    /* WRITE */
    if ($3)
      push_move ($3, $2);
    push_call_2 ("cob_write", $<tree>$, $2);
    /* BEFORE ADVANCING */
    if ($4 && $4->type == COBC_BEFORE)
      {
	if ($4->x)
	  push_call_2 ("cob_write_lines", $<tree>$, make_index ($4->x));
	else
	  push_call_1 ("cob_write_page", $<tree>$);
      }
  }
  opt_invalid_key
  _end_write
;
write_from:
  /* empty */			{ $$ = NULL; }
| FROM value			{ $$ = $2; }
;
write_option:
  /* empty */			{ $$ = NULL; }
| before_or_after _advancing integer_value _line_or_lines
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
_line_or_lines: | LINE | LINES ;
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
  /* empty */				  { $$ = NULL; }
| _on SIZE ERROR imperative_statement	  { $$ = $4; }
;
opt_not_on_size_error_sentence:
  /* empty */				  { $$ = NULL; }
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
  /* empty */				{ $$ = NULL; }
| _on OVERFLOW imperative_statement	{ $$ = $3; }
;
opt_not_on_overflow_sentence:
  /* empty */				{ $$ = NULL; }
| NOT _on OVERFLOW imperative_statement	{ $$ = $4; }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
    push_handler ($<tree>0, 1, $1, 0);
  }
| not_at_end_sentence
  {
    push_handler ($<tree>0, 1, 0, $1);
  }
| at_end_sentence not_at_end_sentence
  {
    push_handler ($<tree>0, 1, $1, $2);
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
  /* empty */
  {
    push_handler ($<tree>0, 2, 0, 0);
  }
| invalid_key
;
invalid_key:
  invalid_key_sentence
  {
    push_handler ($<tree>0, 2, $1, 0);
  }
| not_invalid_key_sentence
  {
    push_handler ($<tree>0, 2, 0, $1);
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    push_handler ($<tree>0, 2, $1, $2);
  }
;
invalid_key_sentence:
  INVALID _key imperative_statement	{ $$ = $3; }
;
not_invalid_key_sentence:
  NOT INVALID _key imperative_statement	{ $$ = $4; }
;


/*******************
 * Expressions
 *******************/

condition:
  expr
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

    int reduce (int prio)
      {
	while (i >= 2 && stack[i-2].token != VALUE && stack[i-2].prio <= prio)
	  {
	    int token = stack[i-2].token;
	    if (stack[i-1].token != VALUE
		&& stack[i-1].token != COBC_COND_AND
		&& stack[i-1].token != COBC_COND_OR)
	      return -1;
	    switch (token)
	      {
	      case '+': case '-': case '*': case '/': case '^':
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  make_expr (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      case COBC_COND_NOT:
		if (!COBC_COND_P (stack[i-1].value))
		  stack[i-1].value =
		    make_cond (last_lefthand, last_operator, stack[i-1].value);
		stack[i-2].token = VALUE;
		stack[i-2].value = make_negative (stack[i-1].value);
		i -= 1;
		break;
	      case COBC_COND_AND:
	      case COBC_COND_OR:
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		if (!COBC_COND_P (stack[i-1].value))
		  stack[i-1].value =
		    make_cond (last_lefthand, last_operator, stack[i-1].value);
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  make_cond (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      default:
		if (stack[i-3].token == COBC_COND_AND
		    || stack[i-3].token == COBC_COND_OR)
		  {
		    last_operator = token;
		    stack[i-2].token = VALUE;
		    stack[i-2].value =
		      make_cond (last_lefthand, token, stack[i-1].value);
		    i -= 1;
		  }
		else
		  {
		    last_lefthand = stack[i-3].value;
		    last_operator = token;
		    stack[i-3].token = VALUE;
		    stack[i-3].value =
		      make_cond (last_lefthand, token, stack[i-1].value);
		    i -= 2;
		  }
		break;
	      }
	  }

	/* handle special case "cmp OR x AND" */
	if (i >= 2
	    && prio == 7
	    && stack[i-2].token == COBC_COND_OR
	    && !COBC_COND_P (stack[i-1].value))
	  {
	    stack[i-1].token = VALUE;
	    stack[i-1].value =
	      make_cond (last_lefthand, last_operator, stack[i-1].value);
	  }
	return 0;
      }

    int shift (int prio, int token, cobc_tree value)
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
			make_expr (cobc_zero, '-', l->next->item);
		      break;
		    }
		  /* fall through */
		case '+':
		  SHIFT (4, token, 0);
		  break;

		  /* conditional operator */
		case '=':
		  SHIFT (5, COBC_COND_EQ, 0);
		  break;
		case '<':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, COBC_COND_LE, 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, COBC_COND_LT, 0);
		  break;
		case '>':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, COBC_COND_GE, 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, COBC_COND_GT, 0);
		  break;
		case LE:
		  SHIFT (5, COBC_COND_LE, 0);
		  break;
		case GE:
		  SHIFT (5, COBC_COND_GE, 0);
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
		    if (i > 0 && stack[i-1].token == COBC_COND_NOT)
		      {
			not_flag = 1;
			i--;
		      }
		    reduce (5);
		    if (i > 0 && stack[i-1].token == VALUE)
		      {
			int cond;
			cobc_tree val;
			switch (token)
			  {
			  case ZERO:
			    cond = COBC_COND_EQ;
			    val = cobc_zero;
			    break;
			  case POSITIVE:
			    cond = COBC_COND_GT;
			    val = cobc_zero;
			    break;
			  case NEGATIVE:
			    cond = COBC_COND_LT;
			    val = cobc_zero;
			    break;
			  default:
			    cond = COBC_COND_CLASS;
			    val = COBC_TREE (class_func);
			    break;
			  }
			stack[i-1].value =
			  make_cond (stack[i-1].value, cond, val);
			if (not_flag)
			  stack[i-1].value =make_negative (stack[i-1].value);
			break;
		      }
		    goto error;
		  }

		  /* logical operator */
		case NOT:
		  switch (look_ahead (l->next))
		    {
		    case '=': SHIFT (5, COBC_COND_NE, 0); l = l->next; break;
		    case '<': SHIFT (5, COBC_COND_GE, 0); l = l->next; break;
		    case '>': SHIFT (5, COBC_COND_LE, 0); l = l->next; break;
		    case LE:  SHIFT (5, COBC_COND_GT, 0); l = l->next; break;
		    case GE:  SHIFT (5, COBC_COND_LT, 0); l = l->next; break;
		    default:  SHIFT (6, COBC_COND_NOT, 0); break;
		    }
		  break;
		case AND: SHIFT (7, COBC_COND_AND, 0); break;
		case OR:  SHIFT (8, COBC_COND_OR, 0); break;
		}
	      break;
	    }
	  default:
	    if (x == cobc_zero)
	      if (stack[i-1].token == VALUE
		  || stack[i-1].token == COBC_COND_NOT)
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
	yyerror_tree ($1->item, _("invalid expression"));
	YYERROR;
      }

    $$ = stack[0].value;
  }
;

expr_item_list:
  expr_item			{ cobc_location = @1; $$ = list ($1); }
| expr_item_list IS		{ $$ = $1; }
| expr_item_list expr_item	{ $$ = list_add ($1, $2); }
;
expr_item:
  value				{ $$ = $1; }
| '(' expr_1 ')'		{ $$ = $2; }
| condition_name		{ $$ = make_cond_name ($1); }
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
| class_name			{ $$ = $1; }
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
      yyerror_loc (&@1, _("`%s' not numeric"), tree_to_string ($1));
    $$ = $1;
  }
;

/* Numeric edited name */

numeric_edited_name:
  data_name
  {
    int category = COBC_FIELD ($1)->category;
    if (category != COB_NUMERIC && category != COB_NUMERIC_EDITED)
      yyerror_loc (&@1, _("`%s' not numeric or numeric edited"),
		   tree_to_string ($1));
    $$ = $1;
  }
;

/* Group name */

group_name:
  data_name
  {
    if (COBC_FIELD ($1)->children == NULL)
      yyerror_loc (&@1, _("`%s' not a group"), tree_to_string ($1));
    $$ = $1;
  }
;

/* Table name */

table_name:
  name
  {
    if (!COBC_FIELD ($1)->index_list)
      {
	yyerror_loc (&@1, _("`%s' not indexed"), tree_to_string ($1));
	yyerror_tree ($1, _("defined here"));
      }
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
      ambiguous_error (&@1, COBC_FIELD ($1)->word);
    $$ = $1;
    field_set_used (COBC_FIELD ($$)->parent);
  }
| CONDITION_NAME in_of qualified_name
  {
    struct cobc_word *w = COBC_FIELD ($1)->word;
    struct cobc_word *qw = lookup_qualified_word (w, COBC_FIELD ($3));
    $$ = $1;
    if (!qw)
      undefined_error (&@1, w, $3);
    else
      {
	$$ = qw->item;
	field_set_used (COBC_FIELD ($$)->parent);
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
	  yyerror_loc (&@1, _("`%s' must be subscripted"),
		       tree_to_string ($1));
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
      yyerror_loc (&@1, _("`%s' not file name"), tree_to_string ($1));
    $$ = $1;
  }
;

/* Record name */

record_name:
  name
  {
    if (!COBC_FIELD_P ($1) || !COBC_FIELD ($1)->file)
      yyerror_loc (&@1, _("`%s' not record name"), tree_to_string ($1));
    $$ = $1;
  }
;

/* Level number */

level_number:
  integer
  {
    $$ = $1;
    if ($1 < 01 || ($1 > 49 && $1 != 66 && $1 != 77 && $1 != 88))
      {
	yyerror_loc (&@1, _("invalid level number `%02d'"), $1);
	$$ = 01;
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
	redefinition_error (&@1, $1->item);
	$$ = $1->item;
      }
    else
      {
	cobc_location = @1;
	$$ = make_label_name ($1);
      }
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
	undefined_error (&@1, $1, 0);
	$$ = make_filler ();
      }
  }
;
qualified_word:
  WORD
  {
    $$ = $1;
    if ($1->count > 1)
      ambiguous_error (&@1, $1);
  }
| WORD in_of qualified_name
  {
    $$ = lookup_qualified_word ($1, COBC_FIELD ($3));
    if (!$$)
      {
	undefined_error (&@1, $1, $3);
	$$ = $1;
      }
  }
;
subref:
 '(' subscript_list ')'
  {
    int need = COBC_FIELD ($<tree>0)->indexes;
    int given = list_length ($2);
    if (given != need)
      {
	const char *name = tree_to_string ($<tree>0);
	switch (need)
	  {
	  case 0:
	    yyerror_loc (&@0, _("`%s' cannot be subscripted"), name);
	    break;
	  case 1:
	    yyerror_loc (&@0, _("`%s' requires one subscript"), name);
	    break;
	  default:
	    yyerror_loc (&@0, _("`%s' requires %d subscripts"), name, need);
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

label_name_list:
  label_name			{ $$ = list ($1); }
| label_name_list label_name	{ $$ = list_add ($1, $2); }
;
label_name:
  label_word
  {
    cobc_location = @1;
    $$ = make_label_name_nodef ($1, 0);
    COBC_LABEL_NAME ($$)->section = current_section;
    label_check_list = cons ($$, label_check_list);
  }
| label_word in_of label_word
  {
    cobc_location = @1;
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
  qualified_predefined_word
  {
    cobc_location = @1;
    $$ = make_predefined ($1);
  }
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
      redefinition_error (&@1, $1->item);
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
      yyerror_loc (&@1, _("numeric value is expected `%s'"),
		   tree_to_string ($1));
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

/* Text */

text_value:
  data_name
| NONNUMERIC_LITERAL
| figurative_constant
;


/*
 * Primitive value
 */

opt_value_list:
  /* empty */			{ $$ = NULL; }
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
    PENDING ("FUNCTION");
    YYABORT;
  }
;


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
| /* empty */
  {
    yywarn (_("`.' is expected after `%s'"), cobc_last_text);
  }
;


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

static struct predefined_record {
  cobc_tree *ptr;
  cobc_tree name;
  struct predefined_record *next;
} *predefined_list = NULL;

static void
register_predefined_name (cobc_tree *ptr, cobc_tree name)
{
  struct predefined_record *p = malloc (sizeof (struct predefined_record));
  *ptr = name;
  p->ptr = ptr;
  p->name = name;
  p->next = predefined_list;
  predefined_list = p;
}

static cobc_tree
resolve_predefined_name (cobc_tree x)
{
  cobc_tree name;
  struct cobc_list *l = COBC_PREDEFINED (x)->words;
  struct cobc_word *p = l->item;
  struct cobc_location *loc = &COBC_TREE_LOC (x);
  if (p->count == 0)
    {
      undefined_error (loc, p, 0);
      return make_filler ();
    }
  else if (p->count > 1)
    {
      ambiguous_error (loc, p);
      return make_filler ();
    }

  name = p->item;
  for (l = l->next; l; l = l->next)
    {
      struct cobc_word *w = l->item;
      p = lookup_qualified_word (w, COBC_FIELD (name));
      if (!p)
	{
	  undefined_error (loc, w, name);
	  return make_filler ();
	}
      name = p->item;
    }
  field_set_used (COBC_FIELD (name));
  return name;
}

static void
resolve_predefined_names (void)
{
  while (predefined_list)
    {
      struct predefined_record *p = predefined_list;
      *p->ptr = resolve_predefined_name (p->name);
      predefined_list = p->next;
      free (p);
    }
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

  if (level == 01 || level == 77)
    {
      if (last_field)
	field_founder (last_field)->sister = current_field;
    }
  else if (!last_field)
    {
      yyerror (_("level number must begin with 01 or 77"));
    }
  else if (last_field->level == 77 && level != 88)
    {
      yyerror (_("level 77 item may include only 88 items"));
    }
  else if (level == 66)
    {
      struct cobc_field *p;
      current_field->parent = field_founder (last_field);
      for (p = current_field->parent->children; p->sister; p = p->sister);
      p->sister = current_field;
    }
  else if (level == 88)
    {
      current_field->parent = last_field;
    }
  else if (level > last_field->level)
    {
      /* lower level */
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
	      redefinition_error (&COBC_TREE_LOC (current_field),
				  COBC_TREE (p));
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
      yyerror (_("field hierarchy broken"));
    }

  /* inherit parent's properties */
  if (current_field->parent)
    {
      current_field->usage = current_field->parent->usage;
      current_field->f.sign_leading = current_field->parent->f.sign_leading;
      current_field->f.sign_separate = current_field->parent->f.sign_separate;
      current_field->f.in_redefines = current_field->parent->f.in_redefines;
      current_field->screen_flag |= current_field->parent->screen_flag;
    }
}

static void
validate_field (struct cobc_field *p)
{
  cobc_tree x = COBC_TREE (p);
  if (p->level == 88)
    {
      /* conditional variable */
      COBC_TREE_CLASS (p) = COB_BOOLEAN;
      if (p->pic)
	yyerror_tree (x, _("level 88 field cannot have PICTURE"));
    }
  else
    {
      /* validate REDEFINES */
      if (p->redefines)
	current_field->f.in_redefines = 1;

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
	  yyerror_tree (x, _("level %02d field cannot have OCCURS"), p->level);

      /* validate JUSTIFIED RIGHT */
      if (p->f.justified)
	{
	  char c = p->category;
	  if (!(c == 'A' || c == 'X' || c == 'N'))
	    yyerror_tree (x, _("cannot have JUSTIFIED RIGHT"));
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
	      yyerror_tree (x, _("VALUE not allowed under REDEFINES"));
	    }
	  else if (p->value == cobc_zero)
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
	      yywarn_tree (x, _("VALUE should be non-numeric"));
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
  if (p->children)
    {
      /* group */
      COBC_TREE_CLASS (p) = COB_ALPHANUMERIC;

      if (p->f.justified)
	yyerror (_("group item cannot have JUSTIFIED RIGHT"));

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
	  if (p->category != COB_NUMERIC)
	    yywarn (_("field must be numeric"));
	  break;
	case COBC_USAGE_INDEX:
	  COBC_TREE_CLASS (p) = COB_NUMERIC;
	  break;
	}

      if (!p->pic)
	{
	  if (p->usage != COBC_USAGE_INDEX)
	    yyerror_tree (COBC_TREE (p), _("must have PICTURE"));
	  p->pic = make_picture ();
	}
    }
}

static void
finalize_file_name (struct cobc_file_name *f, struct cobc_field *records)
{
  char pic[BUFSIZ];
  struct cobc_field *p;

  for (p = records; p; p = p->sister)
    {
      /* check the record size */
      if (f->record_min > 0)
	if (p->size < f->record_min)
	  yyerror (_("record size too small `%s'"), p->word->name);
      if (f->record_max > 0)
	if (p->size > f->record_max)
	  yyerror (_("record size too large `%s'"), p->word->name);
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
  f->record = COBC_FIELD (make_field_3 (f->word, pic, COBC_USAGE_DISPLAY));
  field_set_used (f->record);
  validate_field (f->record);
  f->record->sister = records;
  f->word->count--;

  for (p = records; p; p = p->sister)
    {
      p->file = f;
      p->redefines = f->record;
      field_set_used (p);
    }
}

static struct cobc_label_name *
lookup_label (struct cobc_word *w, struct cobc_label_name *section)
{
  for (; w; w = w->link)
    if (w->item
	&& COBC_LABEL_NAME_P (w->item)
	&& section == COBC_LABEL_NAME (w->item)->section)
      return COBC_LABEL_NAME (w->item);

  yyerror (_("`%s' undefined in section `%s'"), w->name, section->word->name);
  return NULL;
}

static void
validate_label_name (struct cobc_label_name *p)
{
  struct cobc_label_name *label = NULL;

  if (p->in_word)
    {
      /* LABEL IN LABEL */
      if (p->in_word->count == 0)
	yyerror (_("no such section `%s'"), p->in_word->name);
      else if (!COBC_LABEL_NAME_P (p->in_word->item))
	yyerror (_("invalid section name `%s'"), p->in_word->name);
      else
	label = lookup_label (p->word, COBC_LABEL_NAME (p->in_word->item));
    }
  else
    {
      /* LABEL */
      if (p->word->count == 1 && COBC_LABEL_NAME_P (p->word->item))
	label = COBC_LABEL_NAME (p->word->item);
      else if (p->word->count > 0 && p->section)
	label = lookup_label (p->word, p->section);
      else
	yyerror (_("no such section `%s'"), p->word->name);
    }

  if (label != NULL)
    {
      p->cname = label->cname;
      label->need_begin |= p->need_begin;
      label->need_return |= p->need_return;
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
      yyerror (_("not switch name"));
      return -1;
    }
}


static cobc_tree
make_add (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 ("cob_add", f2, f1, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_sub (cobc_tree f1, cobc_tree f2, int round)
{
  return make_call_3 ("cob_sub", f2, f1, round ? cobc_int1 : cobc_int0);
}

static cobc_tree
make_move (cobc_tree f1, cobc_tree f2, int round)
{
  return make_inline_2 (output_move, f1, f2);
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
      yyerror (_("broken condition"));
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
redefinition_error (struct cobc_location *loc, cobc_tree x)
{
  yywarn_loc (loc, _("redefinition of `%s'"), tree_to_string (x));
  yywarn_tree (x, _("previously defined here"));
}

static void
undefined_error (struct cobc_location *loc, struct cobc_word *w, cobc_tree parent)
{
  if (parent)
    yyerror_loc (loc, _("`%s' undefined in `%s'"),
		 w->name, tree_to_string (parent));
  else
    yyerror_loc (loc, _("`%s' undefined"), w->name);
}

static void
ambiguous_error (struct cobc_location *loc, struct cobc_word *w)
{
  if (w->error == 0)
    {
      /* on first time */
      yyerror_loc (loc, _("`%s' ambiguous; need qualification"), w->name);
      w->error = 1;
    }
}


static void
yyprintf (char *file, int line, char *prefix, char *fmt, va_list ap, char *name)
{
  fprintf (stderr, "%s:%d: %s",
	   file ? file : cobc_source_file,
	   line ? line : cobc_source_line,
	   prefix);
  if (name)
    fprintf (stderr, "`%s' ", name);
  vfprintf (stderr, fmt, ap);
  fputs ("\n", stderr);
}

void
yywarn (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "warning: ", fmt, ap, NULL);
  va_end (ap);

  warning_count++;
}

void
yyerror (char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "", fmt, ap, NULL);
  va_end (ap);

  error_count++;
}

void
yywarn_loc (struct cobc_location *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->text, loc->first_line, "warning: ", fmt, ap, NULL);
  va_end (ap);

  warning_count++;
}

void
yyerror_loc (struct cobc_location *loc, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (loc->text, loc->first_line, "", fmt, ap, NULL);
  va_end (ap);

  error_count++;
}

void
yywarn_tree (cobc_tree x, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->loc.text, x->loc.first_line, "warning: ", fmt, ap, tree_to_string (x));
  va_end (ap);

  warning_count++;
}

void
yyerror_tree (cobc_tree x, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->loc.text, x->loc.first_line, "", fmt, ap, tree_to_string (x));
  va_end (ap);

  error_count++;
}
