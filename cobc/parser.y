/* COBOL Parser
 *
 * Copyright (C) 2001  Keisuke Nishida
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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

%expect 632

%{
#define yydebug		cob_trace_parser
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <stdarg.h>
#if defined(SunOS)
va_list __builtin_va_alist;
#endif

#include <ctype.h>

#include "cobc.h"
#include "codegen.h"
#include "_libcob.h"

static unsigned long lbend, lbstart;
static unsigned int perform_after_sw;

/* struct math_var *vl1, *vl2; */
static struct math_ose *tmose=NULL;
static struct ginfo    *gic=NULL;

static int warning_count = 0;
static int error_count = 0;

static void assert_numeric_sy (cob_tree sy);
%}

%union {
    cob_tree tree;
    int ival;               /* int */
    struct coord_pair pval; /* lin,col */
    unsigned long dval;     /* label definition, compacted */
    char *str;
    struct subref *rval;      /* variable reference (with subscripts) */
    struct refmod *rmval;   /* variable with RefMod specifier */
    struct string_from *sfval; /* variable list in string statement */
    struct unstring_delimited *udval;
    struct unstring_destinations *udstval;
    struct tallying_list *tlval;
    struct tallying_for_list *tfval;
    struct replacing_list *repval;
    struct replacing_by_list *rbval;
    struct converting_struct *cvval;
    struct inspect_before_after *baval;
    struct scr_info *sival;
    struct perf_info *pfval;
    struct perform_info *pfvals;
    struct sortfile_node *snval;
    struct selsubject *ssbjval;
    struct list *lstval;        /* generic container list */
    struct math_var *mval;      /* math variables container list */
    struct math_ose *mose;      /* math ON SIZE ERROR variables container */
    struct ginfo    *gic;       /* generic container */
    struct invalid_keys *iks; /* [NOT] INVALID KEY */
    struct invalid_key_element *ike; /* [NOT] INVALID KEY */
    struct condition condval;
    struct pair *pair;
}

%left  '+', '-'
%left  '*', '/'
%left  '^'
%left  OR
%left  AND
%right NOT
%right OF

%token <str>  IDSTRING
%token <tree> SYMBOL,VARIABLE,VARCOND,SUBSCVAR,LABELSTR,PICTURE_TOK
%token <tree> INTEGER_TOK,NLITERAL,CLITERAL

%token EQUAL,GREATER,LESS,TOK_GE,TOK_LE,COMMAND_LINE,ENVIRONMENT_VARIABLE
%token DATE,DAY,DAY_OF_WEEK,TIME,INKEY,READ,WRITE,OBJECT_COMPUTER,INPUT_OUTPUT
%token TO,FOR,IS,ARE,THRU,THAN,NO,CANCEL,ASCENDING,DESCENDING,ZEROS,PORT
%token SOURCE_COMPUTER,BEFORE,AFTER,SCREEN,REVERSE_VIDEO,NUMBER,PLUS,MINUS
%token FOREGROUND_COLOR,BACKGROUND_COLOR,UNDERLINE,HIGHLIGHT,LOWLIGHT,SEPARATE
%token RIGHT,AUTO,REQUIRED,FULL,JUSTIFIED,BLINK,SECURE,BELL,COLUMN,SYNCHRONIZED
%token TOK_INITIAL,FIRST,ALL,LEADING,OF,IN,BY,STRING,UNSTRING
%token START,DELETE,PROGRAM,GLOBAL,EXTERNAL,SIZE,DELIMITED
%token GIVING,ERASE,INSPECT,TALLYING,REPLACING,ON,POINTER,OVERFLOW
%token DELIMITER,COUNT,LEFT,TRAILING,CHARACTER,FILLER,OCCURS,TIMES
%token ADD,SUBTRACT,MULTIPLY,DIVIDE,ROUNDED,REMAINDER,ERROR,SIZE
%token FD,SD,REDEFINES,PICTURE,FILEN,USAGE,BLANK,SIGN,VALUE,MOVE,LABEL
%token PROGRAM_ID,DIVISION,CONFIGURATION,SPECIAL_NAMES
%token FILE_CONTROL,I_O_CONTROL,FROM,UPDATE,SAME,AREA,EXCEPTION
%token WORKING_STORAGE,LINKAGE,DECIMAL_POINT,COMMA,DUPLICATES,WITH
%token RECORD,OMITTED,STANDARD,RECORDS,BLOCK,VARYING,UNTIL,EXIT
%token CONTAINS,CHARACTERS,COMPUTE,GO,STOP,RUN,ACCEPT,PERFORM
%token IF,ELSE,SENTENCE,LINE,PAGE,OPEN,CLOSE,REWRITE,SECTION
%token ADVANCING,INTO,AT,END,NEGATIVE,POSITIVE,SPACES,NOT
%token CALL,USING,INVALID,CONTENT,QUOTES,LOW_VALUES,HIGH_VALUES
%token SELECT,ASSIGN,DISPLAY,UPON,CONSOLE,STD_OUTPUT,STD_ERROR
%token ORGANIZATION,ACCESS,MODE,KEY,STATUS,ALTERNATE,SORT,SORT_MERGE
%token SEQUENTIAL,INDEXED,DYNAMIC,RANDOM,RELATIVE,RELEASE
%token SET,UP,DOWN,TRACE,READY,RESET,SEARCH,WHEN,TEST
%token END_ADD,END_CALL,END_COMPUTE,END_DELETE,END_DIVIDE,END_EVALUATE
%token END_IF,END_MULTIPLY,END_PERFORM,END_READ,END_REWRITE,END_SEARCH
%token END_START,END_STRING,END_SUBTRACT,END_UNSTRING,END_WRITE
%token THEN,EVALUATE,OTHER,ALSO,CONTINUE,CURRENCY,REFERENCE,INITIALIZE
%token NUMERIC,ALPHABETIC,ALPHABETIC_LOWER,ALPHABETIC_UPPER
%token RETURNING,TOK_TRUE,TOK_FALSE,ANY,SUBSCVAR,FUNCTION,OPTIONAL
%token REPORT,RD,CONTROL,LIMIT,FINAL,HEADING,FOOTING,LAST,DETAIL,SUM
%token POSITION,FILE_ID,DEPENDING,TYPE,SOURCE,CORRESPONDING,CONVERTING
%token INITIATE,GENERATE,TERMINATE,TOK_NULL,ADDRESS,NOECHO,LPAR
%token IDENTIFICATION,ENVIRONMENT,DATA,PROCEDURE
%token AUTHOR,DATE_WRITTEN,DATE_COMPILED,INSTALLATION,SECURITY
%token COMMON,RETURN,END_RETURN,PREVIOUS,NEXT,PACKED_DECIMAL
%token INPUT,I_O,OUTPUT,EXTEND,EOL,EOS,BINARY,FLOAT_SHORT,FLOAT_LONG

%type <baval> inspect_before_after
%type <condval> condition,simple_condition,implied_op_condition
%type <cvval> converting_clause
%type <dval> if_then,search_body,search_all_body
%type <dval> search_when,search_when_list,search_opt_at_end
%type <gic>  on_end,opt_read_at_end
%type <ike>  read_invalid_key ,read_not_invalid_key
%type <iks>  opt_read_invalid_key
%type <ival> ext_cond,extended_cond_op
%type <ival> integer,cond_op,conditional,before_after,greater_than,less_than
%type <ival> on_exception_or_overflow,on_not_exception
%type <ival> opt_address_of,display_upon,display_options
%type <ival> flag_all,opt_with_duplicates,opt_with_test,opt_optional
%type <ival> flag_not,selection_subject,selection_object,when_case
%type <ival> flag_rounded,opt_sign_separate,opt_plus_minus
%type <ival> organization_options,access_options,open_mode,equal_to
%type <ival> call_mode,sign_condition,class_condition,replacing_kind
%type <ival> screen_attribs,screen_attrib,screen_sign,opt_separate
%type <ival> sentence_or_nothing,when_case_list,opt_read_next,usage
%type <ival> procedure_using,sort_direction,write_options
%type <lstval> goto_label_list
%type <mose> opt_on_size_error,on_size_error,error_sentence
%type <mval> var_list_name, var_list_gname
%type <pair> parameters
%type <pfval> perform_after
%type <pfvals> opt_perform_after
%type <rbval> replacing_by_list
%type <repval> replacing_list, replacing_clause
%type <rval> subscript,subscript_list
%type <sfval> string_from_list,string_from
%type <sival> screen_clauses
%type <snval> sort_file_list,sort_input,sort_output
%type <ssbjval> selection_subject_set
%type <str> idstring
%type <tree> field_description,label,filename,noallname,paragraph,assign_clause
%type <tree> file_description,redefines_var,function_call
%type <tree> name,gname,numeric_value,opt_gname,opt_def_name,def_name
%type <tree> opt_read_into,opt_write_from,field_name,expr,opt_expr
%type <tree> opt_unstring_count,opt_unstring_delim,unstring_tallying
%type <tree> qualified_var,unqualified_var
%type <tree> returning_options,screen_to_name, opt_goto_depending_on
%type <tree> set_variable,set_variable_or_nlit,set_target,opt_add_to
%type <tree> sort_keys,opt_perform_thru,procedure_section
%type <tree> var_or_nliteral,opt_read_key,file_name,string_with_pointer
%type <tree> variable,sort_range,perform_options,name_or_lit,delimited_by
%type <tree> variable_indexed,search_opt_varying,opt_key_is
%type <tree> from_rec_varying,to_rec_varying
%type <tree> literal,gliteral,without_all_literal,all_literal,special_literal
%type <tree> nliteral,signed_nliteral
%type <tfval> tallying_for_list
%type <tlval> tallying_list, tallying_clause
%type <udstval> unstring_destinations,unstring_dest_var
%type <udval> unstring_delimited_vars,unstring_delimited


%%
/*****************************************************************************
 * COBOL program sequence
 *****************************************************************************/

top:
  program_sequence
  {
    if (error_count)
      YYABORT;
  }
;
program_sequence:
  program
| program_sequence program
;
program:
  identification_division
  environment_division
  data_division
  procedure_division
  opt_end_program
;
opt_end_program:
| END PROGRAM idstring
;


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

identification_division:
  IDENTIFICATION DIVISION '.'
  PROGRAM_ID '.' idstring opt_program_parameter '.'
  identification_division_options
  {
    init_program ($6);
  }
;
opt_program_parameter:
| opt_is TOK_INITIAL opt_program { yywarn ("INITIAL is not supported yet"); }
| opt_is COMMON opt_program  { yywarn ("COMMON is not supported yet"); }
;
identification_division_options:
| identification_division_options identification_division_option
;
identification_division_option:
  AUTHOR '.' comment
| DATE_WRITTEN '.' comment
| DATE_COMPILED '.' comment
| INSTALLATION '.' comment
| SECURITY '.' comment
;
comment: { start_condition = START_COMMENT; };
opt_program: | PROGRAM ;


/*****************************************************************************
 * ENVIRONMENT DIVISION.
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION '.' { curr_division = CDIV_ENVIR; }
  configuration_section
  input_output_section
;


/*******************
 * CONFICURATION SECTION
 *******************/

configuration_section:
| CONFIGURATION SECTION '.' configuration_list
;
configuration_list:
| configuration_list configuration
;
configuration:
  SOURCE_COMPUTER '.' comment
| OBJECT_COMPUTER '.' comment
| SPECIAL_NAMES '.' special_names opt_dot
;
special_names:
| special_names special_name
;
special_name:
  CURRENCY opt_sign opt_is CLITERAL { currency_symbol = $4->name[0]; }
| DECIMAL_POINT opt_is COMMA	{ decimal_comma = 1; }
| CONSOLE opt_is CONSOLE	{ yywarn ("CONSOLE name is ignored"); }
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
| FILE_CONTROL '.' select_statement_list
;
select_statement_list:
| select_statement_list select_statement
;
select_statement:
  SELECT opt_optional def_name ASSIGN opt_to
  {
    $3->type='F';   /* mark as file variable */
    curr_file=$3;
    $3->pic=0;  /* suppose not indexed yet */
    $3->defined=1;
    $3->parent=NULL; /* assume no STATUS yet
			this is "file status" var in files */
    $3->organization = 2;
    $3->access_mode = 1;
    $3->times=-1;
    /*$3->filenamevar=$6;*/ /* this is the variable w/filename */
    $3->alternate=NULL; /* reset alternate key list */
    $3->flags.optional=$2; /* according to keyword */
  }
  assign_clause
  {
    $3->filenamevar=$7; /* this is the variable w/filename */
  }
  select_clauses '.'
  {
    if ((curr_file->organization==ORG_INDEXED) && !(curr_file->ix_desc)) {
      yyerror("indexed file must have a record key");
    }
  }
;
assign_clause:
  PORT { yywarn ("assign target is ignored"); $$=NULL; }
| filename { $$=$1; }
| PORT filename { $$=$2; }
| EXTERNAL filename
  {
    curr_file->access_mode = curr_file->access_mode + 5;
    $$=$2;
  }
| error  { yyerror("Invalid ASSIGN clause in SELECT statement"); }
;
select_clauses:
| select_clauses select_clause
;
select_clause:
  ORGANIZATION opt_is organization_options { curr_file->organization=$3; }
| ACCESS opt_mode opt_is access_options
  {
    /*{ curr_file->access_mode=$4; }*/
    if (curr_file->access_mode < 5) {
      curr_file->access_mode=$4;
    }
    else {
      curr_file->access_mode = $4 + 5;
    }
  }
| FILEN STATUS opt_is SYMBOL { curr_file->parent=$4; }
| RECORD KEY opt_is SYMBOL { curr_file->ix_desc=$4; }
| RELATIVE KEY opt_is SYMBOL { curr_file->ix_desc=$4; }
| ALTERNATE RECORD KEY opt_is SYMBOL opt_with_duplicates
  { add_alternate_key($5,$6); }
| error         { yyerror("invalid clause in select"); }
;
opt_with_duplicates:
  /* nothing */			{ $$ = 0; }
| WITH DUPLICATES		{ $$ = 1; }
;
opt_optional:
  /* nothing */			{ $$ = 0; }
| OPTIONAL			{ $$ = 1; }
;
organization_options:
  INDEXED			{ $$ = 1; }
| SEQUENTIAL			{ $$ = 2; }
| RELATIVE			{ $$ = 3; }
| LINE SEQUENTIAL		{ $$ = 4; }
;
access_options:
  SEQUENTIAL			{ $$ = 1; }
| DYNAMIC			{ $$ = 2; }
| RANDOM			{ $$ = 3; }
;


/*
 * I-O-CONTROL
 */

i_o_control:
| I_O_CONTROL '.' same_statement_list
;
same_statement_list:
| same_statement_list same_statement
;
same_statement:
  SAME i_o_control_param opt_area opt_for filename_list '.'
  {
    yywarn ("I-O-CONTROL is not supported yet");
  }
;
i_o_control_param:
  RECORD
| SORT
| SORT_MERGE
;
filename_list:
  variable { }
| filename_list variable { }
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION '.' { curr_division = CDIV_DATA; }
  file_section
  working_storage_section
  linkage_section
  report_section
  screen_section
  {
    data_trail();
  }
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| FILEN SECTION '.'	{ curr_field=NULL; }
  fd_list		{ close_fields(); }
;
fd_list:
| fd_list
  FD file_name file_attrib '.'
  {
    curr_field=NULL;
    if ($3->filenamevar == NULL)
	yyerror("External file name not defined for file %s", $3->name);
  }
  file_description
  {
    close_fields();
    alloc_file_entry($3);
    gen_fdesc($3,$7);
  }
| fd_list
  SD file_name sort_attrib '.'
  {
    $3->organization=2;
    curr_field=NULL;
  }
  file_description
  {
    close_fields();
    alloc_file_entry($3);
    gen_fdesc($3,$7);
  }
;
file_name:
  { curr_division = CDIV_FD; }
  SYMBOL
  { curr_division = CDIV_DATA; $$ = $2; }
;
file_description:
  field_description       { $$=$1; }
| file_description field_description
  {
    if (($2 != NULL) && ($2->level == 1))
      {
	/* multiple 01 records (for a file descriptor) */
	$2->redefines=$1;
	$$=$2;
      }
    else
      $$=$1;
  }
;
file_attrib:
| file_attrib REPORT opt_is SYMBOL { save_report( $4,$<tree>0 ); }
| file_attrib opt_is GLOBAL     { $<tree>0->type = 'J'; }
| file_attrib opt_is EXTERNAL   { $<tree>0->type = 'K'; }
| file_attrib LABEL rec_or_recs opt_is_are std_or_omitt
| file_attrib BLOCK opt_contains integer opt_to_integer chars_or_recs
| file_attrib DATA rec_or_recs  opt_is_are var_strings
| file_attrib VALUE OF FILE_ID opt_is filename
  {
    if ($<tree>-1->filenamevar != NULL) {
      yyerror("Re-defining file name defined in SELECT statement");
    }
    else {
      $<tree>-1->filenamevar = $<tree>6;
    }
  }
| file_attrib RECORD opt_is VARYING opt_in_size
  from_rec_varying to_rec_varying opt_characters
  DEPENDING opt_on SYMBOL
  {
    set_rec_varying_info ($<tree>-1, $6, $7, $11);
  }
;
var_strings:
  SYMBOL { }
| var_strings SYMBOL { }
;
opt_to_integer:
| TO integer
;
from_rec_varying:
  /* nothing */ { $$ = NULL; }
| FROM nliteral { $$ = $2; }
;
to_rec_varying:
  /* nothing */ { $$ = NULL; }
| TO nliteral   { $$ = $2; }
;
sort_attrib:
| sort_attrib DATA rec_or_recs  opt_is_are var_strings
| sort_attrib RECORD opt_is VARYING opt_in_size
  from_rec_varying to_rec_varying opt_characters
  DEPENDING opt_on SYMBOL
  {
    set_rec_varying_info( $<tree>-1,$6,$7,$11 );
  }
;
rec_or_recs: RECORD | RECORDS ;
std_or_omitt: STANDARD | OMITTED ;
opt_contains: | CONTAINS ;
opt_characters: | CHARACTERS ;
chars_or_recs: CHARACTERS | RECORDS ;


/*******************
 * WORKING-STRAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION '.'	{ curr_field = NULL; }
  field_description_list	{ close_fields (); }
;
field_description_list:
| field_description_list field_description
;
field_description:
  integer field_name	{ define_field ($1, $2); }
  field_options '.'
  {
    update_field ();
    $$ = $2;
  }
;
field_name:
  /* nothing */		{ $$ = make_filler (); }
| FILLER		{ $$ = make_filler (); }
| SYMBOL
  {
    if ($1->defined)
      yyerror ("variable already defined: %s", $1->name);
    $1->defined=1;
    $$ = $1;
  }
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
;


/*
 * REDEFINES clause
 */

redefines_clause:
  REDEFINES			{ curr_division = CDIV_INITIAL; }
  redefines_var
  {
    curr_division = CDIV_DATA;
    curr_field->redefines = lookup_for_redefines ($3);
  }
redefines_var:
  VARIABLE		{ $$ = $1; }
| SUBSCVAR		{ $$ = $1; }
;


/*
 * EXTERNAL clause
 */

external_clause:
  opt_is EXTERNAL	{ save_named_sect (curr_field); }
;


/*
 * GLOBAL clause
 */

global_clause:
  opt_is GLOBAL		{ yywarn ("GLOBAL is not supported"); }
;


/*
 * PICTURE clause
 */

picture_clause:
  PICTURE { start_condition = START_PICTURE; } PICTURE_TOK
;


/*
 * USAGE clause
 */

usage_clause:
  opt_usage opt_is usage
;
usage:
  BINARY /* or COMP, COMP-5, INDEX */
  {
    curr_field->type = 'B';
    curr_field->len  =  0;
  }
| DISPLAY
  {
    /* do nothing */
  }
| FLOAT_SHORT /* or COMP-1 */
  {
    curr_field->type     = 'U';
    curr_field->len      =  4;
    curr_field->decimals =  7;
    curr_field->sign     =  1;
    /* default picture is 14 (max=7->7.7) digits */
    strcpy (picture,"S\x01\x39\x07\x56\x01\x39\x07");
  }
| FLOAT_LONG /* or COMP-2 */
  {
    curr_field->type     = 'U';
    curr_field->len      =  8;
    curr_field->decimals = 15;
    curr_field->sign     =  1;
    /* default picture is 30 (max=15->15.15) digits*/
    strcpy (picture,"S\x01\x39\x0f\x56\x01\x39\x0f");
  }
| PACKED_DECIMAL /* or COMP-3 */
  {
    curr_field->type = 'C';
  }
| POINTER
  {
    curr_field->type     = 'B';
    curr_field->len      =  4;
    curr_field->decimals =  0;
    curr_field->flags.is_pointer = 1;
    strcpy (picture,"9\xa")
  }
;
opt_usage: | USAGE ;


/*
 * SIGN clause
 */

sign_clause:
  opt_sign_is LEADING opt_sign_separate
  {
    curr_field->flags.separate_sign = $3;
    curr_field->flags.leading_sign  = 1;
  }
| opt_sign_is TRAILING opt_sign_separate
  {
    curr_field->flags.separate_sign = $3;
    curr_field->flags.leading_sign  = 0;
  }
;
opt_sign_is:
| SIGN opt_is
;
opt_sign_separate:
  /* nothing */			{ $$ = 0; }
| SEPARATE opt_character	{ $$ = 1; }
;


/*
 * OCCURS clause
 */

occurs_clause:
  OCCURS integer opt_times { curr_field->times = $2; }
  opt_indexed_by
| OCCURS integer TO integer opt_times DEPENDING opt_on
  { curr_division = CDIV_INITIAL; }
  gname
  {
    curr_division = CDIV_DATA;
    curr_field->times = $4;
    curr_field->occurs = malloc (sizeof (struct occurs));
    curr_field->occurs->min = $2;
    curr_field->occurs->max = $4;
    curr_field->occurs->depend = $9;
  }
  opt_indexed_by
;
opt_indexed_by:
| opt_key_is INDEXED opt_by index_name_list { }
;
opt_key_is:
  /* nothing */				{ $$ = NULL; }
| ASCENDING opt_key opt_is SYMBOL	{ $4->level = -1; $$ = $4; }
| DESCENDING opt_key opt_is SYMBOL	{ $4->level = -2; $$ = $4; }
;
index_name_list:
  def_name { define_implicit_field ($1, $<tree>-2, curr_field->times); }
| index_name_list
  def_name { define_implicit_field ($2, $<tree>-2, curr_field->times); }
;
opt_times: | TIMES ;


/*
 * JUSTIFIED clause
 */

justified_clause:
  JUSTIFIED opt_right		{ curr_field->flags.just_r = 1; }
;
opt_right: | RIGHT ;


/*
 * SYNCHRONIZED clause
 */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ curr_field->flags.sync = 1; }
;
left_or_right:
| LEFT
| RIGHT
;


/*
 * BLANK clause
 */

blank_clause:
  BLANK opt_when ZEROS	{ curr_field->flags.blank=1; }
;


/*
 * VALUE clause
 */

value_clause:
  VALUE opt_is_are value_list
;
value_list:
  value
| value_list opt_sep value
;
value:
  gliteral                  { set_variable_values($1,$1); }
| gliteral THRU gliteral    { set_variable_values($1,$3); }
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION '.'		{ at_linkage=1; curr_field=NULL; }
  field_description_list	{ close_fields(); at_linkage=0; }
;


/*******************
 * REPORT SECTION
 *******************/

report_section:
| REPORT SECTION '.' rd_statement_list
;
rd_statement_list:
| rd_statement_list rd_statement
;
rd_statement:
  RD SYMBOL { $2->type='W'; curr_division = CDIV_INITIAL; }
  report_controls { curr_division = CDIV_DATA; }
  report_description
;
report_controls:
| report_controls CONTROL opt_is_are opt_final report_break_list
| report_controls PAGE opt_limit_is integer opt_line
| report_controls
        HEADING opt_is integer
        opt_first_detail opt_last_detail
        opt_footing '.'
;
report_break_list:
| report_break_list name { $2->defined=1; }
;
report_description:
  report_item
| report_description report_item
;
report_item:
  integer opt_def_name
  {
    define_field ($1, $2);
  }
  report_clauses '.'
  {
    update_report_field ($2);
    curr_division = CDIV_DATA;
  }
;
report_clauses:
| report_clauses TYPE opt_is report_type
        report_position { curr_division = CDIV_INITIAL; }
        opt_report_name
| report_clauses LINE opt_number opt_line_rel integer
| report_clauses opt_report_column
        opt_picture_clause { curr_division = CDIV_INITIAL; }
        report_value
;
opt_report_name:
  name { }
| FINAL { }
| /* NOTHING */
;
report_value:
| VALUE opt_is gname
| SOURCE opt_is gname
| SUM opt_of name
;
opt_report_column:
  COLUMN opt_number integer
| /* nothing */
;
opt_number:
| NUMBER
;
opt_line_rel:
| '+'
| PLUS
;
report_position:
  HEADING
| FOOTING
| /* nothing */
;
report_type:
  PAGE
| CONTROL
| DETAIL
;
opt_picture_clause: | picture_clause ;
opt_limit_is: | LIMIT opt_is ;
opt_footing: | FOOTING opt_is integer ;
opt_last_detail: | LAST DETAIL opt_is integer ;
opt_first_detail: | FIRST DETAIL opt_is integer ;


/*******************
 * SCREEN SECTION
 *******************/

screen_section:
| SCREEN SECTION '.'
  {
    screen_io_enable++;
    curr_field=NULL;
    scr_line = scr_column = 1;
  }
  screen_item_list
  {
    close_fields();
  }
;
screen_item_list:
| screen_item_list screen_item
;
screen_item:
  integer opt_def_name { define_field ($1, $2); }
  screen_clauses '.'
  {
    update_screen_field($2,$4);
  }
;
screen_clauses:
  /* nothing */             { $$ = alloc_scr_info(); }
| screen_clauses LINE
    opt_number_is
    opt_plus_minus
    integer                 { scr_set_line($1,$5,$4); $$=$1; }
| screen_clauses COLUMN
    opt_number_is
    opt_plus_minus
    integer                 { scr_set_column($1,$5,$4); $$=$1; }
| screen_clauses
    screen_attrib           { $1->attr |= $2; $$=$1; }
| screen_clauses FOREGROUND_COLOR
    integer                 { $1->foreground = $3; $$=$1; }
| screen_clauses BACKGROUND_COLOR
    integer                 { $1->background = $3; $$=$1; }
| screen_clauses
    screen_source_destination
| screen_clauses
    VALUE opt_is gliteral   { curr_field->value = (cob_tree) $4; $$=$1; }
| screen_clauses picture_clause
;
screen_source_destination:
  USING { curr_division = CDIV_INITIAL; }
  name_or_lit
  {
    curr_division = CDIV_DATA;
    $<sival>0->from = $<sival>0->to = $3;
  }
| FROM { curr_division = CDIV_INITIAL; }
  name_or_lit
  screen_to_name
  {
	curr_division = CDIV_DATA;
	$<sival>0->from = $3;
	$<sival>0->to = $4;
  }
| TO { curr_division = CDIV_INITIAL; }
  name
  {
	curr_division = CDIV_DATA;
	$<sival>0->from = NULL;
	$<sival>0->to = $3;
  }
;
screen_to_name:
  /* nothing */ { $$=NULL; }
  | TO name { $$ = $2; }
;
screen_attribs:
  /* nothing */                  { $$ = 1; }
| screen_attribs screen_attrib { $$ = $1 | $2; }
;
screen_attrib:
  BLANK SCREEN                  { $$ = SCR_BLANK_SCREEN; }
| BLANK LINE                    { $$ = SCR_BLANK_LINE; }
| BELL                          { $$ = SCR_BELL; }
| FULL                          { $$ = SCR_FULL; }
| REQUIRED                      { $$ = SCR_REQUIRED; }
| SECURE                        { $$ = SCR_SECURE; }
| AUTO                          { $$ = SCR_AUTO; }
| JUSTIFIED RIGHT               { $$ = SCR_JUST_RIGHT; }
| JUSTIFIED LEFT                { $$ = SCR_JUST_LEFT; }
| BLINK                         { $$ = SCR_BLINK; }
| REVERSE_VIDEO                  { $$ = SCR_REVERSE_VIDEO; }
| UNDERLINE                     { $$ = SCR_UNDERLINE; }
| LOWLIGHT                      { $$ = SCR_LOWLIGHT; }
| HIGHLIGHT                     { $$ = SCR_HIGHLIGHT; }
| BLANK opt_when ZEROS        { $$ = SCR_BLANK_WHEN_ZERO; }
| NOECHO                        { $$ = SCR_NOECHO; }
| UPDATE                        { $$ = SCR_UPDATE; }
| screen_sign                   { $$ = $1; }
;
screen_sign:
  SIGN opt_is LEADING opt_separate
  {
    $$ = SCR_SIGN_LEADING | SCR_SIGN_PRESENT | $4;
  }
| SIGN opt_is TRAILING opt_separate
  {
    $$ = SCR_SIGN_PRESENT | $4;
  }
;
opt_separate:
  SEPARATE opt_character  { $$ = SCR_SIGN_SEPARATE; }
| /* nothing */           { $$ = 0; }
;
opt_plus_minus:
  /* nothing */ { $$ = 0; }
| PLUS          { $$ = 1; }
| MINUS         { $$ = -1; }
;
opt_character: | CHARACTER ;
opt_number_is: | NUMBER opt_is ;


/*****************************************************************************
 * PROCEDURE DIVISION.
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION { in_procedure = 1; curr_division = CDIV_INITIAL; }
  procedure_using '.'
  {
    proc_header ($4);
  }
  procedure_list
  {
    /* close procedure_list sections & paragraphs */
    close_section (); /* this also closes paragraph */
    resolve_labels ();
    proc_trail ($4);
    in_procedure = 0;
  }
;
procedure_using:
  /* nothing */			{ $$ = 0; }
| USING using_vars		{ $$ = 1; }
;
using_vars:
  gname				{ gen_save_using ($1); }
| using_vars opt_sep gname	{ gen_save_using ($3); }
;
procedure_list:
| procedure_list procedure
;
procedure:
  procedure_section { close_section(); open_section($1); }
| paragraph { close_paragr(); open_paragr($1); }
| statement_list '.'
| error '.'
| '.'
;
procedure_section:
  LABELSTR SECTION '.'
  {
    cob_tree lab=$1;
    if (lab->defined != 0) {
      lab = install(lab->name,SYTB_LAB,2);
    }
    lab->defined = 1;
    $$=lab;
  }
;
paragraph:
  LABELSTR '.'
  {
    cob_tree lab=$1;
    if (lab->defined != 0) {
      if ((lab=lookup_label(lab,curr_section))==NULL) {
	lab = install($1->name,SYTB_LAB,2);
      }
    }
    lab->parent = curr_section;
    lab->defined=1;
    $$=lab;
  }
;


/*******************
 * Statements
 *******************/

statement_list:
  statement
| statement_list statement
;
statement:
  accept_statement
| add_statement
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
| release_statement
| return_statement
| rewrite_statement
| search_statement
| set_statement
| sort_statement
| start_statement
| stoprun_statement
| string_statement
| subtract_statement
| unstring_statement
| write_statement
| INITIATE name     { yyerror ("INITIATE is not implemented yet"); }
| GENERATE name     { yyerror ("GENERATE is not implemented yet"); }
| TERMINATE name    { yyerror ("TERMINATE is not implemented yet"); }
| READY TRACE       { yyerror ("TRACE is not implemented yet"); }
| RESET TRACE       { yyerror ("TRACE is not implemented yet"); }
;

conditional_statement_list:
  conditional_statement
| conditional_statement_list conditional_statement
;
conditional_statement:
  statement
| CONTINUE
| NEXT SENTENCE
;



/*
 * ACCEPT statement
 */

accept_statement:
    ACCEPT name opt_line_pos accept_options
    ;
accept_options:
  screen_attribs		{ gen_accept($<tree>-1, $1, 1); }
| screen_attribs ON EXCEPTION
  {
    screen_io_enable++;
    gen_accept($<tree>-1, $1, 1);
  }
  variable
  {
    gen_store_fnres($5);
    $<dval>$ = gen_check_zero();
  }
  statement_list
  {
    gen_dstlabel($<dval>6);
  }
| FROM DATE			{ gen_accept_from_date($<tree>-1); }
| FROM DAY			{ gen_accept_from_day($<tree>-1); }
| FROM DAY_OF_WEEK		{ gen_accept_from_day_of_week($<tree>-1); }
| FROM TIME			{ gen_accept_from_time($<tree>-1); }
| FROM INKEY			{ gen_accept_from_inkey($<tree>-1); }
| FROM COMMAND_LINE		{ gen_accept_from_cmdline($<tree>-1); }
| FROM ENVIRONMENT_VARIABLE CLITERAL
  {
    save_literal($3, 'X');
    LITERAL ($3)->all=0;
    gen_accept_env_var($<tree>-1, $3);
  }
;


/*
 * ADD statement
 */

add_statement:
    ADD add_body end_add
    ;
add_body:
      var_list_gname TO var_list_name opt_on_size_error
      {
        gen_add1($1, $3, $4);
      }
    | var_list_gname opt_add_to GIVING var_list_name opt_on_size_error
      {
        gen_add2($1, $4, $2, $5);
      }
    | CORRESPONDING gname opt_to name flag_rounded
      {
        gen_addcorr($2, $4, $5);
      }
    ;
opt_add_to:
    /* nothing */ { $$ = NULL; }
    | TO gname    { $$ = $2; }
    ;
end_add: | END_ADD ;


/*
 * CALL statement
 */

call_statement:
    CALL  { curr_call_mode=CM_REF; }
    gname call_using returning_options
    { $<ival>$ = loc_label++; /* exception check */ }
    { $<ival>$ = loc_label++; /* not exception check */ }
    {
      $<ival>$ = gen_call($3,$<ival>6,$<ival>7);
      gen_store_fnres($5);
    }
    on_exception_or_overflow
    on_not_exception
    {
      check_call_except($9,$10,$<ival>6,$<ival>7,$<ival>8);
    }
    opt_end_call
    ;
call_using:
| USING call_parameter_list
;
call_parameter_list:
  call_parameter
| call_parameter_list opt_sep call_parameter
;
call_parameter:
  gname
  {
    if ($1->litflag==1) {
      struct lit *lp=(struct lit *)$1;
      lp->call_mode=curr_call_mode;
    }
    else
      $1->call_mode=curr_call_mode;
    gen_push_using ($1);
  }
| BY call_mode gname
  {
    curr_call_mode = $2;
    if ($3->litflag==1) {
      struct lit *lp=(struct lit *)$3;
      lp->call_mode=curr_call_mode;
    }
    else
      $3->call_mode=curr_call_mode;
    gen_push_using ($3);
  }
;
call_mode:
  REFERENCE		{ $$ = CM_REF; }
| CONTENT		{ $$ = CM_CONT; }
| VALUE			{ $$ = CM_VAL; }
;
returning_options:
    /* nothing */   { $$=0; }
    | RETURNING variable { $$=$2; }
    | GIVING variable { $$=$2; }
    ;
on_exception_or_overflow:
    /* nothing */ { $$ = 0; }
    | ON exception_or_overflow { $<ival>$ = begin_on_except(); }
      statement_list            { gen_jmplabel($<dval>0); $$=$<ival>3; }
    ;
exception_or_overflow:
      EXCEPTION
    | OVERFLOW
    ;
on_not_exception:
    NOT ON EXCEPTION { $<ival>$ = begin_on_except(); }
        statement_list            { gen_jmplabel($<dval>-1); $$=$<ival>4; }
    | /* nothing */ { $$ = 0; }
    ;
opt_end_call:
    | END_CALL
    ;


/*
 * CANCEL statement
 */

cancel_statement:
    CANCEL gname { gen_cancel ($2); }
;


/*
 * CLOSE statement
 */

close_statement:
    CLOSE close_files
    ;
close_files:
      close_file
    | close_files opt_sep close_file
    ;
close_file:
    name { gen_close($1); }
    ;


/*
 * COMPUTE statement
 */

compute_statement:
    COMPUTE var_list_name '=' expr opt_on_size_error opt_end_compute
    {
      gen_compute($2, $4, $5);
    }
  ;
opt_end_compute:
    /* nothing */
  | END_COMPUTE
  ;


/*
 * DELETE statement
 */

delete_statement:
    DELETE name opt_record { gen_delete($2); }
    opt_invalid_key
    opt_end_delete
    ;
opt_end_delete:
    /* nothing */
    | END_DELETE
    ;


/*
 * DISPLAY statement
 */

display_statement:
    DISPLAY
    display_varlist
    opt_upon
    display_upon
    display_options
    opt_line_pos
    { gen_display($4, $5); }
    ;
display_varlist:
    /* nothing */
    | display_varlist opt_sep gname { put_disp_list($3); }
    ;
display_upon:
    /* nothing */   { $$ = 1; }  /* default is CONSOLE (STD_OUTPUT) */
    | CONSOLE       { $$ = 1; }
    | STD_OUTPUT    { $$ = 1; }
    | STD_ERROR     { $$ = 2; }
    ;
display_options:
    /* nothing */                           { $$ = 0; }
    | display_options opt_with NO ADVANCING { $$ = $1 | 1; }
    | display_options ERASE                 { $$ = $1 | 2; }
    | display_options ERASE EOS         { $$ = $1 | 2; }
    | display_options ERASE EOL         { $$ = $1 | 4; }
    ;
opt_line_pos:
    /* nothing */
    | LINE expr POSITION expr
     {
      screen_io_enable++;
      push_expr($2);
      push_expr($4);
      gen_gotoxy_expr();
     }
    | LINE expr COLUMN expr
     {
      screen_io_enable++;
      push_expr($2);
      push_expr($4);
      gen_gotoxy_expr();
     }
    ;


/*
 * DIVIDE statement
 */

divide_statement:
    DIVIDE divide_body opt_end_divide
    ;
divide_body:
      numeric_value BY numeric_value GIVING var_list_name opt_on_size_error
      {
        gen_divide2($5, $1, $3, $6);
      }
    | numeric_value BY numeric_value GIVING name flag_rounded REMAINDER name
      {
        assert_numeric_sy($5);
        gen_divide($1, $3, $5, $8, $6);
      }
    | numeric_value INTO numeric_value GIVING name flag_rounded REMAINDER name
      {
        assert_numeric_sy($5);
        gen_divide($3, $1, $5, $8, $6);
      }
    | numeric_value BY numeric_value GIVING name flag_rounded REMAINDER name on_size_error
      {
        assert_numeric_sy($5);
        gen_dstlabel($9->lbl4); /* generate bypass jump */
        gen_divide($1, $3, $5, $8, $6);
        math_on_size_error3($9);
      }
    | numeric_value INTO numeric_value GIVING name flag_rounded REMAINDER name on_size_error
      {
        assert_numeric_sy($5);
        gen_dstlabel($9->lbl4); /* generate bypass jump */
        gen_divide($3, $1, $5, $8, $6);
        math_on_size_error3($9);
      }
    | numeric_value INTO numeric_value GIVING var_list_name opt_on_size_error
      {
        gen_divide2($5, $3, $1, $6);
      }
    | numeric_value INTO var_list_name opt_on_size_error
      {
        gen_divide1($3, $1, $4);
      }
    ;
opt_end_divide:
    /* nothing */
    | END_DIVIDE
    ;


/*
 * EVALUATE statement
 */

evaluate_statement:
    EVALUATE { $<ival>$ = gen_evaluate_start(); }
                selection_subject_set { }
                when_case_list
                END_EVALUATE { release_sel_subject($<ival>2,$3); }
    ;
selection_subject_set:
          selection_subject { $$=save_sel_subject(NULL,$1); }
        | selection_subject_set ALSO
	    selection_subject { $$=save_sel_subject($1,$3); }
        ;
selection_subject:
    expr
    {
      if (push_expr($1))
	$$=SSUBJ_EXPR;
      else
	{
	  push_field ($1);
	  $$=SSUBJ_STR;
	}
    }
  | condition	{ push_condition(); $$ = SSUBJ_BOOLEAN; }
  | TOK_TRUE	{ push_boolean (1); $$ = SSUBJ_BOOLEAN; }
  | TOK_FALSE	{ push_boolean (0); $$ = SSUBJ_BOOLEAN; }
  ;
when_case_list:
        WHEN { $<ival>$ = loc_label++; /* mark end of "when" case */ }
                { $<ssbjval>$=$<ssbjval>-1; /* store inherited subject set */ }
                when_case
                sentence_or_nothing
                { $$=gen_end_when($<ival>-2,$<ival>2,$5); }
        | when_case_list WHEN { $<ival>$ = loc_label++; }
                { $<ssbjval>$=$<ssbjval>-1; }
                when_case
                { gen_bypass_when_case($1); }
                sentence_or_nothing
                { $$=gen_end_when($<ival>-2,$<ival>3,$7); }
        ;
when_case:
          selection_object
          {
	    $$ = 0;
	    gen_when_check($$,$<ssbjval>0,$1,$<ival>-1);
	  }
        | when_case ALSO selection_object
          {
	    $$ = $1 + 1;
	    gen_when_check($$,$<ssbjval>0,$3,$<ival>-1);
	  }
        | OTHER { $$=-1; }
        ;
selection_object:
    ANY			{ $$ = SOBJ_ANY; }
  | TOK_TRUE		{ push_boolean (1); $$ = SOBJ_BOOLEAN; }
  | TOK_FALSE		{ push_boolean (0); $$ = SOBJ_BOOLEAN; }
  | flag_not condition	{ push_condition (); $$ = SOBJ_BOOLEAN | $1; }
  | flag_not expr
    {
      if ($2 == (cob_tree) spe_lit_ZE)
	$$ = SOBJ_ZERO | $1;
      else if (push_expr($2))
	$$ = SOBJ_EXPR | $1;
      else
	{
	  push_field ($2);
	  $$ = SOBJ_STR | $1;
	}
    }
  | flag_not expr THRU expr
    {
      if (push_expr($2) && push_expr($4))
	$$ = SOBJ_RANGE | $1;
      else
	{
	  push_field ($2);
	  push_field ($4);
	  $$ = SOBJ_RANGE | $1;
	}
    }
  ;
sentence_or_nothing:
    /* nothing */               { $$ = 0; }
    | conditional_statement_list     { $$ = 1; }
    ;


/*
 * EXIT statement
 */

exit_statement:
      EXIT          { gen_exit(0); }
    | EXIT PROGRAM  { gen_exit(1); }
    ;


/*
 * GO TO statement
 */

goto_statement:
    GO opt_to goto_label_list opt_goto_depending_on
    {
      if ($4 == NULL)
        gen_goto($3);
      else
        gen_goto_depending($3, $4);
    }
    ;
goto_label_list:
      label                     { $$ = insert_list(NULL, $1); }
    | goto_label_list label     { $$ = insert_list($1, $2); }
    | goto_label_list ',' label { $$ = insert_list($1, $3); }
    ;
opt_goto_depending_on:
    /* nothing */               { $$ = NULL; }
    | DEPENDING opt_on variable { $$ = $3; }
    ;


/*
 * IF statement
 */

if_statement:
    if_then { gen_dstlabel($1); } opt_end_if
  | if_then ELSE {
      $<dval>$=gen_passlabel();
      gen_dstlabel($1);
    }
    conditional_statement_list {
      gen_dstlabel($<dval>3);
    }
    opt_end_if
  | IF error END_IF
;
if_then:
    IF condition { $<dval>$ = gen_testif(); } opt_then
       conditional_statement_list { $$ = $<dval>3; }
;
opt_end_if: | END_IF ;


/*
 * INITILIZE statement
 */

initialize_statement:
  INITIALIZE initialize_vars
;
initialize_vars:
  gname				{ gen_initialize ($1); }
| initialize_vars opt_sep gname	{ gen_initialize ($3); }
;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT name tallying_clause { gen_inspect($2,(void *)$3,0); }
  replacing_clause { gen_inspect($2,(void *)$5,1); }
  | INSPECT name converting_clause { gen_inspect($2,(void *)$3,2); }
  ;
converting_clause:
        CONVERTING
        noallname TO noallname inspect_before_after {
            $$ = alloc_converting_struct($2,$4,$5);
                }
        ;
tallying_clause:
    TALLYING tallying_list { $$=$2; }
    | /* nothing */        { $$=NULL; }
    ;
tallying_list:
    tallying_list
        name FOR tallying_for_list  {
            $$ = alloc_tallying_list($1,$2,$4); }
    | /* nothing */     { $$ = NULL; }
    ;
tallying_for_list:
    tallying_for_list
        CHARACTERS inspect_before_after {
            $$ = alloc_tallying_for_list($1,INSPECT_CHARACTERS,NULL,$3); }
    | tallying_for_list
        ALL noallname inspect_before_after {
            $$ = alloc_tallying_for_list($1,INSPECT_ALL,$3,$4); }
    | tallying_for_list
        LEADING noallname inspect_before_after {
            $$ = alloc_tallying_for_list($1,INSPECT_LEADING,$3,$4); }
    | /* nothing */     { $$ = NULL; }
    ;
replacing_clause:
    REPLACING
        replacing_list      { $$ = $2; }
    | /* nothing */         { $$ = NULL; }
    ;
replacing_list:
    replacing_list
        CHARACTERS BY noallname inspect_before_after {
            $$ = alloc_replacing_list($1,INSPECT_CHARACTERS,NULL,$4,$5); }
    | replacing_list
        replacing_kind replacing_by_list {
            $$ = alloc_replacing_list($1,$2,$3,NULL,NULL); }
    | /* nothing */     { $$ = NULL; }
    ;
replacing_by_list:
    replacing_by_list
        noallname BY noallname inspect_before_after {
            $$ = alloc_replacing_by_list($1,$2,$4,$5); }
    | /* nothing */         { $$ = NULL; }
    ;
replacing_kind:
    ALL         { $$ = INSPECT_ALL; }
    | LEADING   { $$ = INSPECT_LEADING; }
    | FIRST  { $$ = INSPECT_FIRST; }
    ;
inspect_before_after:
    inspect_before_after
        BEFORE opt_initial noallname
            { $$ = alloc_inspect_before_after($1,1,$4); }
    | inspect_before_after
        AFTER opt_initial noallname
            { $$ = alloc_inspect_before_after($1,2,$4); }
    | /* nothing */  { $$ = alloc_inspect_before_after(NULL,0,NULL); }
    ;
noallname:
    name      { $$ = $1; }
    | without_all_literal { $$ = (cob_tree)$1; }
    ;
opt_initial: | TOK_INITIAL ;


/*
 * MOVE statement
 */

move_statement:
  MOVE gname TO move_vars
| MOVE CORRESPONDING gname TO gname { gen_movecorr($3, $5); }
;
move_vars:
  gname				{ gen_move ($<tree>-1, $1); }
| move_vars opt_sep gname	{ gen_move ($<tree>-1, $3); }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
    MULTIPLY multiply_body opt_end_multiply
    ;
multiply_body:
      numeric_value BY var_list_name opt_on_size_error
      {
        gen_multiply1($3, $1, $4);
      }
    | numeric_value BY numeric_value GIVING var_list_name opt_on_size_error
      {
        gen_multiply2($5, $1, $3, $6);
      }
    ;
opt_end_multiply:
    /* nothing */
    | END_MULTIPLY
    ;


/*
 * OPEN statement
 */

open_statement:
  OPEN open_options
;
open_options:
  open_mode open_varlist { }
| open_options open_mode open_varlist { }
;
open_mode:
  INPUT  { $$=1; }
| I_O    { $$=2; }
| OUTPUT { $$=3; }
| EXTEND { $$=4; }
| error  { yyerror("invalid OPEN mode"); }
;
open_varlist:
  name { gen_open($<ival>0, $<tree>1); }
| open_varlist opt_sep name { gen_open($<ival>0, $<tree>3); }
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM perform_options
  ;
perform_options:
      conditional_statement_list END_PERFORM { $$ = NULL; }
    | gname TIMES
      {
        gen_push_int($1);
        $<dval>$=gen_marklabel();
        gen_perform_test_counter($<dval>$);
      }
      conditional_statement_list
      {
        gen_perform_times($<dval>3);
      }
      END_PERFORM { $$ = NULL; }
    | opt_with_test UNTIL
      {
        if ($1 == 2) {
           lbstart=gen_passlabel();
        }
        $<dval>$=gen_marklabel();
      }
      condition
      {
        $<dval>$=gen_orstart();
        if ($1 == 2) {
           lbend=gen_passlabel();
           gen_dstlabel(lbstart);
        }
      }
      conditional_statement_list
      {
        if ($1 == 2) {
           gen_jmplabel($<dval>3);
           gen_dstlabel(lbend);
           gen_jmplabel(lbstart);
           gen_dstlabel($<dval>5);
        }
        else {
           gen_jmplabel($<dval>3);
           gen_dstlabel($<dval>5);
        }
      }
      END_PERFORM { $$ = NULL; }
    | opt_with_test VARYING name FROM gname opt_by gname UNTIL
      {
        gen_move($5,$3);
        /* BEFORE=1 AFTER=2 */
        if ($1 == 2) {
           lbstart=gen_passlabel();
        }
        $<dval>$=gen_marklabel();
      }
      condition
      {
        $<dval>$=gen_orstart();
        /* BEFORE=1 AFTER=2 */
        if ($1 == 2) {
           gen_add($7,$3,0);
           gen_dstlabel(lbstart);
        }
      }
      opt_perform_after
      conditional_statement_list
      {
        int i;
        struct perf_info *rf;
        /*struct perform_info *rpi;*/
        char *vn;

        /* Check for duplicate varaibles in VARYING/AFTER */
        if ($12 != NULL) {
           if ((vn = check_perform_variables($3, $12)) != NULL) {
              yyerror("Duplicate variable '%s' in VARYING/AFTER clause", vn);
           }
        }

        if ($1 == 2) {
           if ($12 != NULL) {
              for (i=3; i>=0; i--) {
                 rf = $12->pf[i];
                 if (rf != NULL) {
                    gen_jmplabel(rf->ljmp);
                    gen_dstlabel(rf->lend);
                 }
              }
           }
           gen_jmplabel($<dval>9);
           gen_dstlabel($<dval>11);
        }
        else {
           if ($12 != NULL) {
              for (i=3; i>=0; i--) {
                 rf = $12->pf[i];
                 if (rf != NULL) {
                    gen_add(rf->pname1, rf->pname2, 0);
                    gen_jmplabel(rf->ljmp);
                    gen_dstlabel(rf->lend);
                 }
              }
           }
           gen_add($7,$3,0);
           gen_jmplabel($<dval>9);
           gen_dstlabel($<dval>11);
        }
      }
      END_PERFORM { $$ = NULL; }
    | label opt_perform_thru
      {
        gen_perform_thru($1,$2);
        $$ = NULL;
      }
    | label opt_perform_thru opt_with_test UNTIL
      {
        $<dval>$=gen_marklabel();
        /* BEFORE=1 AFTER=2 */
        if ($3 == 2) {
        gen_perform_thru($1,$2);
        }
      }
      condition
      {
        unsigned long lbl;
        lbl=gen_orstart();
        /* BEFORE=1 AFTER=2 */
        if ($3 == 1) {
        gen_perform_thru($1,$2);
        }
        gen_jmplabel($<dval>5);
        gen_dstlabel(lbl);
      }
    | label opt_perform_thru gname TIMES
      {
        unsigned long lbl;
        gen_push_int($3);
        lbl = gen_marklabel();
        gen_perform_test_counter(lbl);
        gen_perform_thru($1,$2);
        gen_perform_times(lbl);
      }
    | label opt_perform_thru opt_with_test VARYING name
      FROM gname opt_by gname UNTIL
      {
        gen_move($7,$5);
        if ($3 == 2) {
           lbstart=gen_passlabel();
        }
        $<dval>$ = gen_marklabel();
      }
      condition
      {
        $<dval>$ = gen_orstart();
        /* BEFORE=1 AFTER=2 */
        if ($3 == 2) {
           gen_add($9,$5, 0);
           gen_dstlabel(lbstart);
        }
      }
      opt_perform_after
      {
        int i;
        struct perf_info *rf;
        /*struct perform_info *rpi;*/
        char *vn = NULL;

        /* Check for duplicate varaibles in VARYING/AFTER */
        if ($14 != NULL) {
           if ((vn = check_perform_variables($5, $14)) != NULL) {
              yyerror("Duplicate variable '%s' in VARYING/AFTER clause", vn);
           }
        }
    gen_perform_thru($1,$2);
        /* BEFORE=1 AFTER=2 */
        if ($3 == 2) {
           if ($14 != NULL) {
              for (i=3; i>=0; i--) {
                 rf = $14->pf[i];
                 if (rf != NULL) {
                    gen_jmplabel(rf->ljmp);
                    gen_dstlabel(rf->lend);
                 }
              }
           }
           gen_jmplabel($<dval>11);
           gen_dstlabel($<dval>13);
        }
        else {
           if ($14 != NULL) {
              for (i=3; i>=0; i--) {
                 rf = $14->pf[i];
                 if (rf != NULL) {
                    gen_add(rf->pname1, rf->pname2, 0);
                    gen_jmplabel(rf->ljmp);
                    gen_dstlabel(rf->lend);
                 }
              }
           }
           gen_add($9,$5,0);
           gen_jmplabel($<dval>11);
           gen_dstlabel($<dval>13);
        }
        $$ = NULL;
      }
    ;

opt_perform_thru:
    /* nothing */ { $$ = NULL; }
    | THRU label { $$ = $2;}
    ;

opt_with_test: { $<ival>$=1; perform_after_sw=1; }
    | opt_with TEST before_after
      {
       $$=$3;
       perform_after_sw=$3;
      }
    ;

opt_perform_after:   /* nothing */ { $$=NULL; }
    | AFTER perform_after
     {
      $<pfvals>$=create_perform_info();
      $<pfvals>$->pf[0] = $2;
      $$=$<pfvals>$;
     }
    | AFTER perform_after AFTER perform_after
     {
      $<pfvals>$=create_perform_info();
      $<pfvals>$->pf[0] = $2;
      $<pfvals>$->pf[1] = $4;
      $$=$<pfvals>$;
     }
    | AFTER perform_after AFTER perform_after
      AFTER perform_after
     {
      $<pfvals>$=create_perform_info();
      $<pfvals>$->pf[0] = $2;
      $<pfvals>$->pf[1] = $4;
      $<pfvals>$->pf[2] = $6;
      $$=$<pfvals>$;
     }
    | AFTER perform_after AFTER perform_after
      AFTER perform_after AFTER perform_after
     {
      $<pfvals>$=create_perform_info();
      $<pfvals>$->pf[0] = $2;
      $<pfvals>$->pf[1] = $4;
      $<pfvals>$->pf[2] = $6;
      $<pfvals>$->pf[3] = $8;
      $$=$<pfvals>$;
     }
    ;

perform_after: name FROM gname
       opt_by gname UNTIL
      {
        gen_move($3,$1);
        /* BEFORE=1 AFTER=2 */
        if (perform_after_sw == 2) {
           lbstart=gen_passlabel();
        }
        $<dval>$ = gen_marklabel();
      }
       condition
      {
        unsigned long lbl;
        lbl=gen_orstart();
        /* BEFORE=1 AFTER=2 */
        if (perform_after_sw == 2) {
           gen_add($5,$1,0);
           gen_dstlabel(lbstart);
           $$ = create_perf_info($5, $1, $<dval>7, lbl);
        }
        else {
           $$ = create_perf_info($5, $1, $<dval>7, lbl);
        }
      }
    ;
before_after:
    BEFORE  { $$=1; }
    | AFTER { $$=2; }
    ;


/*
 * READ statements
 */

read_statement:
  READ read_body opt_end_read
;
read_body:
  name opt_read_next opt_record opt_read_into opt_read_key
  {
    if (gen_reads($1, $4, $5, $2, 0) != 0)
      YYABORT;
  }
| name opt_read_next opt_record opt_read_into opt_read_key opt_read_at_end
  {
    gen_reads($1, $4, $5, $2, 1);
    ginfo_container4($6);
    gic = NULL;
  }
| name opt_read_next opt_record opt_read_into opt_read_key opt_read_invalid_key
  {
    gen_reads($1, $4, $5, $2, 2);
    gen_test_invalid_keys ($6);
  }
;
opt_read_next:
  /* nothing */			{ $$ = 0; }
| NEXT				{ $$ = 1; }
| PREVIOUS			{ $$ = 2; }
;
opt_read_into:
  /* nothing */			{ $$ = NULL; }
| INTO name			{ $$ = $2; }
;
opt_read_key:
  /* nothing */			{ $$ = NULL; }
| KEY opt_is name		{ $$ = $3; }
;
opt_read_at_end:
  NOT opt_at on_end
  {
    ginfo_container2($3, 2);
    $$=ginfo_container3($3, 2);
  }
| AT on_end
  {
    ginfo_container2($2, 1);
    $$=ginfo_container3($2, 1);
  }
| on_end
  {
    ginfo_container2($1, 1);
    $$=ginfo_container3($1, 1);
  }
| AT on_end NOT opt_at
  {
    ginfo_container2($2, 1);
  }
  on_end
  {
    ginfo_container2($6, 2);
    $$=ginfo_container3($6, 3);
  }
| on_end NOT opt_at
  {
    ginfo_container2($1, 1);
  }
  on_end
  {
    ginfo_container2($5, 2);
    $$=ginfo_container3($5, 3);
  }
;
on_end:
  END
  {
    if (gic == NULL)
      gic=ginfo_container0();
    $$=ginfo_container1(gic);
  }
  statement_list
  {
    $$=$<gic>2;
  }
  ;
opt_read_invalid_key:
  read_invalid_key		{ $$ = gen_invalid_keys ($1, NULL); }
| read_not_invalid_key		{ $$ = gen_invalid_keys (NULL, $1); }
| read_invalid_key
  read_not_invalid_key		{ $$ = gen_invalid_keys ($1, $2); }
;
read_invalid_key:
  INVALID opt_key		{ $<ike>$ = gen_before_invalid_key (); }
  statement_list		{ $$ = gen_after_invalid_key ($<ike>3); }
;
read_not_invalid_key:
  NOT INVALID opt_key		{ $<ike>$ = gen_before_invalid_key (); }
  statement_list		{ $$ = gen_after_invalid_key ($<ike>4); }
;
opt_end_read: | END_READ ;


/*
 * RELEASE statement
 */

release_statement:
  RELEASE name opt_write_from
  {
    if ($2->level != 1)
      yyerror ("variable %s could not be used for RELEASE", $2->name);
    gen_release($2, $3);
  }
;


/*
 * RETURN statements
 */

return_statement:
  RETURN return_body opt_end_return
;
return_body:
  name opt_record opt_read_into
  {
    gen_reads ($1, $3, NULL, 1, 4);
  }
| name opt_record opt_read_into opt_read_at_end
  {
    gen_reads ($1, $3, NULL, 1, 5);
    ginfo_container4 ($4);
    gic = NULL;
  }
;
opt_end_return: | END_RETURN ;


/*
 * REWRITE statement
 */

rewrite_statement:
    REWRITE name opt_write_from
    {
      if ($2->level != 1)
        yyerror("variable %s could not be used for REWRITE", $2->name);
      gen_rewrite($2, $3);
    }
    opt_invalid_key
    opt_end_rewrite
    ;
opt_end_rewrite:
    /* nothing */
    | END_REWRITE
    ;


/*
 * SEARCH statement
 */

search_statement:
      SEARCH search_body opt_end_search
    | SEARCH ALL search_all_body opt_end_search
search_body:
      variable_indexed
      {
        $<dval>$=loc_label++; /* determine END label name */
        gen_marklabel();
      }
      search_opt_varying
      {
        $<dval>$=loc_label++; /* determine search loop start label */
        if ($3 == NULL) {
          $3=determine_table_index_name($1);
          if ($3 == NULL) {
             yyerror("Unable to determine search index for table '%s'", $1->name);
          }
        }
        gen_jmplabel($<dval>$); /* generate GOTO search loop start  */
      }
      search_opt_at_end
      {
        gen_jmplabel($<dval>2); /* generate GOTO END  */
        gen_dstlabel($<dval>4); /* generate search loop start label */
        $$ = $<dval>2;
      }
      search_when_list
      {
        /* increment loop index, check for end */
        gen_SearchLoopCheck($5, $3, $1);

        gen_jmplabel($<dval>4); /* generate goto search loop start label */
        gen_dstlabel($<dval>2); /* generate END label */
      }
    ;
search_all_body:
     variable_indexed
     {
        lbend=loc_label++; /* determine END label name */
        gen_marklabel();

        lbstart=loc_label++; /* determine search_all loop start label */

        $<tree>$=determine_table_index_name($1);
        if ($<tree>$ == NULL) {
           yyerror("Unable to determine search index for table '%s'", $1->name);
        }
        else {
          /* Initilize and store search table index boundaries */
          Initialize_SearchAll_Boundaries($1, $<tree>$);
        }

        gen_jmplabel(lbstart); /* generate GOTO search_all loop start  */
     }
     search_opt_at_end
     {
        gen_jmplabel(lbend); /* generate GOTO END  */
        gen_dstlabel(lbstart); /* generate search loop start label */
     }
     search_all_when_list
     {
        /* adjust loop index, check for end */
        gen_SearchAllLoopCheck($3, $<tree>2, $1, curr_field, lbstart, lbend);
     }
    ;
search_opt_varying:
    VARYING variable {  $$=$2; }
    | { $$=NULL; }
    ;
search_opt_at_end:
     opt_at END
     {
       $<dval>$=loc_label++; /* determine ATEND label name */
       gen_dstlabel($<dval>$); /* determine ATEND label name */
     }
     statement_list
     {
       $<dval>$=$<dval>3;
     }
    |
     {
       $<dval>$=loc_label++; /* determine ATEND label name */
       gen_dstlabel($<dval>$); /* determine ATEND label name */
     }
    ;
search_when_list:
     search_when { $$=$1; }
     | search_when_list search_when { $$=$1; }
     ;
search_when:
     WHEN
     search_when_conditional
     { $<dval>$=gen_testif(); }
     conditional_statement_list
     {
        $$ = $<dval>0;
        gen_jmplabel($$); /* generate GOTO END  */
        gen_dstlabel($<dval>3);
     }
     ;
search_when_conditional:
    name cond_op name { gen_compare($1,$2,$3); }
    | name cond_op nliteral { gen_compare($1,$2,(cob_tree)$3); }
    | nliteral cond_op name { gen_compare((cob_tree)$1,$2,$3); }
    | nliteral cond_op nliteral {
                gen_compare((cob_tree)$1,$2,(struct sym*)$3); }
    ;
search_all_when_list:
     search_all_when
     | search_all_when_list search_all_when
     ;
search_all_when:
     WHEN { curr_field = NULL; }
     search_all_when_conditional
     { $<dval>$=gen_testif(); }
     conditional_statement_list
     {
        gen_jmplabel(lbend); /* generate GOTO END  */
        gen_dstlabel($<dval>4);
     }
    ;
search_all_when_conditional:
  variable opt_is equal_to variable
  {
    if (curr_field == NULL)
      curr_field = $1;
    gen_compare($1,$3,$4);
  }
| variable opt_is equal_to literal
  {
    if (curr_field == NULL)
      curr_field = $1;
    gen_compare($1,$3,(cob_tree)$4);
  }
| search_all_when_conditional AND { $<dval>$=gen_andstart(); }
  search_all_when_conditional  { gen_dstlabel($<dval>3); }
;
opt_end_search:
    /* nothing */
    | END_SEARCH
    ;


/*
 * SET statement
 */

set_statement:
   SET set_list
   ;
set_list:
   set_target TO opt_address_of set_variable_or_nlit
   { gen_set($1,SET_TO,$4,0,$3); }
  | variable UP BY var_or_nliteral
   { gen_set($1,SET_UP_BY,$4,0,0); }
  | variable DOWN BY var_or_nliteral
   { gen_set($1,SET_DOWN_BY,$4,0,0); }
  | opt_address_of variable TO opt_address_of set_variable
   { gen_set($2,SET_TO,$5,$1,$4); }
  ;
set_target:
    variable  { $$ = $1; }
  | VARCOND   { $$ = $1; }
  ;
set_variable:
   variable	   { $$ = $1; }
  | TOK_NULL	   { $$ = NULL; }
  ;
opt_address_of:
  /* nothing */ { $$ = 0; }
  | ADDRESS opt_of { $$ = 1; }
  ;
set_variable_or_nlit:
  name_or_lit	  { $$ = $1; }
  | TOK_NULL	  { $$ = NULL; }
  | TOK_TRUE
    {
      $$ = (cob_tree)1;
      /* no (cob_tree) may have this value! */
    }
  ;


/*
 * SORT statement
 */

sort_statement:
    SORT name sort_keys   { gen_sort($2); }
    sort_input sort_output { /*gen_close_sort($2);*/ }
sort_keys:
    /* nothing */   { $$ = NULL; }
    | sort_keys sort_direction KEY name
        {
            $4->direction = $2;
            (cob_tree)$4->sort_data =
                (cob_tree)($<tree>0->sort_data);
            (cob_tree)($<tree>0->sort_data) = $4;
            $$ = $4;
        }
    ;
sort_direction:
  ASCENDING			{ $$ = 1; }
| DESCENDING			{ $$ = 2; }
;
sort_input:
    INPUT PROCEDURE opt_is sort_range { $$=NULL; }
    | USING sort_file_list { gen_sort_using($<tree>-2,$2); $$=$2; }
    ;
sort_output:
    OUTPUT PROCEDURE opt_is sort_range { $$=NULL; }
    | GIVING sort_file_list { gen_sort_giving($<tree>-3,$2); $$=$2; }
    ;
sort_file_list:
  name { $$ = alloc_sortfile_node($1);  }
  | sort_file_list name
   {
    $1->next = alloc_sortfile_node($2); $$=$1;
   }
  ;
sort_range: label opt_perform_thru
      {
        gen_perform_thru($1,$2);
                $$ = ($2 == NULL) ? $1 : $2;
      }
    ;


/*
 * START statement
 */

start_statement:
    START start_body
    opt_invalid_key
    opt_end_start
    ;
start_body:
      name { gen_start($1,0,NULL); }
    | name KEY opt_is cond_op name { gen_start($1,$4,$5); }
    ;
opt_end_start:
    /* nothing */
    | END_START
    ;


/*
 * STOP RUN statement
 */

stoprun_statement:
    STOP RUN { gen_stoprun(); }
    ;


/*
 * STRING statement
 */

string_statement:
  STRING string_from_list
  INTO name string_with_pointer {
    gen_string( $2, $4, $5 );
  }
  opt_on_overflow
  opt_end_string
  ;
opt_end_string:
    | END_STRING
    ;


/*
 * SUBTRACT statement
 */

subtract_statement:
    SUBTRACT subtract_body opt_end_subtract
    ;
subtract_body:
      var_list_gname FROM var_list_name opt_on_size_error
      {
        gen_subtract1($1, $3, $4);
      }
    | var_list_gname FROM numeric_value GIVING var_list_name opt_on_size_error
      {
        gen_subtract2($1, $5, $3, $6);
      }
    | CORRESPONDING gname FROM name flag_rounded
      {
        gen_subtractcorr($2, $4, $5);
      }
    ;
opt_end_subtract:
    | END_SUBTRACT
    ;


/*
 * UNSTRING statement
 */

unstring_statement:
    UNSTRING name
    unstring_delimited
    INTO unstring_destinations
    string_with_pointer
    unstring_tallying {
	gen_unstring( $2, $3, $5, $6, $7 );
    }
    opt_on_overflow
    opt_end_unstring
    ;
unstring_delimited:
    DELIMITED opt_by unstring_delimited_vars { $$=$3; }
    | /* nothing */                          { $$=NULL; }
    ;
unstring_delimited_vars:
    flag_all gname       { $$=alloc_unstring_delimited($1,$2); }
    | unstring_delimited_vars OR flag_all gname {
      struct unstring_delimited *ud;
      ud=alloc_unstring_delimited($3,$4);
      ud->next = $1;
      $$=ud;
    }
    ;
unstring_destinations:
    unstring_dest_var       { $$=$1; }
    | unstring_destinations opt_sep
        unstring_dest_var   {
            $3->next = $1;
            $$ = $3;
        }
    ;
unstring_dest_var:
    name opt_unstring_delim opt_unstring_count {
            $$ = alloc_unstring_dest( $1, $2, $3 );
        }
    ;
opt_unstring_delim:
    /* nothing */           { $$=NULL; }
    | DELIMITER opt_in name { $$=$3; }
    ;
opt_unstring_count:
    /* nothing */           { $$=NULL; }
    | COUNT opt_in name   { $$=$3; }
    ;
unstring_tallying:
    /* nothing */           { $$=NULL; }
    | TALLYING opt_in name  { $$=$3; }
    ;
opt_on_overflow:
    on_overflow
    on_not_overflow
    ;
on_overflow:
    ON OVERFLOW          { $<dval>$ = gen_at_end(-1); }
        statement_list            { gen_dstlabel($<dval>3); }
    | /* nothing */
    ;
on_not_overflow:
    NOT ON OVERFLOW { $<dval>$ = gen_at_end(0); }
        statement_list            { gen_dstlabel($<dval>4); }
    | /* nothing */
    ;
opt_end_unstring:
    | END_UNSTRING
    ;


/*
 * WRITE statement
 */

write_statement:
    WRITE name opt_write_from write_options
    {
      if ($2->level != 1)
        yyerror("variable %s could not be used for WRITE", $2->name);
      gen_write($2, $4, $3);
    }
    opt_invalid_key
    opt_end_write
    ;
opt_write_from:
    /* nothing */       { $$ = NULL; }
    | FROM gname        { $$ = $2; }
    ;
write_options:
    /* nothing */       { $$ = 0; }
    | before_after opt_advancing gname opt_line
                        { gen_loadvar($3); $$ = $1; }
    | before_after opt_advancing PAGE
                        { $$ = -$1; }
    ;
opt_end_write:
    /* nothing */
    | END_WRITE
    ;
opt_advancing: | ADVANCING ;


/*******************
 * Substructures
 *******************/


/*
 * Expressions
 */

expr:
      gname            { $$ = $1; }
    | '(' expr ')'     { $$ = $2; }
    | expr '+' expr    { $$ = make_expr ($1, '+', $3); }
    | expr '-' expr    { $$ = make_expr ($1, '-', $3); }
    | expr '*' expr    { $$ = make_expr ($1, '*', $3); }
    | expr '/' expr    { $$ = make_expr ($1, '/', $3); }
    | expr '^' expr    { $$ = make_expr ($1, '^', $3); }
    ;
/* opt_expr will be NULL or a (cob_tree) pointer if the expression
   was given, otherwise it will be valued -1 */
opt_expr:
    /* nothing */   { $$ = (cob_tree)-1; }
    | expr          { $$ = $1; }
    ;


/*******************
 * Common rules
 *******************/

var_list_name: name flag_rounded opt_sep
     {
      $$ = create_mathvar_info(NULL, $1, $2);
     }
    | var_list_name name flag_rounded opt_sep
     {
      $$ = create_mathvar_info($1, $2, $3);
     }
    ;

var_list_gname:
    gname
    {
      $$ = create_mathvar_info(NULL, $1, 0);
    }
    | var_list_gname opt_sep gname
      {
	$$ = create_mathvar_info($1, $3, 0);
      }
    ;


/*
 * ON SIZE ERROR
 */

opt_on_size_error:
    /* nothing */	{ $$ = NULL; }
  | on_size_error	{ $$ = $1; }
  ;
on_size_error:
  NOT opt_on SIZE error_sentence
  {
    $$=math_on_size_error4($4, 2);
  }
| opt_on SIZE error_sentence
  {
    $$=math_on_size_error4($3, 1);
  }
| opt_on SIZE error_sentence
  NOT opt_on SIZE
  {
    $3->lbl1=$3->ose;
  }
  error_sentence
  {
    tmose = NULL;
    $$ = math_on_size_error4($8, 3);
  }
;
error_sentence:
     ERROR
     {
       if ( tmose == NULL ) {
         tmose = math_on_size_error0();
         $$ = math_on_size_error1(tmose);
       } else {
	 $$ = math_on_size_error1(tmose);
       }
     }
     statement_list
     {
      math_on_size_error2();
      $$=$<mose>2;
     }
    ;

opt_invalid_key:
    opt_invalid_key_sentence
    opt_not_invalid_key_sentence
    ;
opt_invalid_key_sentence:
    INVALID opt_key             { $<dval>$ = gen_at_end(23); }
    statement_list                    { gen_dstlabel($<dval>3); }
    | /* nothing */
    ;
opt_not_invalid_key_sentence:
    NOT INVALID opt_key    { $<dval>$ = gen_at_end(0); }
    statement_list                    { gen_dstlabel($<dval>4); }
    | /* nothing */
    ;
string_with_pointer:
    opt_with POINTER name  { $$ = $3; }
    | /* nothing */        { $$ = NULL; }
    ;
string_from_list:
    string_from             { $$ = $1; }
    | string_from_list opt_sep string_from  {
            $3->next = $1;
            $$ = $3;
        }
    | error { yyerror("variable expected"); }
    ;
string_from:
    gname   {
                $$ = alloc_string_from( $1, NULL );
            }
    | gname DELIMITED opt_by delimited_by {
                $$ = alloc_string_from( $1, $4 );
            }
    ;
delimited_by:
    gname     { $$=$1; }
    | SIZE          { $$=NULL; }
    | error { yyerror("SIZE or identifier expected"); }
    ;


/*
 * Condition
 */

condition:
    simple_condition
    | NOT  condition    { gen_not(); $$=$2; }
    | condition AND     { $<dval>$=gen_andstart(); }
                implied_op_condition { gen_dstlabel($<dval>3); $$=$4; }
    | condition OR      { $<dval>$=gen_orstart(); }
        implied_op_condition { gen_dstlabel($<dval>3); $$=$4; }
    | '(' condition ')' { $$ = $2; }
    | VARCOND {
      gen_condition($1);
      $$.sy=NULL;
      $$.oper=0;
    }
    ;
simple_condition:
    expr extended_cond_op
    {
      if ($2 & COND_UNARY)
	{
	  if ($2 & COND_CLASS)
	    gen_class_check ($1, $2);
	  else
	    gen_compare ($1, $2 & ~COND_UNARY, (cob_tree) spe_lit_ZE);
	}
    }
    opt_expr
    {
      if ($2 & COND_UNARY)
	{
	  if ((int) $4 != -1)
	    yyerror ("class or sign conditions are unary");
	}
      else
	{
	  if ((int) $4 == -1)
	    yyerror ("expression expected in a binary condition");
	  else
	    gen_compare ($1, $2, $4);
	}
      $$.sy = $1;			/* for implied operands */
      $$.oper = $2;
    }
    ;

implied_op_condition:
        condition               { $$ = $1; }
        | cond_op expr  {
	  if ($<condval>-2.sy == NULL) {
	    yyerror("invalid implied condition");
	  } else {
	    gen_compare($<condval>-2.sy,$1,$2);
	  }
	  $$.sy = $<condval>-2.sy;
	  $$.oper = $1;
	}
        | expr          { /* implied both the first operand and the operator */
	  if (($<condval>-2.sy == NULL)||
	      ($<condval>-2.oper & COND_UNARY)) {
	    yyerror("invalid implied condition");
	  } else {
	    gen_compare($<condval>-2.sy,$<condval>-2.oper,$1);
	  }
	  $$.sy = $<condval>-2.sy;
	  $$.oper = $<condval>-2.oper;
	}
        ;
extended_cond_op:
    IS ext_cond                 { $$ = $2; }
    | IS NOT ext_cond           { $$ = $3 ^ 7; }
    | IS ext_cond OR ext_cond   { $$ = $2 | $4; }
    | ext_cond                  { $$ = $1; }
    | NOT opt_is ext_cond       { $$ = $3 ^ 7; }
    | ext_cond OR ext_cond      { $$ = $1 | $3; }
    ;
ext_cond:
    conditional                 { $$ = $1; }
    | class_condition           { $$ = $1 | COND_UNARY | COND_CLASS; }
    | sign_condition            { $$ = $1; }
    ;
cond_op:
      conditional               { $$ = $1; }
    | NOT conditional           { $$ = $2 ^ 7; }
    | conditional OR conditional { $$ = $1 | $3; }
    ;
equal_to:
  EQUAL opt_to			{ $$ = RELATION_EQ; }
| '=' opt_to			{ $$ = RELATION_EQ; }
;
greater_than:
  GREATER opt_than		{ $$ = RELATION_GT; }
| '>' opt_than			{ $$ = RELATION_GT; }
;
less_than:
  LESS opt_than			{ $$ = RELATION_LT; }
| '<' opt_than			{ $$ = RELATION_LT; }
;
conditional:
  equal_to			{ $$ = $1; }
| greater_than			{ $$ = $1; }
| less_than			{ $$ = $1; }
| TOK_GE			{ $$ = RELATION_GE; }
| TOK_LE			{ $$ = RELATION_LE; }
;
class_condition:
  NUMERIC			{ $$ = CLASS_NUMERIC; }
| ALPHABETIC			{ $$ = CLASS_ALPHABETIC; }
| ALPHABETIC_LOWER		{ $$ = CLASS_ALPHABETIC_LOWER; }
| ALPHABETIC_UPPER		{ $$ = CLASS_ALPHABETIC_UPPER; }
;
sign_condition:
  POSITIVE			{ $$ = RELATION_GT | COND_UNARY; }
| NEGATIVE			{ $$ = RELATION_LT | COND_UNARY; }
| ZEROS				{ $$ = RELATION_EQ | COND_UNARY; }
;


/*****************************************************************************
 * Common rules
 *****************************************************************************/

/*
 *
 */

opt_gname:
    /* nothing */ { $$ = NULL; }
    | gname       { $$ = $1; }
    ;
gname:  name    { $$ = $1; }
    | gliteral      { $$ = (cob_tree)$1;}
    | function_call
    ;
function_call:
    FUNCTION idstring '(' parameters ')'
    {
      yyerror ("function call is not supported yet");
      YYABORT;
    }
    ;
numeric_value:
    gname
    {
      if (!is_numeric_sy ($1))
	yyerror ("non-numeric value: %s", $1->name);
      $$ = $1;
    }
  ;
parameters:
      gname			{ $$ = cons ($1, NULL); }
    | parameters opt_sep gname	{ $$ = cons ($3, $1); }
    ;
idstring:
    { start_condition = START_ID; } IDSTRING { $$ = $2; }
    ;

name_or_lit:
    name      { $$ = $1; }
    | literal { $$ = (cob_tree)$1; }
    ;
gliteral:
    without_all_literal
    | all_literal
    ;
without_all_literal:
    literal             { $$=$1; }
    | special_literal   { $$ = $1; }
    ;
all_literal:
    ALL literal { LITERAL ($2)->all=1; $$=$2; }
    | ALL special_literal { $$=$2; }
    ;
special_literal:
  SPACES        { $$=spe_lit_SP; }
| ZEROS         { $$=spe_lit_ZE; }
| QUOTES        { $$=spe_lit_QU; }
| HIGH_VALUES   { $$=spe_lit_HV; }
| LOW_VALUES    { $$=spe_lit_LV; }
;
var_or_nliteral:
    variable        { $$ = $1; }
    | nliteral      { $$ = (cob_tree)$1; }
    ;
literal:
    nliteral		{ $$=$1; }
    | CLITERAL		{ save_literal($1,'X'); LITERAL ($1)->all=0; $$=$1; }
    ;
nliteral:
  signed_nliteral {
      if (strchr (FIELD_NAME ($1), decimal_comma ? '.' : ','))
	yyerror ("wrong decimal point character in numeric literal");
      save_literal($1,'9');
      LITERAL ($1)->all = 0;
      $$=$1;
  }
  ;
signed_nliteral:
        INTEGER_TOK  { $$ = $1; }
  | '+' INTEGER_TOK  { $$ = $2; }
  | '-' INTEGER_TOK  { $$ = invert_literal_sign($2); }
  |     NLITERAL  { $$ = $1; }
  | '+' NLITERAL  { $$ = $2; }
  | '-' NLITERAL  { $$ = invert_literal_sign($2); }
  ;
opt_def_name:
    def_name        { $$ = $1; }
    | /* nothing */ { $$ = make_filler(); }
    ;
def_name:
    SYMBOL  { if ($1->defined)
                yyerror("variable redefined, %s",$1->name);
              $1->defined=1;
              $$=$1;
            }
    | FILLER    { $<tree>$=make_filler(); }
    ;
variable_indexed:
    SUBSCVAR
    {
      if ($1->times == 1)
         yyerror("\"%s\" is not an indexed variable ", $1->name);
      $$=$1;
    }
    ;

filename:
    literal { $$=(cob_tree)$1; }
    | SYMBOL {$$=$1; }
    ;
name:
    variable '(' gname ':' opt_gname ')'
    {
      $$ = (cob_tree) create_refmoded_var($1, $3, $5);
    }
    | variable
    ;
variable:
    qualified_var {
      $$=$1;
      if (need_subscripts) {
	yyerror("this variable \'%s\' must be subscripted or indexed", $1->name);
	need_subscripts=0;
      }
    }
    | qualified_var LPAR subscript_list ')' {
      $$ = (cob_tree)make_subref( $1, $3 );
      }
    ;
qualified_var:
    unqualified_var         { $$=$1; }
    | qualified_var in_of unqualified_var { $$=lookup_variable($1,$3); }
    ;
unqualified_var:
    VARIABLE        { $$=$1; }
    | SUBSCVAR      { need_subscripts=1; $$=$1; }
    ;
subscript_list:
      subscript                         { $$ = $1; }
    | subscript_list opt_sep subscript  { $$ = add_subscript($1, $3); }
    ;
subscript:
      gname                     { $$ = create_subscript( $1 ); }
    | subscript '+' gname       { $$ = add_subscript_item( $1, '+', $3 ); }
    | subscript '-' gname       { $$ = add_subscript_item( $1, '-', $3 ); }
    ;
integer:
  INTEGER_TOK
  {
    char *s = $1->name;
    $$ = 0;
    while (*s)
      $$ = $$ * 10 + *s++ - '0';
  }
;
label:
  LABELSTR in_of LABELSTR
  {
    cob_tree lab = $1;
    if (lab->defined == 0)
      {
	lab->defined = 2;
	lab->parent = $3;
      }
    else if ((lab = lookup_label (lab, $3)) == NULL)
      {
	lab = install ($1->name, SYTB_LAB, 2);
	lab->defined = 2;
	lab->parent = $3;
      }
    $$ = lab;
  }
| LABELSTR
  {
    cob_tree lab = $1;
    if (lab->defined == 0)
      {
	lab->defined = 2;
	lab->parent = curr_section;
      }
    else if ((lab = lookup_label (lab, curr_section)) == NULL)
      {
	lab = install ($1->name, SYTB_LAB, 2);
	lab->defined = 2;
	lab->parent = curr_section;
      }
    $$ = lab;
  }
;
in_of: IN | OF ;


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
flag_rounded:
  /* nothing */			{ $$ = 0; }
| ROUNDED			{ $$ = 1; }
;


/*
 * Common optional words
 */

opt_is_are: | IS | ARE ;
opt_dot: | '.' ;
opt_sign: | SIGN ;
opt_area: | AREA ;
opt_for: | FOR ;
opt_in_size: | IN SIZE ;
opt_is: | IS ;
opt_mode: | MODE ;
opt_at: | AT ;
opt_by: | BY ;
opt_in: | IN ;
opt_key: | KEY ;
opt_on: | ON ;
opt_record: | RECORD ;
opt_sep: | ',' ;
opt_than: | THAN ;
opt_then: | THEN ;
opt_line: | LINE ;
opt_final: | FINAL ;
opt_of: | OF ;
opt_to: | TO ;
opt_upon: | UPON ;
opt_with: | WITH ;
opt_when: | WHEN ;


%%

static void
assert_numeric_sy (cob_tree sy)
{
  if (!is_numeric_sy (sy))
    yyerror ("non numeric variable: %s", sy->name);
}

void
yywarn (char *fmt, ...)
{
  va_list argptr;

  /* Print warning line */
  printf ("%s:%d: warning: ", cob_orig_filename, cob_orig_lineno);

  /* Print error body */
  va_start (argptr, fmt);
  vprintf (fmt, argptr);
  putchar ('\n');
  va_end (argptr);

  /* Count warning */
  warning_count++;
}

void
yyerror (char *fmt, ...)
{
  va_list argptr;

  /* Print error line */
  printf ("%s:%d: ", cob_orig_filename, cob_orig_lineno);

  /* Print error body */
  va_start (argptr, fmt);
  vprintf (fmt, argptr);
  putchar ('\n');
  va_end (argptr);

  /* Count error */
  error_count++;
}
