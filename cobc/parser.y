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

%expect 528

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

extern int screen_io_enable,scr_line,scr_column;
extern int decimal_comma;
extern char currency_symbol;
extern FILE *lexin;
extern FILE *o_src;
extern struct sym *curr_paragr,*curr_section;
extern struct sym *curr_field;
extern short curr_call_mode;
extern struct sym *pgm_id;
extern unsigned stack_offset;   /* offset das variaveis na pilha */
extern unsigned global_offset;  /* offset das variaveis globais (DATA) */
extern int paragr_num;
extern int loc_label;
extern char picture[];
extern int picix,piccnt,decimals,sign,v_flag;
extern int at_linkage,stack_plus;
extern char *toktext;
extern int yylex(void);
extern struct index_to_table_list *index2table;
extern int pgm_segment;
extern struct lit *spe_lit_ZE;
extern struct lit *spe_lit_SP;
extern struct lit *spe_lit_LV;
extern struct lit *spe_lit_HV;
extern struct lit *spe_lit_QU;

struct sym *curr_file;
struct sym *alloc_filler( void );
int curr_division=0;
int need_subscripts=0;
extern char *yytext;

unsigned long lbend, lbstart;
unsigned int perform_after_sw;

/* struct math_var *vl1, *vl2; */
struct math_ose *tmose=NULL;
struct ginfo    *gic=NULL;

extern void yywarn(char *s,...);
static void assert_numeric_sy (struct sym *sy);
%}

%union {
    struct sym *sval;       /* symbol */
    int ival;               /* int */
    struct coord_pair pval; /* lin,col */
    struct lit *lval;       /* literal */
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
}

%nonassoc LOW_PREC

%token <str>  IDSTRING
%token <sval> STRING,VARIABLE,VARCOND,SUBSCVAR
%token <sval> LABELSTR,CMD_LINE,ENVIRONMENT_VARIABLE
%token <ival> CHAR,MULTIPLIER
%token <ival> USAGENUM,ZERONUM,CONDITIONAL
%token <ival> TO,FOR,IS,ARE,THRU,THAN,NO
%token <ival> COMMENTING,DIRECTION,READ,WRITE,INPUT_OUTPUT
%token <lval> NLITERAL,CLITERAL
%token <ival> PORTNUM,DATE_TIME


%left   '+','-'
%left   '*','/'
%left   POW_OP

%left  OR
%left  AND
%right NOT

%token BEFORE,AFTER,SCREEN,REVERSEVIDEO,NUMBERTOK,PLUS,MINUS,SEPARATE
%token FOREGROUNDCOLOR,BACKGROUNDCOLOR,UNDERLINE,HIGHLIGHT,LOWLIGHT
%token RIGHT,AUTO,REQUIRED,FULL,JUST,BLINK,SECURE,BELL,COLUMN,SYNC
%token INITIALTOK,FIRSTTOK,ALL,LEADING,OF,IN,BY,STRINGCMD,UNSTRING
%token START,DELETE,DATE_TIME,PROGRAM,GLOBAL,EXTERNAL,SIZE,DELIMITED
%token GIVING,ERASE,INSPECT,TALLYING,REPLACING,ONTOK,POINTER,OVERFLOWTK
%token DELIMITER,COUNT,LEFT,TRAILING,CHARACTER
%token ADD,SUBTRACT,MULTIPLY,DIVIDE,ROUNDED,REMAINDER,ERRORTOK,SIZE
%token FD,SD,REDEFINES,PIC,FILLER,OCCURS,TIMES
%token PROGRAM_ID,DIVISION,CONFIGURATION,SPECIAL_NAMES
%token FILE_CONTROL,I_O_CONTROL
%token SAME,AREA,EXCEPTION
%token ECHOT,FROM,UPDATE
%token WORKING_STORAGE,LINKAGE,DECIMAL_POINT,COMMA
%token FILEN,USAGE,BLANK,COMP1,COMP2
%token SIGN,VALUE,MOVE,LABEL,DARK
%token RECORD,OMITTED,STANDARD,RECORDS,BLOCK
%token CONTAINS,CHARACTERS,COMPUTE,GO,STOP,RUN
%token ACCEPT,PERFORM,VARYING,UNTIL,EXIT
%token IF,ELSE,SENTENCE,LINE,PAGETOK
%token OPEN,CLOSE,REWRITE
%token ADVANCING,INTO,AT,END,NEGATIVE,POSITIVE,SPACES,NOT
%token CALL,USING,INVALID,CONTENT
%token SELECT,ASSIGN,DISPLAY,UPON,CONSOLE,STD_OUTPUT,STD_ERROR
%token ORGANIZATION,ACCESS,MODE,KEY,STATUS,ALTERNATE
%token SEQUENTIAL,INDEXED,DYNAMIC,RANDOM,RELATIVE
%token SECTION,SORT,DUPLICATES,WITH
%token QUOTES,LOWVALUES,HIGHVALUES,NOTEXCEP
%token SET,UP,DOWN
%token TRACE,READY,RESET
%token SEARCH,WHEN,TEST
%token END_ADD,END_CALL,END_COMPUTE,END_DELETE,END_DIVIDE,END_EVALUATE
%token END_IF,END_MULTIPLY,END_PERFORM,END_READ,END_REWRITE,END_SEARCH
%token END_START,END_STRINGCMD,END_SUBTRACT,END_UNSTRING,END_WRITE
%token THEN,EVALUATE,OTHER,ALSO,CONTINUE,CURRENCY,REFERENCE,INITIALIZE
%token NUMERIC,ALPHABETICTOK,ALPHABETICLOWER,ALPHABETICUPPER
%token RETURNING,TOKTRUE,TOKFALSE,ANY,SUBSCVAR,FUNCTION
%token REPORT,TOKRD,TOKCODE,CONTROL,LIMIT,FINAL
%token HEADING,FOOTING,TOKLAST,DETAIL,GROUP,INDICATE,TOKSUM
%token TOKPOSITION,FILE_ID,DEPENDING,TYPETOK,TOKSOURCE
%token INITIATE,GENERATE,TERMINATE,NULLTOK,ADDRESS,NOECHO,LPAR
%token CORRESPONDING,LENGTH
%token TOKDUMMY
%token CONVERTING,OPTIONAL
%token IDENTIFICATION_TOK,ENVIRONMENT_TOK,DATA_TOK,PROCEDURE_TOK
%token AUTHOR_TOK,DATE_WRITTEN_TOK,DATE_COMPILED_TOK,INSTALLATION_TOK
%token SECURITY_TOK,COMMONTOK,RETURN_TOK,END_RETURN,PREVIOUS,NEXT
%token INPUT,I_O,OUTPUT,EXTEND,EOL_TOK,EOS_TOK

 
%right OF
%nonassoc EOS

%type <ival> organization_options,access_options,open_mode
%type <ival> integer,cond_op,conditional,before_after
%type <ival> IF,ELSE,usage,write_options,opt_read_next
%type <ival> using_options,using_parameters
%type <dval> if_part
%type <sval> anystring,name,gname,opt_gname,opt_def_name,def_name,procedure_section
%type <sval> field_description,label,filename,noallname,paragraph,assign_clause
%type <lval> literal,gliteral,without_all_literal,all_literal,special_literal
%type <lval> nliteral
%type <sval> sort_keys,opt_perform_thru
%type <sval> opt_read_into,opt_write_from
%type <sval> variable,sort_range,perform_options,name_or_lit,delimited_by
%type <sval> string_with_pointer
%type <ival> opt_all,with_duplicates,opt_with_test,opt_optional
%type <rval> subscript,subscript_list
%type <sfval> string_from_list,string_from
%type <sval> opt_unstring_count,opt_unstring_delim,unstring_tallying
%type <udval> unstring_delimited_vars,unstring_delimited
%type <udstval> unstring_destinations,unstring_dest_var
%type <baval> inspect_before_after
%type <tlval> tallying_list, tallying_clause
%type <tfval> tallying_for_list
%type <ival> replacing_kind,opt_plus_minus
%type <repval> replacing_list, replacing_clause
%type <rbval> replacing_by_list
%type <cvval> converting_clause
%type <sval> var_or_nliteral,opt_read_key
%type <sival> screen_clauses
%type <ival> screen_attribs,screen_attrib,sign_clause,opt_separate
%type <sval> variable_indexed,search_opt_varying,opt_key_is
%type <dval> search_body,search_all_body
%type <dval> search_when,search_when_list,search_opt_at_end
%type <ival> parm_type,sign_condition,class_condition
%type <sval> intrinsic_parm_list,intrinsic_parm
%type <sval> parm_list,parameter,expr,opt_expr
%type <sval> cond_name
%type <pfval> perform_after
%type <pfvals> opt_perform_after
%type <ival> ext_cond,extended_cond_op
%type <sval> returning_options
%type <snval> sort_file_list,sort_input,sort_output
%type <ival> opt_not,selection_subject,selection_object,when_case
%type <ssbjval> selection_subject_set
%type <sval> screen_to_name, opt_goto_depending_on
%type <lstval> goto_label_list
%type <lval> signed_nliteral
%type <ival> sentence_or_nothing,when_case_list
%type <ival> opt_rounded
%type <mval> var_list_name, var_list_gname
%type <mose> on_size_error,error_sentence
%type <ival> opt_address_of,display_upon,display_options
%type <sval> set_variable,set_variable_or_nlit,set_target,opt_add_to
%type <condval> condition,implied_op_condition
%type <sval> qualified_var,unqualified_var
%type <ival> opt_end_program,program_sequence
%type <lval> from_rec_varying,to_rec_varying
%type <sval> depend_rec_varying
%type <sval> file_description,redefines_var
%type <ival> on_exception_or_overflow,on_not_exception
%type <gic>  on_end,opt_read_at_end
%type <iks>  opt_read_invalid_key
%type <ike>  read_invalid_key ,read_not_invalid_key

%%

/************   Parser for Cobol Source  **************/

program_sequence:
        program opt_end_program { pgm_segment++; $$=$2; }
        | program_sequence
                program
                opt_end_program { pgm_segment++;
                        if (!$1) yyerror("END PROGRAM expected"); $$=$3; }
        ;
opt_end_program:                
        /* nothing */ { $$=0; }
        | END PROGRAM
        { clear_symtab(); clear_offsets();
                curr_division= CDIV_IDENT; }
        IDSTRING
        { curr_division = CDIV_COMMENT; $$=1; }
        ;
program: identification_division 
         environment_division_opt 
         data_division_opt
         procedure_division_opt
        ;
identification_division: IDENTIFICATION_TOK DIVISION EOS
    {
     curr_division = CDIV_IDENT;
    }
    PROGRAM_ID EOS IDSTRING programid_opts_opt EOS 
    {
     curr_division = CINITIAL; 
     pgm_header($7); 
     define_special_fields();
    }
    identification_division_options_opt
;
programid_opts_opt:  /*nothing */
    | opt_is INITIALTOK PROGRAM
     {
      yyerror("PROGRAM-ID clause INITIAL not currently implimented");
     }
    | opt_is COMMONTOK PROGRAM
     {
      yyerror("PROGRAM-ID clause COMMON not currently implimented");
     }
;
identification_division_options_opt:  /*nothing */
    | identification_division_options_opt identification_division_option
;
identification_division_option: AUTHOR_TOK EOS { curr_division = CDIV_COMMENT; }
    | DATE_WRITTEN_TOK EOS  { curr_division = CDIV_COMMENT; }
    | DATE_COMPILED_TOK EOS { curr_division = CDIV_COMMENT; }
    | INSTALLATION_TOK EOS  { curr_division = CDIV_COMMENT; }
    | SECURITY_TOK EOS { curr_division = CDIV_COMMENT; }
    ;
environment_division_opt: ENVIRONMENT_TOK DIVISION EOS
    {
     curr_division = CDIV_ENVIR;
    }
    opt_configuration opt_input_output
    {
     curr_division = CINITIAL; 
    }
    | /*nothing */
    ;
opt_configuration:
    CONFIGURATION SECTION EOS configuration_section
    | /*nothing */
    ;
opt_input_output:
    INPUT_OUTPUT SECTION EOS input_output_section { }
    | /*nothing */
    ;
configuration_section:
    configuration_section configuration_option  { }
    | /* nothing */                 { }
    ;
configuration_option:
    COMMENTING EOS STRING EOS {
                        if ($1 != SOURCE_COMPUTER && $1 != OBJECT_COMPUTER) {
                                yyerror("expecting SOURCE-COMPUTER or OBJECT-COMPUTER");
                        }
                        $3->type = ';';
                }
    | SPECIAL_NAMES EOS opt_special_names_sentences
    | error { yyerror("invalid format in CONFIGURATION SECTION"); }
    ;
opt_special_names_sentences:
    special_names_sentences
    | /*nothing*/
    ;
special_names_sentences:
    currency_details_opt
    decimal_point_details_opt
    EOS
    | CONSOLE IS CONSOLE EOS
        { yywarn ("ignoring setting CONSOLE"); }
    ;
currency_details_opt:
    currency_details
    | /* nothing */
    ;
currency_details:
    CURRENCY opt_sign IS CLITERAL {
                currency_symbol = $<lval>4->name[0];
                }
    ;
opt_sign:
    SIGN
    | /* nothing */
    ;
decimal_point_details_opt:
    decimal_point_details
    | /* nothing */
    ;
decimal_point_details:
    DECIMAL_POINT IS COMMA {
                decimal_comma=1; }
    ;
input_output_section:
    input_output_section i_o_option { }
    | /* nothing */     { }
    ;
i_o_option:
    FILE_CONTROL EOS file_control
    | I_O_CONTROL EOS io_control
    | error { yyerror("I-O SECTION format wrong"); }
    ;
file_control:
    file_control file_select
    | /* nothing */
    ;

/* SELECT def_name ASSIGN opt_to filename {*/
/*    SELECT def_name ASSIGN opt_to opt_portnum opt_filename {*/
/*    SELECT def_name ASSIGN opt_to assign_clause {*/
file_select:
    SELECT opt_optional def_name ASSIGN opt_to {
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
      assign_clause {
            $3->filenamevar=$7; /* this is the variable w/filename */
      }
      select_clauses EOS {
                        if ((curr_file->organization==ORG_INDEXED) &&
                                !(curr_file->ix_desc)) {
                                yyerror("indexed file must have a record key");
                                YYABORT;
                        }
                }
    ;
assign_clause: PORTNUM { $$=NULL; }
    | filename { $$=$1; }
    | PORTNUM filename { $$=$2; }
    | EXTERNAL filename 
     { 
      curr_file->access_mode = curr_file->access_mode + 5; 
      $$=$2; 
     }
    | error  { yyerror("Invalid ASSIGN clause in SELECT statement"); }
    ;
select_clauses:
    select_clauses select_clause
    | /* nothing */
    ;
select_clause:
    ORGANIZATION opt_is organization_options
                { curr_file->organization=$3; }
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
    | FILEN STATUS opt_is STRING
                { curr_file->parent=$4; }
    | RECORD KEY opt_is STRING { curr_file->ix_desc=$4; }
    | RELATIVE KEY opt_is STRING { curr_file->ix_desc=$4; }
    | ALTERNATE RECORD KEY opt_is STRING
        with_duplicates { add_alternate_key($5,$6); }
    | error         { yyerror("invalid clause in select"); }
    ;
with_duplicates:
    WITH DUPLICATES     { $$=1; }
    | /* nothing */     { $$=0; }
    ;
opt_optional:
    OPTIONAL            { $$=1; }
    | /* nothing */     { $$=0; }
    ;
opt_is:
    IS { }
    | /* nothing */
    ;
opt_mode:
    MODE
    | /* nothing */
    ;
organization_options:
    INDEXED     { $$=1; }
    | SEQUENTIAL    { $$=2; }
    | RELATIVE  { $$=3; }
    | LINE SEQUENTIAL { $$=4; }
    | anystring 
     { yyerror("invalid option, %s",$1->name); }
    ;
access_options:
    SEQUENTIAL  { $$=1; }
    | DYNAMIC   { $$=2; }
    | RANDOM    { $$=3; }
    | anystring 
     { yyerror("invalid access option, %s", $1->name); }
    ;
io_control:
    io_control io_ctrl
    | /* nothing */
    ;
io_ctrl:
    SAME AREA FOR name_list EOS
    ;
name_list:
    variable { }
    | name_list variable { }
    | error { yyerror("variable expected"); }
    ;
/*
data_division:
    opt_file_section
    opt_working_storage
    opt_linkage_section
    opt_screen_section
    opt_record_section
        ;
*/
data_division_opt: DATA_TOK DIVISION EOS 
    {
     curr_division = CDIV_DATA;
    }
    opt_file_section
    opt_working_storage
    opt_linkage_section
    opt_screen_section
    opt_record_section
    { 
     data_trail(); 
     curr_division = CINITIAL; 
    }
    | /* nothing */
;
opt_record_section:
        REPORT SECTION EOS
        report_section
        | /* nothing */
;
opt_screen_section:
    SCREEN SECTION EOS  {
                                        screen_io_enable++;
                                        curr_field=NULL;
                                        scr_line = scr_column = 1;
                        }
    screen_section      { close_fields(); }
    | /* nothing */
    ;
opt_file_section:
    FILEN SECTION EOS  { curr_field=NULL; }
    file_section   { close_fields();  }
    | /* nothing */
    ;
opt_working_storage:
    WORKING_STORAGE SECTION EOS     { curr_field=NULL; }
    working_storage_section         { close_fields(); }
    | /* nothing */
    ;
opt_linkage_section:
    LINKAGE SECTION EOS     { at_linkage=1; curr_field=NULL; }
    linkage_section         { close_fields(); at_linkage=0; }
    | /* nothing */
    ;
report_section:
        report_section TOKRD { /*curr_division = CDIV_FD;*/ }
                STRING { $4->type='W'; curr_division = CDIV_PROC; }
                report_controls { curr_division = CDIV_DATA; }
                report_description
        | /* nothing */
        ;
report_controls:
        /* nothing */
        | report_controls CONTROL opt_is_are opt_final report_break_list
        | report_controls PAGETOK opt_limit_is integer opt_lines
        | report_controls
                HEADING opt_is integer
                opt_first_detail opt_last_detail
                opt_footing EOS
        ;
opt_lines:
        /* nothing */
        | LINE
        ;
opt_limit_is:
        /* nothing */
        | LIMIT opt_is
        ;
opt_footing:
        /* nothing */
        | FOOTING opt_is integer
        ;
opt_last_detail:
        /* nothing */
        | TOKLAST DETAIL opt_is integer
        ;
opt_first_detail:
        /* nothing */
        | FIRSTTOK DETAIL opt_is integer
        ;
opt_final:
        /* nothing */
        | FINAL

report_break_list:
        /* nothing */
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
        report_clauses EOS
        {
	  update_report_field ($2);
	  curr_division = CDIV_DATA;
	}
        ;
report_clauses:
        /* nothing */
        | report_clauses TYPETOK opt_is report_type
                report_position { curr_division = CDIV_PROC; }
                opt_report_name
        | report_clauses report_line
        | report_clauses opt_report_column
                opt_report_pic { curr_division = CDIV_PROC; }
                report_value
        ;
opt_report_name:
	name { }
        | FINAL { }
        | /* NOTHING */
        ;
report_value:
        /* nothing */
        | VALUE opt_is gname
        | TOKSOURCE opt_is gname
        | TOKSUM opt_of name
        ;
opt_of:
        OF
        | /* nothing */
        ;
opt_report_pic:
        /* nothing */
        | picture_clause
        ;
opt_report_column:
        COLUMN opt_number integer
        | /* nothing */
        ;
report_line:
        LINE opt_number opt_line_rel integer
        ;
opt_number:
        /* nothing */
        | NUMBERTOK
        ;
opt_line_rel:
        /* nothing */
        | '+'
        | PLUS
        ;
report_position:
        HEADING
        | FOOTING
        | /* nothing */
        ;
report_type:
        PAGETOK
        | CONTROL
        | DETAIL
        ;
screen_section:
    screen_section screen_item
    | /* nothing */
    ;
screen_item:
    integer opt_def_name
    {
      define_field($1,$2);
    }
    screen_clauses EOS
    {
      update_screen_field($2,$4);
    }
    ;
screen_clauses:
    screen_clauses LINE
        opt_number_is
        opt_plus_minus
        integer                 { scr_set_line($1,$5,$4); $$=$1; }
    | screen_clauses COLUMN
        opt_number_is
        opt_plus_minus
        integer                 { scr_set_column($1,$5,$4); $$=$1; }
    | screen_clauses
        screen_attrib           { $1->attr |= $2; $$=$1; }
    | screen_clauses FOREGROUNDCOLOR
        integer                 { $1->foreground = $3; $$=$1; }
    | screen_clauses BACKGROUNDCOLOR
        integer                 { $1->background = $3; $$=$1; }
    | screen_clauses
        screen_source_destination
    | screen_clauses
        VALUE opt_is gliteral   { curr_field->value = $4; $$=$1; }
    | screen_clauses picture_clause
    | /* nothing */             { $$ = alloc_scr_info(); }
    ;
screen_source_destination:
    USING                   { curr_division = CDIV_PROC; }
    name_or_lit
    {
      curr_division = CDIV_DATA;
      $<sival>0->from = $<sival>0->to = $3;
    }
    | FROM                  { curr_division = CDIV_PROC; }
      name_or_lit
      screen_to_name
      {
	curr_division = CDIV_DATA;
	$<sival>0->from = $3;
	$<sival>0->to = $4;
      }
    | TO
      {
	curr_division = CDIV_PROC;
      }
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
        BLANK SCREEN                    { $$ = SCR_BLANK_SCREEN; }
        | BLANK LINE                    { $$ = SCR_BLANK_LINE; }
        | BELL                          { $$ = SCR_BELL; }
        | sign_clause                   { $$ = $1; }
        | FULL                          { $$ = SCR_FULL; }
        | REQUIRED                      { $$ = SCR_REQUIRED; }
        | SECURE                        { $$ = SCR_SECURE; }
        | AUTO                          { $$ = SCR_AUTO; }
        | JUST RIGHT                    { $$ = SCR_JUST_RIGHT; }
        | JUST LEFT                     { $$ = SCR_JUST_LEFT; }
        | BLINK                         { $$ = SCR_BLINK; }
        | REVERSEVIDEO                  { $$ = SCR_REVERSE_VIDEO; }
        | UNDERLINE                     { $$ = SCR_UNDERLINE; }
        | LOWLIGHT                      { $$ = SCR_LOWLIGHT; }
        | HIGHLIGHT                     { $$ = SCR_HIGHLIGHT; }
        | BLANK opt_when ZERONUM        { $$ = SCR_BLANK_WHEN_ZERO; }
        | NOECHO                        { $$ = SCR_NOECHO; }
        | UPDATE                        { $$ = SCR_UPDATE; }
        ;
sign_clause:
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
        | /* nothing */                 { $$ = 0; }
        ;
opt_character:
        CHARACTER
        | /* nothing */
        ;
opt_plus_minus:
    PLUS            { $$ = 1; }
    | MINUS         { $$ = -1; }
    | /* nothing */ { $$ = 0; }
    ;
opt_number_is:
        NUMBERTOK opt_is
        | /* nothing */
        ;
file_section:
    file_section FD     { curr_division = CDIV_FD; }
    STRING              { curr_division = CDIV_DATA; }
    file_attrib EOS
    {
      curr_field=NULL;
      if ($4->filenamevar == NULL)
	yyerror("External file name not defined for file %s", $4->name);
    }
    file_description        {
                close_fields();
                alloc_file_entry($4);
                gen_fdesc($4,$9);
            }
    | file_section SD { curr_division = CDIV_FD; }
         STRING { curr_division = CDIV_DATA; }
         sort_attrib EOS
            {
                $4->organization=2;
                curr_field=NULL;
            }
    file_description    {
                close_fields();
                alloc_file_entry($4);
                gen_fdesc($4,$9);
            }
    | /* nothing */
    ;
file_description:
      field_description       { $$=$1; }
    | file_description field_description
      {
	if (($2 != NULL) && ($2->level == 1)) {
	  /* multiple 01 records (for a file descriptor) */
	  $2->redefines=$1;
	  $$=$2;
	}
	else
	  $$=$1;
      }
    ;
field_description:
    integer opt_def_name
    {
      define_field($1,$2);
    }
    redefines_clause
    data_clauses EOS
    {
      $$ = $2;
      update_field();
    }
    ;

redefines_clause:
    REDEFINES
    {
      curr_division = CDIV_PROC; /* parsing variable */
    }
    redefines_var
    {       
      curr_division = CDIV_DATA;
      if ($<sval>-1 != NULL)
	$<sval>-1->redefines = lookup_for_redefines($3);
      else
	yyerror("cannot redefine an unnamed field");
    }
    |  /* nothing */
    ;
redefines_var:
    VARIABLE    { $$=$1; }
    | SUBSCVAR  { $$=$1; }
    ;

data_clauses:
    /* nothing */
    | data_clauses data_clause
    ;

data_clause:
    array_options
    | picture_clause
    | usage_option
    | sign_clause       { set_sign_flags($1); }
    | value_option
    | SYNC sync_options {curr_field->flags.sync=1;}
    | JUST RIGHT {curr_field->flags.just_r=1;}
    | EXTERNAL {save_named_sect(curr_field);}
    | BLANK opt_when ZERONUM { curr_field->flags.blank=1; }
    ;

sync_options:
    /* nothing */
    | LEFT
    | RIGHT
    ;

array_options:  OCCURS integer opt_TIMES
       {
	 curr_field->times = $2;
	 curr_field->occurs_flg++;
       }
       opt_indexed_by
     | OCCURS integer TO integer opt_TIMES DEPENDING opt_on
       {
	 curr_division = CDIV_PROC; /* needed for parsing variable */
       }
       gname
       {       
	 curr_division = CDIV_DATA;
	 create_occurs_info($2,$4,$9);
       }
       opt_indexed_by
     ;

opt_key_is:
      DIRECTION opt_key opt_is STRING
     {
       $4->level=0;
       if ($1 == ASCENDING) {
          $4->level=-1;
       }
       if ($1 == DESCENDING) {
          $4->level=-2;
       }
       $$=$4;
     }
     | { $$=NULL; }
     ;

opt_indexed_by: 
        opt_key_is INDEXED opt_by index_name_list
          { 
          /*define_implicit_field($4, $1, curr_field->times);*/
           /* Fix Me: Does not work, thus dup vars can be defined
           if ($4->defined) {
              yyerror("variable redefined, '%s'",$4->name);
              $4->defined=1;
           }
           else {
              define_implicit_field($4, $1, curr_field->times);
           } */
          }
         | /* nothing */
         ;
             
index_name_list:
        def_name        { 
                  define_implicit_field($1, $<sval>-2, curr_field->times);
                  /* Fix Me: Does not work, thus dup vars can be defined
                  if ($4->defined) {
                     yyerror("variable redefined, '%s'",$4->name);
                     $4->defined=1;
                  }
                  else { 
                     define_implicit_field($4, $1, curr_field->times);
                  } */
                } 
        
        | index_name_list def_name {
          define_implicit_field($2,$<sval>-2,curr_field->times);
         }  
        ;
picture_clause:
    PIC {
      curr_division = CDIV_PIC;
      picix=piccnt=v_flag=decimals=0;
      picture[picix]=0;
    }
    opt_is picture_spec {
      picture[picix+2]=0;
      curr_field->decimals=decimals;
    }
    ;
picture_spec:
    picture_element
    | picture_spec picture_element
    ;
picture_element:
      CHAR              { if (!save_pic_char ($1, 1)) YYERROR; }
    | CHAR MULTIPLIER   { if (!save_pic_char ($1, $2)) YYERROR; }
    ;

usage_option :
    opt_USAGE opt_is usage
    {
      switch ($3)
	{
	case USAGE_COMP1:
	  curr_field->len = 4;
	  curr_field->decimals=7;
	  curr_field->type='U';
	  curr_field->sign=1;
	  /* default picture is 14 (max=7->7.7) digits */
	  strcpy(picture,"S\x01\x39\x07\x56\x01\x39\x07");
	  break;
	case USAGE_COMP2:
	  curr_field->len = 8;
	  curr_field->decimals=15;
	  curr_field->type='U';
	  curr_field->sign=1;
	  /* default picture is 30 (max=15->15.15) digits*/
	  strcpy(picture,"S\x01\x39\x0f\x56\x01\x39\x0f");
	  break;
	case COMP3:
	  /*curr_field->len = (piccnt/2)+1;*/
	  curr_field->type='C';
	  break;
	case COMP: 
	  /* field length computed by query_comp_length() */
	  curr_field->len = 0;
	  curr_field->type='B'; /* binary field */
	  break;
	  /*case 10: POINTER*/ 
	case USAGE_POINTER: /*POINTER*/ 
	  curr_field->len=4;
	  curr_field->decimals=0;
	  curr_field->type='B'; /* pointers are binary fields */
	  strcpy(picture,"9\xa"); /* pointer default picture */
	  curr_field->flags.is_pointer=1;
	  break;
	case USAGE_BINARY_CHAR:
	  curr_field->len = 1;
	  curr_field->type='B'; 
	  break; 
	case USAGE_BINARY_SHORT:
	  curr_field->len = 2;
	  curr_field->type='B'; 
	  break; 
	case USAGE_BINARY_LONG:
	  curr_field->len = 4;
	  curr_field->type='B'; 
	  break; 
	case USAGE_BINARY_DOUBLE:
	  curr_field->len = 8;
	  curr_field->type='B'; 
	  break; 
	}
    }
    ;

value_option:  VALUE opt_is_are value_list
    ;

value_list:
        value                           
        | value_list opt_sep value
        ;
value:
    gliteral                    { set_variable_values($1,$1); }
    | gliteral THRU gliteral    { set_variable_values($1,$3); }
    ;

file_attrib:
    /* nothing */
    | file_attrib REPORT opt_is STRING { save_report( $4,$<sval>0 ); }
    | file_attrib opt_is GLOBAL     { $<sval>0->type = 'J'; }
    | file_attrib opt_is EXTERNAL   { $<sval>0->type = 'K'; }
    | file_attrib LABEL rec_or_recs opt_is_are std_or_omitt
    | file_attrib BLOCK opt_contains integer opt_to_integer chars_or_recs
    | file_attrib DATA_TOK rec_or_recs  opt_is_are var_strings { }
    | file_attrib VALUE OF FILE_ID opt_is filename
     {
      if ($<sval>-1->filenamevar != NULL) {
         yyerror("Re-defining file name defined in SELECT statement");
      }
      else {
         $<sval>-1->filenamevar = $<sval>6;
      }
     }
    | file_attrib RECORD opt_is VARYING opt_in_size
      from_rec_varying to_rec_varying opt_characters
      depend_rec_varying 
      { set_rec_varying_info( $<sval>-1,$6,$7,$9 ); }
    ;
var_strings: STRING { }
    | var_strings STRING { }
    ; 
opt_to_integer: /* nothing */
    | TO integer { }
    ;
depend_rec_varying:
        DEPENDING opt_on STRING { $$ = $3; }
        ;
from_rec_varying:
        /* nothing */ { $$ = NULL; }
        | FROM nliteral { $$ = $2; }
        ;
to_rec_varying:
        /* nothing */ { $$ = NULL; }
        | TO nliteral { $$ = $2; }
        ;
sort_attrib:
    /* nothing */
    | sort_attrib  DATA_TOK rec_or_recs  opt_is_are var_strings { }  
    | sort_attrib RECORD opt_is VARYING opt_in_size
      from_rec_varying to_rec_varying opt_characters
      depend_rec_varying 
      { set_rec_varying_info( $<sval>-1,$6,$7,$9 ); }
    ;
rec_or_recs:
    RECORD
    | RECORDS
    ;
std_or_omitt:
    STANDARD
    | OMITTED
    ;
opt_USAGE:
    /* nothing */
    | USAGE
    ;
opt_TIMES:
    /* nothing */
    | TIMES
    ;
opt_when:
    /* nothing */
    | WHEN
    ;
opt_is_are:
    /* nothing */
    | IS { }
    | ARE { }
    ;
opt_contains:
    /* nothing */
    | CONTAINS
    ;
opt_characters:
        | CHARACTERS
        ;
chars_or_recs:
    CHARACTERS
    | RECORDS
    ;
usage:  USAGENUM    { $$=$1; }
    | DISPLAY   { /*$$=9;*/  $$=USAGE_DISPLAY; }
    | POINTER   { /*$$=10;*/ $$=USAGE_POINTER; }
    | COMP1     { $$=USAGE_COMP1; }
    | COMP2     { $$=USAGE_COMP2; }
    ;
working_storage_section:
    working_storage_section
        field_description
    | /* nothing */
    ;
linkage_section:
    /* nothing */
    | linkage_section
        field_description
    ;
/*
procedure_division:
    | procedure_division procedure_decl
    ;
*/
procedure_division_opt: PROCEDURE_TOK DIVISION 
    { 
     curr_division = CDIV_PROC; 
    }
     using_parameters EOS
    { 
     proc_header($4);
    }
     procedure_division
    {
     /* close procedure_division sections & paragraphs */
     close_section(); /* this also closes paragraph */
     resolve_labels();
     proc_trail($4); 
    }
    | /* nothing */
    ;
procedure_division:
    | procedure_division procedure_decl
    ;
procedure_decl:
    procedure_section { close_section(); open_section($1); }
    | paragraph { close_paragr(); open_paragr($1); }
    | {free_expr_list(); stabs_line();} statement_list opt_EOS
    | error { yyerror("unknown or wrong statement"); } EOS
    | EOS
    ;
procedure_section:
     LABELSTR SECTION EOS {
            struct sym *lab=$1;
                        if (lab->defined != 0) {
                                lab = install(lab->name,SYTB_LAB,2);
                        }
                        lab->defined = 1;
            $$=lab;
        }
    ;
paragraph:
    LABELSTR dot_or_eos {
            struct sym *lab=$1;
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
dot_or_eos:
    '.'
    | EOS
    ;

/*
 * Statements
 */

conditional_statement_list:
    statement_list opt_continue
    | CONTINUE {stabs_line();}
    | NEXT SENTENCE {stabs_line();}
    ;
opt_continue:
    /* nothing */
    | CONTINUE {stabs_line();}
    | NEXT SENTENCE {stabs_line();}
    ;
statement_list:
    statement opt_dummy {stabs_line();}
    | statement_list statement opt_dummy {stabs_line();}
    ;
statement:
      accept_statement
    | add_statement
    | call_statement
    | close_statement
    | compute_statement
    | delete_statement
    | display_statement
    | divide_statement
    | evaluate_statement
    | exit_statement
    | goto_statement
    | if_statement
    | inspect_statement
    | move_statement
    | multiply_statement
    | open_statement
    | perform_statement
    | read_statement
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


/* ACCEPT statement */

accept_statement:
    ACCEPT name opt_line_pos accept_options
    ;
accept_options:
      screen_attribs    { gen_accept($<sval>-1, $1, 1); }
    | screen_attribs
        ONTOK EXCEPTION { screen_io_enable++; 
                          gen_accept($<sval>-1, $1, 1); }
        variable        { gen_store_fnres($5); 
                          $<dval>$ = gen_check_zero(); }
        statement_list  { gen_dstlabel($<dval>6); }
    | FROM DATE_TIME    { if ($2 == DATE)
                              gen_accept_from_date($<sval>-1);
                          else if ($2 == DAY)
                              gen_accept_from_day($<sval>-1);
                          else if ($2 == DAY_OF_WEEK)
                              gen_accept_from_day_of_week($<sval>-1);
                          else if ($2 == TIME)
                              gen_accept_from_time($<sval>-1);
                          else if ($2 == INKEY)
                              gen_accept_from_inkey($<sval>-1); }
    | FROM CMD_LINE     { gen_accept_from_cmdline($<sval>-1); }
    | FROM ENVIRONMENT_VARIABLE CLITERAL 
     { 
       save_literal($3,'X');
           $3->all=0;
       gen_accept_env_var($<sval>-1, $3);
     }
    ;


/* UNSTRING statement */

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


/* PERFORM statement */

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


/* INSPECT statement */

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
    | FIRSTTOK  { $$ = INSPECT_FIRST; }
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
opt_initial:
    INITIALTOK
    | /* nothing */
    ;


/* STRING statement */

string_statement:
  STRINGCMD string_from_list
  INTO name string_with_pointer {
    gen_stringcmd( $2, $4, $5 );
  }
  opt_on_overflow
  opt_end_stringcmd
  ;


/* SORT statement */

sort_statement:
    SORT name sort_keys   { gen_sort($2); }
    sort_input sort_output { /*gen_close_sort($2);*/ }
sort_keys:
    /* nothing */   { $$ = NULL; }
    | sort_keys DIRECTION KEY name
        {
            $4->direction = $2;
            (struct sym *)$4->sort_data =
                (struct sym *)($<sval>0->sort_data);
            (struct sym *)($<sval>0->sort_data) = $4;
            $$ = $4;
        }
    ;
sort_input:
    INPUT PROCEDURE_TOK opt_is sort_range { $$=NULL; }
    | USING sort_file_list { gen_sort_using($<sval>-2,$2); $$=$2; }
    ;       
sort_output:
    OUTPUT PROCEDURE_TOK opt_is sort_range { $$=NULL; }
    | GIVING sort_file_list { gen_sort_giving($<sval>-3,$2); $$=$2; }
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


/* SET statement */

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
  | cond_name { $$ = $1; }
  ;
set_variable:
   variable	   { $$ = $1; }
  | NULLTOK	   { $$ = NULL; }
  ;
opt_address_of:
  /* nothing */ { $$ = 0; }
  | ADDRESS opt_of { $$ = 1; }
  ;
set_variable_or_nlit:
  name_or_lit	  { $$ = $1; }
  | NULLTOK	  { $$ = NULL; }
  | TOKTRUE	  
    { 
      $$ = (struct sym *)1;
      /* no (struct sym *) may have this value! */ 
    }
  ;


/* EVALUATE statement */

evaluate_statement:
    EVALUATE { $<ival>$ = gen_evaluate_start(); }
                selection_subject_set { compute_subject_set_size($3); }
                when_case_list
                END_EVALUATE { release_sel_subject($<ival>2,$3); }
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
        { $<sval>$ = NULL; }
        selection_object {
                        gen_when_check(0,$<ssbjval>0,$2,$<ival>-1,$<sval>1);
                        $$=0;
                }
        | when_case ALSO
                        { $<sval>$ = NULL; }
                        selection_object {
                        gen_when_check($1+1,$<ssbjval>0,$4,$<ival>-1,$<sval>3);
                        $$=$1+1;
                }
        | OTHER { $$=-1; }
        ;
selection_subject_set:
        { $<sval>$=NULL; /* to store non-numeric symbols */ }
        selection_subject { $$=save_sel_subject($2,NULL,$<sval>1); }
        | selection_subject_set ALSO
                { $<sval>$=NULL; /* to store non-numeric symbols */ }
                selection_subject
                { $$=save_sel_subject($4,$1,$<sval>3); }
        ;
selection_subject:
        expr    /* this already includes identifiers and literals */
                { if (push_expr($1))
                        $$=SSUBJ_EXPR;
                  else {
                        $<sval>0 = $1;
                        $$=SSUBJ_STR;
                   }
                }
        | condition { push_condition(); $$=SSUBJ_COND; }
        | TOKTRUE       { $$=SSUBJ_TRUE; }
        | TOKFALSE { $$=SSUBJ_FALSE; }
        ;
selection_object:
        ANY     { $$=SOBJ_ANY; }
        | TOKTRUE { $$=SOBJ_TRUE; }
        | TOKFALSE { $$=SOBJ_FALSE; }
        | opt_not expr {
	  if ($2 == (struct sym *) spe_lit_ZE)
	    {
	      if ($1)
		$$ = SOBJ_NEGZERO;
	      else
		$$ = SOBJ_ZERO;
	    }
	  else if (push_expr($2))
	    {
	      if ($1)
		$$ = SOBJ_NEGEXPR;
	      else
		$$ = SOBJ_EXPR;
	    }
	  else
	    {
	      /* non-numeric comparation */
	      $<sval>0 = $2;
	      if ($1)
		$$ = SOBJ_NEGSTR;
	      else
		$$ = SOBJ_STR;
	    }
	}
        | opt_not expr THRU expr {
	  if (push_expr($4) && push_expr($2))
	    {
	      if ($1)
		$$=SOBJ_NEGRANGE;
	      else
		$$=SOBJ_RANGE;
	    }
	  else
	    {
	      yywarn ("unsupported range");
	      if ($1)
		$$=SOBJ_NEGRANGE;
	      else
		$$=SOBJ_RANGE;
	    }
	}
        | opt_not condition {
                if ($1)
                        $$=SOBJ_NEGCOND;
                else
                        $$=SOBJ_COND; }
        ;
sentence_or_nothing:
    /* nothing */               { $$ = 0; }
    | conditional_statement_list     { $$ = 1; }
    ;


/* CALL statement */

call_statement:
    CALL  { curr_call_mode=CM_REF; }
    gname
    using_options       
    returning_options 
    { $<ival>$ = loc_label++; /* exception check */ }
    { $<ival>$ = loc_label++; /* not exception check */ } 
    { 
      $<ival>$ = gen_call((struct lit *)$3,$4,$<ival>6,$<ival>7); 
      gen_store_fnres($5); 
    }
    on_exception_or_overflow 
    on_not_exception { 
      check_call_except($9,$10,$<ival>6,$<ival>7,$<ival>8); }
    opt_end_call
    ;
on_exception_or_overflow:
    /* nothing */ { $$ = 0; }
    | ONTOK exception_or_overflow { $<ival>$ = begin_on_except(); } 
      statement_list            { gen_jmplabel($<dval>0); $$=$<ival>3; }
    ;
exception_or_overflow:
      EXCEPTION
    | OVERFLOWTK
    ;
opt_end_call:
    | END_CALL
    ;


/* IF statement */

if_statement:
      if_part { gen_dstlabel($1); } opt_end_if 
    | if_part ELSE
      { $<dval>$=gen_passlabel(); gen_dstlabel($1); }
        conditional_statement_list { gen_dstlabel($<dval>3); }
      opt_end_if
if_part:
    IF  condition  { $<dval>$=gen_testif(); }
        opt_then
        conditional_statement_list { $<dval>$=$<dval>3; }
    ;
opt_end_if:
    /* nothing */
    | END_IF
    ;


/* SEARCH statement */

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

        $<sval>$=determine_table_index_name($1);
        if ($<sval>$ == NULL) {
           yyerror("Unable to determine search index for table '%s'", $1->name);
        }
        else {
          /* Initilize and store search table index boundaries */
          Initialize_SearchAll_Boundaries($1, $<sval>$);
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
        gen_SearchAllLoopCheck($3, $<sval>2, $1, curr_field, lbstart, lbend);
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
    | name cond_op nliteral { gen_compare($1,$2,(struct sym *)$3); }
    | nliteral cond_op name { gen_compare((struct sym *)$1,$2,$3); }
    | nliteral cond_op nliteral {
                gen_compare((struct sym *)$1,$2,(struct sym*)$3); }
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
     variable opt_is CONDITIONAL opt_to variable 
        {
          if ($3 != EQUAL)
             yyerror("Only = conditional allowed in search all statement");
          if (curr_field == NULL)
             curr_field = $1;
          gen_compare($1,$3,$5);
        }
     | variable opt_is CONDITIONAL opt_to literal
        {
          if ($3 != EQUAL)
             yyerror("Only = conditional allowed in search all statement");
          if (curr_field == NULL)
             curr_field = $1;
          gen_compare($1,$3,(struct sym *)$5);
        }
    | search_all_when_conditional AND { $<dval>$=gen_andstart(); }
      search_all_when_conditional  { gen_dstlabel($<dval>3); }
    ;
opt_end_search:
    /* nothing */
    | END_SEARCH
    ;

/* MOVE statement */

move_statement:
      MOVE gname TO { $<ival>$ = MOVE; } var_list
    | MOVE CORRESPONDING gname TO gname { gen_movecorr($3, $5); }
    | MOVE LENGTH OF gname TO name { gen_movelength($4, $6); }
    | INITIALIZE { $<ival>$ = INITIALIZE; } var_list


/* COMPUTE statement */

compute_statement:
    COMPUTE compute_body opt_end_compute
    ;
compute_body:
      var_list_name CONDITIONAL expr
      {
        if ($2 != EQUAL)
          yyerror("= expected");
        else
          gen_compute($1, $3, NULL);
        delete_mathvar_info($1);
      }
    | var_list_name CONDITIONAL expr on_size_error
      {
        if ($2 != EQUAL)
          yyerror("= expected");
        else
          gen_compute($1, $3, $4);
        delete_mathvar_info($1);
        tmose = NULL;
      }
    ;
opt_end_compute:
    /* nothing */
    | END_COMPUTE
    ;


/* ADD statement */

add_statement:
    ADD add_body opt_end_add
    ;
add_body:
      var_list_gname TO var_list_name on_size_error
      {
        gen_add1($1, $3, $4);
        delete_mathvar_info($1);
        delete_mathvar_info($3);
        tmose = NULL;
      }
    | var_list_gname opt_add_to GIVING var_list_name on_size_error
      {
        gen_add2($1, $4, $2, $5);
        delete_mathvar_info($1);
        delete_mathvar_info($4);
        tmose = NULL;
      }
    | var_list_gname TO var_list_name
      {
        gen_add1($1, $3, NULL);
        delete_mathvar_info($1);
        delete_mathvar_info($3);
      }
    | var_list_gname opt_add_to GIVING var_list_name
      {
        gen_add2($1, $4, $2, NULL);
        delete_mathvar_info($1);
        delete_mathvar_info($4);
      }
    | CORRESPONDING gname opt_to name opt_rounded
      {
        gen_addcorr($2, $4, $5);
      }
    ;
opt_add_to:
    /* nothing */ { $$ = NULL; }
    | TO gname    { $$ = $2; }
    ;
opt_end_add:
    /* nothing */
    | END_ADD
    ;


/* SUBTRACT statement */

subtract_statement:
    SUBTRACT subtract_body opt_end_subtract
    ;
subtract_body:
      var_list_gname FROM var_list_name
      {
        gen_subtract1($1, $3, NULL);
        delete_mathvar_info($1);
        delete_mathvar_info($3);
      }
    | var_list_gname FROM gname GIVING var_list_name
      {
        assert_numeric_sy($3);
        gen_subtract2($1, $5, $3, NULL);
        delete_mathvar_info($1);
        delete_mathvar_info($5);
      }
    | var_list_gname FROM var_list_name on_size_error
      {
        gen_subtract1($1, $3, $4);
        delete_mathvar_info($1);
        delete_mathvar_info($3);
        tmose = NULL;
      }
    | var_list_gname FROM gname GIVING var_list_name on_size_error
      {
        assert_numeric_sy($3);
        gen_subtract2($1, $5, $3, $6);
        delete_mathvar_info($1);
        delete_mathvar_info($5);
        tmose = NULL;
      }
    | CORRESPONDING gname FROM name opt_rounded
      {
        gen_subtractcorr($2, $4, $5);
      }
    ;
opt_end_subtract:
    | END_SUBTRACT
    ;


/* MULTIPLY statement */

multiply_statement:
    MULTIPLY multiply_body opt_end_multiply
    ;
multiply_body:
      gname BY gname GIVING var_list_name
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_multiply2($5, $1, $3, NULL);
        delete_mathvar_info($5);
      }
    | gname BY var_list_name
      {
        assert_numeric_sy($1);
        gen_multiply1($3, $1, NULL);
        delete_mathvar_info($3);
      }
    | gname BY gname GIVING var_list_name on_size_error
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_multiply2($5, $1, $3, $6);
        delete_mathvar_info($5);
        tmose = NULL;
      }
    | gname BY var_list_name on_size_error
      {
        assert_numeric_sy($1);
        gen_multiply1($3, $1, $4);
        delete_mathvar_info($3);
        tmose = NULL;
      }
    ;
opt_end_multiply:
    /* nothing */
    | END_MULTIPLY
    ;


/* DIVIDE statement */

divide_statement:
    DIVIDE divide_body opt_end_divide
    ;
divide_body:
      gname BY gname GIVING var_list_name
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_divide2($5, $1, $3, NULL);
        delete_mathvar_info($5);
      }
    | gname BY gname GIVING name opt_rounded REMAINDER name
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        assert_numeric_sy($5);
        gen_divide($1, $3, $5, $8, $6);
      }
    | gname INTO gname GIVING name opt_rounded REMAINDER name
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        assert_numeric_sy($5);
        gen_divide($3, $1, $5, $8, $6);
      }
    | gname INTO gname GIVING var_list_name
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_divide2($5, $3, $1, NULL);
        delete_mathvar_info($5);
      }
    | gname INTO var_list_name
      {
        assert_numeric_sy($1);
        gen_divide1($3, $1, NULL);
        delete_mathvar_info($3);
      }
    | gname BY gname GIVING var_list_name on_size_error
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_divide2($5, $1, $3, $6);
        delete_mathvar_info($5);
        tmose = NULL;
      }
    | gname BY gname GIVING name opt_rounded REMAINDER name on_size_error
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        assert_numeric_sy($5);
        gen_dstlabel($9->lbl4); /* generate bypass jump */
        gen_divide($1, $3, $5, $8, $6);
        math_on_size_error3($9);
        tmose = NULL;
      }
    | gname INTO gname GIVING name opt_rounded REMAINDER name on_size_error
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        assert_numeric_sy($5);
        gen_dstlabel($9->lbl4); /* generate bypass jump */
        gen_divide($3, $1, $5, $8, $6);
        math_on_size_error3($9);
        tmose = NULL;
      }
    | gname INTO gname GIVING var_list_name on_size_error
      {
        assert_numeric_sy($1);
        assert_numeric_sy($3);
        gen_divide2($5, $3, $1, $6);
        delete_mathvar_info($5);
        tmose = NULL;
      }
    | gname INTO var_list_name on_size_error
      {
        assert_numeric_sy($1);
        gen_divide1($3, $1, $4);
        delete_mathvar_info($3);
        tmose = NULL;
      }
    ;
opt_end_divide:
    /* nothing */
    | END_DIVIDE
    ;


/* DISPLAY statement */

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
    | display_options ERASE EOS_TOK         { $$ = $1 | 2; }
    | display_options ERASE EOL_TOK         { $$ = $1 | 4; }
    ;
opt_line_pos:
    /* nothing */
    | LINE expr TOKPOSITION expr 
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


/* OPEN statement */

open_statement:
    OPEN open_options
    ;
open_options:
      open_mode open_varlist { }
    | open_options open_mode open_varlist { }
    ;
open_mode:
    INPUT    { $$=1; }
    | I_O    { $$=2; }
    | OUTPUT { $$=3; }
    | EXTEND { $$=4; }
    | error  { yyerror("invalid OPEN mode"); }
    ;
open_varlist:
      name { gen_open($<ival>0, $<sval>1); }
    | open_varlist opt_sep name { gen_open($<ival>0, $<sval>3); }
    ;


/* CLOSE statement */

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


/* RETURN statements */

return_statement:
    RETURN_TOK return_body opt_end_return
    ;
return_body:
    name
    opt_record
    opt_read_into
    {    
     if (gen_reads($1, $3, NULL, 1, 4) != 0) {
        YYABORT;
     }
    }
    | name
    opt_record
    opt_read_into
    opt_read_at_end
    {    
     if (gen_reads($1, $3, NULL, 1, 5) != 0) {
        YYABORT;
     }
     else {
        ginfo_container4($4);
        gic = NULL;
     }
    }
    ;


/* READ statements */

read_statement: READ read_body opt_end_read { }
    ;
read_body: 
    name
    opt_read_next
    opt_record
    opt_read_into
    opt_read_key
    {
     if (gen_reads($1, $4, $5, $2, 0) != 0) {
        YYABORT;
     }
    }
    | name
    opt_read_next
    opt_record
    opt_read_into
    opt_read_key
    opt_read_at_end
    {    
     if (gen_reads($1, $4, $5, $2, 1) != 0) {
        YYABORT;
     }
     else {
        ginfo_container4($6);
        gic = NULL;
     }
    }
   | name
    opt_read_next
    opt_record 
    opt_read_into
    opt_read_key
    opt_read_invalid_key 
    {    
     if (gen_reads($1, $4, $5, $2, 2) != 0) {
        YYABORT;
     }
     else {
       gen_test_invalid_keys ($6);
     }
    }
    ;
opt_read_next:
    /* nothing */       { $$ = 0; }
    | NEXT              { $$ = 1; }  
    | PREVIOUS          { $$ = 2; }  
    ;
opt_read_into:
    /* nothing */       { $$ = NULL; }
    | INTO name         { $$ = $2; }
    ;
opt_read_key:
    /* nothing */       { $$ = NULL; }
    | KEY opt_is name   { $$ = $3; }
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
      if ( gic == NULL ) {
         gic=ginfo_container0();
      }
      $$=ginfo_container1(gic);
      stabs_line();
    }
    statement_list
    { 
      $$=$<gic>2;
    }
    ;
opt_read_invalid_key:
    read_invalid_key { $$ = gen_invalid_keys ($1, NULL); }
    | read_not_invalid_key { $$ = gen_invalid_keys (NULL, $1); }
    | read_invalid_key read_not_invalid_key { $$ = gen_invalid_keys ($1, $2); }
    ;
read_invalid_key:
    INVALID opt_key     { $<ike>$ = gen_before_invalid_key (); }
    statement_list      { $$ = gen_after_invalid_key ($<ike>3); }
    ;
read_not_invalid_key:
    NOT INVALID opt_key { $<ike>$ = gen_before_invalid_key (); }
    statement_list      { $$ = gen_after_invalid_key ($<ike>4); }
    ;
opt_end_read:
    /* nothing */
    | END_READ
    ;
opt_end_return:
    /* nothing */
    | END_RETURN
    ;


/* WRITE statement */

write_statement:
    WRITE name opt_write_from write_options
    {
      if ($2->level != 1)
        yyerror("variable %s could not be used for WRITE", $2->name);
      if ($1 == 1)
        gen_release($2, $3);
      else
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
    | before_after opt_advancing PAGETOK
                        { $$ = -$1; }
    ;
opt_end_write:
    /* nothing */
    | END_WRITE
    ;


/* REWRITE statement */

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


/* DELETE statement */

delete_statement:
    DELETE name opt_record { gen_delete($2); }
    opt_invalid_key
    opt_end_delete
    ;
opt_end_delete:
    /* nothing */
    | END_DELETE
    ;


/* START statement */

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


/* GO TO statement */

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


/* EXIT statement */

exit_statement:
      EXIT          { gen_exit(0); }
    | EXIT PROGRAM  { gen_exit(1); }
    ;


/* STOP RUN statement */

stoprun_statement:
    STOP RUN { gen_stoprun(); }
    ;


/*
 * Common rules
 */

var_list_name: name opt_rounded opt_sep
     {
      $$ = create_mathvar_info(NULL, $1, $2);
     }
    | var_list_name name opt_rounded opt_sep
     {
      $$ = create_mathvar_info($1, $2, $3);
     }
    ;

var_list_gname:
    gname opt_sep
    {
      $$ = create_mathvar_info(NULL, $1, 0);
    }
    | var_list_gname gname opt_sep
      {
	$$ = create_mathvar_info($1, $2, 0);
      }
    ;

opt_rounded: { $$=0; }
    | ROUNDED { $$=1; }
    ;

on_size_error:
      NOT opt_on SIZE
      error_sentence
      {
       $$=math_on_size_error4($4, 2);
      }
    | opt_on SIZE
      error_sentence
      {
       $$=math_on_size_error4($3, 1);
      }
    | opt_on SIZE
      error_sentence
      NOT opt_on SIZE
      {
       $3->lbl1=$3->ose;
      }
      error_sentence
      {
       $$=math_on_size_error4($8, 3);
      }
    ;

error_sentence:
     ERRORTOK
     {
      if ( tmose == NULL ) {
         tmose=math_on_size_error0();
         $$=math_on_size_error1(tmose);
      }
      else {
        $$=math_on_size_error1(tmose);
      }
        stabs_line();
         }
     statement_list
     {
      math_on_size_error2();
      $$=$<mose>2;
     }
    ;

opt_in_size:
        | IN SIZE
        ;
opt_end_stringcmd:
    | END_STRINGCMD
    ;
opt_end_unstring:
    | END_UNSTRING
    ;
unstring_delimited:
    DELIMITED opt_by unstring_delimited_vars { $$=$3; }
    | /* nothing */                          { $$=NULL; }
    ;
unstring_delimited_vars:
    opt_all gname       { $$=alloc_unstring_delimited($1,$2); }
    | unstring_delimited_vars OR opt_all gname {
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
opt_all:
    /* nothing */           { $$=0; }
    | ALL                   { $$=1; }
    ;
opt_on_overflow:
    { curr_division = CDIV_EXCEPTION; }
    on_overflow
    on_not_overflow
    { curr_division = CDIV_PROC; }
    ;
on_not_exception:
    NOT ONTOK EXCEPTION { $<ival>$ = begin_on_except(); } 
        statement_list            { gen_jmplabel($<dval>-1); $$=$<ival>4; }
    | /* nothing */ { $$ = 0; }
    ;
on_overflow:
    ONTOK OVERFLOWTK          { $<dval>$ = gen_at_end(-1); }
        statement_list            { gen_dstlabel($<dval>3); }
    | /* nothing */
    ;
on_not_overflow:
    NOTEXCEP ONTOK OVERFLOWTK { $<dval>$ = gen_at_end(0); }
        statement_list            { gen_dstlabel($<dval>4); }
    | /* nothing */
    ;
opt_invalid_key:
    { curr_division = CDIV_EXCEPTION; }
    opt_invalid_key_sentence
    opt_not_invalid_key_sentence
    { curr_division = CDIV_PROC; }
    ;
opt_invalid_key_sentence:
    INVALID opt_key             { $<dval>$ = gen_at_end(23); }
    statement_list                    { gen_dstlabel($<dval>3); }
    | /* nothing */
    ;
opt_not_invalid_key_sentence:
    NOTEXCEP INVALID opt_key    { $<dval>$ = gen_at_end(0); }
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
 * Expression
 */

expr:
      gname            { $$ = $1; }
    | '(' expr ')'     { $$ = $2; }
    | expr '*' expr    { $$ = create_expr('*', $1, $3); }
    | expr '/' expr    { $$ = create_expr('/', $1, $3); }
    | expr '+' expr    { $$ = create_expr('+', $1, $3); }
    | expr '-' expr    { $$ = create_expr('-', $1, $3); }
    | expr POW_OP expr { $$ = create_expr('^', $1, $3); }
    ;
/* opt_expr will be NULL or a (struct sym *) pointer if the expression
   was given, otherwise it will be valued -1 */
opt_expr:
    /* nothing */   { $$ = (struct sym *)-1; }
    | expr          { $$ = $1; }
    ;
using_options:
    /* nothing */   { $$=0; }
    | USING     { $<ival>$=0; /* to save how many parameters */ }
      dummy     { $<ival>$=CALL; }
      parm_list  { $$=$<ival>2; } /* modified to signal calling pgm */
    ;
returning_options:
    /* nothing */   { $$=0; }
    | RETURNING variable { $$=$2; }
    | GIVING variable { $$=$2; }
    ;
dummy: /* nothing */ ;
using_parameters:   /* defined at procedure division */
    /* nothing */       { $$=0; }
    | USING     { $<ival>$=USING; }
      var_list  { $$=1; }
    ;
var_list:
    var_list opt_sep gname
        {   if ($<ival>0 == MOVE)
                gen_move($<sval>-2,$<sval>3);
            else if ($<ival>0 == INITIALIZE)
                gen_initialize($<sval>3);
            else if ($<ival>0 == ADD)
                gen_add($<sval>-2,$<sval>3,0);
            else if ($<ival>0 == USING)
                gen_save_using($<sval>3);
        }
        | gname
        {   if ($<ival>0 == MOVE)
                gen_move($<sval>-2,$<sval>1);
            else if ($<ival>0 == INITIALIZE)
                gen_initialize($<sval>1);
            else if ($<ival>0 == ADD)
                gen_add($<sval>-2,$<sval>1,0);
            else if ($<ival>0 == USING)
                gen_save_using($<sval>1);
        }
    ;
parm_list:
    parm_list opt_sep parameter
        {   if ($<ival>0 == USING)
                gen_save_using($<sval>3);
            else if ($<ival>0 == CALL) {
                gen_push_using($<sval>3);
            }
        }
        | parameter
        {   if ($<ival>0 == USING)
                gen_save_using($<sval>1);
            else if ($<ival>0 == CALL) {
                gen_push_using($<sval>1);
            }
        }
    ;
parameter:
    gname {$$=$1;
            if ($$->litflag==1) {
               struct lit *lp=(struct lit *)$$;
               lp->call_mode=curr_call_mode;
               }
            else
               $$->call_mode=curr_call_mode;
        }
    | BY parm_type gname
        {   $$=$3;
            curr_call_mode=$<ival>2;
            if ($$->litflag==1) {
               struct lit *lp=(struct lit *)$$;
               lp->call_mode=curr_call_mode;
               }
            else
               $$->call_mode=curr_call_mode;
        }
 /*   | OMITTED
        {   $$=save_special_literal('0','9', "%ZEROS%");
            $$->call_mode=CM_VAL;
        } */
    ;
parm_type:
    REFERENCE {$$=CM_REF;}
    | VALUE {$$=CM_VAL;}
    | CONTENT {$$=CM_CONT;}
/*    | DESCRIPTOR {$$=CM_CONT;}*/
    ;
intrinsic_parm_list:
    intrinsic_parm_list opt_sep intrinsic_parm
        { gen_push_using($<sval>3);}
    | intrinsic_parm
        { gen_push_using($<sval>1);}
    ;
intrinsic_parm:
    gname {$$=$1;$$->call_mode=CM_REF;}
    ;

condition:
    expr extended_cond_op
    {
      if ($2 & COND_UNARY)
	{
	  if ($2 & COND_CLASS)
	    gen_class_check ($1, $2);
	  else
	    {
	      struct sym *sy =
		(struct sym *) save_special_literal ('0', '9', "%ZEROS%");
	      gen_compare ($1, $2 & ~COND_UNARY, sy);
	    }
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
    | NOT  condition    { gen_not(); $$=$2; }
    | condition AND     { $<dval>$=gen_andstart(); }
                implied_op_condition { gen_dstlabel($<dval>3); $$=$4; }
    | condition OR      { $<dval>$=gen_orstart(); }
        implied_op_condition { gen_dstlabel($<dval>3); $$=$4; }
    | '(' condition ')' { $$ = $2; }
    | cond_name {
                                /*if ($1->level != 88)
                                        yyerror("condition unknown");*/
                                gen_condition($1);
                                $$.sy=NULL;
                                $$.oper=0;
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
sign_condition:
    POSITIVE        { $$=GREATER; }
    | NEGATIVE      { $$=LESS; }
    | ZERONUM       { $$=EQUAL; }
    ;
class_condition:
    NUMERIC                     { $$=CLASS_NUMERIC; }
    | ALPHABETICTOK             { $$=CLASS_ALPHABETIC; }
    | ALPHABETICLOWER           { $$=CLASS_ALPHABETICLOWER; }
    | ALPHABETICUPPER           { $$=CLASS_ALPHABETICUPPER; }
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
    | sign_condition            { $$ = $1 | COND_UNARY; }
    ;
cond_op:
      conditional               { $$ = $1; }
    | NOT conditional           { $$ = $2 ^ 7; }
    | conditional OR conditional { $$ = $1 | $3; }
    ;
conditional:
    CONDITIONAL opt_than_to     { $$ = $1; }
    ;
opt_sep: | ',' ;
/* this token doesn't really exists, but forces look ahead 
   to keep line numbers synchronized with our position
   because we need to generate correct debug stabs */
opt_dummy: | TOKDUMMY ;
opt_EOS: | EOS ;
opt_not:
    /* nothing */ { $$=0; }
    | NOT { $$=1; }
    ;
opt_then: | THEN ;
opt_key: | KEY ;
opt_line: | LINE ;
opt_advancing: | ADVANCING ;
opt_than_to:
    /* nothing */
    | TO { }
    | THAN { }
    ;
opt_record: | RECORD ;
opt_at: | AT ;
opt_in: | IN ;
in_of: IN | OF ;
opt_by: | BY ;
opt_upon: | UPON ;
opt_with: | WITH ;
opt_on: | ONTOK ;
opt_gname:
    /* nothing */ { $$ = NULL; }
    | gname       { $$ = $1; }
    ;
opt_to: /* nothing */
    | TO { }
gname:  name    {       
                  /*if (!is_variable($1)) {
                                yyerror("The symbol \"%s\" is not an allowed argument here", $1->name);
                     }*/
                        $$ = $1; }
    | gliteral      { $$ = (struct sym *)$1;}
    | FUNCTION LABELSTR '(' {
                                $2->type = 'f'; /* function type */
                                $<ival>$=CALL;
                        }
                intrinsic_parm_list ')' {
                 $$ = gen_intrinsic_call((struct sym *)$2); }
    ;
name_or_lit:
    name      { $$ = $1; }
    | literal { $$ = (struct sym *)$1; }
    ;
noallname:
    name      { $$ = $1; }
    | without_all_literal { $$ = (struct sym *)$1; }
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
    ALL literal { $2->all=1; $$=$2; }
    | ALL special_literal { $$=$2; }
    ;
special_literal:
    SPACES          { $$=spe_lit_SP; }
    | ZERONUM       { $$=spe_lit_ZE; }
    | QUOTES        { $$=spe_lit_QU; }
    | HIGHVALUES    { $$=spe_lit_HV; }
    | LOWVALUES     { $$=spe_lit_LV; }
    ;
var_or_nliteral:
    variable        { $$ = $1; }
    | nliteral      { $$ = (struct sym *)$1; }
    ;
nliteral:
    signed_nliteral    { save_literal($1,'9'); $1->all = 0; $$=$1; }
    ;
literal:
    signed_nliteral        { save_literal($1,'9'); $1->all=0; $$=$1; }
    | CLITERAL      { save_literal($1,'X'); $1->all=0; $$=$1; }
    ;
signed_nliteral:
        NLITERAL                { check_decimal_point($1); $$=$1; }
        | '+' NLITERAL  { check_decimal_point($2); $$=$2; }
        | '-' NLITERAL  { check_decimal_point($2);
                        invert_literal_sign( $2 );
                        $$=$2;
                }
        ;
opt_def_name:
    def_name        { $$ = $1; }
    | /* nothing */ { $$ = alloc_filler(); }
    ;
def_name:
    STRING  { if ($1->defined)
                yyerror("variable redefined, %s",$1->name);
              $1->defined=1;
              $$=$1;
            }
    | FILLER    { $<sval>$=alloc_filler(); }
    ;
variable_indexed:
    SUBSCVAR
    {
      if ($1->occurs_flg == 0)
         yyerror("\"%s\" is not an indexed variable ", $1->name);
      $$=$1;
    }
    ;

filename:
    literal { $$=(struct sym *)$1; }
    | STRING {$$=$1; }
    ;
cond_name:
    VARCOND '('
    {
      curr_division = CDIV_SUBSCRIPTS;
    }
    subscript_list  ')'
    {
      curr_division = CDIV_PROC;
      $$ = (struct sym *)make_subref( $1, $4 );
      /*check_subscripts($$);*/
    }
    | VARCOND  { $<sval>$=$1; }
    ;
name:
    variable '(' gname ':' opt_gname ')'
    {
      $$ = (struct sym *) create_refmoded_var($1, $3, $5);
      check_refmods((struct sym *)$$);
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
    | qualified_var LPAR { curr_division = CDIV_SUBSCRIPTS; }
      subscript_list ')' {
      curr_division = CDIV_PROC;
      $$ = (struct sym *)make_subref( $1, $4 );
      check_subscripts($$);
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
    | subscript_list ',' subscript      { $$ = add_subscript($1, $3); }
    ;
subscript:
      gname                     { $$ = create_subscript( $1 ); }
    | subscript '+' gname       { $$ = add_subscript_item( $1, '+', $3 ); }
    | subscript '-' gname       { $$ = add_subscript_item( $1, '-', $3 ); }
    ;
integer:
    signed_nliteral {
      char *s;
      $$=0;
      s=$1->name;
      while (isdigit(*s))
	$$ = $$ * 10 + *s++ - '0';
      if (*s)
	yyerror("only integers accepted here");
    }
    ;
label:
    LABELSTR in_of LABELSTR
    {
      struct sym *lab=$1;
      if (lab->defined == 0)
	{
	  lab->defined = 2;
	  lab->parent = $3;
	}
      else if ((lab=lookup_label($1,$3))==NULL)
	{
	  lab = install($1->name,SYTB_LAB,2);
	  lab->defined=2;
	  lab->parent = $3;
	}
      $$ = lab;
    }
    | LABELSTR
    {
      struct sym *lab=$1;
      if (lab->defined == 0)
	{
	  lab->defined = 2;
	  lab->parent = curr_section;
	}
      else if ((lab=lookup_label(lab,curr_section))==NULL)
	{
	  lab = install($1->name,SYTB_LAB,2);
	  lab->defined=2;
	  lab->parent = curr_section;
	}
      $$ = lab;
    }
    ;
anystring:
    STRING
    | LABELSTR
    ;

%%

static void
assert_numeric_sy (struct sym *sy)
{
  if (!is_numeric_sy (sy))
    yyerror ("non numeric variable: %s", sy->name);
}

void
yywarn (char *s, ...)
{
  va_list argptr;
  char buff[BUFSIZ];

  /* Build warning */
  va_start (argptr, s);
  vsprintf (buff, s, argptr);
  va_end (argptr);

  /* Display warning */
  printf ("%s:%d: warning: %s before `%s'\n",
	  cob_orig_filename, cob_orig_lineno, buff, yytext);
  cob_warning_count++;
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
  cob_error_count++;
}
