/*							-*- c -*-
 *  Copyright (C) 2001, 2000, 1999 David Essex
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this software; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */ 

%{
/*#undef yywrap*/

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "cobpp.h"
#include "parser.h"	/* definitions from YACC */

/*#define DEBUG_COBPP_SCANNER 		1*/

#define YY_NO_UNPUT

#define MAX_COPY_DEPTH 		10
#define MAX_CPYNAME_LEN 	65
#define MAXLINEBUF         	1025
#define MAXBUF         		1025

#define FIXFORM_6      		6
#define FIXFORM_7      		7
#define FIXFORM_71     		71
#define FIXFORM_72     		72
#define FIXFORM_73     		73

#define lexchar input

static struct fstack {
	YY_BUFFER_STATE buffer;
	char *filename;
	int lineno;
	struct copy_symbols *last_symbol;
} file_stack[10];
 
static struct copy_symbols {
	struct copy_symbols *next;
	char *value;
	char *replacement;
} *copy_symbols=NULL;
struct copy_symbols *append_copy_symbols=NULL;

char *include_cpybook;
char *include_cpylib;

char include_full_filename[MAXBUF];
char toktext[MAXBUF];

extern FILE *yylist;

struct reserved_sym {
	char *name;
	unsigned int token;
} reserved_symbols[] = {
	{"COPY",TOK_COPY},
	{"OFF",TOK_OFF},
	{"OF",TOK_OF},
	{"IN",TOK_IN},
	{"BY",TOK_BY},
	{"REPLACE",TOK_REPLACE},
	{"REPLACING",TOK_REPLACING},
	{NULL,0}
};

static int fsp=0;
int i,j;
unsigned int ret_token;
int copy_state_sw = 0;
int copy_start_sw = 0;
int replace_state_sw = 0;
int replace_start_sw = 0;
int previous_state = 0;
int initial_state = 0;
int lf_add = 0;
int quote_sw = 0;
unsigned int columnLen;
unsigned int columnCount = 0;
unsigned int lineCount = 1;
int lineno = 1;
char *filename;
char lineout[80];

void lineExpand(void);
void tabExpand(void);
void lexError( char * msg );
char *savename(const char *s, int x1, int x2);
void debug_rtn (const char *s, unsigned long Ccnt, unsigned long Lcnt, int Len, const char *s1);

void copy_switch(void);
void copy_do_replacements(char *buffer);
void copy_do_replacements_bk(char *buffer);
char *copy_strip_spaces(char *s);
int lex_fgets(char *buf, int maxsize);

#define DEBUG_RTN(msg) debug_rtn (msg, columnCount, lineCount, yyleng, yytext)
#define DEBUG_RTN1(msg) debug_rtn (msg, columnCount, lineCount, yyleng, yytext+1)
#define DEBUG_RTN_NL(msg) debug_rtn (msg, columnCount, lineCount, yyleng, "<NL>")

#define YY_INPUT(buf,result,max_size) \
	{ \
		result = lex_fgets( buf, max_size ); \
	}

%}

cr	     [\r]
lf	     [\n]
eol	     {lf}
tab	     [\t]
blank        [ ]
Quote	     [\'\"]
NoQuoteEol   [^\'\"\n]
NoDQuote     [^\"]
DQuote	     [\"]
SQuote	     [\']
ws	     [ \n]
Period	     \.
Comma	     [,]
Alpha        [A-Za-z]
AlphaNum     [A-Za-z0-9\-]
Numeric      [0-9]
AnyNoDQuote  [^\"\t\n]
AnyNoSQuote  [^\'\t\n]
EqSign	     [=]
EqSign2	     "=="
NoEqSign     [^\n=]

%x INITAL FREE FIXED FIXED_7 FIXED_A FIXED_LC COMMENT COMMENT_FREE FIXED_ALPHA_LIT

%%

%{
      struct copy_symbols *tmp;
%}
<*>{tab}	{ tabExpand();  }
<FREE>{eol}	{ 
                 DEBUG_RTN_NL ("FREE - {eol} ");
                 lineno++;
                 lineCount++;
                 columnCount=0; 
		 ECHO;
		 if (yylist != NULL) { 
                    fprintf(yylist, "\n%06d ", lineCount); 
                 }
		 if (copy_start_sw != 0) {
                     if (globalEnvPtr->debugFlag != 0) {
                        for (tmp = copy_symbols, i=0; tmp!=NULL; tmp=tmp->next, i++) {
                            fprintf(stderr, "lextrace: <FREE>{eol}  copy_symbols(%d) value=%s replacement=%s\n",
        	            i,
        	            tmp->value,
        	            tmp->replacement 
        	            );
                        }
                     }
                     copy_switch();
                 } 
}

<FREE>^"/"       |
<FREE>^"$"       |       
<FREE>^"*"       {
            	 columnCount = columnCount + yyleng;
                 DEBUG_RTN ("FREE - *|/|$");
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 /* remove lines */  
		BEGIN(COMMENT_FREE);
}

<FREE>{blank} 	{
                DEBUG_RTN ("FREE - blank");
	        columnCount++; 
 		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 
                ECHO; 
}

<FREE>{Numeric}{1,18} {
        DEBUG_RTN ("FREE - {Numeric}");
	columnCount = columnCount + yyleng;
 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           strncpy(toktext, yytext, MAXBUF);   
	   yylval.str = savename(toktext, 0, 0);
 	   return(TOK_LITERAL);
	}
	else {
           ECHO; 
	}

}
<FREE>{EqSign2}{NoEqSign}+{EqSign2}  {
        DEBUG_RTN ("FREE - {EqSign2}{NoEqSign}+{EqSign2}");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
           if (globalEnvPtr->debugFlag != 0) {
              if (copy_state_sw != 0) {
                  fprintf(stderr, 
               	   "lextrace: TOK_PSEUDO_TEXT :%s: copy_state_sw=%d\n", 
               	   yytext, copy_state_sw);
              }
              else{
                  fprintf(stderr, 
               	   "lextrace: TOK_PSEUDO_TEXT :%s: replace_state_sw=%d\n", 
               	   yytext, replace_state_sw);
              }
	   }
           strncpy(toktext, yytext, MAXBUF);   
           yylval.str = savename(toktext, 2, 2);
 	   return(TOK_PSEUDO_TEXT);
	}
	else {
           ECHO; 
	}
}
<FREE>\"{AnyNoDQuote}+\" {
        DEBUG_RTN ("FREE - {DQuote}{AnyNoDQuote}+{DQuote}");
	columnCount = columnCount + yyleng;
 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           if (globalEnvPtr->debugFlag != 0) {
               fprintf(stderr, 
            	   "lextrace: TOK_LITERAL :%s: copy_state_sw=%d\n", 
            	   yytext, copy_state_sw);
           }
           strncpy(toktext, yytext, MAXBUF);   
	   /* yylval.str = savename(toktext, 1, 1); */
	   yylval.str = savename(toktext, 0, 0);
 	   return(TOK_LITERAL);
	}
	else {
           ECHO; 
	}

}
<FREE>\'{AnyNoSQuote}+\' {
        DEBUG_RTN ("FREE - {SQuote}{AnyNoSQuote}+{SQuote}");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 

        if (copy_state_sw != 0) {
           if (globalEnvPtr->debugFlag != 0) {
               fprintf(stderr, 
            	   "lextrace: TOK_LITERAL :%s: copy_state_sw=%d\n", 
            	   yytext, copy_state_sw);
           }
           strncpy(toktext, yytext, MAXBUF);   
	   /*yylval.str = savename(toktext, 1, 1);*/
	   yylval.str = savename(toktext, 0, 0);
 	   return(TOK_LITERAL);
	}
	else {
           ECHO; 
	}
}
<FREE>{Period}/[ \n]  {
         DEBUG_RTN ("FREE - {Period}");
	 columnCount++;
 	 if (yylist != NULL) { 
	    fprintf(yylist, "%s", yytext);
  	 } 
	 if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
	    return(TOK_PERIOD);
	 }
	 else {
           ECHO; 
	}
}
<FREE>[cC][oO][pP][yY] {
        DEBUG_RTN ("FREE - [cC][oO][pP][yY] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 

        copy_state_sw++;
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_COPY=%d, copy_state_sw=%d;\n", 
                 TOK_COPY, copy_state_sw);
        }
	return TOK_COPY;
}
<FREE>[oO][Ff] {
        DEBUG_RTN ("FREE - [oO][Ff] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_OF=%d, copy_state_sw=%d;\n", 
                 TOK_OF, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_OF;
        }
        else {
            ECHO;
        }
}
<FREE>[oO][Ff][Ff] {
        DEBUG_RTN ("FREE - [oO][Ff][Ff] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_OFF=%d, replace_state_sw=%d;\n", 
                 TOK_OFF, replace_state_sw);
        }
        if (replace_state_sw != 0) {
	    return TOK_OFF;
        }
        else {
            ECHO;
        }
}

<FREE>[Ii][nN] {
        DEBUG_RTN ("FREE - [Ii][nN] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_IN=%d, copy_state_sw=%d;\n", 
                 TOK_IN, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_IN;
        }
        else {
            ECHO;
        }
}

<FREE>[Bb][yY] {
        DEBUG_RTN ("FREE - [Bb][yY] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_BY=%d, copy_state_sw=%d, replace_state_sw=%d;\n", 
                 TOK_BY, copy_state_sw, replace_state_sw);
        }
        if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
	    return TOK_BY;
        }
        else {
            ECHO;
        }
}
<FREE>[Rr][Ee][Pp][Ll][Aa][Cc][Ee] {
        DEBUG_RTN ("FREE - [Rr][Ee][Pp][Ll][Aa][Cc][Ee] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        replace_state_sw++;
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_REPLACE=%d, replace_state_sw=%d;\n", 
                 TOK_REPLACE, replace_state_sw);
        }
	return TOK_REPLACE;
}

<FREE>[Rr][Ee][Pp][Ll][Aa][Cc][iI][nN][Gg] {
        DEBUG_RTN ("FREE - [Rr][Ee][Pp][Ll][Aa][Cc][iI][nN][Gg] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_REPLACING=%d, copy_state_sw=%d;\n", 
                 TOK_REPLACING, copy_state_sw);
        }
	if (copy_state_sw != 0) {
	    return TOK_REPLACING;
        }
        else {
            ECHO;
        }
}
<FREE>({AlphaNum}*{Alpha}+{AlphaNum}*){1,30} {
                DEBUG_RTN ("FREE - ({AlphaNum}*{Alpha}+{AlphaNum}*){1,30}"); 
	        columnCount = columnCount + yyleng;

 		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 

	        if (copy_state_sw != 0) {
                   if (globalEnvPtr->debugFlag != 0) {
                      fprintf(stderr, 
                     	  "lextrace: TOK_IDENTIFIER :%s: copy_state_sw=%d\n", 
                     	  yytext, copy_state_sw);
                   }
                   strncpy(toktext, yytext, MAXBUF);   
	           yylval.str = savename(toktext, 0, 0);
 	           return(TOK_IDENTIFIER);
	        }
	        else {
                   ECHO;
	        }
} 
<FREE>{Comma} {
        DEBUG_RTN ("FREE - COMMA ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_IN=%d, copy_state_sw=%d;\n", 
                 TOK_COMMA, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_COMMA;
        }
        else {
            ECHO;
        }
} 
<FREE>. 	{ 
                DEBUG_RTN ("FREE - .");
		columnCount++;

 		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 
	        ECHO;
}	
<COMMENT_FREE>. { 
                 DEBUG_RTN ("COMMENT_FREE - . ");
		 columnCount++;
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 }
}
<COMMENT_FREE>{eol} { 
                 DEBUG_RTN_NL ("COMMENT_FREE - {eol} ");
                 lineno++;
                 lineCount++;
                 columnCount=0; 
		 ECHO;
		 if (yylist != NULL) { 
                    fprintf(yylist, "\n%06d ", lineCount); 
                 }
		 BEGIN(FREE);
}

<FIXED_LC>{eol}.{6}"-"{blank}{4,65}{Quote}	 { 
                 DEBUG_RTN1 ("FIXED_LC - {eol}.{6}-{blank}{4,65}{Quote}");
		 /* Continuation line remove */ 
                 lineno++;
		 lineCount++;
		 columnCount = yyleng - 1; 
 		 if (yylist != NULL) { 
		    fprintf(yylist, "\n%06d %s", lineCount, yytext+1);
		 } 
		 lf_add++; 
		 BEGIN(FIXED_ALPHA_LIT);
}
					
<FIXED_LC>{eol} { 
                DEBUG_RTN_NL ("FIXED_LC - {eol} ");
                lineno++;
		lineCount++;
		columnCount=0; 
		ECHO;

#ifdef DEBUG_COBPP_SCANNER
                 fprintf(stderr, 
                         "debug FIXED - {eol}(%lu,%lu): lf_add=%d,\n", 
                          lineCount, 
                          columnCount, 
                          lf_add);   
#endif

		for (i=0; i<lf_add; i++) {
                    fprintf(yyout, "\n"); 
		}
		lf_add=0;
 		if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		} 

		if (copy_start_sw != 0) {
                    copy_switch();
                } 
		
                BEGIN(FIXED);
}

<COMMENT>{eol} { 
                 DEBUG_RTN_NL ("COMMENT - {eol} ");
                 lineno++;
		 lineCount++;
		 columnCount=0; 
		 ECHO; 
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 BEGIN(FIXED); 
}		

<FIXED_LC>. { 
                 DEBUG_RTN ("FIXED_LC - .");
		 columnCount++;
		 
		 if ( columnCount > 80 ) { 
 		      fprintf(stderr,
			 "%s:%d:(2) Invaild character (%c) in column %d.\n",
			 globalEnvPtr->ifname,
			 lineCount,
			 yytext[0],
			 columnCount
			 );
		 }
                 
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 
}

<FIXED>{eol} { 
                 DEBUG_RTN_NL ("FIXED - {eol}");
		 ECHO;
#ifdef DEBUG_COBPP_SCANNER
                 fprintf(stderr, 
                         "debug FIXED - {eol}(%lu,%lu): lf_add=%d,\n", 
                          lineCount, 
                          columnCount, 
                          lf_add);   
#endif
		 for (i=0; i<lf_add; i++) {
                    fprintf(yyout, "\n"); 
		 }		  
 		 if (yylist != NULL) { 
		    if (columnCount == 0) {
		       fprintf(yylist, "%06d\n", lineCount);
		    }
		    else {
		       fprintf(yylist, "\n");
		    }
		 } 
                 lineno++;
		 lineCount++; 
		 columnCount=0; 
		 lf_add=0; 
		 BEGIN(FIXED);
}
<FIXED>^.{6}"-"{blank}{4,65}{Quote}	 { 
		 columnCount = columnCount + yyleng; 
                 DEBUG_RTN ("FIXED - ^.{6}-{blank}{4,65}{Quote}");
		 /* Continuation line remove */ 
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%06d %s", lineCount, yytext);
		 }
		 lf_add++; 
		 BEGIN(FIXED_ALPHA_LIT);
}
					
<FIXED>^.{6} {
                 DEBUG_RTN ("FIXED - ^.{6}");
		 columnCount+=6; 
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%06d %s", lineCount, yytext);
		 } 
		 BEGIN(FIXED_7); 
}		
<FIXED_7>"*"	|
<FIXED_7>"/" 	|
<FIXED_7>"$" 	{ 
                 DEBUG_RTN ("FIXED_7 - *|/|$");
		 columnCount++;
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 /* comment line remove */ 
		 BEGIN(COMMENT);
}
	
<FIXED_7>[^ \n]	{ 
                 DEBUG_RTN ("FIXED_7 - [^ nl]");
		 globalEnvPtr->errFlag++;
		 fprintf(stderr,
		    	  "%s:%d:(3) Invaild character (%c) in column %d.\n",
			  globalEnvPtr->ifname,
			  lineCount,
			  yytext[0],
			  columnCount
			 );
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 BEGIN(COMMENT);

}

<FIXED_7>{eol}	{ 
                 DEBUG_RTN_NL ("FIXED_7 - {eol}");
                 lineno++;
		 lineCount++;
		 columnCount=0;
		 ECHO;
 		 if (yylist != NULL) { 
		    fprintf(yylist, "\n");
		 } 
		 for (i=0; i<lf_add; i++) {
                    fprintf(yyout, "\n"); 
		 }
		 lf_add=0;
		 if (copy_start_sw != 0) {
                    copy_switch();
                 } 
		 BEGIN(FIXED); 
}

<FIXED_7>{blank} { 
                 DEBUG_RTN ("FIXED_7 - blank");
		 columnCount++;
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 ECHO;
                 if (previous_state == FIXED_ALPHA_LIT) {
                    BEGIN(FIXED_ALPHA_LIT);
                 }
                 else {
                    BEGIN(FIXED_A);
                 }
}
	
<FIXED_A>{blank}   {
                DEBUG_RTN ("FIXED_A - blank");
	        columnCount++; 
 		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 
	        ECHO;
	        if (columnCount < 72) {
		   BEGIN(FIXED_A); 
	        }
	        else {
	           BEGIN(FIXED_LC);
	        }
}

<FIXED_A>{Numeric}{1,18} {
        DEBUG_RTN ("FIXED_A - {Numeric}");

 	if ((columnCount + yyleng) > 71) {
 	   yyless(72 - columnCount); 
 	}

 	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           strncpy(toktext, yytext, MAXBUF);   
	   yylval.str = savename(toktext, 0, 0);
 	   return(TOK_LITERAL);
	}
	else {
           ECHO; 
	}
	
	if (columnCount < 72) {
	   BEGIN(FIXED_A); 
	}
	else {
	   BEGIN(FIXED_LC);
	}

}

<FIXED_A>{EqSign2}{NoEqSign}+{EqSign2}  {
        DEBUG_RTN ("FIXED_A - {EqSign2}{NoEqSign}+{EqSign2}");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
           if (globalEnvPtr->debugFlag != 0) {
              if (copy_state_sw != 0) {
                  fprintf(stderr, 
               	   "lextrace: TOK_PSEUDO_TEXT :%s: copy_state_sw=%d\n", 
               	   yytext, copy_state_sw);
              }
              else{
                  fprintf(stderr, 
               	   "lextrace: TOK_PSEUDO_TEXT :%s: replace_state_sw=%d\n", 
               	   yytext, replace_state_sw);
              }
	   }
           strncpy(toktext, yytext, MAXBUF);   
           yylval.str = savename(toktext, 2, 2);
 	   return(TOK_PSEUDO_TEXT);
	}
	else {
           ECHO; 
	}
}

<FIXED_ALPHA_LIT>\"/[^"] {
        DEBUG_RTN ("FIXED_ALPHA_LIT - {DQuote}/{NoDQuote}");
	columnCount++;
 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           if (globalEnvPtr->debugFlag != 0) {
               fprintf(stderr, 
            	   "lextrace: TOK_LITERAL :%s: copy_state_sw=%d\n", 
            	   yytext, copy_state_sw);
           }
           strcat(toktext, yytext);   
	   /* yylval.str = savename(toktext, 1, 1); */
	   yylval.str = savename(toktext, 0, 0);
           previous_state = 0;
           quote_sw = 0;
           BEGIN(FIXED_A);
 	   return(TOK_LITERAL);
	}
	else {
           ECHO; 
           previous_state = 0;
           quote_sw = 0;
           BEGIN(FIXED_A);
	}
}

<FIXED_ALPHA_LIT>{DQuote}{2} {
        DEBUG_RTN ("FIXED_ALPHA_LIT - {DQuote}{2}");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        
        if (copy_state_sw != 0) {
           strcat(toktext, yytext);
	}
	else {
           ECHO; 
	}

        if (columnCount == FIXFORM_72) {
           previous_state = FIXED_ALPHA_LIT;
           BEGIN(FIXED_LC);
        }
}

<FIXED_ALPHA_LIT>\' {
        DEBUG_RTN ("FIXED_ALPHA_LIT - {SQuote}");
	columnCount++;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 

        if (copy_state_sw != 0) {
           if (globalEnvPtr->debugFlag != 0) {
               fprintf(stderr, 
            	   "lextrace: TOK_LITERAL :%s: copy_state_sw=%d\n", 
            	   yytext, copy_state_sw);
           }
           strcat(toktext, yytext);
	   if (quote_sw == 1) {
 	       /* yylval.str = savename(toktext, 1, 1); */
	       yylval.str = savename(toktext, 0, 0);
               previous_state = 0;
	       quote_sw = 0;
	       BEGIN(FIXED_A);
 	       return(TOK_LITERAL);
           }
	}
	else {
           ECHO; 
	   if (quote_sw == 1) {
              previous_state = 0;
              quote_sw = 0;
              BEGIN(FIXED_A);
           }
	}
}

<FIXED_ALPHA_LIT>{NoQuoteEol} {
        DEBUG_RTN ("FIXED_ALPHA_LIT - {NoQuoteEol}");
	columnCount++;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        
        if (copy_state_sw != 0) {
           strcat(toktext, yytext);
	}
	else {
           ECHO; 
	}

        if (columnCount == FIXFORM_72) {
           previous_state = FIXED_ALPHA_LIT;
           BEGIN(FIXED_LC);
        }
}

<FIXED_ALPHA_LIT>{eol} {
        DEBUG_RTN ("FIXED_ALPHA_LIT - {eol}");
	columnCount=0;
        lineCount++; 
        lineno++;

 	if (yylist != NULL) { 
	   fprintf(yylist, "\n");
	} 

        if (copy_state_sw != 0) {
           if (globalEnvPtr->debugFlag != 0) {
               fprintf(stderr, 
            	   "lextrace: TOK_LITERAL :%s: copy_state_sw=%d\n", 
            	   yytext, copy_state_sw);
           }
	}
	else {
           BEGIN(FIXED);
	}
}

<FIXED_A>\" {
        DEBUG_RTN ("FIXED_A - {DQuote}");
	columnCount++;
	quote_sw = 2;
 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           strncpy(toktext, yytext, MAXBUF);   
	}
	else {
           ECHO; 
	}
        previous_state = 0;
        BEGIN(FIXED_ALPHA_LIT);
}

<FIXED_A>\' {
        DEBUG_RTN ("FIXED_A - {SQuote}");
	columnCount++;
	quote_sw = 1;
 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (copy_state_sw != 0) {
           strncpy(toktext, yytext, MAXBUF);   
	}
	else {
           ECHO; 
	}
        previous_state = 0;
        BEGIN(FIXED_ALPHA_LIT);
}

<FIXED_A>{Period}/[ \n]  {
         DEBUG_RTN ("FIXED_A - {Period}");
	 columnCount++;
 	 if (yylist != NULL) { 
	    fprintf(yylist, "%s", yytext);
  	 } 
	 if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
	    return(TOK_PERIOD);
	 }
	 else {
           ECHO; 
	}
}
           
<FIXED_A>[cC][oO][pP][yY] {
        DEBUG_RTN ("FIXED_A - [cC][oO][pP][yY] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 

        copy_state_sw++;
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_COPY=%d, copy_state_sw=%d;\n", 
                 TOK_COPY, copy_state_sw);
        }
	return TOK_COPY;
}
<FIXED_A>[oO][Ff] {
        DEBUG_RTN ("FIXED_A - [oO][Ff] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_OF=%d, copy_state_sw=%d;\n", 
                 TOK_OF, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_OF;
        }
        else {
            ECHO;
        }
}

<FIXED_A>[oO][Ff][Ff] {
        DEBUG_RTN ("FIXED_A - [oO][Ff][Ff] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_OFF=%d, replace_state_sw=%d;\n", 
                 TOK_OFF, replace_state_sw);
        }
        if (replace_state_sw != 0) {
	    return TOK_OFF;
        }
        else {
            ECHO;
        }
}

<FIXED_A>[Ii][nN] {
        DEBUG_RTN ("FIXED_A - [Ii][nN] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_IN=%d, copy_state_sw=%d;\n", 
                 TOK_IN, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_IN;
        }
        else {
            ECHO;
        }
}

<FIXED_A>[Bb][yY] {
        DEBUG_RTN ("FIXED_A - [Bb][yY] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_BY=%d, copy_state_sw=%d, replace_state_sw=%d;\n", 
                 TOK_BY, copy_state_sw, replace_state_sw);
        }
        if ((copy_state_sw != 0) || (replace_state_sw != 0)) {
	    return TOK_BY;
        }
        else {
            ECHO;
        }
}

<FIXED_A>[Rr][Ee][Pp][Ll][Aa][Cc][Ee] {
        DEBUG_RTN ("FIXED_A - [Rr][Ee][Pp][Ll][Aa][Cc][Ee] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        replace_state_sw++;
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_REPLACE=%d, replace_state_sw=%d;\n", 
                 TOK_REPLACE, replace_state_sw);
        }
	return TOK_REPLACE;
}

<FIXED_A>[Rr][Ee][Pp][Ll][Aa][Cc][iI][nN][Gg] {
        DEBUG_RTN ("FIXED_A - [Rr][Ee][Pp][Ll][Aa][Cc][iI][nN][Gg] ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_REPLACING=%d, copy_state_sw=%d;\n", 
                 TOK_REPLACING, copy_state_sw);
        }
	if (copy_state_sw != 0) {
	    return TOK_REPLACING;
        }
        else {
            ECHO;
        }
}
           
<FIXED_A>({AlphaNum}*{Alpha}+{AlphaNum}*){1,30} {
                DEBUG_RTN ("FIXED_A - ({AlphaNum}*{Alpha}+{AlphaNum}*){1,30}"); 

                if ((columnCount + yyleng) > 71) {
                   yyless(72 - columnCount); 
 		}

 	        columnCount = columnCount + yyleng;

		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 

	        if (copy_state_sw != 0) {
                   if (globalEnvPtr->debugFlag != 0) {
                      fprintf(stderr, 
                	       "lextrace: TOK_IDENTIFIER :%s: copy_state_sw=%d\n", 
                	       yytext, copy_state_sw);
                   }
                   strncpy(toktext, yytext, MAXBUF);   
	           yylval.str = savename(toktext, 0, 0);
 	           return(TOK_IDENTIFIER);
	        }
	        else {
                   ECHO;
	        }

	        if (columnCount < 72) {
		   BEGIN(FIXED_A); 
	        }
	        else {
	           BEGIN(FIXED_LC);
	        }

} 
            
<FIXED_A>{Comma} {
        DEBUG_RTN ("FIXED_A - COMMA  ");
	columnCount = columnCount + yyleng;

 	if (yylist != NULL) { 
	   fprintf(yylist, "%s", yytext);
	} 
        if (globalEnvPtr->debugFlag != 0) {
           fprintf(stderr, 
                 "lextrace: TOK_IN=%d, copy_state_sw=%d;\n", 
                 TOK_COMMA, copy_state_sw);
        }
        if (copy_state_sw != 0) {
	    return TOK_COMMA;
        }
        else {
            ECHO;
        }
} 

<FIXED_A>{eol} { 
                 DEBUG_RTN_NL ("FIXED_A - {eol} ");
		 lineCount++; 
		 columnCount=0; 
		 ECHO;
		 if (yylist != NULL) { 
		   fprintf(yylist,"\n");
		 } 
		 for (i=0; i<lf_add; i++) {
                    fprintf(yyout, "\n"); 
		 }
		 lf_add=0;
		 if (copy_start_sw != 0) {
                     if (globalEnvPtr->debugFlag != 0) {
                        for (tmp = copy_symbols, i=0; tmp!=NULL; tmp=tmp->next, i++) {
                            fprintf(stderr, "lextrace: <FIXED_A>{eol}  copy_symbols(%d) value=%s replacement=%s\n",
        	            i,
        	            tmp->value,
        	            tmp->replacement 
        	            );
                        }
                     }
                     copy_switch();
                 } 
		 BEGIN(FIXED);

}

<FIXED_A>.      { 
                DEBUG_RTN ("FIXED_A - .");
                columnCount++;
 		if (yylist != NULL) { 
		   fprintf(yylist, "%s", yytext);
		} 
	        ECHO;
	        if (columnCount < 71) {
		   BEGIN(FIXED_A); 
	        }
	        else {
	           BEGIN(FIXED_LC);
	        }
}	

<COMMENT>. { 
                 DEBUG_RTN ("COMMENT - . ");
            	 columnCount = columnCount + yyleng; 
		 /*
		 if ( columnCount > 80 ) { 
 		      fprintf(stderr,
			 "%s:%lu:(2) Invaild character (%c) in column %lu.\n",
			 globalEnvPtr->ifname,
			 lineCount,
			 yytext[0],
			 columnCount
			 );
		 }
		 */
 		 if (yylist != NULL) { 
		    fprintf(yylist, "%s", yytext);
		 } 
		 if ( columnCount > 71 ) {
 		    /*ECHO;*/
		    BEGIN(FIXED_LC);
		 }
		 

}


<<EOF>> {
	struct copy_symbols *cs,*cs1;
	if (fsp == 0) {
	        if (globalEnvPtr->debugFlag != 0) {
		//fprintf(stderr,"scanner receiving <<EOF>>\n");
                    fprintf(stderr, "lextrace: receiving <<EOF>> for fsp = %d:\n",
    	            fsp
    	            );
    	        }
		yyterminate();
	}
	else {

// 		tmp = lineno;
// 		yy_delete_buffer( YY_CURRENT_BUFFER );
// 		yy_switch_to_buffer( file_stack[fsp].buffer );
// 		lineno = file_stack[ fsp  ].line;
		/* free all replacements in this dead context */
	        if (globalEnvPtr->debugFlag != 0) {
                    fprintf(stderr, "lextrace: begin delete fsp = %d:\n",
    	            fsp
    	            );
    	        }
		cs1 = file_stack[fsp].last_symbol;
		cs = copy_symbols;
		if (cs == cs1)
			copy_symbols = NULL;
		while (cs) {
			if (cs->next == cs1)
				break;
			cs = cs->next;
		}
		if (cs)
			cs->next = NULL; 
		if (cs1) {
			while (cs1) {
                                if (globalEnvPtr->debugFlag != 0) {
                                    fprintf(stderr, "lextrace: delete level=%d, value=%s, replacement=%s:\n",
    	                            fsp,
    	                            cs1->value,
    	                            cs1->replacement
    	                            );
                                }
				free( cs1->value );
				free( cs1->replacement );
				cs = cs1->next;
				cs1->next = NULL;
				free( cs1 );
				cs1 = cs;
				cs = NULL;
			}
		}
		yy_delete_buffer( YY_CURRENT_BUFFER );
		fsp--;
		yy_switch_to_buffer( file_stack[fsp].buffer );
		free (filename);
		filename = file_stack[fsp].filename;
		lineno   = file_stack[fsp].lineno;
		fprintf (yyout, "# %d \"%s\"\n", lineno, filename);

	}
}

%%


/*
 * print error on tab.
 */
void lineExpand(void) {
  int nlines, j;
  char *s = yytext, delm = '0';
  
  for(j=0; j<columnLen; j++) {
     if( (yytext[j] == '\'') || (yytext[j] == '"') ) {
        delm = yytext[j];
        j = columnLen + 3;
     }
  }
  if(delm == '0') {
    fprintf(stderr, "Error: quote/double expected in litteral, not found");
    return;
  }
  strncpy(lineout, s, 66);
  lineout[66] = '\0';
  printf("%s", lineout);
  columnCount = 66;
  s = yytext + 66;
  nlines = ((columnLen - 66) / 60) + 1;

  for(j=1; j<nlines; j++) {
     sprintf(lineout, "-    %c", delm);
     strncat(lineout, s, 60);
     lineout[66] = '\0';
     s = s + 60;
     columnCount =  columnCount + 66;
     lineCount++;
     lineno++;
     printf("\n%06d%s", lineCount, lineout); 
  }

  if( (j = strlen(s)) > 0) {
    sprintf(lineout, "-    %c", delm);
    strncat(lineout, s, j);
    lineout[6 + j] = '\0';
    lineCount++;
    lineno++;
    printf("\n%06d%s", lineCount, lineout); 
  }
  		    
/*
    fprintf(stderr,
           "%s: Character (%c) in column %d, line %lu.\n",
	    globalEnvPtr->ifname,
	    yytext[0],
            columnCount,
            lineCount);
*/
}

/* Expand tabs if tabs number defined */
void tabExpand(void) {
  int i, j;
  
  if (globalEnvPtr->tab2space > 0) {
     j = globalEnvPtr->tab2space - (columnCount % globalEnvPtr->tab2space);
     for (i=0; i<j; i++) {
         fprintf(yyout, " "); 
     }
     if (yylist != NULL) { 
        for (i=0; i<j; i++) {
           fprintf(yylist, " ");
        }
     } 
     if (globalEnvPtr->debugFlag != 0) {
        fprintf(stderr, 
           "lex debug: column=%d; line=%d; mod=%d; j=%d;\n", 
           columnCount,
           lineCount,
           (columnCount % globalEnvPtr->tab2space), 
           j 
           ); 
     } 
     columnCount = columnCount + j;
  }
  else {
     fprintf(stderr, "%s:%d: Error tab in column %d\n", 
	globalEnvPtr->ifname,
	lineCount, 
	columnCount
	);
      globalEnvPtr->errFlag++;
  }

  return;
}

void lexError( char * msg ) {

	(void)fprintf(stderr,
		"%s:%d: %s\n"
		,globalEnvPtr->ifname
		,lineCount
		,msg
	);
	globalEnvPtr->errFlag++;
	return;
}
 
int yywrap(void) {
 
/* yywrap will be called to open files from the List of Files in the
 * Global Env struct.
 */
	return 1;
}

char *savename(const char *s, int x1, int x2) {
  char *ap, *s1;
  int len;
  if ((ap = (char *) malloc( strlen(s) + 1 )) != NULL) {
     strcpy(ap, s + x1);
     len = strlen(ap);
     s1 = ap + len - x2;
     *s1 = '\0';
     s1++;
     *s1 = '\0';
#ifdef DEBUG_COBPP_SCANNER
        fprintf(stderr, "lextrace: savename :%s:\n", ap);
#endif
  }
  return(ap);
}

void debug_rtn (const char *s, unsigned long Ccnt, unsigned long Lcnt, int Len, const char *s1) {

#ifdef DEBUG_COBPP_SCANNER
     fprintf(stderr, 
             "debug %s(%lu,%lu): length=%d, :%s:\n", 
             s,
             Lcnt, 
             Ccnt, 
             Len,
             s1
             );   
#endif
}

int lookup_reserved(char *s) {
    
    int i=0;
    
    while (reserved_symbols[i].name != NULL) {
       if (strcasecmp(s, reserved_symbols[i].name) == 0) {
          if (globalEnvPtr->debugFlag != 0) {
              fprintf(stderr, 
                  "lextrace: lookup_reserved=(:%s:, :%s:, %d)\n", 
                  s, reserved_symbols[i].name, reserved_symbols[i].token);
          }
          return reserved_symbols[i].token;
       }
       i++;
    }
    
    return 0;
}

int lex_fgets(char *buf, int maxsize) {
	int result;
	char *s;
	char tbuf[MAXLINEBUF];
	s = fgets(buf, maxsize, yyin);
	result = (s==NULL) ? YY_NULL: strlen(s);

// 	if (globalEnvPtr->debugFlag != 0 ) {
//               fprintf(stderr, "lextrace: result[%d]=%d;\n", lineCount, result);
// 	}

//      This code will remove the <CR> from buffer, as found in DOS format files
//
	if (result > 1) {
// 	    if (globalEnvPtr->debugFlag != 0 ) {
//    	       fprintf(stderr,"lextrace: buf(%d) before: |%s|\n", strlen(buf), buf);
//    	    }
	    if (buf[result - 2] == CHR_CR) {
	    	buf[result - 2] = CHR_LF;
	    	buf[result - 1] = CHR_EOS;
	    	result = (s==NULL) ? YY_NULL: strlen(s);
	    }
// 	    if (globalEnvPtr->debugFlag != 0 ) {
//    	       fprintf(stderr,"lextrace: buf(%d)  after: |%s|\n", strlen(buf), buf);
//                fprintf(stderr, "lextrace: result[%d]=%d;\n", lineCount, result);
//    	    }
	}
	
	if (globalEnvPtr->debugFlag != 0 ) {
//   	   fprintf(stderr,"lextrace: buf[%d] before: |%s|\n", strlen(buf), buf);
	   if (fsp > 0) {
              fprintf(stderr, "lextrace: fsp = %d:\n",
    	            fsp
    	            );
          }
        }
	
	if ((result) && (fsp > 0)) {
	        strcpy(tbuf, buf);
	        tbuf[ strlen(tbuf) - 1] = CHR_EOS;
                if (globalEnvPtr->debugFlag != 0) {
  		   fprintf(stderr,"lextrace: tbuf[%d] before: |%s|\n", strlen(tbuf), tbuf);
//  		   fprintf(stderr,"lextrace: buf[%d] before: |%s|\n", strlen(buf), buf);
                }	 
		copy_do_replacements( buf ); 
		result = strlen(buf);

	        strcpy(tbuf, buf);
	        tbuf[ strlen(tbuf) - 1] = CHR_EOS;
                if (globalEnvPtr->debugFlag != 0) {
  		   fprintf(stderr,"lextrace:  tbuf[%d] after: |%s|\n", strlen(tbuf), tbuf); 
//  		   fprintf(stderr,"lextrace: buf[%d] after: |%s|\n", strlen(buf), buf); 
                }	 

	} 
	return result;
}

void setup_scanner_state () {

    /*copy_state_sw = 0;
    copy_start_sw = 0;
    replace_state_sw = 0;
    replace_start_sw = 0;*/

    if ( globalEnvPtr->codeFormat == 1 ) {
      /* free == 1, fixed == 0 */
	    if (yylist != NULL) { 
              fprintf(yylist, "%06d ", lineCount); 
            }
    	    BEGIN( FREE ) ;
    }
    else {
         if ( globalEnvPtr->codeFormat == 0) {
    	    BEGIN( FIXED ) ;
         }
         else {
    	    assert(1);
         }
    }

    fprintf (yyout, "# %d \"%s\"\n", lineno, filename);
}

void copy_switch(void) {

	struct copy_symbols *tmp;

	if (fsp>=MAX_COPY_DEPTH) {
		/* yyerror("Impossible to redirect input!"); */
		fprintf(stderr, "Copy nested too deeply (max=%d)\n", MAX_COPY_DEPTH);
		exit(1);
	}
	file_stack[ fsp ].buffer = YY_CURRENT_BUFFER;
 	file_stack[ fsp ].filename = filename;
 	file_stack[ fsp ].lineno = lineno;
// 	file_stack[ fsp ].copybook = include_cpybook;
// 	file_stack[ fsp ].copylib = include_cpylib;

        if (globalEnvPtr->debugFlag != 0) {
           for (tmp = append_copy_symbols,i=0; tmp!=NULL; tmp=tmp->next, i++) {
               fprintf(stderr, "lextrace: dump2 copy_symbols(%d) value=%s replacement=%s\n",
        	      i,
        	      tmp->value,
        	      tmp->replacement 
        	      );
           }
        }

 	copy_symbols = NULL;

	fsp++;
	file_stack[ fsp ].last_symbol = append_copy_symbols;
        append_copy_symbols = NULL;

        if (globalEnvPtr->debugFlag != 0 ) {
           fprintf(stderr, "lextrace: level=%d, copybook=%s copylib=%s\n", 
             fsp,
	     include_cpybook,
	     include_cpylib
             );
           for (tmp = file_stack[fsp].last_symbol, i=0; tmp!=NULL; tmp=tmp->next, i++) {
               fprintf(stderr, "lextrace: dump3(%d) level=%d, value=%s, replacement=%s\n",
                         i,
                         fsp,
                         tmp->value,
                         tmp->replacement 
                         );
           }
                
        }

        if (find_copybook_file(include_cpybook, include_cpylib) != 0) {

	   fprintf(stderr, "error: copybook file %s not found\n", include_cpybook);

	   exit(1);
        }     
           
	if ((yyin = fopen( include_full_filename, "r" ))==NULL) {
		fprintf(stderr, "Cannot open copy file %s\n", include_full_filename);
		exit(1);
	}

	yy_switch_to_buffer( yy_create_buffer(yyin,YY_BUF_SIZE) );
	filename = strdup (include_full_filename);
	lineno   = 1;
	fprintf (yyout, "# %d \"%s\"\n", lineno, filename);

        copy_start_sw--;

}

void add_copy_replacement(char *orig_value, char *replacement) {

    struct copy_symbols *cs,*tmp;
    int i;

    if (globalEnvPtr->debugFlag != 0 ) {
       fprintf(stderr, "lextrace: add_copy_replacement - %s will be replaced by %s\n",
    	       orig_value,
    	       replacement
    	       );
    }
	cs = malloc(sizeof(struct copy_symbols));
	cs->value = orig_value;
	cs->replacement = replacement;
	cs->next = NULL;
	if (append_copy_symbols==NULL) 
		append_copy_symbols = cs;
	else {
		tmp=append_copy_symbols;
		while (tmp->next) 
			tmp=tmp->next;
		tmp->next = cs;
	}
        if (globalEnvPtr->debugFlag != 0 ) {
           for (tmp = append_copy_symbols, i=0; tmp!=NULL; tmp=tmp->next, i++) {
        	fprintf(stderr, "lextrace: add_copy_replacement dump4(%d): value=%s replacement=%s\n",
                         i,
                         tmp->value,
                         tmp->replacement 
                         );
           }
        }
}

void copy_do_replacements(char *buffer) {
	struct copy_symbols *cs;
	char *s, *s1, *tmp;
	int len, end, i, sw, cnt;
	
	if (globalEnvPtr->debugFlag != 0 ) {
              fprintf(stderr, "lextrace: copy_do_replacements fsp = %d:\n",
    	            fsp
    	            );
        }
	// Replacement table empty
        copy_symbols = file_stack[fsp].last_symbol;
	if (file_stack[fsp].last_symbol == NULL) {
	   if (globalEnvPtr->debugFlag != 0 ) {
               fprintf(stderr, "lextrace: Empty replace list \n");
           }
	   return;
	}
        else {
	   if (globalEnvPtr->debugFlag != 0 ) {
             fprintf(stderr, "lextrace: Replace list is NOT empty \n");
             for (cs=copy_symbols, i=0; cs != NULL; cs=cs->next, i++) {
                  fprintf(stderr, "lextrace: Replace list (%02d) :%s: by :%s:\n",
                          i,
			 cs->value,
 			 cs->replacement
    	          	  );
             }
           }
        }

	// ignore comments
	if (globalEnvPtr->codeFormat == 1) {
	   if (*buffer == CHR_ASTERIX)  {
	      if (globalEnvPtr->debugFlag != 0 ) {
                 fprintf(stderr, "lextrace: Ignoring comment line :%s:\n", buffer);
              }
	      return;
	   }
	}
	else {
	   len = strlen(buffer);
	   if (len > FIXFORM_7) {
	      if (*(buffer + FIXFORM_6) == CHR_ASTERIX)  {
	         if (globalEnvPtr->debugFlag != 0 ) {
                    fprintf(stderr, "lextrace: Ignoring comment line :%s:\n", buffer);
                 }
	         return;
	      }
	   }
	   else {
	      if (globalEnvPtr->debugFlag != 0) {
                 fprintf(stderr, "lextrace: Ignoring empty fixed format line :%s:\n", buffer);
              }
	      return;
	   }
	}

	if (globalEnvPtr->debugFlag != 0 ) {
           fprintf(stderr, "lextrace: After comment check \n");
           fprintf(stderr, "lextrace: Before copy replace :%s:\n", buffer);
        }
	
	tmp = malloc(MAXLINEBUF);
 	strcpy(tmp, "");
	s  = buffer;
	s1 = tmp;
 	len = strlen(buffer);
	if (globalEnvPtr->codeFormat == 0) {
	   for (i=0; i<FIXFORM_7; i++) {
	      *s1++ = *s++;
	   }
           *(s1+1) = '\0';
	   i = FIXFORM_7;
	   if (len > FIXFORM_72) {
	      end = len - (len - FIXFORM_72);
	   }
	   else {
	      end = len;
	   }
	}
	else {
	   i = 0;
	   end = len;
	}

        copy_symbols = file_stack[fsp].last_symbol;

        cnt = 0;
        while (i < end) {
  	    if (*s && (*s == ' ' || *s == '\t')) {
               *s1 = *s;
               s1++;
               *s1 = '\0';
               s++;
               i++;
  	    }
  	    else {
  	       sw = 0;
               for (cs=copy_symbols; cs != NULL; ) {
                   if (!strncmp(s, cs->value, strlen(cs->value))) {
// 				c = *(s+strlen(cs->value));
// 				if (c==' ' || c =='\0') {
                                    strcat(s1, cs->replacement);
                                    cnt++;
				    s  += strlen(cs->value);
				    i  += strlen(cs->value); 
                                    s1 += strlen(cs->replacement);
//                                    *s1++ = ' ';
//                                    *(s1+1) = '\0';
				    if (globalEnvPtr->debugFlag != 0 ) {
                                       fprintf(stderr, "lextrace: Now replacing :%s: by :%s:\n",
    	                                    cs->value,
    	                                    cs->replacement
    	                                    );
                                    }
		                    cs = NULL;
  	                            sw = 1;
		   }
		   else {
		      cs=cs->next;
		   }
               }
               if (sw == 0) {
                  *s1 = *s;
                  s++;
                  s1++;
                  *s1 = '\0';
                  i++;
               }
            }
        }

        if (cnt != 0) {
           // Try to fix 73-80 cols.         
           if (globalEnvPtr->codeFormat == 0) {
           
              len = strlen(tmp);
	      if (globalEnvPtr->debugFlag != 0 ) {
                  fprintf(stderr, "lextrace: Before 73-80 cols fix len=%d :%s:\n",
    	              len,
    	              tmp
    	              );
              }
                 if (len > FIXFORM_72) {
                     sw = 0;
                     for (i=len-1; i>=FIXFORM_71; i--) {
                     	  s1 = tmp + i;
                     	  if (*s1 != ' ') {
                     	     sw++;
                     	     i = 0;
                     	  }
                     	  else {
                     	     s1 = tmp + i;
                     	     *s1 = '\0';
                     	  }
                     }
                     s1 = buffer + FIXFORM_71;
                     strcat(tmp, s1);
                 }
                 else {
                      if (len < FIXFORM_72) {
                         for (i=len - 1; i<FIXFORM_72; i++) {
                              s1 = tmp + i;
                              *s1 = ' ';
                              *(s1+1) = '\0';
                         }
                      }
                      s1 = buffer + FIXFORM_72;
                      strcat(tmp, s1);
                 }
           }
           strcpy(buffer, tmp);
        }
        strcat(buffer, "\n");
        
        if (globalEnvPtr->debugFlag != 0 ) {
           fprintf(stderr, "lextrace: After copy replace loop :%s:", buffer);
        }
	free(tmp);
}

/* 
  Despite the name, copy_strip_spaces eliminates ", " and "; "
  separators too, besides multiples spaces.
  This follows standard CD-1.2 pp.679, rule 7.b 
*/
char *copy_strip_spaces(char *s) {
	char *s1,*new;
	s1 = new = malloc(strlen(s)+2);
	while (*s==' ' || *s=='\t') /* don't strip indenting */
		*s1++=*s++;
	while (*s) {
		if ((*s==',' || *s==';') && *(s+1)==' ') s++;
		if (*s==' ' || *s=='\t' || *s=='\n') {
			*s1++ = ' ';
			if (*s=='\n') break;
			while (*s && (*s==' ' || *s=='\t')) s++;
		}
		else
			*s1++ = *s++;
	}
	*s1=0;
	return new;
}

