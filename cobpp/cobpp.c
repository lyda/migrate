/* COBOL preprocessor
 * 
 * Copyright (C) 1999, 2000  David Essex.
 * Copyright (C) 1998, 1999  Laura Tweedy.
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

#include "../config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <getopt.h>

#include "cobpp.h"

extern FILE *yyin;
extern FILE *yyout;
FILE *yylist;

extern char *filename;

char HTG_COPYDIR[PATHMAX1] = "";
char HTG_FNAME_SUFFIX[PATHMAX1] = "";

/* command line options */
static char option_list[] = { "f:x:o:p:t:I:vV?hd" };

static void
print_version (void)
{
  printf ("%s %s\n", PACKAGE, VERSION);
  printf ("Copyright (C) 1999,2000,2001  David Essex\n");
}

static void
print_usage (void)
{
  printf ("Usage: %s <options> <input_file> [-o <output_file>]\n", PACKAGE);
  puts ("Options:\n\
      -h                Help (display this listing)\n\
      -V	        Display version and exit\n\
      -v	        Verbose mode\n\
      -d	        Turn on debuging mode\n\
      -t <num>          Expand tabs to <num> space(s)\n\
      -o <output_file>  Output file name (default: standard output)\n\
      -p <listing_file> Listing file name \n\
      -x <input_file>   Input source is in X/Open free format\n\
      -f <input_file>   Input source is in standard fixed column format\n\
      -I <copy_dir>     Copybooks search directories");
}

/* Global Env struct due to yywrap, yylex */

int
main (int argc, char **argv)
{

  int rc = 0;

  globalEnvPtr = &globalEnv;

//      setDefaults( &globalEnv );
  setDefaults ();

  /* error, or help/version printed */
  rc = setOptions (&globalEnv, argc, argv);
  if (rc != 0)
    {
      if ((rc > 0) && (rc < 100))
	{
	  print_usage ();
	  CleanUp ();
	}
      return rc;
    }

  /* If everything is OK parse file */

  filename = strdup (globalEnv.ifname);
  yyin = fopen (globalEnv.ifname, "r");
  if (yyin == NULL)
    {
      fprintf (stderr, "Error opening input file: %s\n", globalEnv.ifname);
      rc = 3;
      return rc;
    }

  if (globalEnv.ofname != NULL)
    {
      yyout = fopen (globalEnv.ofname, "w");
      if (yyout == NULL)
	{
	  fprintf (stderr,
		   "Error opening output file: %s\n", globalEnv.ofname);
	  fclose (yyin);
	  rc = 9;
	  return rc;
	}
    }
  else
    {
      yyout = stdout;
    }

  if (globalEnv.lfname != NULL)
    {
      yylist = fopen (globalEnv.lfname, "w");
      if (yylist == NULL)
	{
	  fprintf (stderr,
		   "Error opening listing file: %s\n", globalEnv.lfname);
	  fclose (yyin);
	  if (yyout != stdout)
	    {
	      fclose (yyout);
	    }
	  rc = 11;
	  return rc;
	}
    }

  setup_scanner_state ();
//      yylex();
//      rc = yyparse();
  (void) yyparse ();

  rc = globalEnv.errFlag;
  if (globalEnvPtr->debugFlag != 0)
    {
      fprintf (stderr, "Program return code=%d\n", rc);
    }
  return rc;
}


/* 
 *  Initalize the Env struct passed to this function 
 */
void
setDefaults (void)
{

  Env *gEnv = globalEnvPtr;

  gEnv->codeFormat = -1;	/* 1 is free , 0 is fixed */

#ifdef  DEBUG_COBPP
  gEnv->debugFlag = 1;		/* debug flag */
#else
  gEnv->debugFlag = 0;		/* debug flag */
#endif
  gEnv->verboseFlag = 0;	/* set verbose flag off */
  gEnv->errFlag = 0;		/* no error state by default */
  gEnv->tab2space = 0;		/* expand tabs to 0 spaces */
  gEnv->progName = NULL;
  gEnv->ofname = NULL;
  gEnv->ifname = NULL;
  gEnv->lfname = NULL;

}

/*
 *  Function to clean up memory allocated in program.
 * 
 */
void
CleanUp (void)
{

  Env *gEnv = globalEnvPtr;

  globalEnvPtr = NULL;
  if (gEnv->progName != NULL)
    free (gEnv->progName);

  if (gEnv->ifname != NULL)
    free (gEnv->ifname);

  if (gEnv->ofname != NULL)
    free (gEnv->ofname);

  if (gEnv->lfname != NULL)
    free (gEnv->lfname);

}

/*
 * Read in command line options and set up the Env struct passed in
 * Alloc memory for program name string in Env struct.
 * 
 */
int
setOptions (Env * gEnv, int argc, char **argv)
{

  /*  Read in args for option settings. */

//     char *argument;
//     int option ;
//     int temp, r = 0, len, sw, i;
  int r = 0, len, sw, i;
//    char * tempChar;
//    struct stat tempbuf;
  char tbuf[PATHMAX1] = "";


  len = strlen (argv[0]);
  gEnv->progName = malloc (len + 1);
  if (gEnv->progName == NULL)
    {
      gEnv->errFlag++;
      r = 2;
      return r;
    }
  strncpy (gEnv->progName, argv[0], len);

  /*
   * Check argc for number of args  
   */
//    if (( argc < 2 ) || (argc > 5 )){
  if (argc < 2)
    {
      //print_usage();
      gEnv->errFlag++;
      r = 1;
      return r;
    }

  while ((sw = getopt (argc, argv, option_list)) != EOF)
    {

      switch (sw)
	{

	case 'd':
	  gEnv->debugFlag = 1;
	  break;

	case 'f':
	  if (gEnv->ifname != NULL)
	    {
	      fprintf (stderr,
		       "error: duplicate input file name '%s' ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 20;
	      return r;
	    }
	  len = strlen (optarg);
//         fprintf(stderr, "cobpp debug: optarg=%s: len=%d\n", optarg, len);
	  gEnv->ifname = (char *) malloc (len + 2);
	  if (gEnv->ifname == NULL)
	    {
	      gEnv->errFlag++;
	      r = 21;
	      return r;
	    }
	  strncpy (gEnv->ifname, optarg, len + 1);
//         fprintf(stderr, "cobpp debug: input file name :%s:, len=%d\n", gEnv->ifname, len);
	  gEnv->codeFormat = 0;	/* 1 is free, 0 is fixed */
	  break;

	case 'I':
	  len = strlen (HTG_COPYDIR);
	  if (len > 0)
	    {
	      strcat (HTG_COPYDIR, optarg);
	      strcat (HTG_COPYDIR, STR_COLEN);
	    }
	  else
	    {
	      strcpy (HTG_COPYDIR, optarg);
	      strcat (HTG_COPYDIR, STR_COLEN);
	    }
	  break;

	case 'o':
	  if (gEnv->ofname != NULL)
	    {
	      fprintf (stderr,
		       "error: duplicate output file name '%s' ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 40;
	      return r;
	    }
	  len = strlen (optarg);
	  gEnv->ofname = (char *) malloc (len + 2);
	  if (gEnv->ofname == NULL)
	    {
	      gEnv->errFlag++;
	      r = 41;
	      return r;
	    }
	  strncpy (gEnv->ofname, optarg, len + 1);
//         fprintf(stderr, "cobpp debug: output file name '%s', len=%d\n", gEnv->ofname, len);
	  break;

	case 'p':
	  if (gEnv->lfname != NULL)
	    {
	      fprintf (stderr,
		       "error: duplicate listing file name '%s' ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 50;
	      return r;
	    }
	  len = strlen (optarg);
	  gEnv->lfname = (char *) malloc (len + 2);
	  if (gEnv->lfname == NULL)
	    {
	      gEnv->errFlag++;
	      r = 51;
	      return r;
	    }
	  strncpy (gEnv->lfname, optarg, len + 1);
//         fprintf(stderr, "cobpp debug: print file name '%s', len=%d\n", gEnv->lfname, len);
	  break;

	case 't':
	  if (gEnv->tab2space != 0)
	    {
	      fprintf (stderr,
		       "error: duplicate tabs2space argument '%s' specified ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 60;
	      return r;
	    }
	  strcpy (tbuf, optarg);
//           len = strlen(optarg);
	  len = strlen (tbuf);
	  for (i = 0; i < len; i++)
	    {
//               if (!isdigit(optarg[i])) {
	      if (!isdigit (tbuf[i]))
		{
//                  fprintf(stderr, "error: invalid tabs2space argument '%s' specified ... aborting\n", optarg);
		  fprintf (stderr,
			   "error: invalid tabs2space argument '%s' specified ... aborting\n",
			   tbuf);
		  gEnv->errFlag++;
		  r = 61;
		  return r;
		}
	    }
//           sscanf(optarg, "%d", &gEnv->tab2space);
	  sscanf (tbuf, "%d", &gEnv->tab2space);
	  if ((gEnv->tab2space < 1) || (gEnv->tab2space > 99))
	    {
	      fprintf (stderr,
		       "error: invalid tabs2space argument '%s' specified, max=99 min=1 ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 62;
	      return r;
	    }
//         fprintf(stderr, "cobpp debug: input tab string :%s:, gEnv->tab2space=%d\n", optarg, gEnv->tab2space);
//         fprintf(stderr, "cobpp debug: input tab string :%s:, gEnv->tab2space=%d\n", tbuf, gEnv->tab2space);
	  break;

	case 'x':
	  if (gEnv->ifname != NULL)
	    {
	      fprintf (stderr,
		       "error: duplicate input file name '%s' ... aborting\n",
		       optarg);
	      gEnv->errFlag++;
	      r = 70;
	      return r;
	    }
	  len = strlen (optarg);
//         fprintf(stderr, "cobpp debug: optarg=%s: len=%d\n", optarg, len);
	  gEnv->ifname = (char *) malloc (len + 2);
//           gEnv->ifname = malloc(len + 2);
	  if (gEnv->ifname == NULL)
	    {
	      gEnv->errFlag++;
	      r = 71;
	      return r;
	    }
	  strncpy (gEnv->ifname, optarg, len + 1);
//         strcpy(gEnv->ifname, optarg);
//         fprintf(stderr, "cobpp debug: input file name=%s:, len=%d\n", gEnv->ifname, len);
	  gEnv->codeFormat = 1;	/* 1 is free, 0 is fixed */
	  break;

	case 'V':
	  print_version ();
	  r = 197;
	  break;

	case 'v':
	  // set verbose mode on
	  gEnv->verboseFlag = 1;
	  break;

	case '?':
	case 'h':
	default:
	  r = 5;
//        print_usage();
	  break;

	}
    }

  if (r == 0)
    {
      if (gEnv->ifname == NULL)
	{
	  fprintf (stderr,
		   "error: no input file name selected ... aborting\n");
	  gEnv->errFlag++;
	  r = 80;
	}
    }

  return r;

}

int
find_copybook_file2 (char *fname, char *lname)
{
  int r = 1, len;
  char *pt, fpath[PATHMAX1], fname1[PATHMAX], lname1[PATHMAX1];
  struct stat sbuf;

  if ((*fname == CHR_SQUOTE) || (*fname == CHR_DQUOTE))
    {
      strcpy (fname1, fname + 1);
      len = strlen (fname1);
      fname1[len - 1] = CHR_EOS;
    }
  else
    {
      strcpy (fname1, fname);
    }

  if (lname != NULL)
    {
      if ((*lname == CHR_SQUOTE) || (*lname == CHR_DQUOTE))
	{
	  strcpy (lname1, lname + 1);
	  len = strlen (lname1);
	  lname1[len - 1] = CHR_EOS;
	  strcpy (fpath, lname1);
	}
      else
	{
	  pt = getenv (lname);
	  if (pt != NULL)
	    {
	      strcpy (fpath, pt);
	      strcpy (lname1, pt);
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf
		    ("envoromental variable \'%s\' found, setting search path to \'%s\'\n",
		     lname, fpath);
		}
	    }
	  else
	    {
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("warning: envoromental variable \'%s\' not found\n",
			  lname);
		}
	      strcpy (lname1, "");
	      strcpy (fpath, "");
	    }
	}
    }
  else
    {
      strcpy (lname1, "");
      strcpy (fpath, HTG_COPYDIR);
    }

  if (globalEnvPtr->verboseFlag == 1)
    {
      fprintf (stderr, "Copybook '%s' search path(s) are \'%s\'\n", fname1,
	       fpath);
    }
  pt = strtok (fpath, STR_COLEN);
  if (pt == NULL)
    {
      len = strlen (fpath);
      if (*(fpath + len - 1) == CHR_SLASH)
	{
	  sprintf (include_full_filename, "%s%s", fpath, fname1);
	}
      else
	{
	  sprintf (include_full_filename, "%s/%s", fpath, fname1);
	}
      if (stat (include_full_filename, &sbuf) == 0)
	{
	  r = 0;
	}
    }
  else
    {
      len = strlen (pt);
      if (*(pt + len - 1) == '/')
	{
	  sprintf (include_full_filename, "%s%s", pt, fname1);
	}
      else
	{
	  sprintf (include_full_filename, "%s/%s", pt, fname1);
	}
      if (stat (include_full_filename, &sbuf) == 0)
	{
	  r = 0;
	}
      else
	{
	  pt = strtok (NULL, STR_COLEN);
	  while (pt != NULL)
	    {
	      len = strlen (pt);
	      if (*(pt + len - 1) == '/')
		{
		  sprintf (include_full_filename, "%s%s", pt, fname1);
		}
	      else
		{
		  sprintf (include_full_filename, "%s/%s", pt, fname1);
		}
	      if (stat (include_full_filename, &sbuf) == 0)
		{
		  r = 0;
		  pt = NULL;
		}
	      else
		{
		  pt = strtok (NULL, STR_COLEN);
		}
	    }
	}
    }

  if (globalEnvPtr->verboseFlag == 1)
    {
      fprintf (stderr, "Including copybook: \'%s\'\n", include_full_filename);
    }

  return r;
}

int
find_copybook_file (char *fname, char *lname)
{
  int r = 1, len, sw = 0;
  char *pt, fpath[PATHMAX1], fname1[PATHMAX], lname1[PATHMAX1];

  if (lname == NULL)
    {
      if ((*fname == CHR_SQUOTE) || (*fname == CHR_DQUOTE))
	{
	  sw = STATE_FLIT_LNONE;
	  strcpy (fname1, fname + 1);
	  len = strlen (fname1);
	  fname1[len - 1] = CHR_EOS;
	  r = find_filename_literal (fname1, HTG_COPYDIR);

	}
      else
	{
	  sw = STATE_FNAME_LNONE;
	  strcpy (fname1, fname);
	  strcpy (HTG_FNAME_SUFFIX, STR_SEARCH_SUFFIXES);
	  r = find_filename_text (fname1, HTG_COPYDIR, HTG_FNAME_SUFFIX);
	}
    }
  else
    {
      if ((*lname == CHR_SQUOTE) || (*lname == CHR_DQUOTE))
	{
	  strcpy (lname1, lname + 1);
	  len = strlen (lname1);
	  lname1[len - 1] = CHR_EOS;
	  strcpy (fpath, lname1);
	  len = strlen (fpath);
	  if (len > 0)
	    {
	      if (fpath[len - 1] != CHR_COLEN)
		{
		  strcat (fpath, STR_COLEN);
		}
	    }
	  if ((*fname == CHR_SQUOTE) || (*fname == CHR_DQUOTE))
	    {
	      sw = STATE_FLIT_LLIT;
	      strcpy (fname1, fname + 1);
	      len = strlen (fname1);
	      fname1[len - 1] = CHR_EOS;
	      r = find_filename_literal (fname1, fpath);
	    }
	  else
	    {
	      sw = STATE_FNAME_LLIT;
	      strcpy (fname1, fname);
	      strcpy (HTG_FNAME_SUFFIX, STR_SEARCH_SUFFIXES);
	      r = find_filename_text (fname1, fpath, HTG_FNAME_SUFFIX);
	    }
	}
      else
	{
	  if ((*fname == CHR_SQUOTE) || (*fname == CHR_DQUOTE))
	    {
	      sw = STATE_FLIT_LNAME;
	      strcpy (fname1, fname + 1);
	      len = strlen (fname1);
	      fname1[len - 1] = CHR_EOS;
	      strcpy (lname1, lname);
	      pt = find_env_variable (lname1);
	      if (pt != NULL)
		{
		  strcpy (fpath, pt);
		}
	      else
		{
		  strcpy (fpath, "");
		}
	      len = strlen (fpath);
	      if (len > 0)
		{
		  if (fpath[len - 1] != CHR_COLEN)
		    {
		      strcat (fpath, STR_COLEN);
		    }
		}
	      r = find_filename_literal (fname1, fpath);
	    }
	  else
	    {
	      sw = STATE_FNAME_LNAME;
	      strcpy (fname1, fname);
	      strcpy (lname1, lname);
	      strcpy (HTG_FNAME_SUFFIX, STR_SEARCH_SUFFIXES);
	      pt = find_env_variable (lname1);
	      if (pt != NULL)
		{
		  strcpy (fpath, pt);
		}
	      else
		{
		  strcpy (fpath, "");
		}
	      len = strlen (fpath);
	      if (len > 0)
		{
		  if (fpath[len - 1] != CHR_COLEN)
		    {
		      strcat (fpath, STR_COLEN);
		    }
		}
	      r = find_filename_text (fname1, fpath, HTG_FNAME_SUFFIX);
	    }
	}
    }

  return r;
}

int
find_filename_literal (char *fname, char *fp)
{
  int r = 1, len, sw1 = 0;
  char *pt1;
  struct stat sbuf;
  char fpath[PATHMAX1];

  strcpy (fpath, fp);

  len = strlen (fpath);
  if (len == 0)
    {
      sprintf (include_full_filename, "%s", fname);
      if (stat (include_full_filename, &sbuf) == 0)
	{
	  if (S_ISREG (sbuf.st_mode) &&
	      ((S_IRUSR & sbuf.st_mode) || (S_IRGRP & sbuf.st_mode)
	       || (S_IROTH & sbuf.st_mode)))
	    {
	      r = 0;
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("found copybook name \'%s\'\n",
			  include_full_filename);
		}
	    }
	  else
	    {
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("copybook file \'%s\' not readable\n",
			  include_full_filename);
		}
	    }
	}
      else
	{
	  if (globalEnvPtr->verboseFlag == 1)
	    {
	      printf ("copybook file \'%s\' not found\n",
		      include_full_filename);
	    }
	}
    }
  else
    {
      // If filename is an absolute path ignore library path 
      if (*fname == CHR_SLASH)
	{
	  if (globalEnvPtr->verboseFlag == 1)
	    {
	      printf
		("warning: ignoring other search path(s) in absolute path copybook name \'%s\'\n",
		 fname);
	    }
	  sprintf (include_full_filename, "%s", fname);
	  if (stat (include_full_filename, &sbuf) == 0)
	    {
	      if (S_ISREG (sbuf.st_mode) &&
		  ((S_IRUSR & sbuf.st_mode) || (S_IRGRP & sbuf.st_mode)
		   || (S_IROTH & sbuf.st_mode)))
		{
		  r = 0;
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("found copybook name \'%s\'\n",
			      include_full_filename);
		    }
		}
	      else
		{
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("copybook file \'%s\' not readable\n",
			      include_full_filename);
		    }
		}
	    }
	  else
	    {
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("copybook file \'%s\' not found\n",
			  include_full_filename);
		}
	    }
	}
      else
	{
	  sw1 = 0;
	  strcpy (fpath, fp);
	  pt1 = find_token (fpath, STR_COLEN, 0);
	  if (pt1 == NULL)
	    {
	      pt1 = fpath;
	    }
	  while (sw1 == 0)
	    {
	      sprintf (include_full_filename, "%s", pt1);
	      if (stat (include_full_filename, &sbuf) == 0)
		{
		  if (S_ISDIR (sbuf.st_mode) &&
		      ((S_IRUSR & sbuf.st_mode) || (S_IRGRP & sbuf.st_mode)
		       || (S_IROTH & sbuf.st_mode)))
		    {
		      if (globalEnvPtr->verboseFlag == 1)
			{
			  printf ("searching directory path \'%s\'\n",
				  include_full_filename);
			}
		      sprintf (include_full_filename, "%s/%s", pt1, fname);
		      if (stat (include_full_filename, &sbuf) == 0)
			{
			  if (S_ISREG (sbuf.st_mode) &&
			      ((S_IRUSR & sbuf.st_mode)
			       || (S_IRGRP & sbuf.st_mode)
			       || (S_IROTH & sbuf.st_mode)))
			    {
			      r = 0;
			      if (globalEnvPtr->verboseFlag == 1)
				{
				  printf ("found copybook name \'%s\'\n",
					  include_full_filename);
				}
			      sw1 = 1;
			    }
			  else
			    {
			      if (globalEnvPtr->verboseFlag == 1)
				{
				  printf
				    ("copybook file \'%s\' not readable\n",
				     include_full_filename);
				}
			    }
			}
		      else
			{
			  if (globalEnvPtr->verboseFlag == 1)
			    {
			      printf ("copybook file \'%s\' not found\n",
				      include_full_filename);
			    }
			}
		    }
		  else
		    {
		      if (globalEnvPtr->verboseFlag == 1)
			{
			  printf ("directory path \'%s\' not readable\n",
				  include_full_filename);
			}
		    }
		}
	      else
		{
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("directory path \'%s\' not found\n",
			      include_full_filename);
		    }
		}
	      pt1 = find_token (pt1, STR_COLEN, 1);
	      if (pt1 == NULL)
		{
		  sw1 = 1;
		}
	    }
	}
    }

  return r;
}

int
find_filename_text (char *fname, char *fp, char *fs)
{
  int r = 1, len, sw1, sw2;
  char *pt1, *pt2;
  struct stat sbuf;
  char fpath[PATHMAX1], fsuffix[PATHMAX];

  strcpy (fpath, fp);
  strcpy (fsuffix, fs);

  len = strlen (fpath);
  if (len == 0)
    {
      sw1 = 0;
      pt1 = find_token (fsuffix, STR_COLEN, 0);
      if (pt1 == NULL)
	{
	  pt1 = fsuffix;
	}
      while (sw1 == 0)
	{
	  sprintf (include_full_filename, "%s%s", fname, pt1);
	  if (stat (include_full_filename, &sbuf) == 0)
	    {
	      if (S_ISREG (sbuf.st_mode) &&
		  ((S_IRUSR & sbuf.st_mode) || (S_IRGRP & sbuf.st_mode)
		   || (S_IROTH & sbuf.st_mode)))
		{
		  r = 0;
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("found copybook name \'%s\'\n",
			      include_full_filename);
		    }
		  sw1 = 1;
		}
	      else
		{
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("copybook file \'%s\' not readable\n",
			      include_full_filename);
		    }
		}
	      sw1 = 1;
	    }
	  else
	    {
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("copybook file \'%s\' not found\n",
			  include_full_filename);
		}
	    }
	  pt1 = find_token (pt1, STR_COLEN, 1);
	  if (pt1 == NULL)
	    {
	      sw1 = 1;
	    }
	}
    }
  else
    {
      sw1 = 0;
      sw2 = 0;
      strcpy (fpath, fp);
      strcpy (fsuffix, fs);
      pt1 = find_token (fpath, STR_COLEN, 0);
      if (pt1 == NULL)
	{
	  pt1 = fpath;
	}
      while (sw1 == 0)
	{
	  sprintf (include_full_filename, "%s", pt1);
	  if (stat (include_full_filename, &sbuf) == 0)
	    {
	      if (S_ISDIR (sbuf.st_mode) &&
		  ((S_IRUSR & sbuf.st_mode) || (S_IRGRP & sbuf.st_mode)
		   || (S_IROTH & sbuf.st_mode)))
		{
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("searching directory path \'%s\'\n",
			      include_full_filename);
		    }
		  strcpy (fsuffix, fs);
		  sw2 = 0;
		  pt2 = find_token (fsuffix, STR_COLEN, 0);
		  while (sw2 == 0)
		    {
		      sprintf (include_full_filename, "%s/%s%s", pt1, fname,
			       pt2);
		      if (stat (include_full_filename, &sbuf) == 0)
			{
			  if (S_ISREG (sbuf.st_mode) &&
			      ((S_IRUSR & sbuf.st_mode)
			       || (S_IRGRP & sbuf.st_mode)
			       || (S_IROTH & sbuf.st_mode)))
			    {
			      r = 0;
			      if (globalEnvPtr->verboseFlag == 1)
				{
				  printf ("found copybook name \'%s\'\n",
					  include_full_filename);
				}
			      sw1 = 1;
			      sw2 = 1;
			    }
			  else
			    {
			      if (globalEnvPtr->verboseFlag == 1)
				{
				  printf
				    ("copybook file \'%s\' not readable\n",
				     include_full_filename);
				}
			    }
			  sw2 = 1;
			}
		      else
			{
			  if (globalEnvPtr->verboseFlag == 1)
			    {
			      printf ("copybook file \'%s\' not found\n",
				      include_full_filename);
			    }
			}
		      pt2 = find_token (pt2, STR_COLEN, 1);
		      if (pt2 == NULL)
			{
			  sw2 = 1;
			}
		    }
		}
	      else
		{
		  if (globalEnvPtr->verboseFlag == 1)
		    {
		      printf ("directory path \'%s\' not readable\n",
			      include_full_filename);
		    }
		}
	    }
	  else
	    {
	      if (globalEnvPtr->verboseFlag == 1)
		{
		  printf ("directory path \'%s\' not found\n",
			  include_full_filename);
		}
	    }
	  pt1 = find_token (pt1, STR_COLEN, 1);
	  if (pt1 == NULL)
	    {
	      sw1 = 1;
	    }
	}
    }

  return r;
}

char *
find_env_variable (char *ev)
{
  char *pt, ev1[PATHMAX];
  int i, len;

  len = strlen (ev);
  for (i = 0; i < len; i++)
    {
      if (*(ev + i) == STR_DASH)
	{
	  *(ev1 + i) = STR_UNDERSCORE;
	}
      else
	{
	  *(ev1 + i) = *(ev + i);
	}
    }
  *(ev1 + len) = CHR_EOS;

  pt = getenv (ev1);
  if (pt != NULL)
    {
      if (globalEnvPtr->verboseFlag == 1)
	{
	  printf
	    ("envoromental variable \'%s\' found, setting search path(s) to \'%s\'\n",
	     ev1, pt);
	}
      return pt;
    }

  if (globalEnvPtr->verboseFlag == 1)
    {
      printf ("warning: envoromental variable \'%s\' not found\n", ev1);
    }

  return NULL;
}

char *
find_token (char *p, const char *d, int sw)
{
  int i, len;

  if (globalEnvPtr->debugFlag != 0)
    {
      fprintf (stderr, "find_token trace(1): p=%s; d=%s, sw=%d\n", p, d, sw);
    }

  if (sw == 0)
    {

      len = strlen (p);
      *(p + len + 1) = CHR_EOS;
      for (i = 0; i < len; i++)
	{
	  if (*(p + i) == *d)
	    {
	      *(p + i) = CHR_EOS;
	      if (globalEnvPtr->debugFlag != 0)
		{
		  fprintf (stderr, "find_token trace(2): p=%s; d=%s, sw=%d\n",
			   p, d, sw);
		}
	      return p;
	    }
	}
      if (globalEnvPtr->debugFlag != 0)
	{
	  fprintf (stderr, "find_token trace(3): p=NULL; d=%s, sw=%d\n", d,
		   sw);
	}
      return NULL;

    }
  else
    {

      len = strlen (p);
      p = p + len + 1;
      if (*p != CHR_EOS)
	{
	  len = strlen (p);
	  for (i = 0; i < len; i++)
	    {
	      if (*(p + i) == *d)
		{
		  *(p + i) = CHR_EOS;
		  if (globalEnvPtr->debugFlag != 0)
		    {
		      fprintf (stderr,
			       "find_token trace(4): p=%s; d=%s, sw=%d\n", p,
			       d, sw);
		    }
		  return p;
		}
	    }
	}
      if (globalEnvPtr->debugFlag != 0)
	{
	  fprintf (stderr, "find_token trace(5): p=NULL; d=%s, sw=%d\n", d,
		   sw);
	}
      return NULL;

    }

}
