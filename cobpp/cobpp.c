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

#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#include "cobpp.h"
#include "scanner.h"
#include "lib/getopt.h"
#include "lib/gettext.h"

extern int yyparse (void);


/*
 * Global variables
 */

int cobpp_tab_width = 8;
int cobpp_debug_flag = 0;
int cobpp_exit_status = 0;
int cobpp_warn_trailing_line = 0;
int cobpp_source_format = COBPP_FORMAT_UNKNOWN;
int cobpp_source_format_inferred = 0;
struct cobpp_path *cobpp_include_path = NULL;
struct cobpp_path *cobpp_depend_list = NULL;
FILE *cobpp_depend_file = NULL;
char *cobpp_depend_target = NULL;


/*
 * Local varialbes
 */

static const char *program_name;


/*
 * Command line
 */

static char short_options[] = "hvo:DT:I:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'v'},
  {"debug", no_argument, 0, 'D'},
  {"free", no_argument, &cobpp_source_format, COBPP_FORMAT_FREE},
  {"fixed", no_argument, &cobpp_source_format, COBPP_FORMAT_FIXED},
  {"semi-fixed", no_argument, &cobpp_source_format, COBPP_FORMAT_SEMI_FIXED},
  {"Wtrailing-line", no_argument, &cobpp_warn_trailing_line, 1},
  {"MT", required_argument, 0, '%'},
  {"MF", required_argument, 0, '@'},
  {0, 0, 0, 0}
};

static void
print_version ()
{
  printf ("%s %s\n%s", COBPP_PACKAGE, COBPP_VERSION, COBPP_COPYRIGHT);
}

static void
print_usage ()
{
  printf ("Usage: %s [options] file\n\n", program_name);
  puts (_("General options:\n"
	  "  --help        Display this message\n"
	  "  --version     Display compiler version\n"
	  "  -o <file>     Place the output into <file>\n"
	  "  -MT <target>  Set target file used in dependency list\n"
	  "  -MF <file>    Place dependency list into <file>\n"
	  "\n"
	  "COBOL options:\n"
	  "  -free         Use free source format\n"
	  "  -fixed        Use fixed source format\n"
	  "  -semi-fixed   Use semi-fixed source format\n"
	  "  -debug        Enable debugging lines\n"
	  "  -T <n>        Set tab width to <n> (default 8)\n"
	  "  -I <path>     Add copybook include path\n"
	  "\n"
	  "Warning options:\n"
	  "  -Wtrailing-line  Source line after column 72"
	  ));
}

static int
process_command_line (int argc, char *argv[])
{
  int c, index;

  /* Default options */
  yyout = stdout;

  /* Parse the options */
  while ((c = getopt_long_only (argc, argv, short_options,
				long_options, &index)) >= 0)
    {
      switch (c)
	{
	case 0: break;
	case 'h': print_usage (); exit (0);
	case 'v': print_version (); exit (0);

	case 'o':
	  yyout = fopen (optarg, "w");
	  if (yyout == NULL)
	    {
	      perror (optarg);
	      exit (1);
	    }
	  break;

	case '%': /* -MT */
	  cobpp_depend_target = strdup (optarg);
	  break;

	case '@': /* -MF */
	  cobpp_depend_file = fopen (optarg, "w");
	  if (!cobpp_depend_file)
	    perror (optarg);
	  break;

	case 'I':
	  {
	    struct cobpp_path *path =
	      malloc (sizeof (struct cobpp_path));
	    path->dir = strdup (optarg);
	    path->next = NULL;

	    /* Append at the end */
	    if (!cobpp_include_path)
	      cobpp_include_path = path;
	    else
	      {
		struct cobpp_path *p;
		for (p = cobpp_include_path; p->next; p = p->next);
		p->next = path;
	      }
	  }
	  break;

	case 'D': cobpp_debug_flag = 1; break;
	case 'T': cobpp_tab_width = atoi (optarg); break;

	default: print_usage (); exit (1);
	}
    }

  return optind;
}


/*
 * Main
 */

int
main (int argc, char *argv[])
{
  int index;

#if ENABLE_NLS
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif

  /* Initialize program_name */
  program_name = strrchr (argv[0], '/');
  if (program_name)
    program_name++;
  else
    program_name = argv[0];

  /* Process command line arguments */
  index = process_command_line (argc, argv);

  /* Open input file */
  if (argc == index)
    {
      open_buffer (NULL, NULL);
    }
  else
    {
      if (argc > index + 1)
	fprintf (stderr, _("warning: arguments after `%s' is ignored\n"),
		 argv[index]);
      open_buffer (argv[index], NULL);
    }

  /* Infer source format */
  if (cobpp_source_format == COBPP_FORMAT_UNKNOWN)
    {
      cobpp_source_format = COBPP_FORMAT_FREE;
      cobpp_source_format_inferred = 1;

      if (yyfilename)
	{
	  char buff[7];
	  if (fgets (buff, 7, yyin))
	    if (('0' <= buff[0] && buff[0] <= '9')
		|| strncmp (buff, "      ", 6) == 0)
	      cobpp_source_format = COBPP_FORMAT_FIXED;
	  fseek (yyin, 0, SEEK_SET);
	}
    }

  yyparse ();

  /* Output dependency list */
  if (cobpp_depend_file)
    {
      struct cobpp_path *l;
      if (!cobpp_depend_target)
	{
	  fputs (_("-MT must be given to specify target file\n"), stderr);
	  exit (1);
	}
      fprintf (cobpp_depend_file, "%s: \\\n", cobpp_depend_target);
      for (l = cobpp_depend_list; l; l = l->next)
	fprintf (cobpp_depend_file, " %s%s\n", l->dir, l->next ? " \\" : "");
      for (l = cobpp_depend_list; l; l = l->next)
	fprintf (cobpp_depend_file, "%s:\n", l->dir);
      fclose (cobpp_depend_file);
    }

  return cobpp_exit_status;
}
