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
#include "getopt.h"
#include "gettext.h"

extern int yyparse (void);


/*
 * Global variables
 */

int cob_tab_width = 8;
int cob_debug_flag = 0;
int cob_exit_status = 0;
int cob_file_format = COB_FORMAT_FREE;
struct cob_path *cob_include_path = NULL;
struct cob_path *cob_depend_list = NULL;
FILE *cob_depend_file = NULL;
char *cob_depend_target = NULL;


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
  {"free", no_argument, &cob_file_format, COB_FORMAT_FREE},
  {"fixed", no_argument, &cob_file_format, COB_FORMAT_FIXED},
  {"MT", required_argument, 0, '%'},
  {"MF", required_argument, 0, '@'},
  {0, 0, 0, 0}
};

static void
print_version ()
{
  printf ("%s %s\n%s", COB_PACKAGE, COB_VERSION, COB_COPYRIGHT);
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
	  "  -D, -debug    Enable debugging lines\n"
	  "  -T <n>        Set tab width to <n> (default 8)\n"
	  "  -I <path>     Add copybook include path"));
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
	  cob_depend_target = strdup (optarg);
	  break;

	case '@': /* -MF */
	  cob_depend_file = fopen (optarg, "w");
	  if (!cob_depend_file)
	    perror (optarg);
	  break;

	case 'I':
	  {
	    struct cob_path *path =
	      malloc (sizeof (struct cob_path));
	    path->dir = strdup (optarg);
	    path->next = NULL;

	    /* Append at the end */
	    if (!cob_include_path)
	      cob_include_path = path;
	    else
	      {
		struct cob_path *p;
		for (p = cob_include_path; p->next; p = p->next);
		p->next = path;
	      }
	  }
	  break;

	case 'D': cob_debug_flag = 1; break;
	case 'T': cob_tab_width = atoi (optarg); break;

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

  /* Prepare input */
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
  yyparse ();

  /* Output dependency list */
  if (cob_depend_file)
    {
      struct cob_path *l;
      if (!cob_depend_target)
	{
	  fputs (_("-MT must be given to specify target file\n"), stderr);
	  exit (1);
	}
      fprintf (cob_depend_file, "%s: \\\n", cob_depend_target);
      for (l = cob_depend_list; l; l = l->next)
	fprintf (cob_depend_file, " %s%s\n", l->dir, l->next ? " \\" : "");
      for (l = cob_depend_list; l; l = l->next)
	fprintf (cob_depend_file, "%s:\n", l->dir);
      fclose (cob_depend_file);
    }

  return cob_exit_status;
}
