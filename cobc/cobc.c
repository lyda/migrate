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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libcob.h>

#include "cobc.h"
#include "tree.h"
#include "getopt.h"
#include "gettext.h"
#include "codegen.h"
#include "reserved.h"
#include "defaults.h"

/* from parser.c */
extern int yyparse (void);
#ifdef COB_DEBUG
extern int yy_flex_debug;
extern int yy_bison_debug;
#endif


/*
 * Global variables
 */

int cobc_module_flag = 0;
int cobc_debug_flag = 0;
int cobc_optimize_flag = 0;
int cobc_failsafe_flag = 1;
int cobc_link_style = LINK_DYNAMIC;

char *cobc_index_func;
char *cobc_index_depending_func;

FILE *cobc_out;


/*
 * Local variables
 */

static int save_temps_flag = 0;

static char *program_name;
static char *output_name;

static char cob_cc[FILENAME_MAX];		/* gcc */
static char cob_cobpp[FILENAME_MAX];		/* cobpp */
static char cob_cflags[FILENAME_MAX];		/* -I... */
static char cob_libs[FILENAME_MAX];		/* -L... -lcob */
static char cobpp_flags[FILENAME_MAX];

static enum level {
  stage_preprocess,
  stage_translate,
  stage_compile,
  stage_assemble,
  stage_link
} compile_level;

static struct filename {
  int need_preprocess;
  int need_translate;
  int need_assemble;
  char source[FILENAME_MAX];			/* foo.cob */
  char preprocess[FILENAME_MAX];		/* foo.i */
  char translate[FILENAME_MAX];			/* foo.c */
  char object[FILENAME_MAX];			/* foo.o */
  struct filename *next;
} *file_list;

static int source_format;

#define format_unspecified	0
#define format_fixed		1
#define format_free		2


/*
 * Local functions
 */

static void
init_var (char *var, const char *env, const char *def)
{
  char *p = getenv (env);
  if (p)
    strcpy (var, p);
  else
    strcpy (var, def);
}

static void
init_environment (int argc, char *argv[])
{
  /* Initialize program_name */
  program_name = strrchr (argv[0], '/');
  if (program_name)
    program_name++;
  else
    program_name = argv[0];

  output_name = NULL;

  init_var (cob_cc,     "COB_CC",     COB_CC);
  init_var (cob_cobpp,  "COB_COBPP",  COB_COBPP);
  init_var (cob_cflags, "COB_CFLAGS", COB_CFLAGS);
  init_var (cob_libs,   "COB_LIBS",   COB_LIBS);
  strcpy (cobpp_flags, "");
}

static void
error (const char *str)
{
  fprintf (stderr, "%s: ", program_name);
  perror (str);
  exit (1);
}


/*
 * Command line
 */

static char short_options[] = "hvECScmxgOo:DT:I:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'v'},
  {"debug", no_argument, 0, 'D'},
  {"free", no_argument, &source_format, format_free},
  {"fixed", no_argument, &source_format, format_fixed},
  {"static", no_argument, &cobc_link_style, LINK_STATIC},
  {"dynamic", no_argument, &cobc_link_style, LINK_DYNAMIC},
  {"save-temps", no_argument, &save_temps_flag, 1},
  {"MT", required_argument, 0, '%'},
  {"MF", required_argument, 0, '@'},
#ifdef COB_DEBUG
  {"ts", no_argument, &yy_flex_debug, 1},
  {"tp", no_argument, &yy_bison_debug, 1},
#endif
  {0, 0, 0, 0}
};

static void
print_version ()
{
  printf ("%s %s\n%s", COBC_PACKAGE, COBC_VERSION, COBC_COPYRIGHT);
}

static void
print_usage ()
{
  printf ("Usage: %s [options] file...\n\n", program_name);
  puts (_("General options:\n"
	  "  --help        Display this message\n"
	  "  --version     Display compiler version\n"
	  "  -save-temps   Do not delete intermediate files\n"
	  "  -E            Preprocess only; do not compile, assemble or link\n"
	  "  -C            Translate only; convert COBOL to C\n"
	  "  -S            Compile only; output assembly file\n"
	  "  -c            Compile and assemble, but do not link\n"
	  "  -m            Build a dynamic-linking module\n"
	  "  -g            Produce debugging information in the output\n"
	  "  -O            Optimize speed; minimum run-time error checking\n"
	  "  -o <file>     Place the output into <file>\n"
	  "  -MT <target>  Set target file used in dependency list\n"
	  "  -MF <file>    Place dependency list into <file>\n"
	  "\n"
	  "COBOL options:\n"
	  "  -free         Use free source format\n"
	  "  -fixed        Use fixed source format\n"
	  "  -static       Use static link for subprogram calls if possible\n"
	  "  -dynamic      Use dynamic link for subprogram calls (default)\n"
	  "  -D, -debug    Enable debugging lines\n"
	  "  -I <path>     Add copybook include path"));
#ifdef COB_DEBUG
  puts (_("\n"
	  "Debugging options:\n"
	  "  -ts           Trace scanner\n"
	  "  -tp           Trace parser"));
#endif
}

static int
process_command_line (int argc, char *argv[])
{
  int c, index;

  /* Default options */
  source_format = format_unspecified;
  compile_level = stage_link;
#ifdef COB_DEBUG
  yy_flex_debug = 0;
  yy_bison_debug = 0;
#endif

  /* Parse the options */
  while ((c = getopt_long_only (argc, argv, short_options,
				long_options, &index)) >= 0)
    {
      switch (c)
	{
	case 0: break;
	case 'h': print_usage (); exit (0);
	case 'v': print_version (); exit (0);

	case 'E': compile_level = stage_preprocess; break;
	case 'C': compile_level = stage_translate; break;
	case 'S': compile_level = stage_compile; break;
	case 'c': compile_level = stage_assemble; break;
	case 'm': cobc_module_flag = 1; break;
	case 'o': output_name = strdup (optarg); break;

	case 'g':
	  // cobc_debug_flag = 1;
	  // strcat (cob_cflags, " -g");
	  break;

	case 'O':
	  strcat (cob_cflags, " -O2");
	  cobc_optimize_flag = 1;
	  cobc_failsafe_flag = 0;
	  break;

	case '%':
	  strcat (cobpp_flags, " -MT ");
	  strcat (cobpp_flags, optarg);
	  break;

	case '@':
	  strcat (cobpp_flags, " -MF ");
	  strcat (cobpp_flags, optarg);
	  break;

	case 'I':
	  strcat (cobpp_flags, " -I ");
	  strcat (cobpp_flags, optarg);
	  break;

	case 'D':
	  strcat (cobpp_flags, " -D");
	  break;

	default:
	  print_usage ();
	  exit (1);
	}
    }

  if (cobc_failsafe_flag)
    {
      cobc_index_func = "cob_index";
      cobc_index_depending_func = "cob_index_depending";
    }
  else
    {
      cobc_index_func = "COB_INDEX";
      cobc_index_depending_func = "COB_INDEX_DEPENDING";
    }

  return optind;
}

static void
file_basename (const char *filename, char *buff)
{
  int len;
  const char *startp, *endp;

  /* Remove directory name */
  startp = strrchr (filename, '/');
  if (startp)
    startp++;
  else
    startp = filename;

  /* Remove extension */
  endp = strrchr (filename, '.');
  if (endp > startp)
    len = endp - startp;
  else
    len = strlen (startp);

  /* Copy base name */
  strncpy (buff, startp, len);
  buff[len] = '\0';
}

static const char *
file_extension (const char *filename)
{
  const char *p = strrchr (filename, '.');
  if (p)
    return p + 1;
  else
    return "";
}

static void
temp_name (char *buff, const char *ext)
{
  strcpy (buff, "/tmp/cobXXXXXX");
  close (mkstemp (buff));
  unlink (buff);
  strcat (buff, ext);
}

static struct filename *
process_filename (const char *filename)
{
  char basename[FILENAME_MAX];
  const char *extension;
  struct filename *fn = malloc (sizeof (struct filename));
  fn->need_preprocess = 1;
  fn->need_translate  = 1;
  fn->need_assemble   = 1;
  fn->next = NULL;

  file_basename (filename, basename);
  extension = file_extension (filename);

  /* Check input file type */
  if (strcmp (extension, "i") == 0)
    {
      /* already preprocessed */
      fn->need_preprocess = 0;
    }
  else if (strcmp (extension, "c") == 0 || strcmp (extension, "s") == 0)
    {
      /* already compiled */
      fn->need_preprocess = 0;
      fn->need_translate  = 0;
    }
  else if (strcmp (extension, "o") == 0)
    {
      /* already assembled */
      fn->need_preprocess = 0;
      fn->need_translate  = 0;
      fn->need_assemble   = 0;
    }

  /* Set source filename */
  strcpy (fn->source, filename);

  /* Set preprocess filename */
  if (!fn->need_preprocess)
    strcpy (fn->preprocess, fn->source);
  else if (output_name && compile_level == stage_preprocess)
    strcpy (fn->preprocess, output_name);
  else if (save_temps_flag)
    sprintf (fn->preprocess, "%s.i", basename);
  else
    temp_name (fn->preprocess, ".cob");

  /* Set translate filename */
  if (!fn->need_translate)
    strcpy (fn->translate, fn->source);
  else if (output_name && compile_level == stage_translate)
    strcpy (fn->translate, output_name);
  else if (save_temps_flag || compile_level == stage_translate)
    sprintf (fn->translate, "%s.c", basename);
  else
    temp_name (fn->translate, ".c");

  /* Set object filename */
  if (!fn->need_assemble)
    strcpy (fn->object, fn->source);
  else if (output_name && compile_level == stage_assemble)
    strcpy (fn->object, output_name);
  else if (save_temps_flag || compile_level == stage_assemble)
    sprintf (fn->object, "%s.o", basename);
  else
    temp_name (fn->object, ".o");

  return fn;
}

static int
probe_source_format (const char *filename)
{
  FILE *fp = fopen (filename, "r");
  char buff[7];

  if (!fp)
    error (filename);

  if (fgets (buff, 7, fp))
    if (('0' <= buff[0] && buff[0] <= '9')
	|| (strncmp (buff, "      ", 6) == 0))
      return format_fixed;

  /* Assume to be free format by default */
  return format_free;
}

static int
preprocess (struct filename *fn)
{
  char buff[BUFSIZ];
  sprintf (buff, "%s%s", cob_cobpp, cobpp_flags);

  if (output_name || compile_level > stage_preprocess)
    {
      strcat (buff, " -o ");
      strcat (buff, fn->preprocess);
    }

  if (source_format == format_unspecified)
    source_format = probe_source_format (fn->source);

  strcat (buff, (source_format == format_fixed) ? " -fixed" : " -free");
  strcat (buff, " ");
  strcat (buff, fn->source);
  return system (buff);
}

static int
process_translate (struct filename *fn)
{
  int ret;

  yyin = fopen (fn->preprocess, "r");
  if (!yyin)
    error (fn->preprocess);

  cobc_out = fopen (fn->translate, "w");
  if (!cobc_out)
    error (fn->translate);

  init_constants ();
  init_reserved_words ();

  ret = yyparse ();

  fclose (cobc_out);
  fclose (yyin);

  return ret;
}

static int
process_compile (struct filename *fn)
{
  char buff[BUFSIZ];
  char name[BUFSIZ];
  if (output_name)
    strcpy (name, output_name);
  else
    {
      file_basename (fn->source, name);
      strcat (name, ".s");
    }
  sprintf (buff, "%s -S -o %s %s %s",
	   cob_cc, name, cob_cflags, fn->translate);
  return system (buff);
}

static int
process_assemble (struct filename *fn)
{
  char buff[BUFSIZ];
  sprintf (buff, "%s -c -o %s %s %s",
	   cob_cc, fn->object, cob_cflags, fn->translate);
  return system (buff);
}

static int
process_module (struct filename *fn)
{
  char buff[BUFSIZ];
  char name[BUFSIZ];
  if (output_name)
    strcpy (name, output_name);
  else
    {
      file_basename (fn->source, name);
      strcat (name, ".so");
    }
  sprintf (buff, "%s -shared -Wl,-soname,%s -o %s %s %s",
	   cob_cc, name, name, fn->object, cob_libs);
  return system (buff);
}

static int
process_link (struct filename *file_list)
{
  char buff[8192], objs[4096] = "";
  char name[BUFSIZ];

  for (; file_list; file_list = file_list->next)
    {
      strcat (objs, file_list->object);
      strcat (objs, " ");
      if (!file_list->next)
	file_basename (file_list->source, name);
    }
  if (output_name)
    strcpy (name, output_name);

  sprintf (buff, "%s -o %s %s %s", cob_cc, name, objs, cob_libs);
  return system (buff);
}

int
main (int argc, char *argv[])
{
  int index;
  int status = 1;

  cob_init (0, NULL);

  /* Initialize the global variables */
  init_environment (argc, argv);

  /* Process command line arguments */
  index = process_command_line (argc, argv);

  /* Check the filename */
  if (index == argc)
    {
      print_usage ();
      exit (1);
    }

  file_list = NULL;
  while (index < argc)
    {
      struct filename *fn = process_filename (argv[index++]);
      fn->next = file_list;
      file_list = fn;

      /* Preprocess */
      if (compile_level >= stage_preprocess && fn->need_preprocess)
	if (preprocess (fn) != 0)
	  goto cleanup;

      /* Translate */
      if (compile_level >= stage_translate && fn->need_translate)
	if (process_translate (fn) != 0)
	  goto cleanup;

      /* Compile */
      if (compile_level == stage_compile)
	if (process_compile (fn) != 0)
	  goto cleanup;

      /* Assemble */
      if (compile_level >= stage_assemble && fn->need_assemble)
	if (process_assemble (fn) != 0)
	  goto cleanup;

      /* Build module */
      if (compile_level >= stage_link && cobc_module_flag == 1)
	if (process_module (fn) != 0)
	  goto cleanup;
    }

  /* Link */
  if (compile_level >= stage_link && cobc_module_flag == 0)
    if (process_link (file_list) > 0)
      goto cleanup;

  /* We successfully completed */
  status = 0;

  /* Remove unnecessary files */
 cleanup:
  if (!save_temps_flag)
    {
      struct filename *fn;
      for (fn = file_list; fn; fn = fn->next)
	{
	  if (fn->need_preprocess
	      && (status == 1 || compile_level > stage_preprocess))
	    remove (fn->preprocess);
	  if (fn->need_translate
	      && (status == 1 || compile_level > stage_translate))
	    remove (fn->translate);
	  if (fn->need_assemble
	      && (status == 1 || compile_level > stage_assemble))
	    remove (fn->object);
	}
    }

  return status;
}
