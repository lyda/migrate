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
#include <getopt.h>
#include <unistd.h>

#include "cobc.h"
#include "tree.h"
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
int cobc_optimize_flag = 0;
int cobc_failsafe_flag = 1;
int cobc_link_style = LINK_DYNAMIC;

FILE *cobc_out;


/*
 * Local variables
 */

static int debug_flag = 0;
static int save_temps_flag = 0;

static char *program_name;
static char *output_name;

static char cob_cc[FILENAME_MAX];		/* cc */
static char cob_cobpp[FILENAME_MAX];		/* cobpp */
static char cob_cflags[FILENAME_MAX];		/* -I... */
static char cob_libadd[FILENAME_MAX];		/* -lcob */
static char cobpp_flags[FILENAME_MAX];

static enum format {
  format_unspecified,
  format_fixed,
  format_free
} source_format;

static enum level {
  stage_preprocess,
  stage_translate,
  stage_compile,
  stage_assemble,
  stage_module,
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

  init_var (cob_cc,      "COB_CC",      COB_CC);
  init_var (cob_cobpp,   "COB_COBPP",   COB_COBPP);
  init_var (cob_cflags,  "COB_CFLAGS",  COB_CFLAGS);
  init_var (cob_libadd,  "COB_LIBADD",  COB_LIBADD);
  strcpy (cobpp_flags, "");
}

static void
cob_error (char *s, ...)
{
  va_list argptr;
  va_start (argptr, s);
  printf ("%s: ", program_name);
  vprintf (s, argptr);
  va_end (argptr);
  exit (1);
}


/*
 * Command line
 */

static char short_options[] = "hvECScmxgOo:FXDT:I:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'v'},
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
  printf ("Usage: %s [options] file...\n", program_name);
  puts ("");
  puts ("General options:");
  puts ("  --help        Display this information");
  puts ("  --version     Display compiler version");
  puts ("  -save-temps   Do not delete intermediate files");
  puts ("  -E            Preprocess only; do not compile, assemble or link");
  puts ("  -C            Translate only; do not compile, assemble or link");
  puts ("  -S            Compile only; do not assemble or link");
  puts ("  -c            Compile and assemble, but do not link");
  puts ("  -m            Compile, assemble, and build a .so module");
  puts ("  -g            Generate debug format output");
  puts ("  -O            Do exhaustive optimization");
  puts ("  -o <file>     Place the output into <file>");
  puts ("  -MT <target>  Set target file used in dependency list");
  puts ("  -MF <file>    Place dependency list into <file>");
  puts ("");
  puts ("COBOL options:");
  puts ("  -static       Use static link for subprogram calls if possible");
  puts ("  -dynamic      Use dynamic link for all subprogram calls (default)");
  puts ("  -F            Use standard fixed column format");
  puts ("  -X            Use X/Open free format");
  puts ("  -D            Compile debug lines (i.e., \"D\" lines)");
  puts ("  -I <path>     Add include (copybooks) search path");
#ifdef COB_DEBUG
  puts ("");
  puts ("Debug options:");
  puts ("  -ts           Trace scanner");
  puts ("  -tp           Trace parser");
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
	case 'm': compile_level = stage_module; cobc_module_flag = 1; break;
	case 'o': output_name = strdup (optarg); break;

	case 'g':
	  strcat (cob_cflags, " -g");
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

	case 'X': source_format = format_free; break;
	case 'F': source_format = format_fixed; break;
	case 'D': debug_flag = 1; break;

	default:
	  print_usage ();
	  exit (1);
	}
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

static enum format
probe_source_format (const char *filename)
{
  FILE *fp = fopen (filename, "r");
  char buff[7];

  if (!fp)
    {
      cob_error ("failed to open file: %s\n", filename);
      exit (1);
    }

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

  strcat (buff, (source_format == format_fixed) ? " -F" : " -X");
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
    cob_error ("cannot open file: %s\n", fn->preprocess);

  cobc_out = fopen (fn->translate, "w");
  if (!cobc_out)
    cob_error ("cannot open file: %s\n", fn->translate);

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
	   cob_cc, name, name, fn->object, cob_libadd);
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

  sprintf (buff, "%s -o %s %s %s", cob_cc, name, objs, cob_libadd);
  return system (buff);
}

int
main (int argc, char *argv[])
{
  int index;
  int status = 1;

  /* Initialize the global variables */
  init_environment (argc, argv);

  /* Process command line arguments */
  index = process_command_line (argc, argv);

  /* Check the filename */
  if (index == argc)
    cob_error ("No input files\n");

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
      if (compile_level >= stage_module && cobc_module_flag == 1)
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
