/* COBOL Compiler
 *
 * Copyright (C) 2001  Keisuke Nishida
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
#include "codegen.h"
#include "reserved.h"
#include "defaults.h"


/*
 * Global variables
 */

int cob_stabs_flag = 0;
int cob_debug_flag = 0;
int cob_dynamic_flag = 0;
int cob_verbose_flag = 0;

int cob_trace_scanner = 0;
int cob_trace_parser = 0;
int cob_trace_codegen = 0;
int cob_trace_command = 0;

int cob_warning_count = 0;
int cob_error_count = 0;

int cob_orig_lineno = 0;
char *cob_orig_filename = NULL;
char *cob_source_filename = NULL;
char *cob_include_filename = NULL;

char HTG_COPYDIR[BUFSIZ];

FILE *o_src;

extern int yy_flex_debug;


/*
 * Local variables
 */

/* Compiler options */
static int save_temps_flag = 0;

static char *program_name;
static char *output_filename;

/* Environment variables */
static char cob_cc[FILENAME_MAX];		/* gcc */
static char cob_cobpp[FILENAME_MAX];		/* cobpp */
static char cob_ldadd[BUFSIZ];			/* -lcob -ldb -lm ... */

static enum format {
  format_unspecified,
  format_fixed,
  format_free
} source_format;

static enum level {
  stage_preprocess,
  stage_compile,
  stage_assemble,
  stage_module,
  stage_link
} compile_level;

static struct filename {
  int need_preprocess;
  int need_compile;
  int need_assemble;
  char source[FILENAME_MAX];			/* foo.cob */
  char preprocess[FILENAME_MAX];		/* foo.i */
  char assembly[FILENAME_MAX];			/* foo.s */
  char object[FILENAME_MAX];			/* foo.o */
  char module[FILENAME_MAX];			/* foo.so */
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
  strcpy (HTG_COPYDIR, "");

  /* Initialize program_name */
  program_name = strrchr (argv[0], '/');
  if (program_name)
    program_name++;
  else
    program_name = argv[0];

  output_filename = NULL;

  init_var (cob_cc,      "COB_CC",      COB_CC);
  init_var (cob_cobpp,   "COB_COBPP",   COB_COBPP);
  init_var (cob_ldadd,   "COB_LDADD",   COB_LDADD);
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

static char short_options[] = "hvEScmxgo:FXDI:T:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'v'},
  {"dynamic", no_argument, &cob_dynamic_flag, 1},
  {"save-temps", no_argument, &save_temps_flag, 1},
#if COB_DEBUG
  {"ta", no_argument, 0, 'a'},
  {"ts", no_argument, &cob_trace_scanner, 1},
  {"tp", no_argument, &cob_trace_parser, 1},
  {"tc", no_argument, &cob_trace_codegen, 1},
  {"tx", no_argument, &cob_trace_command, 1},
#endif
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
  printf ("Usage: %s [options] file...\n", program_name);
  puts ("");
  puts ("General options:");
  puts ("  --help        Display this information");
  puts ("  --version     Display compiler version");
  puts ("  -save-temps   Do not delete intermediate files");
  puts ("  -dynamic      Use dynamic link for all subprogram calls");
  puts ("  -E            Preprocess only; do not compile, assemble or link");
  puts ("  -S            Compile only; do not assemble or link");
  puts ("  -c            Compile and assemble, but do not link");
  puts ("  -m            Compile, assemble, and build a .so module");
  puts ("  -x            Compile, assemble, and link to an executable");
  puts ("  -g            Generate debugging output");
  puts ("  -o <file>     Place the output into <file>");
  puts ("");
  puts ("COBOL options:");
  puts ("  -F            Use standard fixed column format");
  puts ("  -X            Use X/Open free format");
  puts ("  -D            Compile debug lines (i.e., \"D\" lines)");
  puts ("  -I <path>     Add include (copybooks) search path");
#ifdef COB_DEBUG
  puts ("");
  puts ("Debug options:");
  puts ("  -ta           Trace all");
  puts ("  -ts           Trace scanner");
  puts ("  -tp           Trace parser");
  puts ("  -tc           Trace codegen");
  puts ("  -tx           Trace compiler");
#endif
}

static int
process_command_line (int argc, char *argv[])
{
  int c, index;

  /* Default options */
  source_format = format_unspecified;
  compile_level = stage_link;

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
	case 'S': compile_level = stage_compile; break;
	case 'c': compile_level = stage_assemble; break;
	case 'm': compile_level = stage_module; break;
	case 'x': compile_level = stage_link; break;

	case 'g': cob_stabs_flag = 1; break;
	case 'o': output_filename = strdup (optarg); break;

	case 'I':
	  {
	    static int first = 1;
	    if (first)
	      {
		strcpy (HTG_COPYDIR, optarg);
		first = 0;
	      }
	    else
	      {
		strcat (HTG_COPYDIR, ":");
		strcat (HTG_COPYDIR, optarg);
	      }
	  }
	  break;

	case 'X': source_format = format_free; break;
	case 'F': source_format = format_fixed; break;
	case 'D': cob_debug_flag = 1; break;

#if COB_DEBUG
	case 'a':
	  cob_trace_scanner = 1;
	  cob_trace_parser = 1;
	  cob_trace_codegen = 1;
	  cob_trace_command = 1;
	  break;
#endif

	default: print_usage (); exit (1);
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
  strcpy (buff, "/tmp/fileXXXXXX");
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
  fn->need_compile    = 1;
  fn->need_assemble   = 1;
  fn->next = NULL;

  file_basename (filename, basename);
  extension = file_extension (filename);

  /* Set source filename */
  strcpy (fn->source, filename);

  /* Set preprocess filename */
  if (strcmp (extension, "i") == 0
      || strcmp (extension, "s") == 0
      || strcmp (extension, "o") == 0)
    {
      fn->need_preprocess = 0;
      strcpy (fn->preprocess, fn->source);
    }
  else if (output_filename && compile_level == stage_preprocess)
    strcpy (fn->preprocess, output_filename);
  else if (save_temps_flag)
    sprintf (fn->preprocess, "%s.i", basename);
  else
    temp_name (fn->preprocess, ".cob");

  /* Set assembly filename */
  if (strcmp (extension, "o") == 0 || strcmp (extension, "s") == 0)
    {
      fn->need_compile = 0;
      strcpy (fn->assembly, fn->source);
    }
  else if (output_filename && compile_level == stage_compile)
    strcpy (fn->assembly, output_filename);
  else if (save_temps_flag || compile_level == stage_compile)
    sprintf (fn->assembly, "%s.s", basename);
  else
    temp_name (fn->assembly, ".s");

  /* Set object filename */
  if (strcmp (extension, "o") == 0)
    {
      fn->need_assemble = 0;
      strcpy (fn->object, fn->source);
    }
  else if (output_filename && compile_level == stage_assemble)
    strcpy (fn->object, output_filename);
  else if (save_temps_flag || compile_level == stage_assemble)
    sprintf (fn->object, "%s.o", basename);
  else
    temp_name (fn->object, ".o");

  /* Set module filename */
  if (output_filename && compile_level == stage_module)
    strcpy (fn->module, output_filename);
  else if (save_temps_flag || compile_level == stage_module)
    sprintf (fn->module, "%s.so", basename);
  else
    temp_name (fn->module, ".so");

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

static void
print_command (const char *command)
{
  if (cob_trace_command)
    {
      fputs (command, stderr);
      fputc ('\n', stderr);
    }
}

static int
preprocess (struct filename *fn)
{
  char buff[BUFSIZ];

  sprintf (buff, "%s ", cob_cobpp);

  if (strlen (HTG_COPYDIR) > 0)
    {
      strcat (buff, "-I ");
      strcat (buff, HTG_COPYDIR);
      strcat (buff, " ");
    }

  if (output_filename || compile_level > stage_preprocess)
    {
      strcat (buff, "-o ");
      strcat (buff, fn->preprocess);
      strcat (buff, " ");
    }

  if (source_format == format_unspecified)
    source_format = probe_source_format (fn->source);

  strcat (buff, (source_format == format_fixed) ? "-F " : "-X ");
  strcat (buff, fn->source);
  print_command (buff);
  return system (buff);
}

static int
process_compile (struct filename *fn)
{
  yyin = fopen (fn->preprocess, "r");
  if (!yyin)
    cob_error ("cannot open file: %s\n", fn->preprocess);

  o_src = fopen (fn->assembly, "w");
  if (!o_src)
    cob_error ("cannot open file: %s\n", fn->assembly);

  init_reserved_words ();

  cob_source_filename = fn->source;
  yy_flex_debug = cob_trace_scanner;
  yyparse ();

  fclose (o_src);
  fclose (yyin);

  return (cob_error_count > 0) ? 1 : 0;
}

static int
process_assemble (struct filename *fn)
{
  char buff[BUFSIZ];
  sprintf (buff, "%s -c -o %s %s",
	   cob_cc, fn->object, fn->assembly);
  print_command (buff);
  return system (buff);
}

static int
process_module (struct filename *fn)
{
  char buff[BUFSIZ];
  sprintf (buff, "%s -shared -Wl,-soname,%s -o %s %s %s",
	   cob_cc, fn->module, fn->module, fn->object, cob_ldadd);
  print_command (buff);
  return system (buff);
}

static int
process_link (struct filename *file_list)
{
  char buff[8192], objs[4096] = "";
  char *exe = output_filename ? output_filename : "a.out";

  for (; file_list; file_list = file_list->next)
    {
      strcat (objs, file_list->object);
      strcat (objs, " ");
    }

  sprintf (buff, "%s -o %s %s %s", cob_cc, exe, objs, cob_ldadd);
  print_command (buff);
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
	if (preprocess (fn) > 0)
	  goto cleanup;

      /* Compile */
      if (compile_level >= stage_compile && fn->need_compile)
	if (process_compile (fn) > 0)
	  goto cleanup;

      /* Assemble */
      if (compile_level >= stage_assemble && fn->need_assemble)
	if (process_assemble (fn) > 0)
	  goto cleanup;

      /* Build module */
      if (compile_level == stage_module)
	if (process_module (fn) > 0)
	  goto cleanup;
    }

  /* Link */
  if (compile_level >= stage_link)
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
	  if (compile_level > stage_preprocess && fn->need_preprocess)
	    remove (fn->preprocess);
	  if (compile_level > stage_compile && fn->need_compile)
	    remove (fn->assembly);
	  if (compile_level > stage_assemble && fn->need_assemble)
	    remove (fn->object);
	}
    }

  return status;
}
