/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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
#include <libcob.h>

#if !(defined __CYGWIN__ || defined __MINGW32__)
#include <unistd.h>
#endif

#ifdef __MINGW32__
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#endif /* __MINGW32__ */

#include "cobc.h"
#include "reserved.h"
#include "lib/getopt.h"


/*
 * Global variables
 */

int cobc_flag_main = 0;
int cobc_flag_call_static = 0;
int cobc_flag_debugging_line = 0;
int cobc_flag_line_directive = 0;

#undef COBC_WARNING
#define COBC_WARNING(sig,var,name,doc) int var = 0;
#include "warning.def"

int errorcount;
int warningcount;

char *cobc_source_file = NULL;
int cobc_source_line = 0;
int cobc_source_format = COBC_FORMAT_FIXED;
int cobc_tab_width = COBC_DEFAULT_TAB_WIDTH;
int cobc_text_column = COBC_DEFAULT_TEXT_COLUMN;

FILE *cobc_depend_file = NULL;
char *cobc_depend_target = NULL;
struct cobc_name_list *cobc_depend_list = NULL;
struct cobc_name_list *cobc_include_list = NULL;

struct cobc_program *current_program = NULL;
struct cobc_label *current_section = NULL, *current_paragraph = NULL;


/*
 * Local variables
 */

static int save_temps = 0;
static int verbose_output = 0;

static char *program_name;
static char *output_name;

static char cob_cc[FILENAME_MAX];		/* gcc */
static char cob_cflags[FILENAME_MAX];		/* -I... */
static char cob_libs[FILENAME_MAX];		/* -L... -lcob */

static enum {
  stage_preprocess,
  stage_translate,
  stage_compile,
  stage_assemble,
  stage_module,
  stage_executable
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
  char *p;

  /* Initialize program_name */
  program_name = strrchr (argv[0], '/');
  if (program_name)
    program_name++;
  else
    program_name = argv[0];

  output_name = NULL;

  init_var (cob_cc,     "COB_CC",     COB_CC);
  init_var (cob_cflags, "COB_CFLAGS", COB_CFLAGS);
  init_var (cob_libs,   "COB_LIBS",   COB_LIBS);

  p = getenv ("COB_LDADD");
  if (p)
    {
      strcat (cob_libs, " ");
      strcat (cob_libs, p);
    }
}

static void
terminate (const char *str)
{
  fprintf (stderr, "%s: ", program_name);
  perror (str);
  exit (1);
}


/*
 * Command line
 */

static char short_options[] = "h?VvECScmgo:I:T:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'V'},
  {"verbose", no_argument, 0, 'v'},
  {"save-temps", no_argument, &save_temps, 1},
  {"static", no_argument, &cobc_flag_call_static, 1},
  {"dynamic", no_argument, &cobc_flag_call_static, 0},
  {"free", no_argument, &cobc_source_format, COBC_FORMAT_FREE},
  {"fixed", no_argument, &cobc_source_format, COBC_FORMAT_FIXED},
  {"column", required_argument, 0, '*'},
  {"MT", required_argument, 0, '%'},
  {"MF", required_argument, 0, '@'},
  {"fmain", no_argument, &cobc_flag_main, 1},
  {"fdebugging-line", no_argument, &cobc_flag_debugging_line, 1},
  {"Wall", no_argument, 0, 'W'},
#undef COBC_WARNING
#define COBC_WARNING(sig,var,name,doc)		\
  {"W"name, no_argument, &var, 1},		\
  {"Wno-"name, no_argument, &var, 0},
#include "warning.def"
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
  puts (_("General options:\n\
  --help                Display this message\n\
  --version             Display compiler version\n\
  --verbose, -v         Display the programs invoked by the compiler\n\
  -save-temps           Do not delete intermediate files\n\
  -E                    Preprocess only; do not compile, assemble or link\n\
  -C                    Translation only; convert COBOL to C\n\
  -S                    Compile only; output assembly file\n\
  -c                    Compile and assemble, but do not link\n\
  -m                    Build a dynamic-linking module\n\
  -g                    Produce debugging information in the output\n\
  -o <file>             Place the output into <file>\n\
  -MT <target>          Set target file used in dependency list\n\
  -MF <file>            Place dependency list into <file>\n\
\n\
COBOL options:\n\
  -free                 Use free source format\n\
  -fixed                Use fixed source format\n\
  -column <n>           Set text area column to <n> (default: 72)\n\
  -static               Use static link for subprogram calls if possible\n\
  -dynamic              Use dynamic link for subprogram calls (default)\n\
  -T <n>                Set tab width to <n> (default: 8)\n\
  -I <path>             Add copybook include path\n\
  -fmain                Include a main function in the output\n\
  -fdebugging-line      Enable debugging lines\n\
\n\
Warning options:\n\
  -Wall                 Enable all warnings"));
#undef COBC_WARNING
#define COBC_WARNING(sig,var,name,doc)		\
  printf ("  -W%-19s %s\n", name, gettext (doc));
#include "warning.def"
  puts ("");
}

static int
process_command_line (int argc, char *argv[])
{
  int c, index;

  /* Default options */
  compile_level = stage_executable;

  /* Parse the options */
  while ((c = getopt_long_only (argc, argv, short_options,
				long_options, &index)) >= 0)
    {
      switch (c)
	{
	case 0: break;
	case 'h':
	case '?': print_usage (); exit (0);
	case 'V': print_version (); exit (0);

	case 'E': compile_level = stage_preprocess; break;
	case 'C': compile_level = stage_translate; break;
	case 'S': compile_level = stage_compile; break;
	case 'c': compile_level = stage_assemble; break;
	case 'm': compile_level = stage_module; break;
	case 'v': verbose_output = 1; break;
	case 'o': output_name = strdup (optarg); break;

	case 'g':
	  cobc_flag_line_directive = 1;
	  strcat (cob_cflags, " -g");
	  break;

	case '%': /* -MT */
	  cobc_depend_target = strdup (optarg);
	  break;

	case '@': /* -MF */
	  cobc_depend_file = fopen (optarg, "w");
	  if (!cobc_depend_file)
	    perror (optarg);
	  break;

	case 'I':
	  {
	    struct cobc_name_list *list =
	      malloc (sizeof (struct cobc_name_list));
	    list->name = strdup (optarg);
	    list->next = NULL;

	    /* Append at the end */
	    if (!cobc_include_list)
	      cobc_include_list = list;
	    else
	      {
		struct cobc_name_list *p;
		for (p = cobc_include_list; p->next; p = p->next);
		p->next = list;
	      }
	  }
	  break;

	case '*': /* -column */
	  cobc_text_column = atoi (optarg);
	  break;

	case 'T':
	  cobc_tab_width = atoi (optarg);
	  break;

	case 'W':
#undef COBC_WARNING
#define COBC_WARNING(sig,var,name,doc)		\
          var = 1;
#include "warning.def"
	  break;

	default:
	  print_usage ();
	  exit (1);
	}
    }

  if (compile_level == stage_executable)
    cobc_flag_main = 1;

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
#ifdef __MINGW32__
  char temp[MAX_PATH];
  GetTempPath (MAX_PATH, temp);
  GetTempFileName (temp, "cob", 0, buff);
  DeleteFile(buff);
  strcpy (buff + strlen (buff) - 4, ext); /* replace ".tmp" by EXT */
#else /* not __MINGW32__ */
  strcpy (buff, "/tmp/cobXXXXXX");
  close (mkstemp (buff));
  unlink (buff);
  strcat (buff, ext);
#endif /* not __MINGW32__ */
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
  else if (save_temps)
    sprintf (fn->preprocess, "%s.i", basename);
  else
    temp_name (fn->preprocess, ".cob");

  /* Set translate filename */
  if (!fn->need_translate)
    strcpy (fn->translate, fn->source);
  else if (output_name && compile_level == stage_translate)
    strcpy (fn->translate, output_name);
  else if (save_temps || compile_level == stage_translate)
    sprintf (fn->translate, "%s.c", basename);
  else
    temp_name (fn->translate, ".c");

  /* Set object filename */
  if (!fn->need_assemble)
    strcpy (fn->object, fn->source);
  else if (output_name && compile_level == stage_assemble)
    strcpy (fn->object, output_name);
  else if (save_temps || compile_level == stage_assemble)
    sprintf (fn->object, "%s.o", basename);
  else
    temp_name (fn->object, ".o");

  return fn;
}

static int
process (const char *cmd)
{
  if (verbose_output)
    fprintf (stderr, "%s\n", cmd);
  return system (cmd);
}

static int
preprocess (struct filename *fn)
{
  errorcount = 0;

  ppout = stdout;
  if (output_name || compile_level > stage_preprocess)
    {
      ppout = fopen (fn->preprocess, "w");
      if (!ppout)
	terminate (fn->preprocess);
    }

  if (ppopen (fn->source, NULL, NULL) != 0)
    exit (1);

  if (verbose_output)
    fprintf (stderr, "preprocessing %s into %s\n",
	     fn->source, fn->preprocess);
  ppparse ();

  fclose (ppout);
  fclose (ppin);

  if (errorcount > 0)
    return -1;

  /* Output dependency list */
  if (cobc_depend_file)
    {
      struct cobc_name_list *l;
      if (!cobc_depend_target)
	{
	  fputs (_("-MT must be given to specify target file\n"), stderr);
	  exit (1);
	}
      fprintf (cobc_depend_file, "%s: \\\n", cobc_depend_target);
      for (l = cobc_depend_list; l; l = l->next)
	fprintf (cobc_depend_file, " %s%s\n", l->name, l->next ? " \\" : "");
      for (l = cobc_depend_list; l; l = l->next)
	fprintf (cobc_depend_file, "%s:\n", l->name);
      fclose (cobc_depend_file);
    }

  return 0;
}

static int
process_translate (struct filename *fn)
{
  int ret;

  yyin = fopen (fn->preprocess, "r");
  if (!yyin)
    terminate (fn->preprocess);

  yyout = fopen (fn->translate, "w");
  if (!yyout)
    terminate (fn->translate);

  cobc_source_file = NULL;
  cobc_source_line = 0;

  init_constants ();
  init_reserved_words ();

  if (verbose_output)
    fprintf (stderr, "translating %s into %s\n",
	     fn->preprocess, fn->translate);
  ret = yyparse ();

  fclose (yyout);
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
  return process (buff);
}

static int
process_assemble (struct filename *fn)
{
  char buff[BUFSIZ];
  sprintf (buff, "%s -c -o %s %s %s",
	   cob_cc, fn->object, cob_cflags, fn->translate);
  return process (buff);
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
      strcat (name, ".");
      strcat (name, COB_MODULE_EXT);
    }
  sprintf (buff, "%s -shared -o %s %s %s",
	   cob_cc, name, fn->object, cob_libs);
  return process (buff);
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
  return process (buff);
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
      if (compile_level == stage_module)
	if (process_module (fn) != 0)
	  goto cleanup;
    }

  /* Link */
  if (compile_level == stage_executable)
    if (process_link (file_list) > 0)
      goto cleanup;

  /* We have successfully completed */
  status = 0;

  /* Remove unnecessary files */
 cleanup:
  if (!save_temps)
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


static void
yyprintf (char *file, int line, char *prefix, const char *fmt, va_list ap)
{
  static struct cobc_label *last_section = NULL;
  static struct cobc_label *last_paragraph = NULL;

  file = file ? file : cobc_source_file;
  line = line ? line : cobc_source_line;

  /* print the paragraph or section name */
  if (current_section != last_section
      || current_paragraph != last_paragraph)
    {
      if (current_paragraph)
	fprintf (stderr, _("%s: In paragraph `%s':\n"),
		 file, current_paragraph->name);
      else
	fprintf (stderr, _("%s: In section `%s':\n"),
		 file, current_section->name);
      last_section = current_section;
      last_paragraph = current_paragraph;
    }

  /* print the error */
  fprintf (stderr, "%s:%d: %s", file, line, prefix);
  vfprintf (stderr, fmt, ap);
  fputs ("\n", stderr);
}

void
yywarn (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "warning: ", fmt, ap);
  va_end (ap);

  warningcount++;
}

void
yyerror (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (0, 0, "", fmt, ap);
  va_end (ap);

  errorcount++;
}

void
pperror (const char *msg)
{
  yyerror (msg);
}

void
yywarn_x (cobc_tree x, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->source_file, x->source_line, "warning: ", fmt, ap);
  va_end (ap);

  warningcount++;
}

void
yyerror_x (cobc_tree x, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  yyprintf (x->source_file, x->source_line, "", fmt, ap);
  va_end (ap);

  errorcount++;
}


void
redefinition_error (cobc_tree x)
{
  struct cobc_word *w = COBC_REFERENCE (x)->word;
  yyerror_x (x, _("redefinition of `%s'"), w->name);
  yyerror_x (w->items->item, _("`%s' previously defined here"), w->name);
}

void
undefined_error (cobc_tree x)
{
  struct cobc_reference *r = COBC_REFERENCE (x);
  if (r->next)
    yyerror_x (x, _("`%s' undefined in `%s'"),
	       r->word->name, r->next->word->name);
  else
    yyerror_x (x, _("`%s' undefined"), r->word->name);
}

void
ambiguous_error (cobc_tree x)
{
  struct cobc_word *w = COBC_REFERENCE (x)->word;
  if (w->error == 0)
    {
      struct cobc_list *l;

      /* display error on the first time */
      yyerror_x (x, _("`%s' ambiguous; need qualification"), w->name);
      w->error = 1;

      /* display all fields with the same name */
      for (l = w->items; l; l = l->next)
	{
	  char buff[BUFSIZ];
	  cobc_tree x = l->item;
	  sprintf (buff, "`%s' ", w->name);
	  switch (COBC_TREE_TAG (x))
	    {
	    case cobc_tag_field:
	      {
		struct cobc_field *p;
		for (p = COBC_FIELD (x)->parent; p; p = p->parent)
		  {
		    strcat (buff, "in `");
		    strcat (buff, p->name);
		    strcat (buff, "' ");
		  }
		break;
	      }
	    case cobc_tag_label:
	      {
		struct cobc_label *l = COBC_LABEL (x);
		if (l->section)
		  {
		    strcat (buff, "in `");
		    strcat (buff, l->section->name);
		    strcat (buff, "' ");
		  }
		break;
	      }
	    default:
	      break;
	    }
	  strcat (buff, _("defined here"));
	  yyerror_x (x, buff);
	}
    }
}
