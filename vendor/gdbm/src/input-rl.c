/* This file is part of GDBM, the GNU data base manager.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

   GDBM is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GDBM is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.    */

#include "gdbmtool.h"
#include <readline/readline.h>
#include <readline/history.h>

static char *pre_input_line;

static int
pre_input (void)
{
  if (pre_input_line)
    {
      rl_insert_text (pre_input_line);
      free (pre_input_line);
      pre_input_line = NULL;
      rl_redisplay ();
    }
  return 0;
}

static int
retrieve_history (char *str)
{
  char *out;
  int rc;

  rc = history_expand (str, &out);
  switch (rc)
    {
    case -1:
      yyerror (out);
      free (out);
      return 1;

    case 0:
      break;

    case 1:
      pre_input_line = out;
      return 1;

    case 2:
      printf ("%s\n", out);
      free (out);
      return 1;
    }
  return 0;
}

ssize_t
input_read (FILE *fp, char *buf, size_t size)
{
  static char *input_line;
  static size_t input_length;
  static size_t input_off;
#define input_ptr() (input_line + input_off)
#define input_size() (input_length - input_off)
  
  if (interactive)
    {
      size_t len = input_size ();
      if (!len)
	{
	  if (input_line)
	    {
	    newline:
	      free (input_line);
	      input_line = NULL;
	      buf[0] = '\n';
	      return 1;
	    }
	  else
	    {
	      char *prompt;
	    again:
	      prompt = make_prompt ();
	      input_line = readline (prompt);
	      free (prompt);
	      if (!input_line)
		return 0;
	      input_length = strlen (input_line);
	      input_off = 0;
	      if (input_length)
		{
		  if (retrieve_history (input_line))
		    {
		      free (input_line);
		      goto again;
		    }
		}
	      else
		goto newline;
	      len = input_size ();
	      add_history (input_line);
	    }
	}

      if (len > size)
	len = size;
      memcpy (buf, input_ptr (), len);
      input_off += len;

      return len;
    }
  return fread (buf, 1, size, fp);
} 

struct history_param
{
  int from;
  int count;
};
  
int
input_history_begin (struct handler_param *param, size_t *exp_count)
{
  struct history_param *p;
  int from = 0, count = history_length;

  switch (param->argc)
    {
    case 1:
      if (getnum (&count, param->argv[0]->v.string, NULL))
	return 1;
      if (count > history_length)
	count = history_length;
      else
	from = history_length - count;
      break;

    case 2:
      if (getnum (&from, param->argv[0]->v.string, NULL))
	return 1;
      if (from)
	--from;
      if (getnum (&count, param->argv[1]->v.string, NULL))
	return 1;

      if (count > history_length)
	count = history_length;
    }
  p = emalloc (sizeof *p);
  p->from = from;
  p->count = count;
  param->data = p;
  if (exp_count)
    *exp_count = count;
  return 0;
}

void
input_history_handler (struct handler_param *param)
{
  struct history_param *p = param->data;
  int i;
  HIST_ENTRY **hlist;
  FILE *fp = param->fp;
  
  hlist = history_list ();
  for (i = 0; i < p->count; i++)
    fprintf (fp, "%4d) %s\n", p->from + i + 1, hlist[p->from + i]->line);
}

#define HISTFILE_PREFIX "~/."
#define HISTFILE_SUFFIX "_history"

static char *
get_history_file_name ()
{
  static char *filename = NULL;

  if (!filename)
    {
      char *hname;

      hname = emalloc (sizeof HISTFILE_PREFIX + strlen (rl_readline_name) +
		       sizeof HISTFILE_SUFFIX - 1);
      strcpy (hname, HISTFILE_PREFIX);
      strcat (hname, rl_readline_name);
      strcat (hname, HISTFILE_SUFFIX);
      filename = tildexpand (hname);
      free (hname);
    }
  return filename;
}
 
static char **
shell_completion (const char *text, int start, int end)
{
  char **matches;

  matches = (char **) NULL;

  /* If this word is at the start of the line, then it is a command
     to complete.  Otherwise it is the name of a file in the current
     directory. */
  if (start == 0)
    matches = rl_completion_matches (text, command_generator);

  return (matches);
}

void
input_init (void)
{
  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = (char *) progname;
  rl_attempted_completion_function = shell_completion;
  rl_pre_input_hook = pre_input;
  if (interactive)
    read_history (get_history_file_name ());
}

void
input_done (void)
{
  if (interactive)
    write_history (get_history_file_name ());
}

