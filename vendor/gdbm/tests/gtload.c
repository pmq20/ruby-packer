/* This file is part of GDBM test suite.
   Copyright (C) 2011, 2016-2017 Free Software Foundation, Inc.

   GDBM is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GDBM is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.
*/
#include "autoconf.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include "gdbm.h"
#include "progname.h"

const char *progname;
int verbose;

void
err_printer (void *data, char const *fmt, ...)
{
  va_list ap;

  fprintf (stderr, "%s: ", progname);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fprintf (stderr, "\n");
}

struct hook_closure
{
  unsigned skip;
  unsigned hits;
  int disabled;
};

static int
hookfn (char const *file, int line, char const *id, void *data)
{
  struct hook_closure *clos = data;

  if (clos->disabled)
    return 0;
  if (clos->skip)
    {
      --clos->skip;
      return 0;
    }
  if (clos->hits)
    {
      if (--clos->hits == 0)
	clos->disabled = 1;
    }
  
  if (verbose)
    fprintf (stderr, "%s:%d: hit debug hook %s\n", file, line, id);
  return -1;
}

size_t
read_size (char const *arg)
{
  char *p;
  size_t ret;
  
  errno = 0;
  ret = strtoul (arg, &p, 10);
	  
  if (errno)
    {
      fprintf (stderr, "%s: ", progname);
      perror (arg);
      exit (1);
    }

  if (*p)
    {
      fprintf (stderr, "%s: bad number: %s\n", progname, arg);
      exit (1);
    }

  return ret;
}

void
install_hook (char *id)
{
  char *p = strchr (id, ';');
  struct hook_closure *clos = malloc (sizeof (*clos));
  assert (clos != NULL);
  memset (clos, 0, sizeof (*clos));
  if (p)
    {
      char *q;
      
      *p++ = 0;
      for (q = strtok (p, ";"); q; q = strtok (NULL, ";"))
	{
	  if (strncmp (q, "skip=", 5) == 0)
	    clos->skip = strtoul (q + 5, NULL, 10);
	  else if (strncmp (q, "hits=", 5) == 0)
	    clos->hits = strtoul (q + 5, NULL, 10);
	  else
	    {
	      fprintf (stderr, "%s: unknown parameter for hook %s: %s",
		       progname, id, q);
	      exit (1);
	    }
	}
    }
#ifdef GDBM_DEBUG_ENABLE
  _gdbm_debug_hook_install (id, hookfn, clos);
#endif
}

#ifdef GDBM_DEBUG_ENABLE
void
debug_printer (char const *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
}
#endif

int
main (int argc, char **argv)
{
  const char *dbname;
  int line = 0;
  char buf[1024];
  datum key;
  datum data;
  int replace = 0;
  int flags = 0;
  int mode = GDBM_WRCREAT;
  int block_size = 0;
  GDBM_FILE dbf;
  int delim = '\t';
  int data_z = 0;
  size_t mapped_size_max = 0;
  int blksize;
  int verbose = 0;
  int recover = 0;
  gdbm_recovery rcvr;
  int rcvr_flags = 0;
  
  progname = canonical_progname (argv[0]);
#ifdef GDBM_DEBUG_ENABLE
  gdbm_debug_printer = debug_printer;
#endif
  
  while (--argc)
    {
      char *arg = *++argv;

      if (strcmp (arg, "-h") == 0)
	{
	  printf ("usage: %s [-replace] [-clear] [-blocksize=N] [-bsexact] [-verbose] [-null] [-nolock] [-nommap] [-maxmap=N] [-sync] [-delim=CHR] DBFILE\n", progname);
	  exit (0);
	}
      else if (strcmp (arg, "-replace") == 0)
	replace |= GDBM_REPLACE;
      else if (strcmp (arg, "-clear") == 0)
	mode = GDBM_NEWDB;
      else if (strcmp (arg, "-null") == 0)
	data_z = 1;
      else if (strcmp (arg, "-nolock") == 0)
	flags |= GDBM_NOLOCK;
      else if (strcmp (arg, "-nommap") == 0)
	flags |= GDBM_NOMMAP;
      else if (strcmp (arg, "-sync") == 0)
	flags |= GDBM_SYNC;
      else if (strcmp (arg, "-bsexact") == 0)
	flags |= GDBM_BSEXACT;
      else if (strcmp (arg, "-verbose") == 0)
	verbose = 1;
      else if (strncmp (arg, "-blocksize=", 11) == 0)
	block_size = atoi (arg + 11);
      else if (strncmp (arg, "-maxmap=", 8) == 0)
	mapped_size_max = read_size (arg + 8);
      else if (strncmp (arg, "-delim=", 7) == 0)
	delim = arg[7];
#if GDBM_DEBUG_ENABLE
      else if (strncmp (arg, "-hook=", 6) == 0)
	{
	  install_hook (arg + 6);
	  recover = 1;
	}
#endif
      else if (strcmp (arg, "-recover") == 0)
	recover = 1;
      else if (strcmp (arg, "-verbose") == 0)
	{
	  verbose = 1;
	  rcvr.errfun = err_printer;
	  rcvr_flags |= GDBM_RCVR_ERRFUN;
	}
      else if (strcmp (arg, "-backup") == 0)
	rcvr_flags |= GDBM_RCVR_BACKUP;
      else if (strncmp (arg, "-max-failures=", 14) == 0)
	{
	  rcvr.max_failures = read_size (arg + 14);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILURES;
	}
      else if (strncmp (arg, "-max-failed-keys=", 17) == 0)
	{
	  rcvr.max_failed_keys = read_size (arg + 17);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILED_KEYS;
	}
      else if (strncmp (arg, "-max-failed-buckets=", 20) == 0)
	{
	  rcvr.max_failures = read_size (arg + 20);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILED_BUCKETS;
	}
#ifdef GDBM_DEBUG_ENABLE
      else if (strncmp (arg, "-debug=", 7) == 0)
	{
	  char *p;

	  for (p = strtok (arg + 7, ","); p; p = strtok (NULL, ","))
	    {
	      int f = gdbm_debug_token (p);
	      if (!f)
		fprintf (stderr, "%s: unknown flag: %s\n", progname, p);
	      else
		gdbm_debug_flags |= f;
	    }
	}
#endif
      else if (strcmp (arg, "--") == 0)
	{
	  --argc;
	  ++argv;
	  break;
	}
      else if (arg[0] == '-')
	{
	  fprintf (stderr, "%s: unknown option %s\n", progname, arg);
	  exit (1);
	}
      else
	break;
    }

  if (argc != 1)
    {
      fprintf (stderr, "%s: wrong arguments\n", progname);
      exit (1);
    }
  dbname = *argv;
  
  dbf = gdbm_open (dbname, block_size, mode|flags, 00664, NULL);
  if (!dbf)
    {
      fprintf (stderr, "gdbm_open failed: %s\n", gdbm_strerror (gdbm_errno));
      exit (1);
    }

  if (mapped_size_max)
    {
      if (gdbm_setopt (dbf, GDBM_SETMAXMAPSIZE, &mapped_size_max,
		       sizeof (mapped_size_max)))
	{
	  fprintf (stderr, "GDBM_SETMAXMAPSIZE failed: %s\n",
		   gdbm_strerror (gdbm_errno));
	  exit (1);
	}
    }  

  if (verbose)
    {
      if (gdbm_setopt (dbf, GDBM_GETBLOCKSIZE, &blksize, sizeof blksize))
	{
	  fprintf (stderr, "GDBM_GETBLOCKSIZE failed: %s\n",
		   gdbm_strerror (gdbm_errno));
	  exit (1);
	}
      printf ("blocksize=%d\n", blksize);
    }
  
  while (fgets (buf, sizeof buf, stdin))
    {
      size_t i, j;
      size_t len = strlen (buf);

      if (buf[len - 1] != '\n')
	{
	  fprintf (stderr, "%s: %d: line too long\n",
		   progname, line);
	  continue;
	}

      buf[--len] = 0;
      
      line++;

      for (i = j = 0; i < len; i++)
	{
	  if (buf[i] == '\\')
	    i++;
	  else if (buf[i] == delim)
	    break;
	  else
	    buf[j++] = buf[i];
	}

      if (buf[i] != delim)
	{
	  fprintf (stderr, "%s: %d: malformed line\n",
		   progname, line);
	  continue;
	}
      buf[j] = 0;
      
      key.dptr = buf;
      key.dsize = j + data_z;
      data.dptr = buf + i + 1;
      data.dsize = strlen (data.dptr) + data_z;
      if (gdbm_store (dbf, key, data, replace) != 0)
	{
	  fprintf (stderr, "%s: %d: item not inserted\n",
		   progname, line);
	  if (gdbm_needs_recovery (dbf) && recover)
	    {
	      int rc = gdbm_recover (dbf, &rcvr, rcvr_flags);
	      if (rc)
		{
		  int ec = errno;
		  fprintf (stderr, "%s: recovery failed: %s",
			   progname, gdbm_strerror (gdbm_errno));
		  if (gdbm_check_syserr (gdbm_errno))
		    fprintf (stderr, ": %s", strerror (ec));
		  fputc ('\n', stderr);
		}
	      --recover;
	    }
	  else
	    {
	      exit (1);
	    }
	}
    }
  gdbm_close (dbf);
  exit (0);
}
