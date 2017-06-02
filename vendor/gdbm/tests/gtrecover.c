/* This file is part of GDBM test suite.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

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
#include "gdbm.h"
#include "progname.h"
#include <assert.h>

const char *progname;

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

int
main (int argc, char **argv)
{
  const char *dbname;
  GDBM_FILE dbf;
  int rc = 0;
  int open_flags = GDBM_WRITER;
  gdbm_recovery rcvr;
  int rcvr_flags = 0;
  char *p;
  
  progname = canonical_progname (argv[0]);
  while (--argc)
    {
      char *arg = *++argv;

      if (strcmp (arg, "-h") == 0)
	{
	  printf ("usage: %s [-nolock] [-nommap] [-verbose] [-backup] [-max-failures=N] [-max-failed-keys=N] [-max-failed-buckets=N] DBFILE\n",
		  progname);
	  exit (0);
	}
      else if (strcmp (arg, "-nolock") == 0)
	open_flags |= GDBM_NOLOCK;
      else if (strcmp (arg, "-nommap") == 0)
	open_flags |= GDBM_NOMMAP;
      else if (strcmp (arg, "-verbose") == 0)
	{
	  rcvr.errfun = err_printer;
	  rcvr_flags |= GDBM_RCVR_ERRFUN;
	}
      else if (strcmp (arg, "-backup") == 0)
	rcvr_flags |= GDBM_RCVR_BACKUP;
      else if (strncmp (arg, "-max-failures=", 14) == 0)
	{
	  rcvr.max_failures = strtoul (arg + 14, &p, 10);
	  assert (*p == 0);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILURES;
	}
      else if (strncmp (arg, "-max-failed-keys=", 17) == 0)
	{
	  rcvr.max_failed_keys = strtoul (arg + 17, &p, 10);
	  assert (*p == 0);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILED_KEYS;
	}
      else if (strncmp (arg, "-max-failed-buckets=", 20) == 0)
	{
	  rcvr.max_failures = strtoul (arg + 20, &p, 10);
	  assert (*p == 0);
	  rcvr_flags |= GDBM_RCVR_MAX_FAILED_BUCKETS;
	}
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

  if (argc < 1)
    {
      fprintf (stderr, "%s: wrong arguments\n", progname);
      exit (1);
    }
  dbname = *argv;
  
  dbf = gdbm_open (dbname, 0, open_flags, 0, NULL);
  if (!dbf)
    {
      fprintf (stderr, "gdbm_open failed: %s\n", gdbm_strerror (gdbm_errno));
      exit (1);
    }

  rc = gdbm_recover (dbf, &rcvr, rcvr_flags);

  gdbm_close (dbf);
  exit (rc);
}
