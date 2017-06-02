/* gdbmreorg.c - Reorganize the database file. */

/* This file is part of GDBM, the GNU data base manager.
   Copyright (C) 1990-1991, 1993, 2007, 2011, 2013, 2016-2017 Free
   Software Foundation, Inc.

   GDBM is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GDBM is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.   */

/* Include system configuration before all else. */
#include "autoconf.h"
#include "gdbmdefs.h"

/* Reorganize the database.  This requires creating a new file and inserting
   all the elements in the old file DBF into the new file.  The new file
   is then renamed to the same name as the old file and DBF is updated to
   contain all the correct information about the new file.  If an error
   is detected, the return value is negative.  The value zero is returned
   after a successful reorganization. */

int
gdbm_reorganize (GDBM_FILE dbf)
{
  gdbm_recovery rcvr;
  
  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, -1);

  rcvr.max_failures = 0;
  return gdbm_recover (dbf, &rcvr, GDBM_RCVR_MAX_FAILURES|GDBM_RCVR_FORCE);
}
