/* gdbmcount.c - get number of items in a gdbm file. */

/* This file is part of GDBM, the GNU data base manager.
   Copyright (C) 1993-1994, 2007, 2011, 2013, 2016-2017 Free Software
   Foundation, Inc.

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

int
gdbm_count (GDBM_FILE dbf, gdbm_count_t *pcount)
{
  int nbuckets = GDBM_DIR_COUNT (dbf);
  gdbm_count_t count = 0;
  int i;
  
  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, -1);
  
  for (i = 0; i < nbuckets; i = _gdbm_next_bucket_dir (dbf, i))
    {
      if (_gdbm_get_bucket (dbf, i))
	return -1;
      count += dbf->bucket->count;
    }
  *pcount = count;
  return 0;
}
