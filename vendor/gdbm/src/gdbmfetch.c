/* gdbmfetch.c - Find a key and return the associated data.  */

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

/* Look up a given KEY and return the information associated with that KEY.
   The pointer in the structure that is  returned is a pointer to dynamically
   allocated memory block.  */

datum
gdbm_fetch (GDBM_FILE dbf, datum key)
{
  datum  return_val;		/* The return value. */
  int    elem_loc;		/* The location in the bucket. */
  char  *find_data;		/* Returned from find_key. */

  GDBM_DEBUG_DATUM (GDBM_DEBUG_READ, key, "%s: fetching key:", dbf->name);

  /* Set the default return value. */
  return_val.dptr  = NULL;
  return_val.dsize = 0;

  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, return_val);
  
  /* Initialize the gdbm_errno variable. */
  gdbm_set_errno (dbf, GDBM_NO_ERROR, FALSE);

  /* Find the key and return a pointer to the data. */
  elem_loc = _gdbm_findkey (dbf, key, &find_data, NULL);

  /* Copy the data if the key was found.  */
  if (elem_loc >= 0)
    {
      /* This is the item.  Return the associated data. */
      return_val.dsize = dbf->bucket->h_table[elem_loc].data_size;
      if (return_val.dsize == 0)
	return_val.dptr = (char *) malloc (1);
      else
	return_val.dptr = (char *) malloc (return_val.dsize);
      if (return_val.dptr == NULL)
	{
	  GDBM_SET_ERRNO2 (dbf, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_READ);
	  return return_val;
	}
      memcpy (return_val.dptr, find_data, return_val.dsize);
      
      GDBM_DEBUG_DATUM (GDBM_DEBUG_READ, return_val,
			"%s: found", dbf->name);
    }
  else
    GDBM_DEBUG (GDBM_DEBUG_READ, "%s: key not found", dbf->name);
  
  return return_val;
}
