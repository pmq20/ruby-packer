/* gdbmseq.c - Routines to visit all keys.  Not in sorted order. */

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

/* Find and read the next entry in the hash structure for DBF starting
   at ELEM_LOC of the current bucket and using RETURN_VAL as the place to
   put the data that is found.

   If no next key is found, gdbm_errno is set to GDBM_ITEM_NOT_FOUND
   and RETURN_VAL remains unmodified.

   On error, gdbm_errno is set.
*/

static void
get_next_key (GDBM_FILE dbf, int elem_loc, datum *return_val)
{
  int   found;			/* Have we found the next key. */
  char  *find_data;		/* Data pointer returned by find_key. */

  /* Find the next key. */
  found = FALSE;
  while (!found)
    {
      /* Advance to the next location in the bucket. */
      elem_loc++;
      if (elem_loc == dbf->header->bucket_elems)
	{
	  /* We have finished the current bucket, get the next bucket.  */
	  elem_loc = 0;

	  /* Find the next bucket.  It is possible several entries in
	     the bucket directory point to the same bucket. */
	  while (dbf->bucket_dir < GDBM_DIR_COUNT (dbf)
		 && dbf->cache_entry->ca_adr == dbf->dir[dbf->bucket_dir])
	    dbf->bucket_dir++;

	  /* Check to see if there was a next bucket. */
	  if (dbf->bucket_dir < GDBM_DIR_COUNT (dbf))
	    {
	      if (_gdbm_get_bucket (dbf, dbf->bucket_dir))
		return;
	    }
	  else
	    {
	      /* No next key, just return. */
	      GDBM_SET_ERRNO2 (dbf, GDBM_ITEM_NOT_FOUND, FALSE,
			       GDBM_DEBUG_LOOKUP);
	      return;
	    }
	}
      found = dbf->bucket->h_table[elem_loc].hash_value != -1;
    }
  
  /* Found the next key, read it into return_val. */
  find_data = _gdbm_read_entry (dbf, elem_loc);
  if (!find_data)
    return;
  return_val->dsize = dbf->bucket->h_table[elem_loc].key_size;
  if (return_val->dsize == 0)
    return_val->dptr = (char *) malloc (1);
  else
    return_val->dptr = (char *) malloc (return_val->dsize);
  if (return_val->dptr == NULL)
    {
      return_val->dsize = 0;
      GDBM_SET_ERRNO2 (dbf, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_LOOKUP);
    }
  else
    memcpy (return_val->dptr, find_data, return_val->dsize);
}


/* Start the visit of all keys in the database.  This produces something in
   hash order, not in any sorted order.  */

datum
gdbm_firstkey (GDBM_FILE dbf)
{
  datum return_val;		/* To return the first key. */

  /* Set the default return value for not finding a first entry. */
  return_val.dptr = NULL;

  GDBM_DEBUG (GDBM_DEBUG_READ, "%s: getting first key", dbf->name);
  
  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, return_val);
  
  /* Initialize the gdbm_errno variable. */
  gdbm_set_errno (dbf, GDBM_NO_ERROR, FALSE);

  /* Get the first bucket.  */
  _gdbm_get_bucket (dbf, 0);

  /* Look for first entry. */
  get_next_key (dbf, -1, &return_val);

  if (return_val.dptr) 
    GDBM_DEBUG_DATUM (GDBM_DEBUG_READ, return_val, "%s: found", dbf->name);
  else
    GDBM_DEBUG (GDBM_DEBUG_READ, "%s: key not found", dbf->name);
  
  return return_val;
}


/* Continue visiting all keys.  The next key following KEY is returned. */

datum
gdbm_nextkey (GDBM_FILE dbf, datum key)
{
  datum  return_val;		/* The return value. */
  int    elem_loc;		/* The location in the bucket. */

  /* Set the default return value for no next entry. */
  return_val.dptr = NULL;

  GDBM_DEBUG_DATUM (GDBM_DEBUG_READ, key, "%s: getting next key", dbf->name);
  
  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, return_val);
  
  /* Initialize the gdbm_errno variable. */
  gdbm_set_errno (dbf, GDBM_NO_ERROR, FALSE);

  /* Do we have a valid key? */
  if (key.dptr == NULL)
    {
      GDBM_DEBUG (GDBM_DEBUG_READ, "%s: key not found", dbf->name);
      GDBM_SET_ERRNO2 (dbf, GDBM_ITEM_NOT_FOUND, /* FIXME: special error code perhaps */
		       FALSE,
		       GDBM_DEBUG_LOOKUP);
      return return_val;
    }
  
  /* Find the key.  */
  elem_loc = _gdbm_findkey (dbf, key, NULL, NULL);
  if (elem_loc == -1) return return_val;
  
  /* Find the next key. */  
  get_next_key (dbf, elem_loc, &return_val);

  if (return_val.dptr) 
    GDBM_DEBUG_DATUM (GDBM_DEBUG_READ, return_val, "%s: found", dbf->name);
  else
    GDBM_DEBUG (GDBM_DEBUG_READ, "%s: key not found", dbf->name);

  return return_val;
}
