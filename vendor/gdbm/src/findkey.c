/* findkey.c - The routine that finds a key entry in the file. */

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
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.    */

/* Include system configuration before all else. */
#include "autoconf.h"

#include "gdbmdefs.h"


/* Read the data found in bucket entry ELEM_LOC in file DBF and
   return a pointer to it.  Also, cache the read value. */

char *
_gdbm_read_entry (GDBM_FILE dbf, int elem_loc)
{
  int rc;
  int key_size;
  int data_size;
  off_t file_pos;
  data_cache_elem *data_ca;
  
  /* Is it already in the cache? */
  if (dbf->cache_entry->ca_data.elem_loc == elem_loc)
    return dbf->cache_entry->ca_data.dptr;

  /* Set sizes and pointers. */
  key_size = dbf->bucket->h_table[elem_loc].key_size;
  data_size = dbf->bucket->h_table[elem_loc].data_size;
  data_ca = &dbf->cache_entry->ca_data;
  
  /* Set up the cache. */
  if (data_ca->dptr != NULL) free (data_ca->dptr);
  data_ca->key_size = key_size;
  data_ca->data_size = data_size;
  data_ca->elem_loc = elem_loc;
  data_ca->hash_val = dbf->bucket->h_table[elem_loc].hash_value;

  if (GDBM_DEBUG_HOOK ("_gdbm_read_entry:malloc-failure"))
    data_ca->dptr = NULL;
  else if (key_size + data_size == 0)
    data_ca->dptr = (char *) malloc (1);
  else
    data_ca->dptr = (char *) malloc (key_size + data_size);
  if (data_ca->dptr == NULL)
    {
      GDBM_SET_ERRNO2 (dbf, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_LOOKUP);
      _gdbm_fatal (dbf, _("malloc error"));
      return NULL;
    }

  /* Read into the cache. */
  file_pos = GDBM_DEBUG_OVERRIDE ("_gdbm_read_entry:lseek-failure",
	      __lseek (dbf, dbf->bucket->h_table[elem_loc].data_pointer, 
		       SEEK_SET));
  if (file_pos != dbf->bucket->h_table[elem_loc].data_pointer)
    {
      GDBM_SET_ERRNO2 (dbf, GDBM_FILE_SEEK_ERROR, TRUE, GDBM_DEBUG_LOOKUP);
      _gdbm_fatal (dbf, _("lseek error"));
      return NULL;
    }
  
  rc = GDBM_DEBUG_OVERRIDE ("_gdbm_read_entry:read-failure",
	    _gdbm_full_read (dbf, data_ca->dptr, key_size+data_size));
  if (rc)
    {
      GDBM_DEBUG (GDBM_DEBUG_ERR|GDBM_DEBUG_LOOKUP|GDBM_DEBUG_READ,
		  "%s: error reading entry: %s",
		  dbf->name, gdbm_db_strerror (dbf));
      dbf->need_recovery = TRUE;
      _gdbm_fatal (dbf, gdbm_strerror (rc));
      return NULL;
    }
  
  return data_ca->dptr;
}

/* Find the KEY in the file and get ready to read the associated data.  The
   return value is the location in the current hash bucket of the KEY's
   entry.  If it is found, additional data are returned as follows:

   If RET_DPTR is not NULL, a pointer to the actual data is stored in it.
   If RET_HASH_VAL is not NULL, it is assigned the actual hash value.

   If KEY is not found, the value -1 is returned and gdbm_errno is
   set to GDBM_ITEM_NOT_FOUND.  */
int
_gdbm_findkey (GDBM_FILE dbf, datum key, char **ret_dptr, int *ret_hash_val)
{
  int    bucket_hash_val;	/* The hash value from the bucket. */
  int    new_hash_val;          /* Computed hash value for the key */
  char  *file_key;		/* The complete key as stored in the file. */
  int    bucket_dir;            /* Number of the bucket in directory. */
  int    elem_loc;		/* The location in the bucket. */
  int    home_loc;		/* The home location in the bucket. */
  int    key_size;		/* Size of the key on the file.  */

  GDBM_DEBUG_DATUM (GDBM_DEBUG_LOOKUP, key, "%s: fetching key:", dbf->name);
  
  /* Compute hash value and load proper bucket.  */
  _gdbm_hash_key (dbf, key, &new_hash_val, &bucket_dir, &elem_loc);

  GDBM_DEBUG (GDBM_DEBUG_LOOKUP, "%s: location = %#4x:%d:%d", dbf->name,
	      new_hash_val, bucket_dir, elem_loc);

  if (ret_hash_val)
    *ret_hash_val = new_hash_val;
  if (_gdbm_get_bucket (dbf, bucket_dir))
    return -1;
  
  /* Is the element the last one found for this bucket? */
  if (dbf->cache_entry->ca_data.elem_loc != -1 
      && new_hash_val == dbf->cache_entry->ca_data.hash_val
      && dbf->cache_entry->ca_data.key_size == key.dsize
      && dbf->cache_entry->ca_data.dptr != NULL
      && memcmp (dbf->cache_entry->ca_data.dptr, key.dptr, key.dsize) == 0)
    {
      GDBM_DEBUG (GDBM_DEBUG_LOOKUP, "%s: found in cache", dbf->name);
      /* This is it. Return the cache pointer. */
      if (ret_dptr)
	*ret_dptr = dbf->cache_entry->ca_data.dptr + key.dsize;
      return dbf->cache_entry->ca_data.elem_loc;
    }
      
  /* It is not the cached value, search for element in the bucket. */
  home_loc = elem_loc;
  bucket_hash_val = dbf->bucket->h_table[elem_loc].hash_value;
  while (bucket_hash_val != -1)
    {
      key_size = dbf->bucket->h_table[elem_loc].key_size;
      if (bucket_hash_val != new_hash_val
	 || key_size != key.dsize
	 || memcmp (dbf->bucket->h_table[elem_loc].key_start, key.dptr,
			(SMALL < key_size ? SMALL : key_size)) != 0) 
	{
	  /* Current elem_loc is not the item, go to next item. */
	  elem_loc = (elem_loc + 1) % dbf->header->bucket_elems;
	  if (elem_loc == home_loc)
	    break;
	  bucket_hash_val = dbf->bucket->h_table[elem_loc].hash_value;
	}
      else
	{
	  /* This may be the one we want.
	     The only way to tell is to read it. */
	  file_key = _gdbm_read_entry (dbf, elem_loc);
	  if (!file_key)
	    {
	      GDBM_DEBUG (GDBM_DEBUG_LOOKUP, "%s: error reading entry: %s",
			  dbf->name, gdbm_db_strerror (dbf));
	      return -1;
	    }
	  if (memcmp (file_key, key.dptr, key_size) == 0)
	    {
	      /* This is the item. */
	      GDBM_DEBUG (GDBM_DEBUG_LOOKUP, "%s: found", dbf->name);
	      if (ret_dptr)
		*ret_dptr = file_key + key.dsize;
	      return elem_loc;
	    }
	  else
	    {
	      /* Not the item, try the next one.  Return if not found. */
	      elem_loc = (elem_loc + 1) % dbf->header->bucket_elems;
	      if (elem_loc == home_loc)
		break;
	      bucket_hash_val = dbf->bucket->h_table[elem_loc].hash_value;
	    }
	}
      GDBM_DEBUG (GDBM_DEBUG_LOOKUP, "%s: next location = %#4x:%d:%d",
		  dbf->name, bucket_hash_val, bucket_dir, elem_loc);
    }

  /* If we get here, we never found the key. */
  GDBM_SET_ERRNO2 (dbf, GDBM_ITEM_NOT_FOUND, FALSE, GDBM_DEBUG_LOOKUP);
  return -1;

}
