/* bucket.c - The routines for playing with hash buckets. */

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

#include "autoconf.h"
#include "gdbmdefs.h"
#include <limits.h>

#define GDBM_MAX_DIR_SIZE INT_MAX
#define GDBM_MAX_DIR_HALF (GDBM_MAX_DIR_SIZE / 2)

/* Initializing a new hash buckets sets all bucket entries to -1 hash value. */
void
_gdbm_new_bucket (GDBM_FILE dbf, hash_bucket *bucket, int bits)
{
  int index;
  
  /* Initialize the avail block. */
  bucket->av_count = 0;

  /* Set the information fields first. */
  bucket->bucket_bits = bits;
  bucket->count = 0;
  
  /* Initialize all bucket elements. */
  for (index = 0; index < dbf->header->bucket_elems; index++)
    bucket->h_table[index].hash_value = -1;
}

/* Find a bucket for DBF that is pointed to by the bucket directory from
   location DIR_INDEX.   The bucket cache is first checked to see if it
   is already in memory.  If not, a bucket may be tossed to read the new
   bucket.  In any case, the requested bucket becomes the "current" bucket
   and dbf->bucket points to the correct bucket. */

int
_gdbm_get_bucket (GDBM_FILE dbf, int dir_index)
{
  int rc;
  off_t bucket_adr;	/* The address of the correct hash bucket.  */
  off_t	file_pos;	/* The return address for lseek. */
  int   index;		/* Loop index. */

  /* Initial set up. */
  dbf->bucket_dir = dir_index;
  bucket_adr = dbf->dir[dir_index];
  
  if (dbf->bucket_cache == NULL)
    {
      if (_gdbm_init_cache (dbf, DEFAULT_CACHESIZE) == -1)
	{
	  _gdbm_fatal (dbf, _("couldn't init cache"));
	  return -1;
	}
    }

  /* Is that one is not already current, we must find it. */
  if (dbf->cache_entry->ca_adr != bucket_adr)
    {
      /* Look in the cache. */
      for (index = 0; index < dbf->cache_size; index++)
        {
	  if (dbf->bucket_cache[index].ca_adr == bucket_adr)
	    {
	      dbf->bucket = dbf->bucket_cache[index].ca_bucket;
	      dbf->cache_entry = &dbf->bucket_cache[index];
	      return 0;
	    }
        }

      /* It is not in the cache, read it from the disk. */
      dbf->last_read = (dbf->last_read + 1) % dbf->cache_size;
      if (dbf->bucket_cache[dbf->last_read].ca_changed)
	{
	  if (_gdbm_write_bucket (dbf, &dbf->bucket_cache[dbf->last_read]))
	    return -1;
	}
      dbf->bucket_cache[dbf->last_read].ca_adr = bucket_adr;
      dbf->bucket = dbf->bucket_cache[dbf->last_read].ca_bucket;
      dbf->cache_entry = &dbf->bucket_cache[dbf->last_read];
      dbf->cache_entry->ca_data.elem_loc = -1;
      dbf->cache_entry->ca_changed = FALSE;

      /* Read the bucket. */
      file_pos = GDBM_DEBUG_OVERRIDE ("_gdbm_get_bucket:seek-failure",
				      __lseek (dbf, bucket_adr, SEEK_SET));
      if (file_pos != bucket_adr)
	{
	  _gdbm_fatal (dbf, _("lseek error"));
	  GDBM_SET_ERRNO (dbf, GDBM_FILE_SEEK_ERROR, TRUE);
	  return -1;
	}
      
      rc = GDBM_DEBUG_OVERRIDE ("_gdbm_get_bucket:read-failure",
		_gdbm_full_read (dbf, dbf->bucket, dbf->header->bucket_size));
      if (rc)
	{
	  GDBM_DEBUG (GDBM_DEBUG_ERR,
		      "%s: error reading bucket: %s",
		      dbf->name, gdbm_db_strerror (dbf));
	  dbf->need_recovery = TRUE;
	  _gdbm_fatal (dbf, gdbm_db_strerror (dbf));
	  return -1;
	}
    }

  return 0;
}

int
_gdbm_read_bucket_at (GDBM_FILE dbf, off_t off, hash_bucket *bucket,
		      size_t size)
{
  off_t file_pos;
  int i;

  if (dbf->cache_entry && dbf->cache_entry->ca_adr == off)
    {
      memcpy (bucket, dbf->bucket, size);
      return 0;
    }

  /* Look in the cache. */
  for (i = 0; i < dbf->cache_size; i++)
    {
      if (dbf->bucket_cache[i].ca_adr == off)
	{
	  memcpy (bucket, dbf->bucket_cache[i].ca_bucket, size);
	  return 0;
	}
    }

  /* Read the bucket. */
  file_pos = __lseek (dbf, off, SEEK_SET);
  if (file_pos != off)
    {
      GDBM_SET_ERRNO (dbf, GDBM_FILE_SEEK_ERROR, TRUE);
      return -1;
    }
  if (_gdbm_full_read (dbf, bucket, size))
    {
      GDBM_DEBUG (GDBM_DEBUG_ERR,
		  "%s: error reading bucket: %s",
		  dbf->name, gdbm_db_strerror (dbf));
      return -1;
    }
  return 0;
}

/* Split the current bucket.  This includes moving all items in the bucket to
   a new bucket.  This doesn't require any disk reads because all hash values
   are stored in the buckets.  Splitting the current bucket may require
   doubling the size of the hash directory.  */
int
_gdbm_split_bucket (GDBM_FILE dbf, int next_insert)
{
  hash_bucket *bucket[2]; 	/* Pointers to the new buckets. */

  int          new_bits;	/* The number of bits for the new buckets. */
  int	       cache_0;		/* Location in the cache for the buckets. */
  int	       cache_1;
  off_t        adr_0;		/* File address of the new bucket 0. */
  off_t        adr_1;		/* File address of the new bucket 1. */
  avail_elem   old_bucket;	/* Avail Struct for the old bucket. */

  off_t        dir_start0;	/* Used in updating the directory. */
  off_t        dir_start1;
  off_t        dir_end;

  off_t       *new_dir;		/* Pointer to the new directory. */
  off_t        dir_adr; 	/* Address of the new directory. */
  int          dir_size;	/* Size of the new directory. */
  off_t        old_adr[GDBM_HASH_BITS];  /* Address of the old directories. */
  int          old_size[GDBM_HASH_BITS]; /* Size of the old directories. */
  int	       old_count;	/* Number of old directories. */

  int          index;		/* Used in array indexing. */
  int          index1;		/* Used in array indexing. */
  int          elem_loc;	/* Location in new bucket to put element. */
  bucket_element *old_el;	/* Pointer into the old bucket. */
  int	       select;		/* Used to index bucket during movement. */

  /* No directories are yet old. */
  old_count = 0;

  if (dbf->bucket_cache == NULL)
    {
      if (_gdbm_init_cache (dbf, DEFAULT_CACHESIZE) == -1)
	{
	  _gdbm_fatal (dbf, _("couldn't init cache"));
	  return -1;
	}
    }

  while (dbf->bucket->count == dbf->header->bucket_elems)
    {
      /* Initialize the "new" buckets in the cache. */
      do
	{
	  dbf->last_read = (dbf->last_read + 1) % dbf->cache_size;
	  cache_0 = dbf->last_read;
	}      
      while (dbf->bucket_cache[cache_0].ca_bucket == dbf->bucket);
      bucket[0] = dbf->bucket_cache[cache_0].ca_bucket;
      if (dbf->bucket_cache[cache_0].ca_changed)
	{
	  if (_gdbm_write_bucket (dbf, &dbf->bucket_cache[cache_0]))
	    return -1;
	}
      do
	{
	  dbf->last_read = (dbf->last_read + 1) % dbf->cache_size;
	  cache_1 = dbf->last_read;
	}      
      while (dbf->bucket_cache[cache_1].ca_bucket == dbf->bucket);
      bucket[1] = dbf->bucket_cache[cache_1].ca_bucket;
      if (dbf->bucket_cache[cache_1].ca_changed)
	{
	  if (_gdbm_write_bucket (dbf, &dbf->bucket_cache[cache_1]))
	    return -1;
	}
      new_bits = dbf->bucket->bucket_bits + 1;
      _gdbm_new_bucket (dbf, bucket[0], new_bits);
      _gdbm_new_bucket (dbf, bucket[1], new_bits);
      adr_0 = _gdbm_alloc (dbf, dbf->header->bucket_size);
      if (adr_0 == 0)
	return -1;
      dbf->bucket_cache[cache_0].ca_adr = adr_0;
      adr_1 = _gdbm_alloc (dbf, dbf->header->bucket_size);
      if (adr_1 == 0)
	return -1;
      dbf->bucket_cache[cache_1].ca_adr = adr_1;

      /* Double the directory size if necessary. */
      if (dbf->header->dir_bits == dbf->bucket->bucket_bits)
	{
	  if (dbf->header->dir_size >= GDBM_MAX_DIR_HALF)
	    {
	      GDBM_SET_ERRNO (dbf, GDBM_DIR_OVERFLOW, TRUE);
	      _gdbm_fatal (dbf, _("directory overflow"));
	      return -1;
	    }
	  dir_size = dbf->header->dir_size * 2;
	  dir_adr  = _gdbm_alloc (dbf, dir_size);
	  if (dir_adr == 0)
	    return -1;
	  new_dir = GDBM_DEBUG_ALLOC ("_gdbm_split_bucket:malloc-failure",
				      malloc (dir_size));
	  if (new_dir == NULL)
	    {
	      GDBM_SET_ERRNO (dbf, GDBM_MALLOC_ERROR, TRUE);
	      _gdbm_fatal (dbf, _("malloc error"));
	      return -1;
	    }

	  for (index = 0; index < GDBM_DIR_COUNT (dbf); index++)
	    {
	      new_dir[2*index]   = dbf->dir[index];
	      new_dir[2*index+1] = dbf->dir[index];
	    }
	  
	  /* Update header. */
	  old_adr[old_count] = dbf->header->dir;
	  dbf->header->dir = dir_adr;
	  old_size[old_count] = dbf->header->dir_size;
	  dbf->header->dir_size = dir_size;
	  dbf->header->dir_bits = new_bits;
	  old_count++;
	  
	  /* Now update dbf.  */
	  dbf->header_changed = TRUE;
	  dbf->bucket_dir *= 2;
	  free (dbf->dir);
	  dbf->dir = new_dir;
	}

      /* Copy all elements in dbf->bucket into the new buckets. */
      for (index = 0; index < dbf->header->bucket_elems; index++)
	{
	  old_el = &dbf->bucket->h_table[index];
	  select = (old_el->hash_value >> (GDBM_HASH_BITS - new_bits)) & 1;
	  elem_loc = old_el->hash_value % dbf->header->bucket_elems;
	  while (bucket[select]->h_table[elem_loc].hash_value != -1)
	    elem_loc = (elem_loc + 1) % dbf->header->bucket_elems;
	  bucket[select]->h_table[elem_loc] = *old_el;
	  bucket[select]->count++;
	}
      
      /* Allocate avail space for the bucket[1]. */
      bucket[1]->bucket_avail[0].av_adr
	= _gdbm_alloc (dbf, dbf->header->block_size);
      if (bucket[1]->bucket_avail[0].av_adr == 0)
	return -1;
      bucket[1]->bucket_avail[0].av_size = dbf->header->block_size;
      bucket[1]->av_count = 1;
      
      /* Copy the avail elements in dbf->bucket to bucket[0]. */
      bucket[0]->av_count = dbf->bucket->av_count;
      index = 0;
      index1 = 0;
      if (bucket[0]->av_count == BUCKET_AVAIL)
	{
	  /* The avail is full, move the first one to bucket[1]. */
	  _gdbm_put_av_elem (dbf->bucket->bucket_avail[0],
			     bucket[1]->bucket_avail,
			     &bucket[1]->av_count, FALSE);
	  index = 1;
	  bucket[0]->av_count--;
	}
      for (; index < dbf->bucket->av_count; index++)
	{
	  bucket[0]->bucket_avail[index1++] = dbf->bucket->bucket_avail[index];
	}
      
      /* Update the directory.  We have new file addresses for both buckets. */
      dir_start1 = (dbf->bucket_dir >> (dbf->header->dir_bits - new_bits)) | 1;
      dir_end = (dir_start1 + 1) << (dbf->header->dir_bits - new_bits);
      dir_start1 = dir_start1 << (dbf->header->dir_bits - new_bits);
      dir_start0 = dir_start1 - (dir_end - dir_start1);
      for (index = dir_start0; index < dir_start1; index++)
	dbf->dir[index] = adr_0;
      for (index = dir_start1; index < dir_end; index++)
	dbf->dir[index] = adr_1;
      
      
      /* Set changed flags. */
      dbf->bucket_cache[cache_0].ca_changed = TRUE;
      dbf->bucket_cache[cache_1].ca_changed = TRUE;
      dbf->bucket_changed = TRUE;
      dbf->directory_changed = TRUE;
      dbf->second_changed = TRUE;
      
      /* Update the cache! */
      dbf->bucket_dir = _gdbm_bucket_dir (dbf, next_insert);
      
      /* Invalidate old cache entry. */
      old_bucket.av_adr  = dbf->cache_entry->ca_adr;
      old_bucket.av_size = dbf->header->bucket_size;
      dbf->cache_entry->ca_adr = 0;
      dbf->cache_entry->ca_changed = FALSE;
      
      /* Set dbf->bucket to the proper bucket. */
      if (dbf->dir[dbf->bucket_dir] == adr_0)
	{
	  dbf->bucket = bucket[0];
	  dbf->cache_entry = &dbf->bucket_cache[cache_0];
	  _gdbm_put_av_elem (old_bucket,
			     bucket[1]->bucket_avail,
			     &bucket[1]->av_count, FALSE);
	}
      else
	{
	  dbf->bucket = bucket[1];
	  dbf->cache_entry = &dbf->bucket_cache[cache_1];
	  _gdbm_put_av_elem (old_bucket,
			     bucket[0]->bucket_avail,
			     &bucket[0]->av_count, FALSE);
	}
      
    }

  /* Get rid of old directories. */
  for (index = 0; index < old_count; index++)
    _gdbm_free (dbf, old_adr[index], old_size[index]);

  return 0;
}


/* The only place where a bucket is written.  CA_ENTRY is the
   cache entry containing the bucket to be written. */

int
_gdbm_write_bucket (GDBM_FILE dbf, cache_elem *ca_entry)
{
  int rc;
  off_t file_pos;	/* The return value for lseek. */

  file_pos = GDBM_DEBUG_OVERRIDE ("_gdbm_write_bucket:seek-failure",
				  __lseek (dbf, ca_entry->ca_adr, SEEK_SET));
  if (file_pos != ca_entry->ca_adr)
    {
      GDBM_SET_ERRNO (dbf, GDBM_FILE_SEEK_ERROR, TRUE);
      _gdbm_fatal (dbf, _("lseek error"));
      return -1;
    }
  rc = GDBM_DEBUG_OVERRIDE ("_gdbm_write_bucket:write-failure",
        _gdbm_full_write (dbf, ca_entry->ca_bucket, dbf->header->bucket_size));
  if (rc)
    {
      GDBM_DEBUG (GDBM_DEBUG_STORE|GDBM_DEBUG_ERR,
		  "%s: error writing bucket: %s",
		  dbf->name, gdbm_db_strerror (dbf));	  
      _gdbm_fatal (dbf, gdbm_strerror (rc));
      return -1;
    }

  ca_entry->ca_changed = FALSE;
  ca_entry->ca_data.hash_val = -1;
  ca_entry->ca_data.elem_loc = -1;
  return 0;
}
