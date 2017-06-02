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
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.   */

#include "autoconf.h"
#include "gdbmdefs.h"

#define TMPSUF ".XXXXXX"

int
gdbm_copy_meta (GDBM_FILE dst, GDBM_FILE src)
{
  struct stat st;

  if (fstat (src->desc, &st))
    {
      GDBM_SET_ERRNO (src, GDBM_FILE_STAT_ERROR, src->need_recovery);
      return -1;
    }
  if (fchown (dst->desc, st.st_uid, st.st_gid))
    {
      GDBM_SET_ERRNO (dst, GDBM_ERR_FILE_OWNER, dst->need_recovery);
      return -1;
    }
  if (fchmod (dst->desc, st.st_mode & 0777))
    {
      GDBM_SET_ERRNO (dst, GDBM_ERR_FILE_MODE, dst->need_recovery);
      return -1;
    }
  return 0;
}

static char *
backup_name (char const *name)
{
  char *buf;
  size_t len;
  size_t suf_pos;
  size_t suf_len;
  
#define INITIAL_SUFFIX ".~1~"
  
  len = strlen (name + sizeof (INITIAL_SUFFIX));
  buf = malloc (len);
  if (!buf)
    return NULL;
  strcpy (buf, name);
  suf_pos = strlen (buf) + 2; 
  suf_len = 1;
  strcat (buf, INITIAL_SUFFIX);

  while (access (buf, F_OK) == 0)
    {
      size_t i = suf_len;
      while (buf[suf_pos + i - 1] == '9')
	{
	  buf[suf_pos + i - 1] = '0';
	  i--;
	  if (i == 0)
	    {
	      char *p = realloc (buf, ++len);
	      if (!p)
		{
		  SAVE_ERRNO (free (buf));
		  return NULL;
		}
	      memmove (p + suf_pos + 1, p + suf_pos, suf_len + 2);
	      buf = p;
	      suf_len++;
	      i++;
	    }
	}
      ++buf[suf_pos + i - 1];
    }
  return buf;
}  

static int
_gdbm_finish_transfer (GDBM_FILE dbf, GDBM_FILE new_dbf,
		       gdbm_recovery *rcvr, int flags)
{
  int i;
  
  /* Write everything. */
  if (_gdbm_end_update (new_dbf))
    {
      gdbm_close (new_dbf);
      return -1;
    }
  gdbm_sync (new_dbf);

  if (gdbm_copy_meta (new_dbf, dbf))
    {
      gdbm_close (new_dbf);
      return -1;
    }
  
#if HAVE_MMAP
  _gdbm_mapped_unmap (dbf);
#endif

  if (flags & GDBM_RCVR_BACKUP)
    {
      char *bkname = backup_name (dbf->name);
      if (!bkname)
	{
	  SAVE_ERRNO (gdbm_close (new_dbf));
	  GDBM_SET_ERRNO (NULL, GDBM_BACKUP_FAILED, FALSE);
	  return -1;
	}
      if (rename (dbf->name, bkname) != 0)
	{
	  SAVE_ERRNO (gdbm_close (new_dbf); free (bkname));
	  GDBM_SET_ERRNO (NULL, GDBM_BACKUP_FAILED, FALSE);
	  return -1;
	}
      rcvr->backup_name = bkname;
    }
  
  /* Move the new file to old name. */

  if (rename (new_dbf->name, dbf->name) != 0)
    {
      GDBM_SET_ERRNO (NULL, GDBM_REORGANIZE_FAILED, FALSE);
      gdbm_close (new_dbf);
      return -1;
    }

  /* Fix up DBF to have the correct information for the new file. */
  if (dbf->file_locking)
    _gdbm_unlock_file (dbf);
  close (dbf->desc);
  free (dbf->header);
  free (dbf->dir);

  if (dbf->bucket_cache != NULL)
    {
      for (i = 0; i < dbf->cache_size; i++)
	 {
	   free (dbf->bucket_cache[i].ca_bucket);
	   free (dbf->bucket_cache[i].ca_data.dptr);
	 }
      free (dbf->bucket_cache);
   }

   dbf->desc              = new_dbf->desc;
   dbf->header            = new_dbf->header;
   dbf->dir               = new_dbf->dir;
   dbf->bucket            = new_dbf->bucket;
   dbf->bucket_dir        = new_dbf->bucket_dir;
   dbf->last_read         = new_dbf->last_read;
   dbf->bucket_cache      = new_dbf->bucket_cache;
   dbf->cache_size        = new_dbf->cache_size;
   dbf->header_changed    = new_dbf->header_changed;
   dbf->directory_changed = new_dbf->directory_changed;
   dbf->bucket_changed    = new_dbf->bucket_changed;
   dbf->second_changed    = new_dbf->second_changed;

   free (new_dbf);
   
 #if HAVE_MMAP
   /* Re-initialize mapping if required */
   if (dbf->memory_mapping)
     _gdbm_mapped_init (dbf);
 #endif

   /* Make sure the new database is all on disk. */
   __fsync (dbf);

   /* Force the right stuff for a correct bucket cache. */
   dbf->cache_entry    = &dbf->bucket_cache[0];
   return _gdbm_get_bucket (dbf, 0);
 }

int
_gdbm_next_bucket_dir (GDBM_FILE dbf, int bucket_dir)
{
  int dir_count = GDBM_DIR_COUNT (dbf);
  if (bucket_dir < 0 || bucket_dir >= dir_count)
    bucket_dir = dir_count;
  else
    {
      off_t cur = dbf->dir[bucket_dir];
      while (++bucket_dir < dir_count && cur == dbf->dir[bucket_dir])
	;
    }
  return bucket_dir;
}

static int
check_db (GDBM_FILE dbf)
{
  int bucket_dir, i;
  int nbuckets = GDBM_DIR_COUNT (dbf);

  for (bucket_dir = 0; bucket_dir < nbuckets;
       bucket_dir = _gdbm_next_bucket_dir (dbf, bucket_dir))
    {      
      if (_gdbm_get_bucket (dbf, bucket_dir))
	return 1;
      else
	{
	  if (dbf->bucket->count < 0
	      || dbf->bucket->count > dbf->header->bucket_elems)
	    return 1;
	  for (i = 0; i < dbf->header->bucket_elems; i++)
	    {
	      char *dptr;
	      datum key;
	      int hashval, bucket, off;

	      if (dbf->bucket->h_table[i].hash_value == -1)
		continue;
	      dptr = _gdbm_read_entry (dbf, i);
	      if (!dptr)
		return 1;
	      
	      key.dptr   = dptr;
	      key.dsize  = dbf->bucket->h_table[i].key_size;

	      if (memcmp (dbf->bucket->h_table[i].key_start, key.dptr,
			  (SMALL < key.dsize ? SMALL : key.dsize)))
		return 1;
	      
	      _gdbm_hash_key (dbf, key, &hashval, &bucket, &off);
	      if (bucket >= nbuckets)
		return 1;
	      if (hashval != dbf->bucket->h_table[i].hash_value)
		return 1;
	      if (dbf->dir[bucket] != dbf->dir[bucket_dir])
		return 1;
	    }
	}
    }
  return 0;
}

static int
run_recovery (GDBM_FILE dbf, GDBM_FILE new_dbf, gdbm_recovery *rcvr, int flags)
{
  int bucket_dir, i;
  int nbuckets = GDBM_DIR_COUNT (dbf);

  for (bucket_dir = 0; bucket_dir < nbuckets;
       bucket_dir = _gdbm_next_bucket_dir (dbf, bucket_dir))
    {
      
      if (_gdbm_get_bucket (dbf, bucket_dir))
	{
	  if (flags & GDBM_RCVR_ERRFUN)
	    rcvr->errfun (rcvr->data, _("can't read bucket #%d: %s"),
			  bucket_dir,
			  gdbm_db_strerror (dbf));
	  rcvr->failed_buckets++;
	  if ((flags & GDBM_RCVR_MAX_FAILED_BUCKETS)
	      && rcvr->failed_buckets == rcvr->max_failed_buckets)
	    return -1;
	  if ((flags & GDBM_RCVR_MAX_FAILURES)
	      && (rcvr->failed_buckets + rcvr->failed_keys) == rcvr->max_failures)
	    return -1;
	}
      else
	{
	  rcvr->recovered_buckets++;
	  for (i = 0; i < dbf->header->bucket_elems; i++)
	    {
	      char *dptr;
	      datum key, data;
	    
	      if (dbf->bucket->h_table[i].hash_value == -1)
		continue;
	      dptr = _gdbm_read_entry (dbf, i);
	      if (dptr)
		rcvr->recovered_keys++;
	      else
		{
		  if (flags & GDBM_RCVR_ERRFUN)
		    rcvr->errfun (rcvr->data,
				  _("can't read key pair %d:%d (%lu:%d): %s"),
				  bucket_dir, i,
				  (unsigned long) dbf->bucket->h_table[i].data_pointer,
				  dbf->bucket->h_table[i].key_size
				    + dbf->bucket->h_table[i].data_size,
				  gdbm_db_strerror (dbf));
		  rcvr->failed_keys++;
		  if ((flags & GDBM_RCVR_MAX_FAILED_KEYS)
		      && rcvr->failed_keys == rcvr->max_failed_keys)
		    return -1;
		  if ((flags & GDBM_RCVR_MAX_FAILURES)
		      && (rcvr->failed_buckets + rcvr->failed_keys) == rcvr->max_failures)
		    return -1;
		  continue;
		}

	      key.dptr   = dptr;
	      key.dsize  = dbf->bucket->h_table[i].key_size;

	      data.dptr  = dptr + key.dsize;
	      data.dsize = dbf->bucket->h_table[i].data_size;
	    
	      if (gdbm_store (new_dbf, key, data, GDBM_INSERT) != 0)
		{
		  if (flags & GDBM_RCVR_ERRFUN)
		    rcvr->errfun (rcvr->data,
				  _("fatal: can't store element %d:%d (%lu:%d): %s"),
				  bucket_dir, i,
				  (unsigned long) dbf->bucket->h_table[i].data_pointer,
				  dbf->bucket->h_table[i].key_size
				    + dbf->bucket->h_table[i].data_size,
				  gdbm_db_strerror (new_dbf));
		  return -1;
		}	
	    }
	}
    }
  
  return 0;
}

int
gdbm_recover (GDBM_FILE dbf, gdbm_recovery *rcvr, int flags)
{ 
  GDBM_FILE new_dbf;	     /* The new file. */
  char *new_name;	     /* A temporary name. */
  size_t len;
  int fd;
  int rc;
  gdbm_recovery rs;
  
  /* Readers can not reorganize! */
  if (dbf->read_write == GDBM_READER)
    {
      GDBM_SET_ERRNO (dbf, GDBM_READER_CANT_REORGANIZE, dbf->need_recovery);
      return -1;
    }

  /* Initialize gdbm_recovery structure */
  if (!rcvr)
    {
      rcvr  = &rs;
      flags = 0;
    }
  rcvr->recovered_keys = 0;
  rcvr->recovered_buckets = 0;
  rcvr->failed_keys = 0;
  rcvr->failed_buckets = 0;
  rcvr->backup_name = NULL;

  rc = 0;
  if ((flags & GDBM_RCVR_FORCE) || check_db (dbf))
    {
      len = strlen (dbf->name);
      new_name = malloc (len + sizeof (TMPSUF));
      if (!new_name)
	{
	  GDBM_SET_ERRNO (NULL, GDBM_MALLOC_ERROR, FALSE);
	  return -1;
	}
      strcat (strcpy (new_name, dbf->name), TMPSUF);
  
      fd = mkstemp (new_name);
      if (fd == -1)
	{
	  GDBM_SET_ERRNO (NULL, GDBM_FILE_OPEN_ERROR, FALSE);
	  free (new_name);
	  return -1;
	}
  
      new_dbf = gdbm_fd_open (fd, new_name, dbf->header->block_size,
			      GDBM_WRCREAT
			      | (dbf->cloexec ? GDBM_CLOEXEC : 0)
			      | GDBM_CLOERROR, dbf->fatal_err);
  
      SAVE_ERRNO (free (new_name));
  
      if (new_dbf == NULL)
	{
	  GDBM_SET_ERRNO (NULL, GDBM_REORGANIZE_FAILED, FALSE);
	  return -1;
	}

      rc = run_recovery (dbf, new_dbf, rcvr, flags);
  
      if (rc == 0)
	rc = _gdbm_finish_transfer (dbf, new_dbf, rcvr, flags);
      else
	gdbm_close (new_dbf);
    }

  if (rc == 0)
    {
      gdbm_clear_error (dbf);
      dbf->need_recovery = FALSE;
    }

  return rc;
}
