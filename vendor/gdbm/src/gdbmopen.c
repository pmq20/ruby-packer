/* gdbmopen.c - Open the dbm file and initialize data structures for use. */

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

/* Determine our native magic number and bail if we can't. */
#if SIZEOF_OFF_T == 4
# define GDBM_MAGIC	GDBM_MAGIC32
#elif SIZEOF_OFF_T == 8
# define GDBM_MAGIC	GDBM_MAGIC64
#else
# error "Unsupported off_t size, contact GDBM maintainer.  What crazy system is this?!?"
#endif

static void
compute_directory_size (GDBM_FILE dbf, blksize_t block_size,
			int *ret_dir_size, int *ret_dir_bits)
{
  /* Create the initial hash table directory.  */
  int dir_size = 8 * sizeof (off_t);
  int dir_bits = 3;

  while (dir_size < block_size && dir_bits < GDBM_HASH_BITS - 3)
    {
      dir_size <<= 1;
      dir_bits++;
    }

  *ret_dir_size = dir_size;
  *ret_dir_bits = dir_bits;
}

GDBM_FILE 
gdbm_fd_open (int fd, const char *file_name, int block_size,
	      int flags, void (*fatal_func) (const char *))
{
  GDBM_FILE dbf;		/* The record to return. */
  struct stat file_stat;	/* Space for the stat information. */
  off_t       file_pos;		/* Used with seeks. */
  int 	      index;		/* Used as a loop index. */
  
  /* Initialize the gdbm_errno variable. */
  gdbm_set_errno (NULL, GDBM_NO_ERROR, FALSE);

  /* Get the status of the file. */
  if (fstat (fd, &file_stat))
    {
      if (flags & GDBM_CLOERROR)
	SAVE_ERRNO (close (fd));
      GDBM_SET_ERRNO2 (NULL, GDBM_FILE_STAT_ERROR, FALSE, GDBM_DEBUG_OPEN);
      return NULL;
    }
  
  /* Allocate new info structure. */
  dbf = (GDBM_FILE) malloc (sizeof (*dbf));
  if (dbf == NULL)
    {
      if (flags & GDBM_CLOERROR)
	SAVE_ERRNO (close (fd));
      GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
      return NULL;
    }

  dbf->desc = fd;
  
  /* Initialize some fields for known values.  This is done so gdbm_close
     will work if called before allocating some structures. */
  dbf->dir  = NULL;
  dbf->bucket = NULL;
  dbf->header = NULL;
  dbf->bucket_cache = NULL;
  dbf->cache_size = 0;

  dbf->memory_mapping = FALSE;
  dbf->mapped_size_max = SIZE_T_MAX;
  dbf->mapped_region = NULL;
  dbf->mapped_size = 0;
  dbf->mapped_pos = 0;
  dbf->mapped_off = 0;
  
  /* Save name of file. */
  dbf->name = strdup (file_name);
  if (dbf->name == NULL)
    {
      if (flags & GDBM_CLOERROR)
	close (fd);
      free (dbf);
      GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
      return NULL;
    }

  /* Initialize the fatal error routine. */
  dbf->fatal_err = fatal_func;

  dbf->fast_write = TRUE;	/* Default to setting fast_write. */
  dbf->file_locking = TRUE;	/* Default to doing file locking. */
  dbf->central_free = FALSE;	/* Default to not using central_free. */
  dbf->coalesce_blocks = FALSE; /* Default to not coalesce blocks. */

  dbf->need_recovery = FALSE;
  dbf->last_error = GDBM_NO_ERROR;
  dbf->last_syserror = 0;
  dbf->last_errstr = NULL;
  
  /* GDBM_FAST used to determine whether or not we set fast_write. */
  if (flags & GDBM_SYNC)
    {
      /* If GDBM_SYNC has been requested, don't do fast_write. */
      dbf->fast_write = FALSE;
    }
  if (flags & GDBM_NOLOCK)
    {
      dbf->file_locking = FALSE;
    }

  dbf->cloexec = !!(flags & GDBM_CLOEXEC);
  
  /* Zero-length file can't be a reader... */
  if (((flags & GDBM_OPENMASK) == GDBM_READER) && (file_stat.st_size == 0))
    {
      if (flags & GDBM_CLOERROR)
	close (dbf->desc);
      free (dbf->name);
      free (dbf);
      GDBM_SET_ERRNO2 (NULL, GDBM_EMPTY_DATABASE, FALSE, GDBM_DEBUG_OPEN);
      return NULL;
    }

  /* Record the kind of user. */
  dbf->read_write = (flags & GDBM_OPENMASK);

  /* Lock the file in the appropriate way. */
  if (dbf->file_locking)
    {
      if (_gdbm_lock_file (dbf) == -1)
	{
	  if (flags & GDBM_CLOERROR)
	    close (dbf->desc);
	  free (dbf->name);
	  free (dbf);
          GDBM_SET_ERRNO2 (NULL,
			   (flags & GDBM_OPENMASK) == GDBM_READER
			     ? GDBM_CANT_BE_READER : GDBM_CANT_BE_WRITER,
			   FALSE,
			   GDBM_DEBUG_OPEN);
	  return NULL;
	}
    }

  /* If we do have a write lock and it was a GDBM_NEWDB, it is 
     now time to truncate the file. */
  if ((flags & GDBM_OPENMASK) == GDBM_NEWDB && file_stat.st_size != 0)
    {
      TRUNCATE (dbf);
      fstat (dbf->desc, &file_stat);
    }

  /* Decide if this is a new file or an old file. */
  if (file_stat.st_size == 0)
    {
      /* This is a new file.  Create an empty database.  */
      int dir_size, dir_bits;
      
      /* Start with the blocksize. */
      if (block_size < GDBM_MIN_BLOCK_SIZE)
	{
	  block_size = STATBLKSIZE (file_stat);
	  flags &= ~GDBM_BSEXACT;
	}
      compute_directory_size (dbf, block_size, &dir_size, &dir_bits);
      GDBM_DEBUG (GDBM_DEBUG_OPEN, "%s: computed dir_size=%d, dir_bits=%d",
		  dbf->name, dir_size, dir_bits);
      /* Check for correct block_size. */
      if (dir_size != block_size)
	{
	  if (flags & GDBM_BSEXACT)
	    {
	      if (!(flags & GDBM_CLOERROR))
		dbf->desc = -1;
	      gdbm_close (dbf);
	      GDBM_SET_ERRNO2 (NULL, GDBM_BLOCK_SIZE_ERROR, FALSE,
			       GDBM_DEBUG_OPEN);
	      return NULL;
	    }
	  else
	    block_size = dir_size;
	}
      GDBM_DEBUG (GDBM_DEBUG_OPEN, "%s: block_size=%d", dbf->name, block_size);
      
      /* Get space for the file header. It will be written to disk, so
         make sure there's no garbage in it. */
      dbf->header = (gdbm_file_header *) calloc (1, block_size);
      if (dbf->header == NULL)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  gdbm_close (dbf);
	  GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}

      /* Set the magic number and the block_size. */
      dbf->header->header_magic = GDBM_MAGIC;
      dbf->header->block_size = block_size;
      dbf->header->dir_size = dir_size;
      dbf->header->dir_bits = dir_bits;

      /* Allocate the space for the directory. */
      dbf->dir = (off_t *) malloc (dbf->header->dir_size);
      if (dbf->dir == NULL)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  gdbm_close (dbf);
	  GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}
      dbf->header->dir = dbf->header->block_size;

      /* Create the first and only hash bucket. */
      dbf->header->bucket_elems =
	(dbf->header->block_size - sizeof (hash_bucket))
	/ sizeof (bucket_element) + 1;
      dbf->header->bucket_size  = dbf->header->block_size;
      dbf->bucket = (hash_bucket *) malloc (dbf->header->bucket_size);
      if (dbf->bucket == NULL)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  gdbm_close (dbf);
	  GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}
      _gdbm_new_bucket (dbf, dbf->bucket, 0);
      dbf->bucket->av_count = 1;
      dbf->bucket->bucket_avail[0].av_adr = 3*dbf->header->block_size;
      dbf->bucket->bucket_avail[0].av_size = dbf->header->block_size;

      /* Set table entries to point to hash buckets. */
      for (index = 0; index < GDBM_DIR_COUNT (dbf); index++)
	dbf->dir[index] = 2*dbf->header->block_size;

      /* Initialize the active avail block. */
      dbf->header->avail.size
	= ( (dbf->header->block_size - sizeof (gdbm_file_header))
	 / sizeof (avail_elem)) + 1;
      dbf->header->avail.count = 0;
      dbf->header->avail.next_block = 0;
      dbf->header->next_block  = 4*dbf->header->block_size;

      /* Write initial configuration to the file. */
      /* Block 0 is the file header and active avail block. */
      if (_gdbm_full_write (dbf, dbf->header, dbf->header->block_size))
	{
	  GDBM_DEBUG (GDBM_DEBUG_OPEN|GDBM_DEBUG_ERR,
		      "%s: error writing header: %s",
		      dbf->name, gdbm_db_strerror (dbf));
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}

      /* Block 1 is the initial bucket directory. */
      if (_gdbm_full_write (dbf, dbf->dir, dbf->header->dir_size))
	{
	  GDBM_DEBUG (GDBM_DEBUG_OPEN|GDBM_DEBUG_ERR,
		      "%s: error writing directory: %s",
		      dbf->name, gdbm_db_strerror (dbf));	  
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}

      /* Block 2 is the only bucket. */
      if (_gdbm_full_write (dbf, dbf->bucket, dbf->header->bucket_size))
	{
	  GDBM_DEBUG (GDBM_DEBUG_OPEN|GDBM_DEBUG_ERR,
		      "%s: error writing bucket: %s",
		      dbf->name, gdbm_db_strerror (dbf));	  
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}

      /* Wait for initial configuration to be written to disk. */
      __fsync (dbf);

      free (dbf->bucket);
    }
  else
    {
      /* This is an old database.  Read in the information from the file
	 header and initialize the hash directory. */

      gdbm_file_header partial_header;  /* For the first part of it. */

      /* Read the partial file header. */
      if (_gdbm_full_read (dbf, &partial_header, sizeof (gdbm_file_header)))
	{
	  GDBM_DEBUG (GDBM_DEBUG_ERR|GDBM_DEBUG_OPEN,
		      "%s: error reading partial header: %s",
		      dbf->name, gdbm_db_strerror (dbf));
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}

      /* Is the magic number good? */
      if (partial_header.header_magic != GDBM_MAGIC
	  && partial_header.header_magic != GDBM_OMAGIC)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  gdbm_close (dbf);
	  switch (partial_header.header_magic)
	    {
	      case GDBM_OMAGIC_SWAP:
	      case GDBM_MAGIC32_SWAP:
	      case GDBM_MAGIC64_SWAP:
		GDBM_SET_ERRNO2 (NULL, GDBM_BYTE_SWAPPED, FALSE,
				 GDBM_DEBUG_OPEN);
		break;
	      case GDBM_MAGIC32:
	      case GDBM_MAGIC64:
		GDBM_SET_ERRNO2 (NULL, GDBM_BAD_FILE_OFFSET, FALSE,
				 GDBM_DEBUG_OPEN);
		break;
	      default:
		GDBM_SET_ERRNO2 (NULL, GDBM_BAD_MAGIC_NUMBER, FALSE,
				 GDBM_DEBUG_OPEN);
	    }
	  return NULL;
	}

      /* It is a good database, read the entire header. */
      dbf->header = (gdbm_file_header *) malloc (partial_header.block_size);
      if (dbf->header == NULL)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}
      memcpy (dbf->header, &partial_header, sizeof (gdbm_file_header));
      if (_gdbm_full_read (dbf, &dbf->header->avail.av_table[1],
			   dbf->header->block_size - sizeof (gdbm_file_header)))
	{
	  GDBM_DEBUG (GDBM_DEBUG_ERR|GDBM_DEBUG_OPEN,
		      "%s: error reading av_table: %s",
		      dbf->name, gdbm_db_strerror (dbf));
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}
	
      /* Allocate space for the hash table directory.  */
      dbf->dir = (off_t *) malloc (dbf->header->dir_size);
      if (dbf->dir == NULL)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  gdbm_close (dbf);
	  GDBM_SET_ERRNO2 (NULL, GDBM_MALLOC_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}

      /* Read the hash table directory. */
      file_pos = __lseek (dbf, dbf->header->dir, SEEK_SET);
      if (file_pos != dbf->header->dir)
	{
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  GDBM_SET_ERRNO2 (NULL, GDBM_FILE_SEEK_ERROR, FALSE, GDBM_DEBUG_OPEN);
	  return NULL;
	}

      if (_gdbm_full_read (dbf, dbf->dir, dbf->header->dir_size))
	{
	  GDBM_DEBUG (GDBM_DEBUG_ERR|GDBM_DEBUG_OPEN,
		      "%s: error reading dir: %s",
		      dbf->name, gdbm_db_strerror (dbf));
	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}

    }

#if HAVE_MMAP
  if (!(flags & GDBM_NOMMAP))
    {
      if (_gdbm_mapped_init (dbf) == 0)
	dbf->memory_mapping = TRUE;
      else
	{
	  /* gdbm_errno should already be set. */
	  GDBM_DEBUG (GDBM_DEBUG_ERR|GDBM_DEBUG_OPEN,
		      "%s: _gdbm_mapped_init failed: %s",
		      dbf->name, strerror (errno));

	  if (!(flags & GDBM_CLOERROR))
	    dbf->desc = -1;
	  SAVE_ERRNO (gdbm_close (dbf));
	  return NULL;
	}
    }
#endif

  /* Finish initializing dbf. */
  dbf->last_read = -1;
  dbf->bucket = NULL;
  dbf->bucket_dir = 0;
  dbf->cache_entry = NULL;
  dbf->header_changed = FALSE;
  dbf->directory_changed = FALSE;
  dbf->bucket_changed = FALSE;
  dbf->second_changed = FALSE;

  GDBM_DEBUG (GDBM_DEBUG_ALL, "%s: opened successfully", dbf->name);

  /* Everything is fine, return the pointer to the file
     information structure.  */
  return dbf;
}
  
/* Initialize dbm system.  FILE is a pointer to the file name.  If the file
   has a size of zero bytes, a file initialization procedure is performed,
   setting up the initial structure in the file.  BLOCK_SIZE is used during
   initialization to determine the size of various constructs.  If the value
   is less than GDBM_MIN_BLOCK_SIZE, the file system blocksize is used,
   otherwise the value of BLOCK_SIZE is used.  BLOCK_SIZE is ignored if the
   file has previously initialized.  If FLAGS is set to GDBM_READ the user
   wants to just read the database and any call to dbm_store or dbm_delete
   will fail. Many readers can access the database at the same time.  If FLAGS
   is set to GDBM_WRITE, the user wants both read and write access to the
   database and requires exclusive access.  If FLAGS is GDBM_WRCREAT, the user
   wants both read and write access to the database and if the database does
   not exist, create a new one.  If FLAGS is GDBM_NEWDB, the user want a
   new database created, regardless of whether one existed, and wants read
   and write access to the new database.  Any error detected will cause a 
   return value of null and an approprate value will be in gdbm_errno.  If
   no errors occur, a pointer to the "gdbm file descriptor" will be
   returned. */
   

GDBM_FILE 
gdbm_open (const char *file, int block_size, int flags, int mode,
     	   void (*fatal_func) (const char *))
{
  int fd;
  /* additional bits for open(2) flags */
  int fbits = 0;

  switch (flags & GDBM_OPENMASK)
    {
      case GDBM_READER:
	fbits = O_RDONLY;
	break;

      case GDBM_WRITER:
	fbits = O_RDWR;
	break;

      case GDBM_NEWDB:
	fbits = O_RDWR|O_CREAT;
	break;

      default:
	fbits = O_RDWR|O_CREAT;
    }
  if (flags & GDBM_CLOEXEC)
    fbits |= O_CLOEXEC;
  
  fd = open (file, fbits, mode);
  if (fd < 0)
    {
      GDBM_SET_ERRNO2 (NULL, GDBM_FILE_OPEN_ERROR, FALSE, GDBM_DEBUG_OPEN);
      return NULL;
    }
  return gdbm_fd_open (fd, file, block_size, flags | GDBM_CLOERROR,
		       fatal_func);
}

/* Initialize the bucket cache. */
int
_gdbm_init_cache (GDBM_FILE dbf, size_t size)
{
  int index;

  if (dbf->bucket_cache == NULL)
    {
      dbf->bucket_cache = GDBM_DEBUG_ALLOC ("_gdbm_init_cache:malloc-failure",
	                      malloc (sizeof(cache_elem) * size));
      if (dbf->bucket_cache == NULL)
        {
          GDBM_SET_ERRNO (dbf, GDBM_MALLOC_ERROR, TRUE);
          return -1;
        }
      dbf->cache_size = size;

      for (index = 0; index < size; index++)
        {
	  (dbf->bucket_cache[index]).ca_bucket = 
	    GDBM_DEBUG_ALLOC ("_gdbm_init_cache:bucket-malloc-failure",
	                      malloc (dbf->header->bucket_size));
          if ((dbf->bucket_cache[index]).ca_bucket == NULL)
	    {
              GDBM_SET_ERRNO (dbf, GDBM_MALLOC_ERROR, TRUE);
	      return -1;
            }
          (dbf->bucket_cache[index]).ca_adr = 0;
          (dbf->bucket_cache[index]).ca_changed = FALSE;
          (dbf->bucket_cache[index]).ca_data.hash_val = -1;
          (dbf->bucket_cache[index]).ca_data.elem_loc = -1;
          (dbf->bucket_cache[index]).ca_data.dptr = NULL;
        }
      dbf->bucket = dbf->bucket_cache[0].ca_bucket;
      dbf->cache_entry = &dbf->bucket_cache[0];
    }
  return 0;
}
