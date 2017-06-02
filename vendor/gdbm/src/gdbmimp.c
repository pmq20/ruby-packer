/* gdbmimp.c - Import a GDBM database. */

/* This file is part of GDBM, the GNU data base manager.
   Copyright (C) 2007, 2011, 2013, 2016-2017 Free Software Foundation,
   Inc.

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

# include "autoconf.h"
# include <arpa/inet.h>
# include <limits.h>

# include "gdbmdefs.h"
# include "gdbm.h"

int
gdbm_import_from_file (GDBM_FILE dbf, FILE *fp, int flag)
{
  int seenbang, seennewline, rret;
  unsigned long rsize, size;
  int ec;
  char *kbuffer, *dbuffer;
  size_t kbufsize, dbufsize;
  datum key, data;
  int count = 0;

  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, -1);
  
  seenbang = 0;
  seennewline = 0;
  kbuffer = NULL;
  dbuffer = NULL;

  /* Read (and discard) four lines begining with ! and ending with \n. */
  while (1)
    {
      if ((rret = fgetc (fp)) == -1)
	{
	  GDBM_SET_ERRNO (NULL, GDBM_FILE_READ_ERROR, FALSE);
	  return -1;
	}
      
      if (rret == '!')
	seenbang++;
      if (rret == '\n')
	{
	  if (seenbang > 3 && seennewline > 2)
	    {
	      /* End of last line. */
	      break;
	    }
	  seennewline++;
	}
    }

  /* Allocate buffers. */
  kbufsize = GDBM_MIN_BLOCK_SIZE;
  kbuffer = malloc (kbufsize);
  if (kbuffer == NULL)
    {
      GDBM_SET_ERRNO (NULL, GDBM_MALLOC_ERROR, FALSE);
      return -1;
    }
  dbufsize = GDBM_MIN_BLOCK_SIZE;
  dbuffer = malloc (dbufsize);
  if (dbuffer == NULL)
    {
      free (kbuffer);
      GDBM_SET_ERRNO (NULL, GDBM_MALLOC_ERROR, FALSE);
      return -1;
    }

  ec = GDBM_NO_ERROR;
  /* Insert/replace records in the database until we run out of file. */
  while ((rret = fread (&rsize, sizeof (rsize), 1, fp)) == 1)
    {
      /* Read the key. */
      size = ntohl (rsize);
      if (size > INT_MAX)
	{
	  ec = GDBM_ILLEGAL_DATA;
	  break;
	}
      
      if (size > kbufsize)
	{
	  kbufsize = (size + GDBM_MIN_BLOCK_SIZE);
	  kbuffer = realloc (kbuffer, kbufsize);
	  if (kbuffer == NULL)
	    {
	      ec = GDBM_MALLOC_ERROR;
	      break;
	    }
	}
      if (fread (kbuffer, size, 1, fp) != 1)
	{
	  ec = GDBM_FILE_READ_ERROR;
	  break;
	}

      key.dptr = kbuffer;
      key.dsize = (int) size;

      /* Read the data. */
      if (fread (&rsize, sizeof (rsize), 1, fp) != 1)
	{
	  ec = GDBM_FILE_READ_ERROR;
	  break;
	}

      size = ntohl (rsize);
      if (size > INT_MAX)
	{
	  ec = GDBM_ILLEGAL_DATA;
	  break;
	}
      if (size > dbufsize)
	{
	  dbufsize = (size + GDBM_MIN_BLOCK_SIZE);
	  dbuffer = realloc (dbuffer, dbufsize);
	  if (dbuffer == NULL)
	    {
	      ec = GDBM_MALLOC_ERROR;
	      break;
	    }
	}
      if (fread (dbuffer, size, 1, fp) != 1)
	{
	  ec = GDBM_FILE_READ_ERROR;
	  break;
	}

      data.dptr = dbuffer;
      data.dsize = (int) size;

      if (gdbm_store (dbf, key, data, flag) != 0)
	{
	  /* Keep the existing errno. */
	  ec = gdbm_errno;
	  break;
	}

      count++;
    }

  if (rret < 0)
    ec = GDBM_FILE_READ_ERROR;
  
  free (kbuffer);
  free (dbuffer);

  if (ec == GDBM_NO_ERROR)
    return count;

  GDBM_SET_ERRNO (NULL, ec, FALSE);
  return -1;
}

int
gdbm_import (GDBM_FILE dbf, const char *importfile, int flag)
{
  FILE *fp;
  int rc;
  
  fp = fopen (importfile, "r");
  if (!fp)
    {
      GDBM_SET_ERRNO (NULL, GDBM_FILE_OPEN_ERROR, FALSE);
      return -1;
    }
  rc = gdbm_import_from_file (dbf, fp, flag);
  fclose (fp);
  return rc;
}

