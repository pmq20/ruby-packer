/* This file is part of GDBM, the GNU data base manager.
   Copyright (C) 2011, 2013, 2017 Free Software Foundation, Inc.

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

/* Read exactly SIZE bytes of data into BUFFER.  Return value is 0 on
   success, and -1 on error.  In the latter case, gdbm_errno is set to
   GDBM_FILE_EOF, if not enough data is available, and to
   GDBM_FILE_READ_ERROR, if a read error occurs. */
int
_gdbm_full_read (GDBM_FILE dbf, void *buffer, size_t size)
{
  char *ptr = buffer;
  while (size)
    {
      ssize_t rdbytes = __read (dbf, ptr, size);
      if (rdbytes == -1)
	{
	  if (errno == EINTR)
	    continue;
	  GDBM_SET_ERRNO (dbf, GDBM_FILE_READ_ERROR, FALSE);
	  return -1;
	}
      if (rdbytes == 0)
	{
	  GDBM_SET_ERRNO (dbf, GDBM_FILE_EOF, FALSE);
	  return -1;
	}
      ptr += rdbytes;
      size -= rdbytes;
    }
  return 0;
}

/* Write exactly SIZE bytes of data from BUFFER tp DBF.  Return 0 on
   success, and -1 (setting gdbm_errno to GDBM_FILE_READ_ERROR) on error. */
int
_gdbm_full_write (GDBM_FILE dbf, void *buffer, size_t size)
{
  char *ptr = buffer;
  while (size)
    {
      ssize_t wrbytes = __write (dbf, ptr, size);
      if (wrbytes == -1)
	{
	  if (errno == EINTR)
	    continue;
	  GDBM_SET_ERRNO (dbf, GDBM_FILE_WRITE_ERROR, TRUE);
	  return -1;
	}
      if (wrbytes == 0)
	{
	  errno = ENOSPC;
	  GDBM_SET_ERRNO (dbf, GDBM_FILE_WRITE_ERROR, TRUE);
	  return -1;
	}
      ptr += wrbytes;
      size -= wrbytes;
    }
  return 0;
}
