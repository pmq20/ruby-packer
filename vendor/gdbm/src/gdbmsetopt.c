/* gdbmsetopt.c - set options pertaining to a GDBM descriptor. */

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

static int
getbool (void *optval, int optlen)
{
  int n;
  
  if (!optval || optlen != sizeof (int) ||
      (((n = *(int*)optval) != TRUE) && n != FALSE))
    return -1;
  return n;
}

static int
get_size (void *optval, int optlen, size_t *ret)
{
  if (!optval)
    return -1;
  if (optlen == sizeof (unsigned))
    *ret = *(unsigned*) optval;
  else if (optlen == sizeof (unsigned long))
    *ret = *(unsigned long*) optval;
  else if (optlen == sizeof (size_t))
    *ret = *(size_t*) optval;
  else
    return -1;
  return 0;
}

static int
setopt_gdbm_setcachesize (GDBM_FILE dbf, void *optval, int optlen)
{
  size_t sz;

  /* Optval will point to the new size of the cache. */
  if (dbf->bucket_cache != NULL)
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ALREADY_SET, FALSE);
      return -1;
    }
  
  if (get_size (optval, optlen, &sz))
    {     
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }  
  return _gdbm_init_cache (dbf, (sz > 9) ? sz : 10);
}

static int
setopt_gdbm_getcachesize (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (size_t))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(size_t*) optval = dbf->cache_size;
  return 0;
}

/* Obsolete form of GDBM_SETSYNCMODE. */
static int
setopt_gdbm_fastmode (GDBM_FILE dbf, void *optval, int optlen)
{
  int n;

  if ((n = getbool (optval, optlen)) == -1)
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  dbf->fast_write = n;
  return 0;
}

static int
setopt_gdbm_setsyncmode (GDBM_FILE dbf, void *optval, int optlen)
{
  int n;

  /* Optval will point to either true or false. */
  if ((n = getbool (optval, optlen)) == -1)
    { 
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  dbf->fast_write = !n;
  return 0;
}

static int
setopt_gdbm_getsyncmode (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (int))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(int*) optval = !dbf->fast_write;
  return 0;
}

/* CENTFREE - set or get the stat of the central block repository */
static int
setopt_gdbm_setcentfree (GDBM_FILE dbf, void *optval, int optlen)
{
  int n;
  
  /* Optval will point to either true or false. */
  if ((n = getbool (optval, optlen)) == -1)
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  dbf->central_free = n;
  return 0;
}

static int
setopt_gdbm_getcentfree (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (int))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(int*) optval = !dbf->central_free;
  return 0;
}

/* Coalesce state: */
static int
setopt_gdbm_setcoalesceblks (GDBM_FILE dbf, void *optval, int optlen)
{
  int n;
  
  /* Optval will point to either true or false. */
  if ((n = getbool (optval, optlen)) == -1)
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  dbf->coalesce_blocks = n;
  return 0;
}

static int
setopt_gdbm_getcoalesceblks (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (int))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(int*) optval = dbf->coalesce_blocks;
  return 0;
}

#if HAVE_MMAP  
static int
setopt_gdbm_setmmap (GDBM_FILE dbf, void *optval, int optlen)
{
  int n;
  
  if ((n = getbool (optval, optlen)) == -1)
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  __fsync (dbf);
  if (n == dbf->memory_mapping)
    return 0;
  if (n)
    {
      if (_gdbm_mapped_init (dbf) == 0)
	dbf->memory_mapping = TRUE;
      else
	return -1;
    }
  else
    {
      _gdbm_mapped_unmap (dbf);
      dbf->memory_mapping = FALSE;
    }
  return 0;
}

static int
setopt_gdbm_getmmap (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (int))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(int*) optval = dbf->memory_mapping;
  return 0;
}

/* Maximum size of a memory mapped region */
static int
setopt_gdbm_setmaxmapsize (GDBM_FILE dbf, void *optval, int optlen)
{
  size_t page_size = sysconf (_SC_PAGESIZE);
  size_t sz;

  if (get_size (optval, optlen, &sz))
    { 
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  dbf->mapped_size_max = ((sz + page_size - 1) / page_size) * page_size;
  _gdbm_mapped_init (dbf);
  return 0;
}

static int
setopt_gdbm_getmaxmapsize (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (size_t))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  *(size_t*) optval = dbf->mapped_size_max;
  return 0;
}

static int
setopt_gdbm_getflags (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (int))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  else
    {
      int flags = dbf->read_write;
      if (!dbf->fast_write)
	flags |= GDBM_SYNC;
      if (!dbf->file_locking)
	flags |= GDBM_NOLOCK;
      if (!dbf->memory_mapping)
	flags |= GDBM_NOMMAP;
      *(int*) optval = flags;
    }
  return 0;
}
#endif

static int
setopt_gdbm_getdbname (GDBM_FILE dbf, void *optval, int optlen)
{
  if (!optval || optlen != sizeof (char*))
    {
      GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
      return -1;
    }
  else
    {
      char *p = strdup (dbf->name);
      if (!p)
	{
	  GDBM_SET_ERRNO (dbf, GDBM_MALLOC_ERROR, FALSE);
	  return -1;
	}
      *(char**) optval = p;
    }
  return 0;
}

static int
setopt_gdbm_getblocksize (GDBM_FILE dbf, void *optval, int optlen)
{
  if (optval && optlen == sizeof (int))
    {
      *(int*) optval = dbf->header->block_size;
      return 0;
    }
  
  GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
  return -1;
}

typedef int (*setopt_handler) (GDBM_FILE, void *, int);

static setopt_handler setopt_handler_tab[] = {
  [GDBM_SETCACHESIZE] = setopt_gdbm_setcachesize,
  [GDBM_GETCACHESIZE] = setopt_gdbm_getcachesize,
  [GDBM_FASTMODE]     = setopt_gdbm_fastmode,
  [GDBM_SETSYNCMODE]  = setopt_gdbm_setsyncmode,
  [GDBM_GETSYNCMODE]  = setopt_gdbm_getsyncmode,
  [GDBM_SETCENTFREE]  = setopt_gdbm_setcentfree,
  [GDBM_GETCENTFREE]  = setopt_gdbm_getcentfree,
  [GDBM_SETCOALESCEBLKS] = setopt_gdbm_setcoalesceblks,
  [GDBM_GETCOALESCEBLKS] = setopt_gdbm_getcoalesceblks,
#if HAVE_MMAP  
  [GDBM_SETMMAP]         = setopt_gdbm_setmmap,
  [GDBM_GETMMAP]         = setopt_gdbm_getmmap,
  [GDBM_SETMAXMAPSIZE]   = setopt_gdbm_setmaxmapsize,
  [GDBM_GETMAXMAPSIZE]   = setopt_gdbm_getmaxmapsize,
  [GDBM_GETFLAGS]        = setopt_gdbm_getflags,
#endif
  [GDBM_GETDBNAME]       = setopt_gdbm_getdbname,
  [GDBM_GETBLOCKSIZE]    = setopt_gdbm_getblocksize,
};
  
int
gdbm_setopt (GDBM_FILE dbf, int optflag, void *optval, int optlen)
{
  /* Return immediately if the database needs recovery */	
  GDBM_ASSERT_CONSISTENCY (dbf, -1);

  if (optflag >= 0
  && optflag < sizeof (setopt_handler_tab) / sizeof (setopt_handler_tab[0]))
    return setopt_handler_tab[optflag] (dbf, optval, optlen);
  
  GDBM_SET_ERRNO (dbf, GDBM_OPT_ILLEGAL, FALSE);
  return -1;
}
