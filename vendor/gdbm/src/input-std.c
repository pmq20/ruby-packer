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
   along with GDBM. If not, see <http://www.gnu.org/licenses/>.    */

#include "gdbmtool.h"

ssize_t
input_read (FILE *fp, char *buf, size_t size)
{
  if (interactive)
    {
      print_prompt_at_bol ();
      if (fgets (buf, size, fp) == NULL)
	return 0;
      return strlen (buf);
    }
  return fread (buf, 1, size, fp);
}

void
input_init (void)
{
  /* nothing */
}

void
input_done (void)
{
  /* nothing */
}

