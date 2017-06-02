/* This file is part of GDBM, the GNU data base manager.
   Copyright 2016-2017 Free Software Foundation, Inc.

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
#include <ctype.h>

gdbm_debug_printer_t gdbm_debug_printer;
int gdbm_debug_flags;

struct gdbm_debug_token_desc
{
  char const *name;
  int flag;
};

struct gdbm_debug_token_desc const gdbm_debug_token_tab[] = {
  { "err",    GDBM_DEBUG_ERR },
  { "open",   GDBM_DEBUG_OPEN },
  { "store",  GDBM_DEBUG_STORE },
  { "read",   GDBM_DEBUG_READ },
  { "lookup", GDBM_DEBUG_LOOKUP },
  { "all",    GDBM_DEBUG_ALL },
  { NULL, 0 }
};

int
gdbm_debug_token (char const *tok)
{
  int i;

  for (i = 0; gdbm_debug_token_tab[i].name; i++)
    if (strcmp (gdbm_debug_token_tab[i].name, tok) == 0)
      return gdbm_debug_token_tab[i].flag;

  return 0;
}

void
gdbm_debug_parse_state (int (*f) (void *, int, char const *), void *d)
{
  int i;
  
  for (i = 0; gdbm_debug_token_tab[i].name; i++)
    {
      if (gdbm_debug_token_tab[i].flag == GDBM_DEBUG_ALL)
	continue;
      if (gdbm_debug_flags & gdbm_debug_token_tab[i].flag)
	{
	  if (f (d, gdbm_debug_token_tab[i].flag, gdbm_debug_token_tab[i].name))
	    break;
	}
    }
}

#define DATBUFSIZE 69

static int
datbuf_format (char vbuf[DATBUFSIZE], const char *buf, size_t size)
{
  char *p = vbuf;
  char *q = vbuf + 51;
  int i;
  size_t j = 0;
  static char hexchar[] = "0123456789ABCDEF";
  
  for (i = 0; i < 16; i++)
    {
      unsigned c;
      if (j < size)
	{
	  c = *(const unsigned char*)buf++;
	  j++;

	  *p++ = hexchar[c >> 4];
	  *p++ = hexchar[c & 0xf];
	  *p++ = ' ';
	  
	  *q++ = isprint (c) ? c : '.';
	  if (i == 7)
	    {
	      *p++ = ' ';
	      *q++ = ' ';
	    }
	}
      else
	{
	  *p++ = ' ';
	  *p++ = ' ';
	  *p++ = ' ';
	  *q++ = ' ';
	}
    }
  *p++ = ' ';
  *p = ' ';
  *q = 0;
  return j;
}

void
gdbm_debug_datum (datum dat, char const *pfx)
{
  char const *buf = dat.dptr;
  size_t size = dat.dsize;
  unsigned off;
  char vbuf[DATBUFSIZE];

  if (!buf)
    {
      gdbm_debug_printer ("%s%s\n", pfx, "NULL");
      return;
    }

  gdbm_debug_printer ("size=%d\n", size);
  off = 0;
  while (size)
    {
      size_t rd = datbuf_format (vbuf, buf, size);
      gdbm_debug_printer ("%s%04x:  %s\n", pfx, off, vbuf);
      size -= rd;
      buf += rd;
      off += rd;
    }
}


struct hook_list
{
  struct hook_list *next;
  struct hook_list *prev;
  char *id;
  gdbm_debug_hook hook;
  void *data;
  int retval;
};

static struct hook_list *hook_head, *hook_tail;
static struct hook_list *hook_recent;

static struct hook_list *
hook_lookup_or_install (char const *id, int install)
{
  struct hook_list *p;

  for (p = hook_head; p; p = p->next)
    {
      int res = strcmp (p->id, id);
      if (res == 0)
	return p;
      if (res > 0)
	break;
    }

  if (install)
    {
      struct hook_list *elt = malloc (sizeof *elt);
      if (!elt)
	return NULL;
      elt->id = strdup (id);
      if (!elt->id)
	{
	  SAVE_ERRNO (free (elt));
	  return NULL;
	}
      elt->hook = NULL;
      elt->next = p;
      if (p)
	{
	  if (p->prev)
	    p->prev->next = elt;
	  else
	    hook_head = elt;
	  elt->prev = p->prev;
	}
      else
	{
	  elt->prev = hook_tail;
	  if (hook_tail)
	    hook_tail->next = elt;
	  else
	    hook_head = elt;
	  hook_tail = elt;
	}
      return elt;
    }
  
  return NULL;
}

static struct hook_list *
hook_lookup (char const *id)
{
  if (!(hook_recent && strcmp (hook_recent->id, id) == 0))
    hook_recent = hook_lookup_or_install (id, FALSE);
  return hook_recent;
}

static void
hook_remove (char const *id)
{
  struct hook_list *p;

  p = hook_lookup (id);
  if (!p)
    return;

  hook_recent = NULL;
  
  if (p->prev)
    p->prev->next = p->next;
  else
    hook_head = p->next;

  if (p->next)
    p->next->prev = p->prev;
  else
    hook_tail = p->prev;

  free (p->id);
  free (p);
}

static int
default_hook (char const *file, int line, char const *id, void *data)
{
  fprintf (stderr, "%s:%d: hit debug hook %s\n", file, line, id);
  return 1;
}
  
void
_gdbm_debug_hook_install (char const *id, gdbm_debug_hook hook, void *data)
{
  struct hook_list *p;

  p = hook_lookup_or_install (id, TRUE);
  p->hook = hook ? hook : default_hook;
  p->data = data;
}

void
_gdbm_debug_hook_remove (char const *id)
{
  hook_remove (id);
}

int
_gdbm_debug_hook_check (char const *file, int line, char const *id)
{
  struct hook_list *p = hook_lookup (id);
  if (p)
    return p->retval = p->hook (file, line, id, p->data);
  return 0;
}

int
_gdbm_debug_hook_val (char const *id)
{
  struct hook_list *p = hook_lookup (id);
  if (p)
    return p->retval;
  return 0;
}
