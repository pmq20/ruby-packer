/**********************************************************************

  main.c -

  $Author: ko1 $
  created at: Fri Aug 19 13:19:58 JST 1994

  Copyright (C) 1993-2007 Yukihiro Matsumoto

**********************************************************************/

#undef RUBY_EXPORT
#include "ruby.h"
#include "vm_debug.h"
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef RUBY_DEBUG_ENV
#include <stdlib.h>
#endif

sqfs *enclose_io_fs;

int
main(int argc, char **argv)
{
    int ret;
    sqfs_err enclose_io_ret;
    enclose_io_fs = malloc(sizeof(sqfs));
    assert(NULL != enclose_io_fs);
    memset(enclose_io_fs, 0, sizeof(sqfs));
    enclose_io_ret = sqfs_open_image(enclose_io_fs, enclose_io_memfs, 0);
    assert(SQFS_OK == enclose_io_ret);

    int new_argc = argc;
    char **new_argv = argv;
    if (NULL == getenv("ENCLOSE_IO_USE_ORIGINAL_RUBY")) {
	ENCLOSE_IO_ENTRANCE;

	#ifdef ENCLOSE_IO_CHDIR_AT_STARTUP
	chdir(ENCLOSE_IO_CHDIR_AT_STARTUP);
	#endif
    }


#ifdef RUBY_DEBUG_ENV
    ruby_set_debug_option(getenv("RUBY_DEBUG"));
#endif
#ifdef HAVE_LOCALE_H
    setlocale(LC_CTYPE, "");
#endif

    ruby_sysinit(&new_argc, &new_argv);
    {
	RUBY_INIT_STACK;
	ruby_init();
	ret = ruby_run_node(ruby_options(new_argc, new_argv));
    }

    sqfs_destroy(enclose_io_fs);
    free(enclose_io_fs);
    return ret;
}
