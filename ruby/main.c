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

// --------- [Enclose.io Hack start] ---------
#include "enclose_io.h"
// --------- [Enclose.io Hack end] ---------

int
main(int argc, char **argv)
{
// --------- [Enclose.io Hack start] ---------
    #ifdef _WIN32
      BOOL bool_ret;
    #endif
    int ret;
    sqfs_err enclose_io_ret;
    enclose_io_ret = squash_start();
    assert(SQFS_OK == enclose_io_ret);
    enclose_io_fs = malloc(sizeof(sqfs));
    assert(NULL != enclose_io_fs);
    memset(enclose_io_fs, 0, sizeof(sqfs));
    enclose_io_ret = sqfs_open_image(enclose_io_fs, enclose_io_memfs, 0);
    assert(SQFS_OK == enclose_io_ret);

    #ifdef _WIN32
      if (NULL == getenv("ENCLOSE_IO_USE_ORIGINAL_RUBY")) {
        #ifdef ENCLOSE_IO_ENV_BUNDLE_GEMFILE
          bool_ret = SetEnvironmentVariable("BUNDLE_GEMFILE", ENCLOSE_IO_ENV_BUNDLE_GEMFILE);
          assert(0 != bool_ret);
        #endif // ENCLOSE_IO_ENV_BUNDLE_GEMFILE
      }
    #else // ifdef _WIN32
      #ifdef ENCLOSE_IO_ENTRANCE
        int new_argc = argc;
        char **new_argv = argv;
        char *argv_memory = NULL;
        if (NULL == getenv("ENCLOSE_IO_USE_ORIGINAL_RUBY")) {
            #ifdef ENCLOSE_IO_ENV_BUNDLE_GEMFILE
              ret = setenv("BUNDLE_GEMFILE", ENCLOSE_IO_ENV_BUNDLE_GEMFILE, 1);
              assert(0 == ret);
            #endif // ENCLOSE_IO_ENV_BUNDLE_GEMFILE
            new_argv = (char **)malloc( (argc + 1) * sizeof(char *));
            assert(new_argv);
            new_argv[0] = argv[0];
            new_argv[1] = ENCLOSE_IO_ENTRANCE;
            size_t i;
            for (i = 1; i < argc; ++i) {
                   new_argv[2 + i - 1] = argv[i];
            }
            new_argc = argc + 1;
        
            #ifndef _WIN32
            /* argv memory should be adjacent. */
            size_t total_argv_size = 0;
            for (i = 0; i < new_argc; ++i) {
                   total_argv_size += strlen(new_argv[i]) + 1;
            }
            argv_memory = (char *)malloc( (total_argv_size) * sizeof(char));
            assert(argv_memory);
            for (i = 0; i < new_argc; ++i) {
                   memcpy(argv_memory, new_argv[i], strlen(new_argv[i]) + 1);
                   new_argv[i] = argv_memory;
                   argv_memory += strlen(new_argv[i]) + 1;
            }
            assert(argv_memory - new_argv[0] == total_argv_size);
            #endif
        
            argc = new_argc;
            argv = new_argv;
        }
      #endif // ENCLOSE_IO_ENTRANCE
    #endif // ifdef _WIN32
// --------- [Enclose.io Hack end] ---------

#ifdef RUBY_DEBUG_ENV
    ruby_set_debug_option(getenv("RUBY_DEBUG"));
#endif
#ifdef HAVE_LOCALE_H
    setlocale(LC_CTYPE, "");
#endif

    ruby_sysinit(&argc, &argv);
    {
	RUBY_INIT_STACK;
	ruby_init();
	return ruby_run_node(ruby_options(argc, argv));
    }
}
