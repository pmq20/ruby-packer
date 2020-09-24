/**********************************************************************

  main.c -

  $Author$
  created at: Fri Aug 19 13:19:58 JST 1994

  Copyright (C) 1993-2007 Yukihiro Matsumoto

**********************************************************************/

/*!
 * \mainpage Developers' documentation for Ruby
 *
 * This documentation is produced by applying Doxygen to
 * <a href="https://github.com/ruby/ruby">Ruby's source code</a>.
 * It is still under construction (and even not well-maintained).
 * If you are familiar with Ruby's source code, please improve the doc.
 */
#undef RUBY_EXPORT
#include "ruby.h"
#include "vm_debug.h"
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#if RUBY_DEVEL && !defined RUBY_DEBUG_ENV
# define RUBY_DEBUG_ENV 1
#endif
#if defined RUBY_DEBUG_ENV && !RUBY_DEBUG_ENV
# undef RUBY_DEBUG_ENV
#endif
#ifdef RUBY_DEBUG_ENV
#include <stdlib.h>
#endif

// --------- [Enclose.IO Hack start] ---------
#include "enclose_io.h"
#ifdef _WIN32
#include <direct.h>
#include <windows.h>
#else // ifdef _WIN32
#include <unistd.h>
extern uint8_t _binary_enclose_io_memfs_squashfs_start[];
#endif // ifdef _WIN32
extern SQUASH_OS_PATH mkdir_workdir;
extern char *enclose_io_mkdir_scope;
// --------- [Enclose.IO Hack end] ---------

int
main(int argc, char **argv)
{
// --------- [Enclose.IO Hack start] ---------
	const uint8_t *enclose_io_memfs = NULL;
	int ret;
	sqfs_err enclose_io_ret;
#ifdef _WIN32
	BOOL bool_ret;
	HRSRC myResource;
	HGLOBAL myResourceData;
#else // ifdef _WIN32
	int new_argc;
	char **new_argv;
	char *argv_memory;
	size_t i;
	size_t total_argv_size;
#endif // ifdef _WIN32

#ifdef _WIN32
	myResource = FindResource(NULL, MAKEINTRESOURCE(101), RT_RCDATA);
	assert(NULL != myResource);
	myResourceData = LoadResource(NULL, myResource);
	assert(NULL != myResourceData);
	enclose_io_memfs = (const uint8_t *)(LockResource(myResourceData));
	assert(NULL != enclose_io_memfs);
	enclose_io_ret = squash_start();
	assert(SQFS_OK == enclose_io_ret);
	enclose_io_fs = malloc(sizeof(sqfs));
	assert(NULL != enclose_io_fs);
	memset(enclose_io_fs, 0, sizeof(sqfs));
	enclose_io_ret = sqfs_open_image(enclose_io_fs, enclose_io_memfs, 0);
	assert(SQFS_OK == enclose_io_ret);
	if (NULL == getenv("ENCLOSE_IO_USE_ORIGINAL_RUBY")) {
#ifdef ENCLOSE_IO_GEMFILE
		bool_ret = SetEnvironmentVariable("BUNDLE_GEMFILE", "/__enclose_io_memfs__/local/Gemfile");
		assert(0 != bool_ret);
#endif // ifdef ENCLOSE_IO_GEMFILE
#ifdef ENCLOSE_IO_RAILS
		assert(NULL == mkdir_workdir);
		enclose_io_mkdir_scope = "/__enclose_io_memfs__/local";
		bool_ret = SetEnvironmentVariable("ENCLOSE_IO_RAILS", "1");
		assert(0 != bool_ret);
#endif // ifdef ENCLOSE_IO_RAILS
	}
#else // ifdef _WIN32
	enclose_io_memfs = (const uint8_t *)(_binary_enclose_io_memfs_squashfs_start);
	assert(NULL != enclose_io_memfs);
	enclose_io_ret = squash_start();
	assert(SQFS_OK == enclose_io_ret);
	enclose_io_fs = malloc(sizeof(sqfs));
	assert(NULL != enclose_io_fs);
	memset(enclose_io_fs, 0, sizeof(sqfs));
	enclose_io_ret = sqfs_open_image(enclose_io_fs, enclose_io_memfs, 0);
	assert(SQFS_OK == enclose_io_ret);
#ifdef ENCLOSE_IO_ENTRANCE
	new_argc = argc;
	new_argv = argv;
	argv_memory = NULL;
	if (NULL == getenv("ENCLOSE_IO_USE_ORIGINAL_RUBY")) {
#ifdef ENCLOSE_IO_GEMFILE
		ret = setenv("BUNDLE_GEMFILE", "/__enclose_io_memfs__/local/Gemfile", 1);
		assert(0 == ret);
#endif // ifdef ENCLOSE_IO_GEMFILE
#ifdef ENCLOSE_IO_RAILS
		assert(NULL == mkdir_workdir);
		enclose_io_mkdir_scope = "/__enclose_io_memfs__/local";
		ret = setenv("ENCLOSE_IO_RAILS", "1", 1);
		assert(0 == ret);
#endif // ifdef ENCLOSE_IO_RAILS
		new_argv = (char **)malloc( (argc + 1) * sizeof(char *));
		assert(new_argv);
		new_argv[0] = argv[0];
		new_argv[1] = ENCLOSE_IO_ENTRANCE;
		for (i = 1; i < argc; ++i) {
			new_argv[2 + i - 1] = argv[i];
		}
		new_argc = argc + 1;
		/* argv memory should be adjacent. */
		total_argv_size = 0;
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
		argc = new_argc;
		argv = new_argv;
	}
#endif // ifdef ENCLOSE_IO_ENTRANCE
#endif // ifdef _WIN32
// --------- [Enclose.IO Hack end] ---------

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