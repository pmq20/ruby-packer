# This file is part of GDBM.                                   -*- autoconf -*-
# Copyright (C) 2007, 2009, 2011, 2013-2014, 2016-2017 Free Software
# Foundation, Inc.
#
# GDBM is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GDBM is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GDBM. If not, see <http://www.gnu.org/licenses/>. */

m4_define([_GDBM_VERSION_MAJOR], 1)
m4_define([_GDBM_VERSION_MINOR], 13)
dnl m4_define([_GDBM_VERSION_PATCH], 90)

AC_INIT([gdbm],
        _GDBM_VERSION_MAJOR._GDBM_VERSION_MINOR[]m4_ifdef([_GDBM_VERSION_PATCH],._GDBM_VERSION_PATCH),
	[bug-gdbm@gnu.org],,
	[http://www.gnu.org/software/gdbm])
AC_PREREQ(2.69)
AC_CONFIG_SRCDIR([src/gdbmdefs.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([autoconf.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([gnits 1.11 std-options silent-rules])

# Enable silent rules by default:
AM_SILENT_RULES([yes])

AC_SUBST([GDBM_VERSION_MAJOR], _GDBM_VERSION_MAJOR)
AC_SUBST([GDBM_VERSION_MINOR], _GDBM_VERSION_MINOR)
AC_SUBST([GDBM_VERSION_PATCH], m4_ifdef([_GDBM_VERSION_PATCH],_GDBM_VERSION_PATCH,0))

AC_ARG_ENABLE([memory-mapped-io],
  AC_HELP_STRING(
    [--enable-memory-mapped-io]
    [Use mmap(2) for disk I/O. (Default is YES.)]),
    [mapped_io=$enableval],
    [mapped_io=yes])

AC_ARG_ENABLE([libgdbm-compat],
  AC_HELP_STRING(
    [--enable-libgdbm-compat]
    [Build and install libgdbm_compat. (Default is NO.)]),
    [want_compat=$enableval],
    [want_compat=no])
AC_ARG_VAR([COMPATINCLUDEDIR],[installation directory for dbm.h and ndbm.h])
test -z "$COMPATINCLUDEDIR" && COMPATINCLUDEDIR='$(includedir)'

AC_ARG_ENABLE([gdbm-export],
  AC_HELP_STRING(
    [--enable-gdbm-export],
    [Build and install gdbmexport with specified gdbm 1.8 library. (Default is NO.)]),
    [want_export=$enableval],
    [want_export=no])

AC_ARG_WITH([gdbm183-library],
  AC_HELP_STRING(
    [--with-gdbm183-library],
    [Build gdbmexport with specified (static) library.]),
    [GDBM183_LIBRARY=$withval],
    [GDBM183_LIBRARY="-lgdbm"])

AC_ARG_WITH([gdbm183-libdir],
  AC_HELP_STRING(
    [--with-gdbm183-libdir],
    [Build gdbmexport with the gdbm library in the specified directory.]),
    [GDBM183_LIBDIR=$withval],
    [GDBM183_LIBDIR="/usr/local/lib"])

AC_ARG_WITH([gdbm183-includedir],
  AC_HELP_STRING(
    [--with-gdbm183-includedir],
    [Build gdbmexport with gdbm.h in the specified directory.]),
    [GDBM183_INCLUDEDIR=$withval],
    [GDBM183_INCLUDEDIR="/usr/local/include"])
    
dnl Check for programs
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_SYS_LARGEFILE
AC_PROG_YACC
AC_PROG_LEX
AC_C_CONST
AC_TYPE_UNSIGNED_LONG_LONG_INT

AC_SUBST(GDBM_COUNT_T)
if test $ac_cv_type_unsigned_long_long_int = yes; then
	GDBM_COUNT_T="unsigned long long int"
else
	GDBM_COUNT_T="unsigned long"
fi

dnl Internationalization macros.
AM_GNU_GETTEXT([external], [need-ngettext])
AM_GNU_GETTEXT_VERSION(0.18)

AC_CHECK_HEADERS([sys/file.h sys/termios.h string.h locale.h getopt.h])

AC_CHECK_LIB(dbm, main)
AC_CHECK_LIB(ndbm, main)
AC_CHECK_FUNCS([ftruncate flock lockf fsync setlocale getopt_long])

if test x$mapped_io = xyes
then
  AC_FUNC_MMAP()
  AC_CHECK_FUNCS([msync])
fi
AC_TYPE_OFF_T
AC_CHECK_SIZEOF(off_t)
AC_CHECK_MEMBERS([struct stat.st_blksize])

AC_SUBST(GDBM183_LIBRARY)
AC_SUBST(GDBM183_LIBDIR)
AC_SUBST(GDBM183_INCLUDEDIR)
AM_CONDITIONAL([COMPAT_OPT], [test "$want_compat" = yes])
AM_CONDITIONAL([ENABLE_EXPORT], [test "$want_export" = yes])

# Check for Curses libs.
for lib in ncurses curses termcap
do
  AC_CHECK_LIB($lib, tputs, [CURSES_LIBS="-l$lib"; break])
done

AC_SUBST(CURSES_LIBS)
# Readline
AC_ARG_WITH([readline],
            AC_HELP_STRING([--without-readline],
                           [do not use readline]),
            [
case "${withval}" in
  yes) status_readline=yes ;;
  no)  status_readline=no ;;
  *)   AC_MSG_ERROR(bad value ${withval} for --without-readline) ;;
esac],[status_readline=probe])

AC_SUBST(READLINE_LIBS)

if test "$status_readline" != "no"; then
  dnl FIXME This should only link in the curses libraries if it's
  dnl really needed!
  saved_LIBS=$LIBS
  LIBS="$LIBS $CURSES_LIBS"
  AC_CHECK_LIB(readline, readline,
    [status_readline=yes],
    [if test "$status_readline" = "yes"; then
       AC_MSG_ERROR(readline requested but does not seem to be installed)
     else
       status_readline=no
     fi])
  LIBS=$saved_LIBS

  if test "$status_readline" = "yes"; then
    AC_CHECK_HEADERS(readline/readline.h,
      AC_DEFINE(WITH_READLINE,1,[Enable use of readline]))
      READLINE_LIBS="-lreadline $CURSES_LIBS"
      saved_LIBS=$LIBS
      LIBS="$LIBS $READLINE_LIBS"
      AC_CHECK_FUNCS(rl_completion_matches)
      LIBS=$saved_LIBS
  fi
else
  status_readline=no
fi

AM_CONDITIONAL([GDBM_COND_READLINE], [test "$status_readline" = "yes"])

# Additional debugging
AC_ARG_ENABLE([debug],
              AC_HELP_STRING([--enable-debug],
                             [provide additional debugging information]),
              [status_debug=$withval],
	      [status_debug=no])

AC_SUBST([GDBM_DEBUG_ENABLE])
if test "$status_debug" = "yes"; then
  GDBM_DEBUG_ENABLE=1
else
  GDBM_DEBUG_ENABLE=0
fi  
AM_CONDITIONAL([GDBM_COND_DEBUG_ENABLE], [test "$status_debug" = "yes"])

# Initialize the test suite.
AC_CONFIG_TESTDIR(tests)
AC_CONFIG_FILES([tests/Makefile tests/atlocal po/Makefile.in])
AM_MISSING_PROG([AUTOM4TE], [autom4te])

AC_CONFIG_COMMANDS([status],[
cat <<EOF

*******************************************************************
GDBM settings summary:

Compatibility library ......................... $status_compat
Compatibility export tool ..................... $status_export
Memory mapped I/O ............................. $mapped_io
GNU Readline .................................. $status_readline
Debugging support ............................. $status_debug
*******************************************************************

EOF
],
[status_compat=$want_compat
status_export=$want_export
mapped_io=$mapped_io
status_readline=$status_readline
status_debug=$status_debug])

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 src/gdbm.h
		 doc/Makefile
		 compat/Makefile
		 export/Makefile])

AC_OUTPUT
