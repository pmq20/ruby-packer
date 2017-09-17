AM_CFLAGS = -I$(top_srcdir)/include

check_PROGRAMS = test

TESTS = test
test_SOURCES = test.c
test_LDADD = -lcrypto -lssl $(top_builddir)/src/libtls.la
