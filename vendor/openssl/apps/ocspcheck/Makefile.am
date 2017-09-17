include $(top_srcdir)/Makefile.am.common

bin_PROGRAMS = ocspcheck

EXTRA_DIST = ocspcheck.8
EXTRA_DIST += CMakeLists.txt

ocspcheck_LDADD = $(abs_top_builddir)/crypto/libcrypto.la
ocspcheck_LDADD += $(abs_top_builddir)/ssl/libssl.la
ocspcheck_LDADD += $(abs_top_builddir)/tls/libtls.la
ocspcheck_LDADD += $(PLATFORM_LDADD) $(PROG_LDADD)

ocspcheck_SOURCES = http.c
ocspcheck_SOURCES += ocspcheck.c
noinst_HEADERS = http.h

if !HAVE_INET_NTOP
ocspcheck_SOURCES += compat/inet_ntop.c
endif

if !HAVE_MEMMEM
ocspcheck_SOURCES += compat/memmem.c
endif
