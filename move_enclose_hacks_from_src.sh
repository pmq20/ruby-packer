#!/usr/bin/env bash

mkdir -p ruby-enclose
mkdir -p ruby-enclose/lib/rubygems
mkdir -p ruby-enclose/win32
mkdir -p ruby-enclose/tool
mkdir -p ruby-enclose/template

## Patched ruby source files
cp ruby/common.mk ruby-enclose/common.mk

cp ruby/dir.c ruby-enclose/dir.c
cp ruby/dln.c ruby-enclose/dln.c
cp ruby/file.c ruby-enclose/file.c
cp ruby/io.c ruby-enclose/io.c
cp ruby/main.c ruby-enclose/main.c
cp ruby/process.c ruby-enclose/process.c
cp ruby/ruby.c ruby-enclose/ruby.c
cp ruby/util.c ruby-enclose/util.c
cp ruby/lib/rubygems/path_support.rb ruby-enclose/lib/rubygems/path_support.rb
cp ruby/tool/mkconfig.rb ruby-enclose/tool/mkconfig.rb
cp ruby/win32/file.c ruby-enclose/win32/file.c
cp ruby/win32/win32.c ruby-enclose/win32/win32.c

cp ruby/template/Makefile.in ruby-enclose/template/Makefile.in
cp ruby/win32/Makefile.sub ruby-enclose/win32/Makefile.sub

## Enclose specific files
cp ruby/enclose_io_common.h ruby-enclose/
cp ruby/enclose_io_prelude.h ruby-enclose/
cp ruby/enclose_io_unix.c ruby-enclose/
cp ruby/enclose_io_unix.h ruby-enclose/
cp ruby/enclose_io_win32.c ruby-enclose/
cp ruby/enclose_io_win32.h ruby-enclose/
cp ruby/enclose_io_winapi.h ruby-enclose/
cp ruby/enclose_io.h ruby-enclose/

## Libsquash files
mkdir -p ruby-enclose/squash
cp -r ruby/squash/ ruby-enclose/squash/
cp ruby/squash_* ruby-enclose/
cp ruby/squash.h ruby-enclose/

## Patched vendor files
mkdir -p vendor-enclose/gdbm/src
cp vendor/gdbm/src/gdbmopen.c vendor-enclose/gdbm/src/gdbmopen.c