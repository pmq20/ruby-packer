#!/usr/bin/env bash

# Scripts for downloading and extracting the source code of the libraries
# used by the ruby-packer build process.

# LIBGDBM_VERSION=1.13 # Original version as of pmq20 source
LIBGDBM_VERSION=1.23 # Latest to date of 1.18.x

# https://github.com/libffi/libffi
# Note also update in compiler.rb
# LIBFFI_VERSION=3.2.1 # Original version as of pmq20 source
# LIBFFI_VERSION=3.4.2 # Latest to date of 3.x
LIBFFI_VERSION=3.4.4 # Latest to date of 3.x

# https://pyyaml.org/wiki/LibYAML
# LIBYAML_VERSION=0.1.7 # Original version as of pmq20 source
LIBYAML_VERSION=0.2.5 # Latest to date of 0.2.x

# https://www.openssl.org/source/
# OPENSSL_VERSION=1.1.1g # Original version as of pmq20 source
# OPENSSL_VERSION=1.1.1v # Latest to date of 1.1.x
# OPENSSL_VERSION=3.0.10 # Latest to date of 3.0.x
OPENSSL_VERSION=3.1.2 # Latest to date of 3.1.x

# https://ftp.gnu.org/gnu/ncurses/
# NCURSES_VERSION=6.0 # Original version as of pmq20 source
# NCURSES_VERSION=6.1 # Original version as of pmq20 source
NCURSES_VERSION=6.4 # the newer 6.2 and 6.3 are not build on ARM64

# https://zlib.net/
# ZLIB_VERSION=1.2.13 # Original version as of pmq20 source
ZLIB_VERSION=1.2.13 # Latest to date of 1.2.x
# https://ftp.gnu.org/gnu/readline/
# READLINE_VERSION=7.0 # Original version as of pmq20 source
READLINE_VERSION=8.2 # Latest to date of 8.x

RUNTIME_DIR=vendor
mkdir -p $RUNTIME_DIR
function download_and_extract()
{
	local BASENAME="$1"
	local LIB="$2"
	local LIB_PLUS_VERSION="$3"
	local URL="$4"
	local regex='\.bz2$'
    cd $BASENAME
    curl --fail -L -o "$LIB_PLUS_VERSION.tmp" "$URL"
	if [[ "$URL" =~ $regex ]]; then
		tar xjf "$LIB_PLUS_VERSION.tmp"
	else
		tar xzf "$LIB_PLUS_VERSION.tmp"
	fi
    mv "$LIB_PLUS_VERSION" "$LIB"
    rm -rf "$LIB_PLUS_VERSION.tmp"
    cd ..
}

rm -rf "vendor/libffi"
download_and_extract vendor libffi libffi-$LIBFFI_VERSION https://github.com/libffi/libffi/releases/download/v$LIBFFI_VERSION/libffi-$LIBFFI_VERSION.tar.gz

rm -rf "vendor/yaml"
download_and_extract vendor yaml yaml-$LIBYAML_VERSION https://pyyaml.org/download/libyaml/yaml-$LIBYAML_VERSION.tar.gz

rm -rf "vendor/openssl"
download_and_extract vendor openssl openssl-$OPENSSL_VERSION https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz

rm -rf "vendor/ncurses"
download_and_extract vendor ncurses ncurses-$NCURSES_VERSION https://ftp.gnu.org/pub/gnu/ncurses/ncurses-$NCURSES_VERSION.tar.gz

rm -rf "vendor/zlib"
download_and_extract vendor zlib zlib-$ZLIB_VERSION https://zlib.net/zlib-$ZLIB_VERSION.tar.gz

rm -rf "vendor/readline"
download_and_extract vendor readline readline-$READLINE_VERSION https://ftp.gnu.org/gnu/readline/readline-$READLINE_VERSION.tar.gz

rm -rf "vendor/gdbm"
download_and_extract vendor gdbm gdbm-$LIBGDBM_VERSION https://ftp.gnu.org/gnu/gdbm/gdbm-$LIBGDBM_VERSION.tar.gz