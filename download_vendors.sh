#!/usr/bin/env bash

LIBFFI_VERSION=3.4.2
LIBYAML_VERSION=0.2.5
# 1.1.1l 
OPENSSL_VERSION=1.1.1p
NCURSES_VERSION=6.1 # the newer 6.2 and 6.3 are not build on ARM64

RUNTIME_DIR=vendor
function download_and_extract()
{
	local BASENAME="$1"
	local LIB="$2"
	local LIB_PLUS_VERSION="$3"
	local URL="$4"
	local regex='\.bz2$'
    cd $BASENAME
	# if [[ ! -e "$BASENAME" ]]; then
    rm -f "$LIB_PLUS_VERSION.tmp"
    curl --fail -L -o "$LIB_PLUS_VERSION.tmp" "$URL"
    # mv "$LIB_PLUS_VERSION.tmp" "$LIB_PLUS_VERSION"
	# # fi
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

# download_and_extract yaml-$LIBYAML_VERSION.tar.gz \
#     https://pyyaml.org/download/libyaml/yaml-$LIBYAML_VERSION.tar.gz

download_and_extract vendor openssl openssl-$OPENSSL_VERSION https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz

# download_and_extract ncurses-$NCURSES_VERSION.tar.bz2 \
# 	https://ftp.gnu.org/pub/gnu/ncurses/ncurses-$NCURSES_VERSION.tar.gz