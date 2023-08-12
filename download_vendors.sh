#!/usr/bin/env bash

LIBFFI_VERSION=3.4.2
LIBYAML_VERSION=0.2.5
OPENSSL_VERSION=1.1.1p
NCURSES_VERSION=6.1 # the newer 6.2 and 6.3 are not build on ARM64

RUNTIME_DIR=vendor
function download_and_extract()
{
	local BASENAME="$1"
	local DIRNAME="$2"
	local URL="$3"
	local regex='\.bz2$'

	if [[ ! -e "$BASENAME" ]]; then
		run rm -f "$BASENAME.tmp"
		run curl --fail -L -o "$BASENAME.tmp" "$URL"
		run mv "$BASENAME.tmp" "$BASENAME"
	fi
	if [[ "$URL" =~ $regex ]]; then
		run tar xjf "$BASENAME"
	else
		run tar xzf "$BASENAME"
	fi

	echo "Entering $RUNTIME_DIR/$DIRNAME"
	pushd "$DIRNAME" >/dev/null
}

download_and_extract libffi-$LIBFFI_VERSION.tar.bz2 \
    https://github.com/libffi/libffi/releases/download/v$LIBFFI_VERSION/libffi-$LIBFFI_VERSION.tar.gz

# download_and_extract yaml-$LIBYAML_VERSION.tar.gz \
#     https://pyyaml.org/download/libyaml/yaml-$LIBYAML_VERSION.tar.gz

# download_and_extract openssl-$OPENSSL_VERSION.tar.gz \
#         https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz

# download_and_extract ncurses-$NCURSES_VERSION.tar.bz2 \
# 	https://ftp.gnu.org/pub/gnu/ncurses/ncurses-$NCURSES_VERSION.tar.gz