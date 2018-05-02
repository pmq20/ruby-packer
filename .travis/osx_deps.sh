#!/bin/sh

set -x
set -e

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    brew update
    brew install squashfs
    brew install texinfo
fi

