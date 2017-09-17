#!/bin/sh
set -e

rm -f man/*.1 man/*.3 include/openssl/*.h
./autogen.sh
./configure
make -j2 distcheck
