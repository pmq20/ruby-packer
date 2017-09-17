#!/bin/sh

# Run this periodically to ensure that the manpage links are up to date
(
    cd /usr/src/usr.bin/mandoc/
    make obj
    make cleandir
    make depend
    make
    cd /usr/src/regress/usr.bin/mandoc/db/mlinks/
    make obj
    make cleandir
    make
)

makewhatis -a .

echo "# This is an auto-generated file by $0" > links
/usr/src/regress/usr.bin/mandoc/db/mlinks/obj/mlinks mandoc.db | sort >> links
