#!/usr/bin/env bash

TMPFILE=$(mktemp)

curl https://ftp.pcre.org/pub/pcre/pcre2-10.32.zip -o $TMPFILE
unzip -d . $TMPFILE
rm $TMPFILE

mkdir priv
cd pcre2-10.32
./configure --prefix=$(echo -n `cd ../priv; pwd`) && make && make install
