#!/usr/bin/env bash
rm -rf ruby
./download-ruby-source.sh
mv ruby-3.2.2 ruby
git status -s -uno | wc -l | grep -q 62 || exit 1
find ruby-enclose -type f | grep -q 62 || exit 1
find ruby-enclose -type f 
cp -r ruby-enclose/* ruby