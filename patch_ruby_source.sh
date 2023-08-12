#!/usr/bin/env bash
rm -rf ruby
./download-ruby-source.sh
mv ruby-3.2.2 ruby
git status -s -uno | wc -l | grep -q 62 && echo "successful diff against ruby source" || echo "unexpected diff count" && exit 1
find ruby-enclose -type f | wc -l | grep -q 62 && echo "successful count against enclose patch" || echo "not enough files to patch" && exit 1
cp -r ruby-enclose/* ruby