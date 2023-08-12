#!/usr/bin/env bash
RUBY_VERSION=3.2.2
RUBY_MAJOR=`echo $RUBY_VERSION | cut -d . -f 1`
RUBY_MINOR=`echo $RUBY_VERSION | cut -d . -f 2`
RUBY_PATCH=`echo $RUBY_VERSION | cut -d . -f 3 | cut -d - -f 1`
RUBY_PREVIEW=`echo $RUBY_VERSION | grep -e '-' | cut -d - -f 2`
RUBY_MAJOR_MINOR="$RUBY_MAJOR.$RUBY_MINOR"
echo "RUBY_MAJOR=$RUBY_MAJOR"
echo "RUBY_MINOR=$RUBY_MINOR"
echo "RUBY_PATCH=$RUBY_PATCH"
echo "RUBY_PREVIEW=$RUBY_PREVIEW"
echo "RUBY_MAJOR_MINOR=$RUBY_MAJOR_MINOR"


rm -rf ruby
./download-ruby-source.sh
mv ruby-$RUBY_VERSION ruby
# git status -s -uno | wc -l | grep -q 62 || exit 1
find ruby-enclose -type f | grep -q 62 || exit 1
find ruby-enclose -type f 
cp -r ruby-enclose/* ruby