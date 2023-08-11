#!/usr/bin/env bash

RUBY_VERSION=3.1.0
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

if [[ ! -e ruby-$RUBY_VERSION.tar.gz ]]; then
	"Downloading Ruby source"
	rm -f ruby-$RUBY_VERSION.tar.gz.tmp
	wget -O ruby-$RUBY_VERSION.tar.gz.tmp \
		http://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR_MINOR/ruby-$RUBY_VERSION.tar.gz
	mv ruby-$RUBY_VERSION.tar.gz.tmp ruby-$RUBY_VERSION.tar.gz
	echo
fi

echo "Extracting source code"
rm -rf ruby-$RUBY_VERSION
tar xzf ruby-$RUBY_VERSION.tar.gz
echo "Entering ruby-$RUBY_VERSION"
cd ruby-$RUBY_VERSION
ls
