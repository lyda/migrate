#!/bin/sh

if test ! -e config.rpath; then
  echo "Running gettextize..."
  gettextize --force --no-changelog
fi

if test ! -e ltmain.sh; then
  echo "Running libtoolize..."
  libtoolize --force --automake
fi

echo "Running aclocal..."
aclocal

echo "Running autoheader..."
autoheader

echo "Running automake..."
automake --add-missing --force-missing

echo "Running autoconf..."
autoconf

echo 'Done.  Run "configure --enable-maintainer-mode" now'
