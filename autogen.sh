#!/bin/sh

if test ! -e ltmain.sh; then
  echo "Running libtoolize..."
  libtoolize --force --automake
fi

echo "Running autopoint..."
autopoint --force
rm mkinstalldirs~

echo "Running aclocal..."
aclocal

echo "Running autoheader..."
autoheader

echo "Running automake..."
automake --add-missing --force-missing

echo "Running autoconf..."
autoconf

echo 'Done.  Run "configure --enable-maintainer-mode" now'
