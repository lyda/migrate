#!/bin/sh

echo "Running libtoolize..."
libtoolize --automake --force --copy

echo "Running aclocal..."
aclocal -I .

echo "Running autoheader..."
autoheader

echo "Running automake..."
automake -a -c

echo "Running autoconf..."
autoconf

echo 'Done.  Run "configure --enable-maintainer-mode" now'
