#!/bin/sh

# echo "Running libtoolize..."
# libtoolize --automake --force --ltdl

echo "Running aclocal..."
aclocal -I .

echo "Running autoheader..."
autoheader

echo "Running automake..."
automake -a

echo "Running autoconf..."
autoconf

echo 'Done.  Run "configure --enable-maintainer-mode" now'
