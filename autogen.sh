#!/bin/sh

echo "Running autopoint..."
autopoint --force

echo "Running aclocal..."
aclocal -I m4

echo "Running autoheader..."
autoheader

echo "Running automake..."
automake --copy --add-missing --force-missing

echo "Running autoconf..."
autoconf

echo 'Done.  Run "configure --enable-maintainer-mode" now'
