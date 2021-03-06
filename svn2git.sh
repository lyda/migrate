#!/bin/bash

. ./common || exit 1

if [[ ! -d ../gnucobol ]]; then
  git svn clone https://svn.code.sf.net/p/open-cobol/code --ignore-paths='^external-doc/' -A authors-svn.list --stdlayout ../gnucobol
else
  cd ../gnucobol && git svn fetch -A $base_dir/authors-svn.list
fi
