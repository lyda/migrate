#!/bin/bash

set -e

if [[ ! -d gnucobol ]]; then
  git svn clone https://svn.code.sf.net/p/open-cobol/code --ignore-paths='^external-doc/' -A COB-SVN2GIT.DAT --stdlayout gnucobol
else
  cd gnucobol git svn fetch -A ../COB-SVN2GIT.DAT
fi
