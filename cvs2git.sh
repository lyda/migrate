#!/bin/bash

set -e

# CVS source: https://sourceforge.net/p/open-cobol/cvs/

base_dir=$(readlink -f $(dirname $0))
if [[ ! -d ../open-cobol ]]; then
  mkdir ../open-cobol
  cd ../open-cobol
  cvs -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol login
  cvs -z3 -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol co -P .
  git cvsimport -C ../open-cobol.git open-cobol
  cd $base_dir
fi

if [[ ! -f $base_dir/authors-cvs.orig ]]; then
  cd ../open-cobol.git || exit 1
  git log --format='%aN' --all | sort -u > $base_dir/authors-cvs.orig
fi
