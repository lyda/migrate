#!/bin/bash

. ./common || exit 1

# CVS source: https://sourceforge.net/p/open-cobol/cvs/

if [[ ! -d $cvs_dir ]]; then
  mkdir $cvs_dir
  cd $cvs_dir
  cvs -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol login
  cvs -z3 -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol co -P .
  git cvsimport -C $cvs_git_dir open-cobol
  cd $base_dir
fi

if [[ ! -f $base_dir/authors-cvs.orig ]]; then
  cd $cvs_git_dir || exit 1
  git log --format='%aN' --all | sort -u > $base_dir/authors-cvs.orig
fi
