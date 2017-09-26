#!/bin/bash

# See:
# https://sourceforge.net/p/open-cobol/cvs/
if [[ ! -d open-cobol ]]; then
  mkdir open-cobol
  cvs -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol login
  cvs -z3 -d:pserver:anonymous@open-cobol.cvs.sourceforge.net:/cvsroot/open-cobol co -P .
  git cvsimport -C ../open-cobol.git open-cobol
fi
