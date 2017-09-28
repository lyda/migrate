#!/bin/bash

. ./common || exit 1

cd "$migration_work_dir"
git checkout svn/import-review

# Remove generated files.
$base_dir/bfg.sh -D '{configure,parser.c}'
$base_dir/bfg.sh -D '{printcbl|gnucobol|opencobol}.pdf'

# Do the final replace to graft the svn and cvs trees together.
git tag -a -m "First Subversion commit" \
        svn/first-commit $(git rev-list --max-parents=0 HEAD)
git replace --graft $(git rev-list --max-parents=0 HEAD) cvs/import-review
