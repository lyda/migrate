#!/bin/bash

. ./common || exit 1

cd "$migration_work_dir"
git checkout svn/import-review

# Do the final replace to graft the svn and cvs trees together.
git tag -a -m "First Subversion commit" $(git rev-list --max-parents=0 HEAD) svn/first-commit
git replace --graft $(git rev-list --max-parents=0 HEAD) cvs/import-review
