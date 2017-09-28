#!/bin/bash

. ./common || exit 1

cd "$migration_work_dir"

git remote add github git@github.com:lyda/migrate.git

# TODO: Add remote delete branches here.

git push github cvs/import-review
git push github svn/import-review
