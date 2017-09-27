#!/bin/bash

. ./common || exit 1

if false; then
  # This is best to do in the svn-imported dir but i don't want to
  # screw up my import each time i run.  Will need to think about this,
  # so currently blocked off.
  exit 1
  cd $svn_work_dir

  # svn:ignore -> .gitignore
  # TODO: loop through branches on this?
  git svn show-ignore -i trunk > .gitignore
  git add .gitignore
  git commit -m "Recreate svn:ignores in git."

  # TODO: From here, can be done out of subversion tree.

  # TODO: What to do with these:
  #         - remotes/origin/fileiorewrite@61
  #         - remotes/origin/tags/gnu-cobol-1.1@177
  #         - remotes/origin/tags/gnu-cobol-1.1@98
  #         - remotes/origin/tags/open-cobol-ce@97

  # TODO: Fix the list of branches being converted here instead of
  #       looking them up.
  git for-each-ref --format='%(refname)' refs/heads/tags |
    cut -d/ -f4 |
    while read ref; do
      git tag -a -m "$ref" "$ref" "refs/heads/tags/$ref";
      git branch -D "tags/$ref";
    done
fi

cd "$migration_work_dir"
git checkout svn/import-review

# Do the final replace to graft the svn and cvs trees together.
git tag -a -m "First Subversion commit" $(git rev-list --max-parents=0 HEAD) svn/first-commit
git replace --graft $(git rev-list --max-parents=0 HEAD) cvs/import-review
