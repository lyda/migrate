#!/bin/bash

. ./common || exit 1

if [[ -d "$migration_dir" ]]; then
  echo "Press C-c to abort generating a fresh trial migration tree"
  echo "or press return to continue."
  read dummy
fi
if [[ -d "$migration_dir" ]]; then
  rm -rf "$migration_dir"
fi

git init --bare "$migration_dir"

# Push migration branch.
git push "$migration_dir" meta/2017-migration

# Push CVS branch.
cd "$cvs_git_dir"
git push "$migration_dir" master:cvs/import-review

# Push SVN branch.
## Remove workdir to redo the svn migrations and still leave
## the original svn import repo untouched.
if [[ -d "$svn_work_dir" ]]; then
  rm -rf "$svn_work_dir"
fi
cp -a "$svn_git_dir" "$svn_work_dir"

## Do svn->git cleanups.
cd "$svn_work_dir"

### Create branches.
for branch in $(git branch -r | grep origin/ | grep -v /tags/ \
                  | grep -v /trunk | sed s@origin/@@); do
  git branch $branch origin/$branch
done

### Create tags.
for tag in $(git branch -r | grep origin/tags/ sed s@origin/@@); do
  git tag -m "SVN converted tag: $tag" $tag origin/tags/$tag
done

### svn:ignore -> .gitignore
for branch in $(git for-each-ref --format='%(refname:short)' refs/heads); do
  git checkout "$branch"
  svn_branch=$branch
  test "$branch" = master && svn_branch=trunk
  git svn show-ignore -i $svn_branch > .gitignore
  git add .gitignore
  git commit -m "Recreate svn:ignores in git."
done

git checkout master
git push "$migration_dir" master:svn/import-review

# Configure the repo.
cd "$migration_dir"
git symbolic-ref HEAD refs/heads/meta/2017-migration

# Make the work dir.
if [[ -d "$migration_work_dir" ]]; then
  rm -rf "$migration_work_dir"
fi
git clone "$migration_dir" "$migration_work_dir"
