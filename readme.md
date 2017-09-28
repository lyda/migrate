# Migration Notes

These are possible steps in the migration.

1. First, put `master` in this repo into a branch called
   `meta/2017-migration`.  Make this the current default branch.
   (done)
2. Second, put the `master` of the CVS import into a branch called
   `cvs/import-review`  (done)
3. Third, put the `master` of the subversion import into a branch called
   `svn/import-review`  Note: see *Subversion import* section below.  (done)

At this point the two `-review` branches should be reviewed.  It's
important to note that there will be multiple [for-each-ref][1] runs
over both branches.  Those will be recorded and saved in a migration
script in the `meta/2017-migration` branch.  However pulls from these
branches will need to be from scratch for each pass as the hashes will
be forcibly pushed.

We will be looking for the following things:

1. Fixing email addresses.  This is more an issue for the CVS tree
   as email address fixes will be done in `authors-svn.list` for the
   subversion import.  (done)
2. Removing generated code and other innappropriate files.

Mapping of subversion revision numbers to git hashes is possible with
`git svn log --show-commit --oneline`. We could, if desired, generate tags
pointing to specific svn revisions that are deemed important.

Next with the migration script ready, we set the subversion tree to
read-only.  The git repo is left with only the `meta/2017-migration`
branch.  We then import the CVS and subversion trees and run the
migration script.

## Subversion Migration

These steps need to be done before pushing into the `svn/import-review`
branch. They will also need to be done before the actual migration
post-review.

1. The `git svn` import put svn tags into branches. Turn these into
   actual tags.  Remove the old branches.
2. The svn ignore metadata needs to be converted into `.gitignore` files.
3. Local branches need to be generated from the remaining branches.
4. The tags `cvs/master` and `svn/master` will be set to the imported
   heads of their respective repo.

Next up is to link up the CVS to the svn tree.  This involves using the
`git replace` [command][2] to graft the two trees together.  As part of
this an `svn/first` tag will be set to the initial commit of the svn tree.

The resulting repository will actually contain three repos:

* `meta/2017-migration` this branch with all the data, scripts and
  historical information on how the migration was done.
* `cvs/master` the complete CVS history as imported.
* `master` This is the ongoing head of development built on the
  complese subversion import.  And while it is a standalone tree, there's
  a graft between it and the CVS tree that all git commands will follow
  as if the trees really are connected.

Note that operations that work over a range of commits, you can limit
it to just the subversion and current history by limiting the range to
`svn/first..master`.

This way you'll get a complete view of the history with the ability to trace back changes all the way back to the beginning.

TODO: Provide a `git blame` example that might provide a clearer view
of where a feature actually originated.

TODO: Provide a `git bisect` example.

[1]: https://git-scm.com/docs/git-for-each-ref [2]:
https://git-scm.com/blog/2010/03/17/replace.html
