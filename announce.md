Per a conversation with Simon I went and combined the CVS and subversion
repos into a [single one](https://github.com/lyda/migrate). I think
Simon's intent was an subversion one, but we can look into migrating it
back afterwards. It's just easier (for me at least) to manipulate repos
in git.

This repo is split into three branches:
* `meta/2017-migration` - Which contains the notes and scripts needed
  to do the migration.
* `cvs/import-review` - this is the imported CVS main branch. It contains
  all the commits going back to 2002.
* `svn/import-review` - this is the imported subversion trunk. It is
  grafted onto the CVS tree. However the first subversion commit is
  marked with the `svn/first` tag.

What's not done are `filter-branch` runs to remove generated files. I'm
not sure what's safe to remove and not. Suggestions from Simon would
be helpful. That said, the entire tree when cloned is only 18M. Though
that will grow as we add in the other branches. Nowhere near the 195M
that the full subversion repo is though.

In addition, the `authors-svn.list` file in the `meta/2017-migration`
could probably be made more accurate.

All of the CVS and subversion branches are re-created. For the review
process I only published the main branches of the two trees as I need
to delete/recreate the `import-review` branches as I get feedback on this.

One thing to try is this:

```
git clone https://github.com/lyda/migrate.git
cd migrate
git checkout svn/import-review
git blame -w -C -M log cobc/cobc.c
```

This will show changes to `cobc.c` going all the way back to 2002. The
`-w` is like in `diff` (ignores whitespace) and the `-M` and `-C` flags
are for following lines better as they move within and between files. So
with this if you really want to know why some code is in a file you can
track it all the way back to its first commit.

PS  Regarding the large files, I ran this in the full subversion
    repository (which has all the branches and tags). They do all seem
    to be generated, yes.

```
git rev-list --objects --all \
  | grep "$(git verify-pack -v .git/objects/pack/*.idx | sort -k 3 -n \
              | tail -20 | awk '{print$1}')"
c7194ad8201b7e9371e7548d54dfc17c97d3f87c ABOUT-NLS
0c055ab17fb9cfcd6723d540d20ac94661d3af93 cobc/parser.c
07c2206b3b3e167bd57be2fd1dcf2b6dcb8bbb9c tests/run
f8321bc1d29f7767527f89aede5d82909562def4 cobc/parser.c
1c4151061781a61d9060de467e1f90ecfef4e408 configure
f5fb97af454bbb0aae61ac95d9dbc17f51f6ad67 configure
75c4f0cf270a47dd778b8e9e6b41d3c8c3175614 tests/testsuite
6b3e79dec5f301a36f0ee75d8fefe8fb89ec8813 cobc/parser.c
5e325aba22c0e61c6e93d4820ae1bd8268310605 tests/testsuite
85c845a4f21d6df39e79bce0f66879b1456fdc0a cobc/parser.cpp
4b3efe84ee5052dff2f3540d28f354ab40d1f851 tests/testsuite
b2d503eb96fd5280bfe6daa96b2a7a589e9d8604 tests/testsuite
75573d05a438be0e6ca9f79a3b3c99f48a7c716d cobc/parser.c
d9e4df4cea5f73358dfd699e26968f98674bba0a cobc/parser.c
f847cbf4ff495d1ee53c105b6e410449302410f9 cobc/parser.c
635a12214c83ce677010afd2e4aad5254af82256 cobc/parser.c
edf04b46c3887614112904cb49e63f86619ffb21 tests/testsuite
843d6078fca7431171c5a948874f47d016c17476 cobc/parser.c
2e60131091b190a2012cc2c6a6f5de28c0a7148b tests/run
48c83598a33d41920612f5f05443f6243c5b6b9b configure
```
