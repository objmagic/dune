  $ rm -rf x c* symlink*

  $ mkdir -p c1
  $ cd c1 && ln -s . x
  $ cd c1 && ln -s . y
  $ cd c1 && dune build
  Path . has already been scanned. Cannot scan it again through symlink x
  [1]

  $ mkdir -p c2/{a,b}
  $ cd c2 && ln -s ../b a/x
  $ cd c2 && ln -s ../a b/x
  $ cd c2 && dune build
  Path a has already been scanned. Cannot scan it again through symlink a/x/x
  [1]

  $ mkdir symlink-outside-root
  $ cd symlink-outside-root && ln -s ../sample-exe sample
  $ cd symlink-outside-root && jbuilder exec --root . -- sample/foo.exe
  foo

  $ mkdir -p symlink-outside-root2/{root,other/{a,b}}
  $ cd symlink-outside-root2 && ln -s ../b other/a/x
  $ cd symlink-outside-root2 && ln -s ../a other/b/x
  $ cd symlink-outside-root2 && ln -s ../other root/src
  $ cd symlink-outside-root2/root && dune build
  Path b has already been scanned. Cannot scan it again through symlink src/a/x/x/x
  [1]
