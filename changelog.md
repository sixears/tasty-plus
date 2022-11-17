1.5.2.11 2022-11-17
===================
- upgrade to callPackage-based versions

1.5.2.10 2022-11-13
===================
- fix fixed-package-name typo in flake-build-utils

1.5.2.9 2022-11-04
==================
- fix package names

1.5.2.8 2022-11-03
==================
- remove redundant "output" flake-utils

1.5.2.7 2022-11-03
==================
- flake-build-utils->1.0.0.6

1.5.2.6 2022-11-03
==================
- base0->0.0.4.2; base0t->0.0.1.2

1.5.2.5 2022-11-03
==================
- base0->0.0.4.1; base0t->0.0.1.1

1.5.2.4 2022-11-02
==================
- natural->0.0.1.2

1.5.2.3 2022-11-02
==================
- more-unicode -> 0.0.17.2

1.5.2.2 2022-11-02
==================
- upgrade flake-build-utils to 1.0.0.3

1.5.2.1 2022-10-27
==================
- add flake
- use ghc-8.10.7 for tfmt

1.5.2.0 2022-10-07
==================
- add TestCmp, lits, litt, shrinkList, shrinkText

1.5.1.1 2022-04-07
==================
- upgrade dependencies

1.5.1.0 2021-07-24
==================
- add assertJust, assertIsJust

1.5.0.2 2021-07-03
==================
- fix testInTempDir to actually cd to tempdir as advertised

1.5.0.1 2021-06-09
==================
- use tasty >= 1.3 with amended suiteOptionParser typesig

1.5.0.0 2021-05-02
==================
- remove assertIOException*; and dependency on monaderror-io

1.4.3.0 2021-05-01
==================
- add assertIOError; deprecate assertIOException*: towards removing dependency
  on monaderror-io

1.4.2.0 2021-04-26
==================
- add testInTempDir

1.4.1.0 2021-04-20
==================
- add withResourceCleanup

1.4.0.0 2020-09-15
==================
- add Equish, assertCmp', assertListCmp, withResource2{,'}

1.3.0.0 2020-02-15
==================
- better Printable handling for property tests; also add some tests

1.2.0.0 2020-02-14
==================
- use Printable rather than Show for many functions

1.0.1.0 2019-09-16
==================
- merged in from fpath: add propAssociative, propInvertibleString,
  propInvertibleString, propInvertibleUtf8, runTestTree', runTestsReplay,
  mainTests
