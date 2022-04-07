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
