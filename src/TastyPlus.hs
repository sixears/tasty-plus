{- |

Description: utility functions for working with Tasty testing

-}

module TastyPlus
  ( TastyOpts( TastyOpts ), TastyRunResult(..)

  , (#?=), (#=?), (≣), (≟)

  , optParser, tastyOptParser

  , assertAnyException, assertAnyExceptionIO
  , assertCmp'
  , assertException, assertExceptionIO
  , assertIOError
  , assertJust, assertIsJust, assertIsLeft, assertLeft, assertRight
  , assertListCmp, assertListCmpIO
  , assertListEq, assertListEqIO, assertListEqIO'
  , assertListEq', assertListEqR, assertListEqR', assertListEqRS
  , assertListEqS
  , assertSuccess

  , ioTests, mainTests

  , propAssociative
  , propInvertibleString, propInvertibleText, propInvertibleUtf8

  , runTests, runTests_, runTestsP, runTestsP_, runTestTree, runTestTree'
  , runTestsReplay

  , testInTempDir
  , withResource', withResourceCleanup, withResource2, withResource2'

  -- for testing this module
  , tests
  )
where

import Prelude  ( (*), fromIntegral )

-- base --------------------------------

import Control.Applicative     ( (<*>) )
import Control.Exception       ( Exception, SomeException
                               , evaluate, handle, onException )
import Control.Monad           ( (>>=), return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( bool )
import Data.Eq                 ( Eq( (==) ) )
import Data.Foldable           ( Foldable, concatMap, length, toList )
import Data.Function           ( ($), const, flip )
import Data.Functor            ( fmap )
import Data.Int                ( Int )
import Data.List               ( intercalate, zip, zipWith3 )
import Data.Maybe              ( fromMaybe  )
import Data.Monoid             ( (<>), mempty )
import Data.Ratio              ( Rational )
import Data.Tuple              ( snd )
import GHC.Generics            ( Generic )
import GHC.Stack               ( CallStack, HasCallStack, callStack )
import Numeric.Natural         ( Natural )
import System.Environment      ( getProgName )
import System.Exit             ( ExitCode( ExitFailure, ExitSuccess ) )
import System.IO               ( FilePath, IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print ), Textual
                     , parseString, parseText, parseUtf8, toString, toText
                     , toUtf8
                     )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData, force )

-- directory ---------------------------

import System.Directory  ( getCurrentDirectory, getTemporaryDirectory
                         , removePathForcibly, setCurrentDirectory )

-- exited ------------------------------

import Exited  ( Exited( Exited ), doMain', exitWith )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹, pattern 𝕿 )
import Data.MoreUnicode.Either   ( 𝔼, pattern 𝕽, pattern 𝕷 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad    ( (⪼), (≫) )
import Data.MoreUnicode.String   ( 𝕊 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT, throwError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )
import Options.Applicative.Types    ( Parser )

-- safe --------------------------------

import Safe  ( atMay )

-- tasty -------------------------------

import Test.Tasty          ( TestName, TestTree
                           , defaultIngredients, testGroup, withResource )
import Test.Tasty.Options  ( OptionSet )
import Test.Tasty.Runners  ( TestPattern, suiteOptionParser, tryIngredients )
import Test.Tasty.Options  ( singleOption )
import Test.Tasty.Runners  ( parseTestPattern )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion
                         , (@=?), (@?=), assertBool, assertFailure, testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, QuickCheckReplay( QuickCheckReplay )
                              , (===), testProperty )

-- temporary ---------------------------

import System.IO.Temp  ( createTempDirectory )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-------------------------------------------------------------------------------

{- | An exception for testing with, including NFData & CallStack. -}
data AnException = AnException 𝕊 CallStack
  deriving (Generic,NFData,Show)

instance Exception AnException

instance Eq AnException where
  AnException s _ == AnException s' _ = s == s'

anException ∷ HasCallStack ⇒ 𝕊 → AnException
anException s = AnException s callStack

throwE ∷ MonadError AnException η ⇒ 𝕊 → η ()
throwE s = throwError $ anException s

st ∷ AnException → 𝕊
st (AnException s _) = s

------------------------------------------------------------

-- | we need to provide a test tree to the optparser, anrd also to runTests;
--   to ensure consistency we encompass them in a single TastyOpts datum

data TastyOpts = TastyOpts { testTree ∷ TestTree, optSet ∷ OptionSet }

------------------------------------------------------------

data TastyRunResult = TestSuccess | TestsFailed | TestRunFailure
  deriving (Eq, Show)

rrExitCode ∷ TastyRunResult → ExitCode
rrExitCode TestSuccess    = ExitSuccess
rrExitCode TestsFailed    = ExitFailure 1
rrExitCode TestRunFailure = ExitFailure 2

------------------------------------------------------------

newtype ShowEqPrintable α = ShowEqPrintable α
  deriving Eq

instance Printable α ⇒ Show (ShowEqPrintable α) where
  show (ShowEqPrintable a) = toString a

{- | Pronounced 'test', this tests for equality; it's a variant of `(@=?)` that
     uses `Printable` rather than `Show` for error messages; note that the 'got'
     or 'actual' value is the last argument, to allow for easier partial
     application.
 -}
infix 1 ≟
(≟) ∷ (Eq α, Printable α) ⇒ α → α → Assertion
x ≟ y = ShowEqPrintable x @=? ShowEqPrintable y

----------------------------------------

{- | Almost-synonym for `===`, but using `Printable` instead of `Show`. -}
infix 4 ≣
(≣) ∷ (Eq α, Printable α) ⇒ α → α → Property
x ≣ y = ShowEqPrintable x === ShowEqPrintable y

----------------------------------------

{- | Unconditionally signals success. -}
assertSuccess ∷ Text → Assertion
assertSuccess t = assertBool (toString t) 𝕿

----------------------------------------

{- | Run some tests with given tasty options; return success if all pass, 1 if
     some fail, 2 if setup failed -}
runTests_ ∷ MonadIO μ ⇒ TastyOpts → μ TastyRunResult
runTests_ (TastyOpts{..}) =
  liftIO $ case tryIngredients defaultIngredients optSet testTree of
    𝕵 run_tests → bool TestsFailed TestSuccess ⊳ run_tests
    𝕹        → return TestRunFailure

{- | Run some tests, return exit code on failure (0 = success; 1 = some tests
     failed; 2 = failed to run). -}
runTests ∷ MonadIO μ ⇒ TastyOpts → μ ExitCode
runTests = (rrExitCode ⊳) ∘ runTests_

----------------------------------------

{- | Run a test tree, with default options. -}
runTestTree_ ∷ MonadIO μ ⇒ TestTree → μ TastyRunResult
runTestTree_ tree = runTests_ (TastyOpts tree mempty)

----------------------------------------

{- | Run a test tree, with default options.  See `runTests` for exit codes. -}
runTestTree ∷ MonadIO μ ⇒ TestTree → μ ExitCode
runTestTree tree = rrExitCode ⊳ runTestTree_ tree

----------------------------------------

{- | Run a test tree, with default options.  Designed for simple "main"
     invocations, e.g., as part of t/*.hs -}
runTestTree' ∷ TestTree → IO ()
runTestTree' = doMain' ∘ runTestTree

----------------------------------------

{- | Run tests, with a given pattern (use "" to run everything). -}
runTestsP_ ∷ (MonadIO μ) ⇒ TestTree → 𝕊 → μ TastyRunResult
runTestsP_ ts "" =
  runTests_ (TastyOpts ts mempty)
runTestsP_ ts pat =
  case parseTestPattern $ toString pat of
    𝕵 p  → runTests_ (TastyOpts ts (singleOption p))
    𝕹 → return TestSuccess

----------------------------------------

{- | Run tests, with a given pattern (use "" to run everything). -}
runTestsP ∷ (MonadIO μ) ⇒ TestTree → 𝕊 → μ ExitCode
runTestsP ts pat = rrExitCode ⊳ runTestsP_ ts pat

----------------------------------------

runTestsReplay_ ∷ TestTree → 𝕊 → Natural → IO TastyRunResult
runTestsReplay_ ts s r = do
  let replayO ∷ Natural → OptionSet
      replayO = singleOption ∘ QuickCheckReplay ∘ 𝕵 ∘ fromIntegral
      tryOpt ∷ TestPattern → TestTree → 𝕄 (IO 𝔹)
      tryOpt p = tryIngredients defaultIngredients $
                     singleOption p ⊕ replayO r

  case parseTestPattern s of
    𝕵 p  → fromMaybe (return TestRunFailure) $
                fmap (bool TestsFailed TestSuccess) ⊳ tryOpt p ts
    𝕹 → return TestRunFailure

{- | Run some tests (matching a pattern) with a replay code.  Use "" to run
     all tests -}
runTestsReplay ∷ TestTree → 𝕊 → Natural → IO ExitCode
runTestsReplay ts s r = rrExitCode ⊳ runTestsReplay_ ts s r

----------------------------------------

-- | provide an OptParse-Applicative Parser for tasty options (to allow for
--   integration of tasty testing & options into an executable)
optParser ∷ TestTree → Parser TastyOpts
optParser testTree =
  TastyOpts testTree ⊳ snd (suiteOptionParser defaultIngredients testTree)

-- | alternate name for client convenience
tastyOptParser ∷ TestTree → Parser TastyOpts
tastyOptParser = optParser

----------------------------------------

{- | Wrapper for tests as main, e.g., in t/*.hs . -}
mainTests ∷ MonadIO μ ⇒ 𝕊 → TestTree → μ ()
mainTests desc ts = do
  tastyOpts ← liftIO $
              customExecParser (prefs showHelpOnError) $
                info (helper <*> tastyOptParser ts)
                     (fullDesc ⊕ progDesc desc ⊕ failureCode 254)

  Exited ← runTests tastyOpts >>= exitWith
  return ()

----------------------------------------

assertCmp' ∷ HasCallStack ⇒
             (α → Text) → (β → Text) → (α → β → 𝔹) → α → 𝕄 β → Assertion
assertCmp' toTa _ _ expected 𝕹 =
       assertFailure ("expected: " ⊕ toString (toTa expected)
                                   ⊕ "\nbut got Nothing")
assertCmp' toTa toTb cmp expected (𝕵 got) =
  let toSa = toString ∘ toTa
      toSb = toString ∘ toTb
   in -- equalize prefix lengths to make it easier to diff strings, etc.
       assertBool ("expected: " ⊕ toSa expected ⊕ "\nbut got : " ⊕ toSb got)
                  (cmp expected got)

{- | Compare two lists for compatibility, with customized, itemized testing.
     We take the inputs as IO to allow for, well, IO.
 -}
assertListCmpIO ∷ (Foldable ψ, Foldable φ, Printable σ, HasCallStack) ⇒
                    (α → Text) → (β → Text) → (α → β → 𝔹) → σ → ψ α → IO (φ β)
                  → TestTree
assertListCmpIO toTa toTb cmp name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) =
        testCase (show i)
                 (got ≫ \ g → assertCmp' toTa toTb cmp e (atMay g i))

   in testGroup (toString name) $
          testCase "count" (got ≫ lengthCheck expect)
        : (assertItem ⊳ zip [0..] expect)

{- | Compare two lists for equality, with itemized testing and IO. -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ, HasCallStack) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → TestTree
assertListEqIO' toT = assertListCmpIO toT toT (≡)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α, HasCallStack) ⇒
                Text → ψ α → IO (φ α) → TestTree
assertListEqIO = assertListEqIO' toText

--------------------

{- | Compare two lists for compatibility, with itemized testing. -}
assertListCmp ∷ (Foldable ψ, Foldable φ, Printable σ, HasCallStack) ⇒
                  (α → Text) → (β → Text) → (α → β → 𝔹) → σ → ψ α → φ β
                 → TestTree
assertListCmp toTa toTb cmp name exp got =
  assertListCmpIO toTa toTb cmp name exp (return got)

--------------------

{- | Compare two lists for equality, with itemized testing. -}
assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ, HasCallStack) ⇒
               Text → ψ α → φ α → TestTree
assertListEq name expect got = assertListEqIO name expect (return got)

--------------------

assertListEqTests ∷ TestTree
assertListEqTests =
  assertListEq "listTest" [ "foo", "bar", "baz" ∷ 𝕊 ]
                          [ "foo", "bar", "baz" ]

assertListEqTestsF ∷ TestTree
assertListEqTestsF =
  testGroup "assertListEq fail" $
    [ assertListEq "listTest<" [ "foo", "bar" ]
                               [ "foo", "bar", "baz" ∷ Text ]
    , assertListEq "listTest>" [ "foo", "bar", "baz" ]
                               [ "foo", "bar" ∷ Text ]
    , assertListEq "listTest!" [ "foo", "bar", "baz" ]
                               [ "foo", "rab", "baz" ∷ 𝕊 ]
    ]

----------------------------------------

-- | like `assertListEq`, but using Show rather than Printable
assertListEqS ∷ (Foldable ψ, Foldable φ, Eq α, Show α) ⇒
                 𝕊 → ψ α → φ α → [TestTree]
assertListEqS = assertListEq' (pack ∘ show)

----------------------------------------

-- | compare two lists for equality, with itemized testing
assertListEq' ∷ (Foldable ψ, Foldable φ, Eq α) ⇒
                (α → Text) → 𝕊 → ψ α → φ α → [TestTree]
assertListEq' toT name gotL expectL =
  let got    = toList gotL
      expect = toList expectL
      lCheck g e =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck g e = lCheck (length g) (length e)
      assertItem gt exp i = let nm = name <> ": " <> show i
                             in testCase nm $ assertEq' toT gt exp
   in   testCase (name <> ": count") (lengthCheck got expect)
      : zipWith3 assertItem got expect [1 ∷ Int ..]

----------------------------------------

-- | like `assertListEq`, but takes an Either which must be a Right
assertListEqR ∷ (Foldable ψ, Foldable φ, Eq α, Printable α, Show ε) ⇒
                 𝕊 → 𝔼 ε (ψ α) → (φ α) → [TestTree]
assertListEqR = assertListEqR' toText

--------------------

assertListEqRTests ∷ TestTree
assertListEqRTests =
  testGroup "assertListEq" $
    assertListEqR "listTestR"
                  (𝕽 [ "foo", "bar", "baz" ] ∷ 𝔼 𝕊 [𝕊])
                  [ "foo", "bar", "baz" ]

assertListEqRTestsF ∷ TestTree -- tests that should fail!
assertListEqRTestsF =
  testGroup "assertListEq fail" $
    assertListEqR "listTestR" (𝕷 "weebles" ∷ 𝔼 𝕊 [𝕊])
                              [ "foo", "bar", "baz" ]

----------------------------------------

-- | like `assertListEq`, but takes an Either which must be a Right
assertListEqR' ∷ (Foldable ψ, Foldable φ, Eq α, Show ε) ⇒
                 (α → Text) → 𝕊 → 𝔼 ε (ψ α) → φ α → [TestTree]
assertListEqR' toT name got expect =
  case got of
    𝕷  e → [testCase name (assertFailure ("got a Left: " <> show e))]
    𝕽 r → assertListEq' toT name r expect

----------------------------------------

assertListEqRS ∷ (Foldable ψ, Foldable φ, Eq α, Show ε, Show α) ⇒
                  𝕊 → 𝔼 ε (ψ α) → φ α → [TestTree]
assertListEqRS = assertListEqR' (pack ∘ show)

----------------------------------------

-- | test that we got a 'Right' value, satisfying the given assertion
assertRight ∷ Show γ ⇒ (ρ → Assertion) → 𝔼 γ ρ → Assertion
assertRight assertion got =
  case got of 𝕽 g → assertion g
              𝕷  e → assertFailure (show e)

--------------------

assertRightTests ∷ TestTree
assertRightTests =
  testGroup "assertRight"
    [ testCase "right" $
      assertRight ((@?= 4) ∘ length) (𝕽 "good" ∷ 𝔼 Int 𝕊)
    ]

assertRightTestsF0 ∷ TestTree
assertRightTestsF0 =
  testGroup "assertRight fail (0)"
    [ testCase "right" $
      assertRight ((@?= 4) ∘ length) (𝕷 7 ∷ 𝔼 Int 𝕊)
    ]

assertRightTestsF1 ∷ TestTree
assertRightTestsF1 =
  testGroup "assertRight"
    [ testCase "right fail (1)" $
      assertRight ((@?= 4) ∘ length) (𝕽 "bad" ∷ 𝔼 Int 𝕊)
    ]


----------------------------------------

{- | Test that we got a 'Left' value, satisfying the given assertion. -}
assertLeft ∷ Show ρ ⇒ (γ → Assertion) → 𝔼 γ ρ → Assertion
assertLeft assertion got =
  case got of 𝕽 r → assertFailure (show r)
              𝕷 l → assertion l

----------------------------------------

{- | Merely check that a value is a Left <something>. -}
assertIsLeft ∷ Show β ⇒ 𝔼 α β → Assertion
assertIsLeft = assertLeft (const $ assertSuccess "is Left")

----------------------------------------

{- | Test that we got a 'Just' value, satisfying the given assertion. -}
assertJust ∷ (γ → Assertion) → 𝕄 γ → Assertion
assertJust assertion got =
  case got of 𝕹 → assertFailure "got Nothing"
              𝕵 x → assertion x

----------------------------------------

{- | Merely check that a value is a Just <something>. -}
assertIsJust ∷ 𝕄 α → Assertion
assertIsJust = assertJust (const $ assertSuccess "is Just")

----------------------------------------

{- | Note that this is to check errors thrown within IO; use `assertIOError` to
     check for `MonadError`/`ExceptT ε IO` errors. -}
assertExceptionIO ∷ (NFData α) ⇒ 𝕊 → (SomeException → 𝔹) → IO α → IO ()
assertExceptionIO n p io =
  handle (return ∘ 𝕷) (𝕽 ⊳ (io >>= evaluate ∘ force)) >>= \ case
    𝕷 e → assertBool n (p e)
    𝕽 _ → assertFailure ("no exception thrown: " ⊕ n)

{- | Check that an exception is thrown.  Any exception that is thrown is
     checked by the given predicate; the predicate pass to indicate that the
     exception is as desired; and thus a @False@ will cause a test failure.  The
     test itself, if it returns a value (without an exception) will pass; but
     note that being IO, it can itself run tests...
 -}
assertException ∷ (NFData α) ⇒ 𝕊 → (SomeException → 𝔹) → α → IO ()
assertException n p v = assertExceptionIO n p (return v)

{- | Check that some (any) exception is thrown.  Any exception will cause the
     test to pass; no exception will cause it to fail.
 -}
assertAnyException ∷ (NFData α) ⇒ 𝕊 → α → IO ()
assertAnyException n = assertException n (const 𝕿)

assertAnyExceptionIO ∷ (NFData α) ⇒ 𝕊 → IO α → IO ()
assertAnyExceptionIO n = assertExceptionIO n (const 𝕿)

----------------------------------------

{- | Test that an ExceptT IO throws an expected error. -}
assertIOError ∷ Show ρ ⇒ (ε → Assertion) → ExceptT ε IO ρ → Assertion
assertIOError p io = (runExceptT io) >>= assertLeft p

assertIOErrorTests ∷ TestTree
assertIOErrorTests =
  testCase "assertIOError" $ assertIOError (\ e → "z" ≟ st e) $ throwE "z"

-- these tests should fail
assertIOErrorTestsF ∷ TestTree
assertIOErrorTestsF =
  testGroup "assertIOError - fail"
    [ -- prefix test name with '!' to indicate that it should fail
      testCase "wrong exception" $
        assertIOError (\ e → "y" ≟ st e) $ throwE "z"
    , testCase "no exception" $
        assertIOError (\ _ → assertBool "const" 𝕿) $ return ()
    ]

----------------------------------------

assertEq' ∷ (Eq t) ⇒ (t → Text) → t → t → Assertion
assertEq' toT e e' =
  let toS = toString ∘ toT
      -- extra space in 'but got ' to align with 'expected'
   in assertBool ("expected: " ⊕ toS e' ⊕ "\nbut got : " ⊕ toS e) (e ≡ e')

-- | a bit like `assertEqual`, but generates its own message based on `toText`
--   of the arguments (because that may be more readable than show).

assertEq ∷ (Eq t, Printable t) ⇒ t → t → Assertion
assertEq = assertEq' toText

-- | infix version of `assertEq` thus akin to `@=?` (but using `toText` rather
--   than `show`)

(#=?) ∷ (Eq t, Printable t) ⇒ t → t → Assertion
(#=?) = flip assertEq

-- | infix version of `assertEq` thus akin to `@?=` (but using `toText` rather
--   than `show`)
(#?=) ∷ (Eq t, Printable t) ⇒ t → t → Assertion
(#?=) = assertEq

--------------------

assertEqTests ∷ TestTree
assertEqTests =
  testGroup "assertEq" [ testCase "foo" $ "foo" #=? ("foo" ∷ 𝕊) ]

assertEqTestsF ∷ TestTree
assertEqTestsF =
  testGroup "assertEqF" [ testCase "foo" $ "foo" #=? ("bar" ∷ Text) ]

----------------------------------------

{- | Construct a test group, wherein each test is passed a value that has been
     pre-initialized in some IO.  Note that the IO is not run for each test, it
     is run no more than once (and that, of course, only if the tests are run).
 -}
ioTests ∷ TestName → [(TestName, α → Assertion)] → IO α → TestTree
ioTests name ts ioa =
  testGroup name $ (\ (tname,t) → testCase tname $ ioa >>= t) ⊳ ts

----------------------------------------

{- | like `withResource`, but with a no-op release resource -}
withResource' ∷ IO α → (IO α → TestTree) → TestTree
withResource' = flip withResource (const $ return ())

----------------------------------------

withResource2 ∷ IO α → (α → IO()) → IO β → (β → IO ()) → (IO α → IO β →TestTree)
              → TestTree
withResource2 gain lose gain' lose' ts =
  withResource gain lose (\ x → withResource gain' lose' (\ x' → ts x x'))

----------------------------------------

withResource2' ∷ IO α → IO β → (IO α → IO β → TestTree)
              → TestTree
withResource2' gain gain' ts =
  withResource' gain (\ x → withResource' gain' (\ x' → ts x x'))

----------------------------------------

{- | For complex tests that need an IO setup; where having acquired the
     resource, the setup may fail; regular `withResource` doesn't clean that up
     (since the acquire step throws an exception, the return value is never seen
     and so cannot be passed to the release); this version splits acquisition
     and setup.

     If acquisition fails, there should be nothing to release.
     But if setup fails, the release is called (though of course no tests are
     run). If the setup succeeds, tests are run, and cleanup is called.
 -}
withResourceCleanup ∷ IO α → (α → IO ()) → (α → IO ()) → (IO α → TestTree)
                    → TestTree
withResourceCleanup acquire setup release test =
  let -- safely acquire and run setup; if setup throws an IOException, release
      -- acquireAndSetup ∷ IO α
      acquireAndSetup = acquire ≫ \ resource → do
        onException (setup resource) (release resource)
        return resource
   in withResource acquireAndSetup release test

------------------------------------------------------------

{- | Perform tests within a temporary directory, with a bespoke temp dir setup
     function.  The setup function is called with the name of the temp dir
     (as a filepath: FPath is not available here, as FPath uses TastyPlus); and
     the tempdir is also provided to the test (as an IO FilePath as a Tasty
     quirk).
 -}
testInTempDir ∷ (FilePath → IO()) → (IO FilePath → TestTree) → TestTree
testInTempDir setup doTests =
  withResourceCleanup
    -- Create a temp dir; return the name of the cwd, and the tmpdir.
    (do t ← getTemporaryDirectory
        p ← getProgName
        c ← getCurrentDirectory
        d ← createTempDirectory t p
        return (c,d))
    -- cd and then run the setup; we do the cd here because we can't do it
    -- before the createTempDirectory (since the dir won't exist), and we
    -- don't want to do it after (since the cd might fail, and we don't clean
    -- up on failure of acquisition; see `withResourceCleanup`.
    (\ (_,d) → setCurrentDirectory d ⪼ setup d)
    -- cd back to the original dir before we remove the temp dir
    (\ (c,d) → setCurrentDirectory c ⪼ removePathForcibly d)
    (\ io → doTests (snd ⊳ io))

-- Common Properties ---------------------------------------

newtype P α = P α
  deriving Eq

instance Printable α ⇒ Printable (P (Parsed α)) where
  print (P (Parsed a))       = P.string (toString a)
  print (P (Malformed [] s)) =
    let quote t = "'" <> t <> "'"
     in P.string $ "MALFORMED: " <> quote s
  print (P (Malformed ss s)) =
    let quote     t  = "'" <> t <> "'"
        bracketsp t  = "[ " <> t <> " ]"
        list    ts = bracketsp $ intercalate ", " (quote ⊳ ts)
     in P.string $ "MALFORMED: " <> quote s <> " " <> list ss

propInvertibleString ∷ (Eq α, Printable α, Textual α) ⇒ α → Property
propInvertibleString d = P (parseString (toString d)) ≣ P (Parsed d)

propInvertibleText ∷ (Eq α, Printable α, Textual α) ⇒ α → Property
propInvertibleText d = P (parseText (toText d)) ≣ P (Parsed d)

propInvertibleUtf8 ∷ (Eq α, Printable α, Textual α) ⇒ α → Property
propInvertibleUtf8 d = P (parseUtf8 (toUtf8 d)) ≣ P (Parsed d)

propAssociative ∷ (Eq α, Printable α) ⇒ (α → α → α) → α → α → α → Property
propAssociative f a b c = f a (f b c)  ≣ f (f a b) c

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_test0 ∷ IO TastyRunResult
_test0 = -- test that runTestsP correctly selects only the working tests
         runTestsP_ tests "simpleTest/t"

_test1 ∷ IO TastyRunResult
_test1 = runTestsP_ tests "normal"

----------------------------------------

{- | Simple tests, with a failure, to allow for a pattern to select only the
     passing tests.
-}

mkSimpleTests ∷ Foldable t ⇒
                t ((𝕊 → Int → Int → TestTree) → [TestTree]) → TestTree
mkSimpleTests ts =
  let tC ∷ 𝕊 → Int → Int → TestTree
      tC name got expect = testCase name $ got @?= expect
   in testGroup "simple" $ concatMap ($ tC) ts

----------------------------------------

simpleTestsS ∷ (𝕊 → Int → Int → TestTree) → [TestTree]
simpleTestsS tC = [ tC "two" 2 2, tC "three" 3 3 ]

----------------------------------------

simpleTestsF ∷ (𝕊 → Int → Int → TestTree) → [TestTree]
simpleTestsF tC = [ tC "one" 1 2 {- deliberate fail -} ]



----------------------------------------

unitTests ∷ TestTree
unitTests = testGroup "unitTests" [ hunitGroup ]

hunitGroup ∷ TestTree
hunitGroup = testGroup "hunit" [ mkSimpleTests [ simpleTestsS ]
                               , normalTests ]

normalTests ∷ TestTree
normalTests = testGroup "normal" [ assertEqTests
                                 , assertListEqRTests
                                 , assertListEqTests
                                 , assertRightTests
                                 , assertIOErrorTests
                                 ]

_failTests ∷ TestTree
_failTests =
  let failIt name tree = testCase name $ runTestTree_ tree >>= (@?= TestsFailed)
   in testGroup "fail"
                 [ failIt "simpleTests"         (mkSimpleTests [simpleTestsF])
                 , failIt "assertEq"            assertEqTestsF
                 , failIt "assertRightTestsF0"  assertRightTestsF0
                 , failIt "assertRightTestsF1"  assertRightTestsF1
                 , failIt "assertListEqTestsF"  assertListEqTestsF
                 , failIt "assertListEqRTestsF" assertListEqRTestsF
                 , failIt "assertIOErrorTestsF" assertIOErrorTestsF
                 ]

pTests ∷ TestTree
pTests =
  testGroup "P.normal"
            [ testCase "malformed (0)" $
                  "MALFORMED: 'foo'"
                ≟ toString (P $ Malformed @𝕊 [] "foo")
            , testCase "malformed (1)" $
                  "MALFORMED: 'foo' [ 'a', 'b', 'c' ]"
                ≟ toString (P $ Malformed @𝕊 ["a","b","c"] "foo")
            , testCase "parsed" $ "bar" ≟ toString (P $ Parsed @𝕊 "bar")
            ]

propTests ∷ TestTree
propTests =
  testGroup "prop.normal"
            [ testGroup "Rational - Invertible"
                        [ testProperty "String" (propInvertibleString @Rational)
                        , testProperty "Text"   (propInvertibleText   @Rational)
                        , testProperty "Utf8"   (propInvertibleUtf8   @Rational)
                        ]
            , testGroup "Rational - Associative"
                        [ testProperty "*" (propAssociative @Rational (*))]
            ]

_test ∷ IO ()
_test = do
  TestSuccess ← _test0
  TestSuccess ← _test1
  return ()

_ftest ∷ IO ()
_ftest = do
  TestSuccess ← runTestTree_ _failTests
  return ()

tests ∷ TestTree
tests = testGroup "tests" [ unitTests, pTests, propTests ]

-- that's all, folks! ----------------------------------------------------------
