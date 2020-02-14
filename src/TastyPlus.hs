{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE ViewPatterns          #-}

{- |

Description: utility functions for working with Tasty testing

-}

module TastyPlus
  ( TastyOpts( TastyOpts ), TastyRunResult(..)

  , (#?=), (#=?), (≣), (≟)

  , optParser, tastyOptParser

  , assertAnyException, assertAnyExceptionIO
  , assertException, assertExceptionIO
  , assertIOException, assertIOException'
  , assertIsLeft, assertLeft, assertRight
  , assertListEq, assertListEqIO, assertListEqIO'
  , assertListEq', assertListEqR, assertListEqR', assertListEqRS
  , assertListEqS
  , assertSuccess

  , ioTests, mainTests

  , propAssociative
  , propInvertibleString, propInvertibleText, propInvertibleUtf8

  , runTests, runTests_, runTestsP, runTestsP_, runTestTree, runTestTree'
  , runTestsReplay

  , withResource'

  -- for testing this module
  , tests
  )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative     ( (<*>) )
import Control.Exception       ( SomeException, evaluate, handle )
import Control.Monad           ( (>>=), return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( True ), bool )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq )
import Data.Foldable           ( Foldable, concat, concatMap, length, toList )
import Data.Function           ( ($), const, flip )
import Data.Functor            ( (<$>), fmap )
import Data.Int                ( Int )
import Data.List               ( zip, zipWith3 )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe  )
import Data.Monoid             ( (<>), mempty )
import Data.String             ( String )
import Numeric.Natural         ( Natural )
import System.Exit             ( ExitCode( ExitFailure, ExitSuccess ) )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), Printable, Textual, parseString
                     , parseText, parseUtf8, toString, toText, toUtf8 )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData, force )

-- exited ------------------------------

import Exited  ( Exited( Exited ), doMain', exitWith )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError, IOError )

-- mtl ---------------------------------

import Control.Monad.Except  ( runExceptT )

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
                              , (===) )

-- text --------------------------------

import Data.Text  ( Text, pack )

-------------------------------------------------------------------------------

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
assertSuccess t = assertBool (toString t) True

----------------------------------------

{- | Run some tests with given tasty options; return success if all pass, 1 if
     some fail, 2 if setup failed -}
runTests_ ∷ MonadIO μ ⇒ TastyOpts → μ TastyRunResult
runTests_ (TastyOpts{..}) =
  liftIO $ case tryIngredients defaultIngredients optSet testTree of
    Just run_tests → bool TestsFailed TestSuccess <$> run_tests
    Nothing        → return TestRunFailure

{- | Run some tests, return exit code on failure (0 = success; 1 = some tests
     failed; 2 = failed to run). -}
runTests ∷ MonadIO μ ⇒ TastyOpts → μ ExitCode
runTests = (rrExitCode <$>) ∘ runTests_

----------------------------------------

{- | Run a test tree, with default options. -}
runTestTree_ ∷ MonadIO μ ⇒ TestTree → μ TastyRunResult
runTestTree_ tree = runTests_ (TastyOpts tree mempty)

----------------------------------------

{- | Run a test tree, with default options.  See `runTests` for exit codes. -}
runTestTree ∷ MonadIO μ ⇒ TestTree → μ ExitCode
runTestTree tree = rrExitCode <$> runTestTree_ tree

----------------------------------------

{- | Run a test tree, with default options.  Designed for simple "main"
     invocations, e.g., as part of t/*.hs -}
runTestTree' ∷ TestTree → IO ()
runTestTree' = doMain' ∘ runTestTree

----------------------------------------

{- | Run tests, with a given pattern (use "" to run everything). -}
runTestsP_ ∷ (MonadIO μ) ⇒ TestTree → String → μ TastyRunResult
runTestsP_ ts "" =
  runTests_ (TastyOpts ts mempty)
runTestsP_ ts pat =
  case parseTestPattern $ toString pat of
    Just p  → runTests_ (TastyOpts ts (singleOption p))
    Nothing → return TestSuccess

----------------------------------------

{- | Run tests, with a given pattern (use "" to run everything). -}
runTestsP ∷ (MonadIO μ) ⇒ TestTree → String → μ ExitCode
runTestsP ts pat = rrExitCode <$> runTestsP_ ts pat

----------------------------------------

runTestsReplay_ ∷ TestTree → String → Natural → IO TastyRunResult
runTestsReplay_ ts s r = do
  let replayO ∷ Natural → OptionSet
      replayO = singleOption ∘ QuickCheckReplay ∘ Just ∘ fromIntegral
      tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
      tryOpt p = tryIngredients defaultIngredients $
                     singleOption p ⊕ replayO r

  case parseTestPattern s of
    Just p  → fromMaybe (return TestRunFailure) $
                fmap (bool TestsFailed TestSuccess) <$> tryOpt p ts
    Nothing → return TestRunFailure

{- | Run some tests (matching a pattern) with a replay code.  Use "" to run
     all tests -}
runTestsReplay ∷ TestTree → String → Natural → IO ExitCode
runTestsReplay ts s r = rrExitCode <$> runTestsReplay_ ts s r

----------------------------------------

-- | provide an OptParse-Applicative Parser for tasty options (to allow for
--   integration of tasty testing & options into an executable)
optParser ∷ TestTree → Parser TastyOpts
optParser testTree =
  TastyOpts testTree <$> suiteOptionParser defaultIngredients testTree

-- | alternate name for client convenience
tastyOptParser ∷ TestTree → Parser TastyOpts
tastyOptParser = optParser

----------------------------------------

{- | Wrapper for tests as main, e.g., in t/*.hs . -}
mainTests ∷ MonadIO μ ⇒ String → TestTree → μ ()
mainTests desc ts = do
  tastyOpts ← liftIO $
              customExecParser (prefs showHelpOnError) $
                info (helper <*> tastyOptParser ts)
                     (fullDesc ⊕ progDesc desc ⊕ failureCode 254)

  Exited ← runTests tastyOpts >>= exitWith
  return ()

----------------------------------------

{- | Compare two lists for equality, with itemized testing.  We take the inputs
     as IO to allow for, well, IO.
 -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → [TestTree]
assertListEqIO' toT name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) = testCase (toString name ⊕ ": "⊕ show i)
                                  (got >>= \ g → assertEq' toT' (Just e) (atMay g i))
      toT' Nothing  = "Nothing"
      toT' (Just a) = "Just " ⊕ toT a

   in testCase (toString name ⊕ ": count") (got >>= lengthCheck expect)
    : (assertItem <$> zip [0..] expect)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α) ⇒
                Text → ψ α → IO (φ α) → [TestTree]
assertListEqIO = assertListEqIO' toText

-- | compare two lists for equality, with itemized testing
assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ) ⇒
               Text → ψ α → φ α → [TestTree]
assertListEq name expect got = assertListEqIO name expect (return got)

--------------------

assertListEqTests ∷ TestTree
assertListEqTests =
  testGroup "assertListEq" $
    assertListEq "listTest" [ "foo", "bar", "baz" ∷ String ]
                            [ "foo", "bar", "baz" ]

assertListEqTestsF ∷ TestTree
assertListEqTestsF =
  testGroup "assertListEq fail" $
    concat [ assertListEq "listTest<" [ "foo", "bar" ]
                                      [ "foo", "bar", "baz" ∷ Text ]
           , assertListEq "listTest>" [ "foo", "bar", "baz" ]
                                      [ "foo", "bar" ∷ Text ]
           , assertListEq "listTest!" [ "foo", "bar", "baz" ]
                                      [ "foo", "rab", "baz" ∷ String ]
           ]

----------------------------------------

-- | like `assertListEq`, but using Show rather than Printable
assertListEqS ∷ (Foldable ψ, Foldable φ, Eq α, Show α) ⇒
                 String → ψ α → φ α → [TestTree]
assertListEqS = assertListEq' (pack ∘ show)

----------------------------------------

-- | compare two lists for equality, with itemized testing
assertListEq' ∷ (Foldable ψ, Foldable φ, Eq α) ⇒
                (α → Text) → String → ψ α → φ α → [TestTree]
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
                 String → Either ε (ψ α) → (φ α) → [TestTree]
assertListEqR = assertListEqR' toText

--------------------

assertListEqRTests ∷ TestTree
assertListEqRTests =
  testGroup "assertListEq" $
    assertListEqR "listTestR"
                  (Right [ "foo", "bar", "baz" ] ∷ Either String [String])
                  [ "foo", "bar", "baz" ]

assertListEqRTestsF ∷ TestTree -- tests that should fail!
assertListEqRTestsF =
  testGroup "assertListEq fail" $
    assertListEqR "listTestR" (Left "weebles" ∷ Either String [String])
                              [ "foo", "bar", "baz" ]

----------------------------------------

-- | like `assertListEq`, but takes an Either which must be a Right
assertListEqR' ∷ (Foldable ψ, Foldable φ, Eq α, Show ε) ⇒
                 (α → Text) → String → Either ε (ψ α) → φ α → [TestTree]
assertListEqR' toT name got expect =
  case got of
    Left  e → [testCase name (assertFailure ("got a Left: " <> show e))]
    Right r → assertListEq' toT name r expect

----------------------------------------

assertListEqRS ∷ (Foldable ψ, Foldable φ, Eq α, Show ε, Show α) ⇒
                  String → Either ε (ψ α) → φ α → [TestTree]
assertListEqRS = assertListEqR' (pack ∘ show)

----------------------------------------

-- | test that we got a 'Right' value, satisfying the given assertion
assertRight ∷ Show γ ⇒ (ρ → Assertion) → Either γ ρ → Assertion
assertRight assertion got =
  case got of Right g → assertion g
              Left  e → assertFailure (show e)

--------------------

assertRightTests ∷ TestTree
assertRightTests =
  testGroup "assertRight"
    [ testCase "right" $
      assertRight ((@?= 4) ∘ length) (Right "good" ∷ Either Int String)
    ]

assertRightTestsF0 ∷ TestTree
assertRightTestsF0 =
  testGroup "assertRight fail (0)"
    [ testCase "right" $
      assertRight ((@?= 4) ∘ length) (Left 7 ∷ Either Int String)
    ]

assertRightTestsF1 ∷ TestTree
assertRightTestsF1 =
  testGroup "assertRight"
    [ testCase "right fail (1)" $
      assertRight ((@?= 4) ∘ length) (Right "bad" ∷ Either Int String)
    ]


----------------------------------------

-- | test that we got a 'Left' value, satisfying the given assertion
assertLeft ∷ Show ρ ⇒ (γ → Assertion) → Either γ ρ → Assertion
assertLeft assertion got =
  case got of Right r → assertFailure (show r)
              Left  l → assertion l

----------------------------------------

{- | Check that a value is a Left, but nothing more -}
assertIsLeft ∷ Show β ⇒ Either α β → Assertion
assertIsLeft = assertLeft (const $ assertSuccess "is Left")

----------------------------------------


{- | Check that any exception is thrown.  Any exception will cause the test
     to pass; no exception will cause it to fail.
 -}
assertAnyException ∷ (NFData α) ⇒ String → α → IO ()
assertAnyException n = assertException n (const True)

assertAnyExceptionIO ∷ (NFData α) ⇒ String → IO α → IO ()
assertAnyExceptionIO n = assertExceptionIO n (const True)

{- | Check that an exception is thrown.  Any exception that is thrown is
     checked by the given predicate; the predicate pass to indicate that the
     exception is as desired; and thus a @False@ will cause a test failure.  The
     test itself, if it returns a value (without an exception) will pass; but
     note that being IO, it can itself run tests...
 -}
assertException ∷ (NFData α) ⇒ String → (SomeException → Bool) → α → IO ()
assertException n p v = assertExceptionIO n p (return v)

assertExceptionIO ∷ (NFData α) ⇒ String → (SomeException → Bool) → IO α → IO ()
assertExceptionIO n p io =
  handle (return ∘ Left) (Right <$> (io >>= evaluate ∘ force)) >>= \ case
    Left e → assertBool n (p e)
    Right _ → assertFailure ("no exception thrown: " ⊕ n)

----------------------------------------

-- | test that we got an IOException (note, not just any Exception), and that
--   it matches a given predicate
assertIOException ∷ (AsIOError ε, Show ρ) ⇒
                     (ε → Assertion) → IO ρ → Assertion
assertIOException p io = (runExceptT $ asIOError io) >>= assertLeft p

assertIOException' ∷ (Show ρ) ⇒
                      (IOError → Assertion) → IO ρ → Assertion
assertIOException' = assertIOException

----------------------------------------

assertEq' ∷ (Eq t) ⇒ (t → Text) → t → t → Assertion
assertEq' toT e e' =
  let toS = toString ∘ toT
   in assertBool ("expected: " ⊕ toS e' ⊕ "\nbut got: " ⊕ toS e) (e ≡ e')

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
  testGroup "assertEq" [ testCase "foo" $ "foo" #=? ("foo" ∷ String) ]

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
  testGroup name $ (\ (tname,t) → testCase tname $ ioa >>= t) <$> ts

----------------------------------------

{- | like `withResource`, but with a no-op release resource -}
withResource' ∷ IO α → (IO α → TestTree) → TestTree
withResource' = flip withResource (const $ return ())

-- Common Properties ---------------------------------------

propInvertibleString ∷ (Eq α, Printable α, Printable (Parsed α), Textual α) ⇒
                       α → Property
propInvertibleString d =
  parseString (toString d) ≣ Parsed d

propInvertibleText ∷ (Eq α, Printable α, Printable (Parsed α), Textual α) ⇒
                     α → Property
propInvertibleText d = parseText (toText d) ≣ Parsed d

propInvertibleUtf8 ∷ (Eq α, Printable α, Printable (Parsed α), Textual α) ⇒
                     α → Property
propInvertibleUtf8 d = parseUtf8 (toUtf8 d) ≣ Parsed d

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

_test ∷ IO ()
_test = do
  TestSuccess ← _test0
  TestSuccess ← _test1
--  TestSuccess ← runTestTree _failTests
  return ()

_ftest ∷ IO ()
_ftest = do
  TestSuccess ← runTestTree_ _failTests
  return ()

----------------------------------------

{- | Simple tests, with a failure, to allow for a pattern to select only the
     passing tests.
-}

mkSimpleTests ∷ Foldable t ⇒
                t ((String → Int → Int → TestTree) → [TestTree]) → TestTree
mkSimpleTests ts =
  let tC ∷ String → Int → Int → TestTree
      tC name got expect = testCase name $ got @?= expect
   in testGroup "simple" $ concatMap ($ tC) ts

----------------------------------------

simpleTestsS ∷ (String → Int → Int → TestTree) → [TestTree]
simpleTestsS tC = [ tC "two" 2 2, tC "three" 3 3 ]

----------------------------------------

simpleTestsF ∷ (String → Int → Int → TestTree) → [TestTree]
simpleTestsF tC = [ tC "one" 1 2 {- deliberate fail -} ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "tests" [ unitTests ]

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
                 ]

-- that's all, folks! ----------------------------------------------------------
