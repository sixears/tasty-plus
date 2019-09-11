{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.T.Tasty
  ( tests )
where

import Prelude ( Int )

-- base --------------------------------

import Control.Monad  ( (>>=), return )
import Data.Either    ( Either( Left, Right ) )
import Data.Foldable  ( Foldable, concat, concatMap )
import Data.Function  ( (.), ($) )
import Data.String    ( String )
import System.IO      ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Foldable  ( length )
import Fluffy.Tasty     ( TastyRunResult( TestsFailed, TestSuccess )
                        , (#=?)
                        , assertListEq, assertListEqR, assertRight
                        , runTestsP, runTestTree
                        )

-------------------------------------------------------------------------------

_test0 :: IO TastyRunResult
_test0 = -- test that runTestsP correctly selects only the working tests
         runTestsP tests "simpleTest/t"

_test1 :: IO TastyRunResult
_test1 = runTestsP tests "normal"

_test :: IO ()
_test = do
  TestSuccess <- _test0
  TestSuccess <- _test1
  TestSuccess <- runTestTree failTests
  return ()

tests :: TestTree
tests = testGroup "tests" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "unitTests" [ hunitGroup ]

hunitGroup :: TestTree
hunitGroup = testGroup "hunit" [ mkSimpleTests [ simpleTestsS ]
                               , normalTests ]

normalTests :: TestTree
normalTests = testGroup "normal" [ assertEqTests
                                 , assertListEqRTests
                                 , assertListEqTests
                                 , assertRightTests
                                 ]

failTests :: TestTree
failTests =
  let failIt name tree = testCase name $ runTestTree tree >>= (@?= TestsFailed)
   in testGroup "fail"
                 [ failIt "simpleTests"         (mkSimpleTests [simpleTestsF])
                 , failIt "assertEq"            assertEqTestsF
                 , failIt "assertRightTestsF0"  assertRightTestsF0
                 , failIt "assertRightTestsF1"  assertRightTestsF1
                 , failIt "assertListEqTestsF"  assertListEqTestsF
                 , failIt "assertListEqRTestsF" assertListEqRTestsF
                 ]

------------------------------------------------------------

-- | simple tests, with a failure, to allow for a pattern to select only the
--   passing tests
-- mkSimpleTests :: TestTree
mkSimpleTests :: Foldable t =>
                 t ((String -> Int -> Int -> TestTree) -> [TestTree])
              -> TestTree
mkSimpleTests ts =
  let tC :: String -> Int -> Int -> TestTree
      tC name got expect = testCase name $ got @?= expect
   in testGroup "simple" $ concatMap ($ tC) ts

----------------------------------------

simpleTestsS :: (String -> Int -> Int -> TestTree) -> [TestTree]
simpleTestsS tC = [ tC "two" 2 2, tC "three" 3 3 ]

----------------------------------------

simpleTestsF :: (String -> Int -> Int -> TestTree) -> [TestTree]
simpleTestsF tC = [ tC "one" 1 2 {- deliberate fail -} ]

-- that's all, folks! ---------------------------------------------------------
