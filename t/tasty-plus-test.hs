{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Applicative  ( (<*>) )
import Control.Monad        ( return )
import Data.Function        ( ($) )
import Data.Monoid          ( (<>) )
import System.IO            ( IO )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TastyPlus    ( runTests_, tastyOptParser, tests )

-------------------------------------------------------------------------------

main ∷ IO ()
main = do
  tastyOpts ← customExecParser (prefs showHelpOnError) $
                 info (helper <*> tastyOptParser tests)
                      (fullDesc <> progDesc "tests tasty-plus"
                                <> failureCode 254)

  _ ← runTests_ tastyOpts
  return ()

-- that's all, folks! ---------------------------------------------------------
