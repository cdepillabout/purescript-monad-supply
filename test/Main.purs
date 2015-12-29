module Test.Main where

import Prelude
    ( class Eq, class Show, Unit, bind, ($), (<<<), pure, (<>), (>>=), map
    , show, id, (+), (<$>), const, return
    )

import Data.Functor (($>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Writer.Class (listen, tell, writer)
import Data.Foldable (sequence_)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Test.Spec (describe, it)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Supply (Supply, evalSupply, fresh)

main :: forall eff . Eff (process :: Process, console :: CONSOLE, random :: RANDOM, err :: EXCEPTION| eff) Unit
main = run [consoleReporter] $
    describe "Control.Monad.Logger" $
        it "fresh works" $ do
            let res = evalSupply 0 $ do
                        a <- fresh
                        b <- fresh
                        pure $ Tuple a b
            res `shouldEqual` Tuple 0 1

