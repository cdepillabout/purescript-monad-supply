module Test.Main where

import Prelude (Unit, bind, pure, ($))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Tuple (Tuple(..))
import Test.Spec (describe, it)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Supply (evalSupply, fresh)

main :: forall eff . Eff (process :: Process, console :: CONSOLE | eff) Unit
main = run [consoleReporter] $
    describe "Control.Monad.Logger" $
        it "fresh works" $ do
            let res = evalSupply 0 $ do
                        a <- fresh
                        b <- fresh
                        pure $ Tuple a b
            res `shouldEqual` Tuple 0 1

