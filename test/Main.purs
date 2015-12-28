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
import Control.Monad.Eff.Ref (REF, readRef, writeRef, newRef)
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

import Control.Monad.Supply (Supply(..))

main :: forall eff . Eff (process :: Process, console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, ref :: REF | eff) Unit
main = run [consoleReporter] do
    describe "Control.Monad.Logger" do
        it "needs some tests" $ pure unit
