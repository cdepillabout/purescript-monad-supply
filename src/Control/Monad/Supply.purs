
module Control.Monad.Supply where

import Prelude

import Control.Monad.Supply.Trans
import Data.Identity
import Data.Tuple

type Supply = SupplyT Identity

runSupply :: forall a . Int -> Supply a -> Tuple a Int
runSupply n = runIdentity <<< runSupplyT n

evalSupply :: forall a . Int -> Supply a -> a
evalSupply n = runIdentity <<< evalSupplyT n
