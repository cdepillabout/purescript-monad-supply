
module Control.Monad.Supply
    ( module Control.Monad.Supply
    , module Control.Monad.Supply.Class
    , module Control.Monad.Supply.Trans
    ) where

import Prelude

import Data.Identity
import Data.Tuple

import Control.Monad.Supply.Class (MonadSupply, fresh)
import Control.Monad.Supply.Trans (SupplyT(..), evalSupplyT, runSupplyT)

type Supply = SupplyT Identity

runSupply :: forall a . Int -> Supply a -> Tuple a Int
runSupply n = runIdentity <<< runSupplyT n

evalSupply :: forall a . Int -> Supply a -> a
evalSupply n = runIdentity <<< evalSupplyT n
