
module Control.Monad.Supply
    ( module Control.Monad.Supply
    , module Control.Monad.Supply.Class
    , module Control.Monad.Supply.Trans
    ) where

import Prelude ((<<<), bind)

import Data.Identity (Identity, runIdentity)
import Data.Tuple (Tuple)

import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Control.Monad.Supply.Trans (SupplyT(..), evalSupplyT, runSupplyT)

type Supply = SupplyT Identity

runSupply :: forall a . Int -> Supply a -> Tuple a Int
runSupply n = runIdentity <<< runSupplyT n

evalSupply :: forall a . Int -> Supply a -> a
evalSupply n = runIdentity <<< evalSupplyT n
