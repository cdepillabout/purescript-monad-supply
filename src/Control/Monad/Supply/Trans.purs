
module Control.Monad.Supply.Trans where

import Prelude

import Control.Monad.State.Trans
import Control.Monad.Supply.Class
import Data.Tuple

newtype SupplyT m a = SupplyT (StateT Int m a)

unSupplyT :: forall m a . SupplyT m a -> StateT Int m a
unSupplyT (SupplyT stateT) = stateT
-- deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r)

instance functorSupplyT :: (Monad m) => Functor (SupplyT m) where
    map f (SupplyT stateT) = SupplyT $ map f stateT

instance applySupplyT :: (Monad m) => Apply (SupplyT m) where
    apply (SupplyT stateTFAToB) (SupplyT stateTFA) = SupplyT $ apply stateTFAToB stateTFA

instance applicativeSupplyT :: (Monad m) => Applicative (SupplyT m) where
    pure a = SupplyT $ pure a

instance bindSupplyT :: (Monad m) => Bind (SupplyT m) where
    bind (SupplyT stateT) f = SupplyT $ do
        a <- stateT
        unSupplyT $ f a

-- instance monadSupplySupplyT :: (Monad m) => MonadSupply (SupplyT m) where
--   fresh = SupplyT $ do
--     n <- get
--     put (n + 1)
--     return n

-- instance (MonadSupply m) => MonadSupply (StateT s m) where
--   fresh = lift fresh

runSupplyT :: forall m a . Int -> SupplyT m a -> m (Tuple a Int)
runSupplyT n = flip runStateT n <<< unSupplyT

evalSupplyT :: forall m a . (Functor m) => Int -> SupplyT m a -> m a
evalSupplyT n = map fst <<< runSupplyT n
