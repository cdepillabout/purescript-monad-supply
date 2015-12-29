
module Control.Monad.Supply.Trans where

import Prelude

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Trans
import Control.Monad.Writer.Class
import Data.Tuple

import Control.Monad.Supply.Class

newtype SupplyT m a = SupplyT (StateT Int m a)

unSupplyT :: forall m a . SupplyT m a -> StateT Int m a
unSupplyT (SupplyT stateT) = stateT

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

instance monadSupplyT :: (Monad m) => Monad (SupplyT m)

instance monadTransSupplyT :: MonadTrans (SupplyT) where
    lift = SupplyT <<< lift

instance monadErrorSupplyT :: (MonadError e m) => MonadError e (SupplyT m) where
    catchError (SupplyT stateT) errorHandler = SupplyT $ catchError stateT (\e -> unSupplyT $ errorHandler e)
    throwError e = SupplyT <<< lift $ throwError e

instance monadWriterSupplyT :: (MonadWriter w m) => MonadWriter w (SupplyT m) where
    pass (SupplyT stateT) = SupplyT $ pass stateT
    listen (SupplyT stateT) = SupplyT $ listen stateT
    writer = SupplyT <<< lift <<< writer

instance monadReaderSupplyT :: (MonadReader r m) => MonadReader r (SupplyT m) where
    ask = SupplyT ask
    local f (SupplyT stateT) = SupplyT $ local f stateT

instance monadSupplySupplyT :: (Monad m) => MonadSupply (SupplyT m) where
    fresh = SupplyT $ do
        n <- get
        put (n + 1)
        return n

runSupplyT :: forall m a . Int -> SupplyT m a -> m (Tuple a Int)
runSupplyT n = flip runStateT n <<< unSupplyT

evalSupplyT :: forall m a . (Functor m) => Int -> SupplyT m a -> m a
evalSupplyT n = map fst <<< runSupplyT n
