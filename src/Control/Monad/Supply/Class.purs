
module Control.Monad.Supply.Class where

import Prelude

import Control.Monad.State.Trans

class (Monad m) <= MonadSupply m where
  fresh :: m Int

freshName :: forall m . (MonadSupply m) => m String
freshName = do
    int <- fresh
    pure $ "$" <> show int

instance monadSupplyStateT :: (MonadSupply m) => MonadSupply (StateT s m) where
    fresh = lift fresh
