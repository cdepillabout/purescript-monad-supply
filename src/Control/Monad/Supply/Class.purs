
module Control.Monad.Supply.Class where

import Prelude

class (Monad m) <= MonadSupply m where
  fresh :: m Int

freshName :: forall m . (MonadSupply m) => m String
freshName = do
    int <- fresh
    pure $ "$" <> show int

