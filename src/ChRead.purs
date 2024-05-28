module ChRead where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

newtype StateT s m a = StateT  (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> (s->m (Tuple a s))
runStateT (StateT mf) = mf

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mf) = StateT \s -> (\(Tuple a s') -> Tuple (f a) s')  <$> mf s

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply (StateT fmf) (StateT fmx) = StateT \s -> do
    Tuple f s' <- fmf s
    Tuple x s'' <- fmx s'
    pure $ Tuple (f x) s''

instance applicativeStateT :: (Monad m) => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT fma)  fmb = StateT \s -> do
    Tuple a s' <- fma s
    runStateT  (fmb a) s'
  
instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT  $ pure <<< f


test :: Effect Unit
test = do
  log "test"