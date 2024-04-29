module Ch21a where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State (get, put)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (runWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Debug (spy)

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT mf) = mf

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f mg = StateT \s -> runStateT mg s <#> \(Tuple x s') -> Tuple (f x) s'

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply fmf fmx = StateT \s -> do
    Tuple f s' <- runStateT fmf s
    Tuple x s'' <- runStateT fmx s'
    pure $ Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind sma fm = StateT \s -> do
    Tuple x s' <- runStateT sma s
    runStateT (fm x) s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask :: StateT s m r
  ask = lift ask

instance monadTellStateT :: (MonadTell w m) => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance monadTransStateT :: MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \x -> Tuple x s

instance monadThrowStateT :: (MonadThrow e m) => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance monadErrorStateT :: (MonadError e m) => MonadError e (StateT s m) where
  catchError fmx f = StateT \s -> catchError (runStateT fmx s) \e -> runStateT (f e) s

type StackResult = Tuple (Tuple (Either String Unit) String) Int

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

type AppM = StateT Int (ExceptT String (WriterT String Effect)) Unit

type AppResult = Tuple (Maybe String) AppEffects

type AppEffects =
  { log :: String
  , state :: Int
  , result :: Maybe Unit
  }

results :: StackResult -> AppResult
results (Tuple (Tuple (Left e) w) s) = Tuple (Just e) { log: w, state: s, result: Nothing }
results (Tuple (Tuple (Right res) w) s) = Tuple Nothing { log: w, state: s, result: Just res }

runApp
  :: Int
  -> AppM
  -> Effect (Tuple (Either String (Tuple Unit Int)) String)
runApp s = runWriterT <<< runExceptT <<< flip runStateT s

validate :: Int -> AppM
validate n = do
  s <- get
  let x = spy "s in validate" s
  log "HEY!!!!!!!!!!!!!!!!!!!" -- This will NOT be lost on the error case
  put 10 -- This will be lost on the error case but kept on success
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"

log :: ∀ m. MonadTell String m ⇒ String → m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "Starting App..."
  n <- get
  catchError (validate n)
    ( \err -> do
        s <- get
        let x = spy "s in error handler" s
        log $ "We encountered an error: (" <> err <> ")"
        put 100
    )
  s <- get
  let x = spy "s in app" s
  put $ s + 1
  log "Incremented State"
  pure unit

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  Console.log $ show $ result1
  result2 <- runApp 99 app
  Console.log $ show $ result2
