module Ch where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)


type AppM e w s a =  ExceptT e (WriterT w (StateT s Effect)) a

app :: AppM String String Int Unit
app = do
  tell "this is writer"
  count <- get
  when (count > 100) $ void $ throwError "bigger than 100"
  put $ count * 10
  pure unit

runApp :: Int -> AppM String String Int Unit -> Effect (Tuple (Tuple (Either String Unit) String) Int)
runApp n = runExceptT >>> runWriterT >>> flip runStateT n

test :: Effect Unit
test = do
  res1 <- flip runApp app 666
  log $ show $ res1
  res2 <- flip runApp app 99
  log $ show $ res2
