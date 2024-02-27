module Ch5 where

import Data.Boolean (otherwise)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+), (==), (<), (-), negate, (>=), (<=), (/=))

-- 5.4 
flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (x : xs) y = x : snoc xs y

length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go :: List a -> List a
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs
  go _ = Nil

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (x : xs) idx
  | idx < 0 = Nothing
  | idx == 0 = Just x
  | otherwise = index xs (idx - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred = go 0
  where
  go _ Nil = Nothing
  go idx (x : xs)
    | pred x = Just idx
    | otherwise = go (idx + 1) xs

test :: Effect Unit
test = do
  -- log "Ch5 test"
  -- log (show (flip const 1 2))
  -- log $ show $ flip const 1 2
  -- flip const 1 2 # show # log
  -- log $ show $ singleton "xyz"
  -- log $ show $ null Nil
  -- log $ show $ null ("abc" : Nil)
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- log $ show $ length $ 1 : 2 : 3 : Nil
  -- log $ show $ head (Nil :: List Unit)
  -- log $ show $ head ("abc" : "123" : Nil)
  -- log $ show $ tail (Nil :: List Unit)
  -- log $ show $ tail ("abc" : "123" : Nil)
  -- log $ show $ (last Nil :: Maybe Unit)
  -- log $ show $ last ("a" : "b" : "c" : Nil)
  -- log $ show $ init (Nil :: List Unit)
  -- log $ show $ init (1 : Nil)
  -- log $ show $ init (1 : 2 : Nil)
  -- log $ show $ init (1 : 2 : 3 : Nil)
  -- log $ show $ uncons (1 : Nil)
  -- log $ show $ uncons (1 : 2 : 3 : Nil)
  -- log $ show $ index (1 : Nil) 4
  -- log $ show $ index (1 : 2 : 3 : Nil) 1
  -- log $ show $ index (Nil :: List Unit) 0
  -- log $ show $ index (1 : 2 : 3 : Nil) (-99)
  -- log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)

