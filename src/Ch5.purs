module Ch5 where

import Data.Boolean (otherwise)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Ord (max)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+), (==), (<), (-), negate, (>=), (<=), (/=), type (~>), (>), (<<<))

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

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred = go 0 Nothing
  where
  go _ Nothing Nil = Nothing
  go idx targetIdx (x : xs) = go (idx + 1) (if pred x then Just idx else targetIdx) xs
  go _ targetIdx Nil = targetIdx

reverse :: List ~> List
reverse = go Nil
  where
  go nl Nil = nl
  go nl (x : xs) = go (x : nl) xs

concat :: ∀ a. List (List a) -> List a
concat = go Nil
  where
  go nl Nil = reverse nl
  go nl (Nil : ls) = go nl ls
  go nl ((x : xs) : xss) = go (x : nl) (xs : xss)

concat' :: ∀ a. List (List a) -> List a
concat' Nil = Nil
concat' (Nil : xss) = concat xss
concat' ((x : xs) : xss) = x : concat (xs : xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter pred = go Nil
  where
  go nl Nil = reverse nl
  go nl (x : xs) = go (if pred x then x : nl else nl) xs

filter' :: forall a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' pred (x : xs) = if pred x then x : filter' pred xs else filter' pred xs

filter'' :: forall a. (a -> Boolean) -> List a -> List a
filter'' _ Nil = Nil
filter'' pred (x : xs)
  | pred x = x : filter' pred xs
  | otherwise = filter' pred xs

catMaybes :: ∀ a. List (Maybe a) -> Maybe (List a)
catMaybes = go Nil
  where
  go Nil Nil = Nothing
  go l Nil = Just $ reverse l
  go nl ((Just x) : xs) = go (x : nl) xs
  go nl (Nothing : xs) = go nl xs

catMaybes' :: ∀ a. List (Maybe a) -> List a
catMaybes' Nil = Nil
catMaybes' (x : xs) = case x of
  Nothing -> catMaybes' xs
  (Just y) -> y : catMaybes' xs

range :: Int -> Int -> List Int
range = go Nil
  where
  go l s e
    | s == e = reverse $ s : l
    | otherwise = go (s : l) (if s > e then s - 1 else s + 1) e

range' :: Int -> Int -> List Int
range' s e
  | s == e = singleton s
  | otherwise = s : range' (s + if s > e then (-1) else 1) e

range'' :: Int -> Int -> List Int
range'' s e = go (if s > e then (-1) else 1) s
  where
  go step start
    | start == e = singleton start
    | otherwise = start : go step (start + step)

range''' :: Int -> Int -> List Int
range''' s e = go s
  where
  go :: Int -> List Int
  go start
    | start == e = singleton start
    | otherwise = start : go (start + step)
  step = if s > e then (-1) else 1

range'''' :: Int -> Int -> List Int
range'''' s e = go Nil e
  where
  go :: List Int -> Int -> List Int
  go rl start
    | start == s = start : rl
    | otherwise = go (start : rl) (start + step)
  step = if s > e then 1 else (-1)

take :: ∀ a. Int -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take i (x : xs)
  | i < 0 = Nil
  | otherwise = x : take (i - 1) xs

take' :: ∀ a. Int -> List a -> List a
take' idx = reverse <<< go Nil idx
  where
  go rl _ Nil = rl
  go rl 0 _ = rl
  go rl i (x : xs)
    | i < 0 = Nil
    | otherwise = go (x : rl) (i - 1) xs

take'' :: ∀ a. Int -> List a -> List a
take'' idx = reverse <<< go Nil (max 0 idx)
  where
  go rl _ Nil = rl
  go rl 0 _ = rl
  go rl i (x : xs) = go (x : rl) (i - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 ol = ol
drop i (_ : xs) = drop (i - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs)
  | pred x = x : takeWhile pred xs
  | otherwise = Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs)
  | pred x = dropWhile pred xs
  | otherwise = l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd _ Nil = Nil
takeEnd i l = drop (max 0 $ (length l - i)) l

takeEnd' :: ∀ a. Int -> List a -> List a
takeEnd' i = snd <<< go
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \tup@(Tuple len nl) -> if len == i then tup else Tuple (len + 1) $ (x : nl)

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd i = snd <<< go
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \(Tuple len l) -> Tuple (len + 1) $ if len > i then (x : l) else Nil

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip = go $ Tuple Nil Nil
  where
  go (Tuple ll rl) Nil = Tuple (reverse ll) (reverse rl)
  go (Tuple ll rl) ((Tuple l r) : xs) = go (Tuple (l : ll) (r : rl)) xs

unzip' :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip' Nil = Tuple Nil Nil
unzip' ((Tuple l r) : xs) = unzip' xs # \(Tuple ll rl) -> Tuple (l : ll) (r : rl)

test :: Effect Unit
test = do
  log $ show $ unzip' (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip' (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip' (Nil :: List (Tuple Unit Unit))
-- log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
-- log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
-- log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
-- log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
-- log $ show $ dropEnd 10 (1 : Nil)
-- log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
-- log $ show $ takeEnd 10 (1 : Nil)
-- log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
-- log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
-- log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
-- log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
-- log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
-- log $ show $ drop 10 (Nil :: List Unit)
-- log $ show $ take'' 5 (12 : 13 : 14 : Nil)
-- log $ show $ take'' (-2) (12 : 13 : 14 : Nil)
-- log $ show $ take'' 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
-- log $ show $ take 5 (12 : 13 : 14 : Nil)
-- log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
-- log $ show $ range'''' 1 10
-- log $ show $ range'''' 3 (-3)
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
-- log $ show $ index (1 : 2 : 3 : Nil) (-99)
-- log $ show $ (1 : 2 : 3 : Nil) !! 1
-- log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
-- log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
-- log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
-- log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
-- log $ show $ reverse (10 : 20 : 30 : Nil)
-- log $ show $
--   concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
-- log $ show $
--   concat' ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
-- log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
-- log $ show $ filter' (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
-- log $ show $
--   catMaybes' (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)

