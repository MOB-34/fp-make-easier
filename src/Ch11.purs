module Ch11 where

import Data.Boolean (otherwise)
import Data.CommutativeRing (class Semiring)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.List (List(..), foldl, singleton, (:))
import Data.List (List, foldl)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Ord (class Ord)
import Data.Semiring (zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, show, ($), (+), (<<<), (<>), (>))

reverse :: List ~> List
reverse = foldl (\acc x -> x : acc) Nil

max :: ∀ a. Ord a => a -> a -> a
max fst snd
  | fst > snd = fst
  | otherwise = snd

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ go first l
  where
  go mx Nil = mx
  go mx (x : xs) = go (max x mx) xs

findMax' :: ∀ a. Ord a => List a -> Maybe a
findMax' Nil = Nothing
findMax' l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty def l)) = foldl max def l

findMaxNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE' (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (def :| l) = foldl f def l

sum :: List Int -> Int
sum Nil = 0
sum (x : xs) = x + sum xs

sum' :: List Int -> Int
sum' = foldl (+) 0

sum'' :: ∀ a. Semiring a => List a -> a
sum'' = go zero
  where
  go acc Nil = acc
  go acc (x : xs) = go (x + acc) xs

sum''' :: ∀ a f. Semiring a => Foldable f => f a -> a
sum''' = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf a) = singleton a
toList (Node lt rt) = toList lt <> toList rt

instance foldableTreeOnLeft :: Foldable Tree where
  foldl f def = foldl f def <<< toList
  foldr f def = foldr f def <<< toList
  foldMap f = foldMap f <<< toList

test :: Effect Unit
test = do
  -- log $ show $ reverse (10 : 20 : 30 : Nil)
  -- log $ show $ max (-1) 99
  -- log $ show $ max "aa" "z"
  -- log $ show $ findMax' (37 : 311 : -1 : 2 : 84 : Nil)
  -- log $ show $ findMax' ("a" : "bbb" : "c" : Nil)
  -- log $ show $ findMaxNE' (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  -- log $ show $ findMaxNE' (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum' (1 : 2 : 3 : Nil)
  log $ show $ sum'' (1 : 2 : 3 : Nil)
  log $ show $ sum''' [ 1, 2, 3 ]
  log $ show $ sum''' [ 1.0, 2.0, 3.0 ]
  log $ show $ sum'''
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
