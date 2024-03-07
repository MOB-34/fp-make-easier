module Ch7a where

import Data.Bounded (compare)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (<), (<=), (==), (>), (||))

data Maybe a = Nothing | Just a

-- derive instance eqMaybe :: Eq a => Eq (Maybe a)

-- data Ordering = LT | EQ | GT

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq (Just x) (Just y) = x == y
  eq Nothing Nothing = true
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare Nothing _ = LT
  compare _ _ = GT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == GT || cmp == EQ
  where
  cmp = compare x y

infixl 4 greaterThanOrEq as >=

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42)  :: Either Unit _)
-- log $ show $ Just "abc"
-- log $ show $ Just "abc"
-- log $ show $ (Nothing :: Maybe Unit)  log $ show $ (Nothing :: Maybe Unit)
-- log "------------------"
-- log $ show $ Just 1 < Just 5
-- log $ show $ Just 5 <= Just 5
-- log $ show $ Just 5 > Just 10
-- log $ show $ Just 10 >= Just 10
-- log $ show $ Just 99 > Nothing
-- log $ show $ Just 99 < Nothing
-- log "------------------"
-- log $ show $ Just 4 == Just 5
-- log $ show $ Just 5 == Just 2
-- log $ show $ Just 5 == Nothing
-- log $ show $ Nothing == Just 5
-- log $ show $ Nothing == (Nothing :: Maybe Unit)
