module Ch13 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, discard, identity, show, ($), (*), (/), (<<<), (<>), (==))

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

derive instance genericMaybe :: Generic (Maybe a) _
derive instance eqMaybe :: Eq a => Eq (Maybe a)
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right x) = Right $ f x

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ f (Right x) = Right $ f x

lmap :: ∀ a b c f. Bifunctor f => (a -> c) -> f a b -> f c b
lmap f = bimap f identity

rmap :: ∀ a b d f. Bifunctor f => (b -> d) -> f a b -> f a d
rmap = bimap identity

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

data Tuple a b = Tuple a b

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple a b) = Tuple a $ f b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

data Threeple a b c = Threeple a b c

instance functorTreeple :: Functor (Threeple a b) where
  map f (Threeple a b c) = Threeple a b $ f c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeplu :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap f g (Threeple x y z) = Threeple x (f y) (g z)

test :: Effect Unit
test = do
  -- log $ show $ (_ / 2) <$> Just 10
  -- log $ show $ (_ / 2) <$> Nothing
  -- log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  -- log $ show $ (_ / 2) <$> Left "error reason"
  -- log $ show $ (_ / 2) <$> Tuple 10 20
  -- log $ show $ (_ / 2) <$> Threeple 10 20 40
  -- log $ show $ "Maybe Identity for Nothing: "
  --   <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  -- log $ show $ "Maybe Identity for Just: "
  --   <> show ((identity <$> Just [ 1, 2 ]) == Just [ 1, 2 ])
  -- let
  --   f x = x * 3
  --   g x = x * 2
  -- log $ show $ "Maybe Composition for Nothing: "
  --   <> show (map (f <<< g) Nothing == (map f <<< map g) Nothing)
  -- log $ show $ "Maybe Composition for Just: "
  --   <> show (map (f <<< g) (Just 3) == (map f <<< map g) (Just 3))
  -- log $ show $ "Tuple Identity: "
  --   <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
  -- log $ show $ "Tuple Composition : "
  --   <> show ((map (g <<< f) (Tuple 10 20)) == (map f <<< map g) (Tuple 10 20))
  -- log $ show $ rmap (_ * 2) $ Left "error reason"
  -- log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  -- log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  -- log $ show $ lmap toUpper $ Right 10
  -- log $ show $ rmap (_ * 2) $ Tuple 80 40
  -- log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ rmap (_ * 2) $ Threeple 99 80 40
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40
