module Ch17
  ( Age(..)
  , FamilyAges(..)
  , FamilyAgesRow
  , Validation(..)
  , Either(..)
  , createFamilyAges
  , test
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

derive instance genriceMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

derive instance orderEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance functorEither :: Functor (Either a)

derive instance genriceEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left y) _ = Left y

instance applicativeEither :: Applicative (Either a) where
  pure = Right

newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation a b) _
derive instance genericValidation :: Generic (Validation a b) _
derive newtype instance functorValidation :: Functor (Validation a)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive newtype instance eqValidation :: (Eq a, Eq b) => Eq (Validation a b)
derive newtype instance ordValidation :: (Ord a, Ord b) => Ord (Validation a b)
instance showValidation :: (Show a, Show b) => Show (Validation a b) where
  show = genericShow

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply (Validation (Right f)) x = f <$> x
  apply (Validation (Left errs)) (Validation (Left err)) = Validation $ Left $ errs <> err
  apply (Validation (Left errs)) _ = Validation $ Left errs

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< pure

newtype Age = Age Int
newtype FullName = FullName String

type FamilyAgesRow r =
  (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)

type FamilyNamesRow r =
  (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }

derive instance genericAge :: Generic Age _
derive instance genericFullname :: Generic FullName _
derive instance genericFamily :: Generic Family _

instance showAge :: Show Age where
  show = genericShow

instance showFullname :: Show FullName where
  show = genericShow

instance showFamily :: Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }

-- derive newtype instance showFamilyAge :: Show FamilyAges
derive instance genericFamilyAge :: Generic FamilyAges _
instance showFamilyAge :: Show FamilyAges where
  show = genericShow

newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge low) (UpperAge upper) (Age age) who
  | age < low = Validation $ Left [ who <> " is too young" ]
  | age > upper = Validation $ Left [ who <> " is too old" ]
  | otherwise = Validation $ Right $ Age age

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge: fa, motherAge: ma, childAge: ca } =
  FamilyAges <$>
    ( { fatherAge: _, motherAge: _, childAge: _ }
        <$> validateAge (LowerAge 18) (UpperAge 100) fa "father"
        <*> validateAge (LowerAge 18) (UpperAge 100) ma "mother"
        <*> validateAge (LowerAge 1) (UpperAge 18) ca "child"
    )

test :: Effect Unit
test = do
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
  log $ show $ createFamilyAges
    { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 } -- -- LAW: Associative Composition
-- -- (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
-- log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) ==
--   (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
-- -- LAW: Identity
-- -- pure identity <*> x = x
-- log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
-- -- LAW: Homomorphism
-- -- pure (f x) = pure f <*> pure x
-- log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
-- -- LAW: Interchange
-- -- u <*> pure x = pure (_ $ x) <*> u
-- log $ show $ (pure negate <*> pure 1) ==
-- -- u <*> pure x = pure (_ $ x) <*> u
-- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)  -- log $ show $ (pure negate <*> pure 1) ==
--   (pure (_ $ 1) <*> pure negate :: Either Unit Int)
-- log $ show $ (+) <$> Just 21 <*> Just 21
-- log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
-- log $ show $ pure (+) <*> Just 17 <*> Just 25
