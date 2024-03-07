module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive instance newtypeFullName :: Newtype FullName _
derive instance newtypeAge :: Newtype Age _
derive instance newtypeCSV :: Newtype CSV _
instance showFullName :: Show FullName where
  show (FullName name) = name

derive newtype instance showAge :: Show Age
derive newtype instance eqFullName :: Eq FullName
derive instance eqOccupation :: Eq Occupation
derive newtype instance eqAge :: Eq Age
derive newtype instance showCSV :: Show CSV
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

derive newtype instance eqCSV :: Eq CSV

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> "," <> show age <> "," <> show occupation

class FromCSV a where
  fromCSV :: CSV -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" = Just Dentist
toOccupation "Lawyer" = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _ = Nothing

-- instance eqPerson :: Eq Person where
--   eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.occupation == p2.occupation

derive instance eqPerson :: Eq Person

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV c) = case split (Pattern ",") c of
    [ name, age, occupation ] -> case fromString age of
      (Just age') -> case toOccupation occupation of
        (Just occupation') -> Just $ Person
          { name: FullName name
          , age: Age age'
          , occupation: occupation'
          }
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

p :: Person
p = Person
  { name: FullName "Sue Smith"
  , age: Age 23
  , occupation: Doctor

  }

csvP :: CSV
csvP = toCSV p

test :: Effect Unit
  log $ show $ fromCSV csvP == Just p
-- log $ show $
--   toCSV
--     ( Person
--         { name: FullName "Sue Smith"
--         , age: Age 23
--         , occupation: Doctor
--         }
--     ) == CSV "Sue Smith,23,Doctor"
--
