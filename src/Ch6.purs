module Ch6 where

import Data.Array (sort)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (Ordering(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, ($), show)

data OrderingT = First | Second | Third

instance eqOrderingT :: Eq OrderingT where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordOrderingT :: Ord OrderingT where
  compare First First = EQ
  compare Second Second = EQ
  compare Third Third = EQ
  compare First _ = LT
  compare Second First = GT
  compare Second Third = LT
  compare Third _ = GT

derive instance genericOrderingT :: Generic OrderingT _

instance showOrderingT :: Show OrderingT where
  show = genericShow

x :: Array OrderingT
x = [ Second, First, Third ]

test :: Effect Unit
test = do
  log $ show $ sort x
