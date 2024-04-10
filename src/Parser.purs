module Parser where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

class ParserError e where
  eof :: e

data PError = EOF

derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p = Parser (\s -> map f <$> parse p s)

instance applyParser :: Apply (Parser e) where
  apply p1 p2 = ap

instance applicaiveParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance bindParser :: Bind (Parser e) where
  bind p1 mf = Parser \s -> do
    Tuple s1 x <- parse p1 s
    parse (mf x) s1

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left $ eof
  Just { head, tail } -> Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = (\x y z -> fromCharArray [ x, y, z ]) <$> char <*> char <*> char

count :: ∀ e a f. Unfoldable f => Traversable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

test :: Effect Unit
test = do
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
-- log $ show $ parse' char "ab"
-- log $ show $ parse' twoChars "ab"
-- log $ show $ parse' threeChars "abc"
-- log $ show $ parse' threeChars "ab"
