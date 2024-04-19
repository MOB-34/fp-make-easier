module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

class ParserError e where
  eof :: e
  invalidString :: String -> e

data PError = EOF | InvalidString String

derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidString = InvalidString

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p = Parser (\s -> map f <$> parse p s)

instance applyParser :: Apply (Parser e) where
  apply = ap

instance applicaiveParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance altParser :: Alt (Parser e) where
  alt p1 p2 = Parser \s -> case parse p1 s of
    Left _ -> parse p2 s
    Right x -> Right x

instance bindParser :: Bind (Parser e) where
  bind p1 mf = Parser \s -> do
    Tuple s1 x <- parse p1 s
    parse (mf x) s1

instance monadParser :: Monad (Parser e)

instance lazyParser :: Lazy (Parser e a) where
  defer f = Parser \s -> parse (f unit) s

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left $ eof
  Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\x y z -> fromCharArray [ x, y, z ]) <$> char <*> char <*> char

threeCharsB :: ∀ e. Parser e String
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [ c1, c2, c3 ]

count :: ∀ e a f. Unfoldable f => Traversable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy exception f = do
  c1 <- char
  if f c1 then
    pure c1
  else
    fail $ invalidString exception

fail :: ∀ e a. ParserError e => e -> Parser e a
fail err = Parser $ const $ Left err

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidString "alphaNum")

newtype Year = Year Int
newtype Month = Month Int
newtype Day = Day Int

data DateFormat = YearFirst | MonthFirst

derive instance genericYear :: Generic (Year) _
derive instance genericMonth :: Generic (Month) _
derive instance genericDay :: Generic (Day) _
derive instance genericDateFormat :: Generic DateFormat _

instance showYear :: Show Year where
  show = genericShow

instance showMonth :: Show Month where
  show = genericShow

instance showDay :: Show Day where
  show = genericShow

instance showDateFormat :: Show DateFormat where
  show = genericShow

type DateParts =
  { year :: Year
  , month :: Month
  , day :: Day
  , format :: DateFormat
  }

atMost :: ∀ e a f. ParserError e => Unfoldable f => Int -> (a -> f a -> f a) -> Parser e a -> Parser e (f a)
atMost i cons p
  | i <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMost (i - 1) cons p

optional :: ∀ e a. ParserError e => a -> Parser e a -> Parser e a
optional defVal p = p <|> pure defVal

atMost' :: ∀ e. ParserError e => Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost n (:) p

range :: ∀ e a f. ParserError e => Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min < 0 || max <= 0 || min > max = pure none
  | otherwise = do
      a1 <- count min p
      a2 <- atMost (max - min) cons p
      pure $ a1 <> a2

range' :: ∀ e. ParserError e => Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  pure $ { year, month, day, format: YearFirst }

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure $ { year, month, day, format: MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

some :: ∀ a f m. Alt m => Applicative m => Lazy (m (f a)) => Unfoldable f => (a -> f a -> f a) -> m a -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: ∀ a f m. Alt m => Applicative m => Unfoldable f => Lazy (m (f a)) => (a -> f a -> f a) -> m a -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  g1 <- range' 1 4 digit
  constChar ','
  constChar ' '
  g2 <- some' (letter <|> constChar' ' ')
  g3 <- many' digit
  pure $ [ g1, g2, g3 ]

test :: Effect Unit
test = do
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"
-- log $ show $ parse' (some' digit) "2343423423abc"
-- log $ show $ parse' (many' digit) "_2343423423abc"
-- log $ show $ parse' (some' digit) "_2343423423abc"
-- log $ show $ parse' yearFirst "1999-12-31"
-- log $ show $ parse' monthFirst "12/31/1999"
-- log $ show $ parse' date "1999-12-31"
-- log $ show $ parse' date "12/31/1999"
-- log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
-- log $ show $ parse' (atMost' 2 alphaNum) "$_$"
-- log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
-- log $ show $ parse' (count' 3 digit) "123456"
-- log $ show $ parse' (count' 3 digit) "abc456"
-- log $ show $ parse' (count' 4 letter) "Freddy"
-- log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
-- log $ show $ parse' (count' 10 alphaNum) "######"
-- log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
-- log $ show $ parse' char "ab"
-- log $ show $ parse' twoChars "ab"
-- log $ show $ parse' threeChars "abc"
-- log $ show $ parse' threeChars "ab"
