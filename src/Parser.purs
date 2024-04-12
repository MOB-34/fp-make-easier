module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
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
  pure $ Tuple  c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\x y z -> fromCharArray [ x, y, z ]) <$> char <*> char <*> char


threeCharsB :: ∀ e. Parser e String
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1,c2,c3]

count :: ∀ e a f. Unfoldable f => Traversable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

count' :: ∀ e. Int ->Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

satisfy :: ∀ e.ParserError e=> String -> (Char -> Boolean) -> Parser e Char
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
letter= satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidString "alphaNum")


test :: Effect Unit
test = do
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"
  -- log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
-- log $ show $ parse' char "ab"
-- log $ show $ parse' twoChars "ab"
-- log $ show $ parse' threeChars "abc"
-- log $ show $ parse' threeChars "ab"
