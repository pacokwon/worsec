{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (stripPrefix)
import Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  x : xs | predicate x -> Right $ ParseResult x xs
  _ -> Left "[satisfy] Doesn't satisfy predicate"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = Parser $ \case
  x : xs -> Right $ ParseResult x xs
  _ -> Left "[anyChar] Input is empty!"

str :: String -> Parser String
str prefix = Parser $ \input ->
  case stripPrefix prefix input of
    Just remaining -> Right $ ParseResult prefix remaining
    Nothing -> Left "[str] Given string not found"

digit :: Parser Char
digit = satisfy isDigit

oneOf :: [Char] -> Parser Char
oneOf candidates = Parser $ \case
  x : xs | elem x candidates -> Right $ ParseResult x xs
  _ -> Left "[oneOf] Candidate character not found"

eof :: Parser ()
eof = Parser $ \case
  x : xs -> Left "[EOF] Expected EOF. Encountered input"
  [] -> Right $ ParseResult () ""

choice :: [Parser a] -> Parser a
choice = foldr (<|>) err
  where
    err = Parser $ \_ -> Left "[Choice] None of the given parsers work"

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest <- many p
  return $ first : rest

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> return []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = do
  first <- p
  rest <- many $ s >> p
  return $ first : rest

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space
