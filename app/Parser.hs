module Parser where

import Control.Applicative

data ParseResult a = ParseResult {result :: a, rest :: String} deriving (Show)

newtype Parser a = Parser {runParser :: String -> Either String (ParseResult a)}

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $
    \input -> do
      parsed <- runParser p input
      pure $ parsed {result = f $ result parsed}

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure result = Parser $ \input -> Right $ ParseResult result input

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $
    \input -> do
      parsed <- runParser p1 input
      runParser (fmap (result parsed) p2) (rest parsed)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \input -> do
    parsed <- runParser p input
    runParser (f $ result parsed) (rest parsed)

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \input -> Left ""

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    Left _ -> runParser p2 input
    res@(Right _) -> res
