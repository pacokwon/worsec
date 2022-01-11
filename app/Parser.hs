{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V

data ParseResult a = ParseResult
  { result :: a
  , rest :: InputState
  } deriving (Show)

data Position = Position
  { col :: Int
  , line :: Int
  } deriving (Show)

data InputState = InputState
  { input :: Vector String
  , pos   :: Position
  } deriving (Show)

fromString :: String -> InputState
fromString str = InputState { input, pos }
  where
    input = V.fromList . lines $ str
    pos   = Position 0 0

newtype Parser a = Parser { runParser :: InputState -> Either String (ParseResult a) }

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
