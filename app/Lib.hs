module Lib where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (stripPrefix)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Parser

getLineAndPos :: InputState -> (String, Int)
getLineAndPos s@InputState{ input, pos } =
  let col'        = col pos
      line'       = line pos
      currentLine = input V.! line'
   in (currentLine, col')

nextChar :: Parser Char
nextChar = Parser aux
  where
    aux :: InputState -> Either String (ParseResult Char)
    aux s@InputState{ input, pos } =
      | line' >= linesCount = Left "Expected character. Encountered EOF"
      | col' >= lineLength  = let nextLineNo = line' + 1
                                  nextLine   = input V.! nextLineNo
                                  position   = Position 1 nextLineNo
                                  inputState = InputState input position
                               in ParseResult (head nextLine) inputState
      | otherwise           = let position   = Position (col' + 1) line'
                                  inputState = InputState input position
                               in ParseResult (currentLine !! col') inputState
      where
        col'        = col pos
        line'       = line pos
        linesCount  = V.length input
        currentLine = input V.! line'
        lineLength  = length currentLine

-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy predicate = Parser aux
--   where
--     aux :: InputState -> Either String (ParseResult Char)
--     aux s@InputState{ input, pos } =
--       where
--         (currentLine, col') = getLineAndPos s

-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy predicate = Parser $ \case
--   x : xs | predicate x -> Right $ ParseResult x xs
--   _ -> Left "[satisfy] Doesn't satisfy predicate"

-- char :: Char -> Parser Char
-- char c = satisfy (== c)

-- alpha :: Parser Char
-- alpha = satisfy isAlpha

-- alphaNum :: Parser Char
-- alphaNum = satisfy isAlphaNum

-- anyChar :: Parser Char
-- anyChar = Parser $ \case
--   x : xs -> Right $ ParseResult x xs
--   _ -> Left "[anyChar] Input is empty!"

-- str :: String -> Parser String
-- str prefix = Parser $ \input ->
--   case stripPrefix prefix input of
--     Just remaining -> Right $ ParseResult prefix remaining
--     Nothing -> Left "[str] Given string not found"

-- digit :: Parser Char
-- digit = satisfy isDigit

-- oneOf :: [Char] -> Parser Char
-- oneOf candidates = Parser $ \case
--   x : xs | elem x candidates -> Right $ ParseResult x xs
--   _ -> Left "[oneOf] Candidate character not found"

-- eof :: Parser ()
-- eof = Parser $ \case
--   x : xs -> Left "[EOF] Expected EOF. Encountered input"
--   [] -> Right $ ParseResult () ""

-- choice :: [Parser a] -> Parser a
-- choice = foldr (<|>) err
--   where
--     err = Parser $ \_ -> Left "[Choice] None of the given parsers work"

-- many :: Parser a -> Parser [a]
-- many p = many1 p <|> return []

-- many1 :: Parser a -> Parser [a]
-- many1 p = do
--   first <- p
--   rest <- many p
--   return $ first : rest

-- sepBy :: Parser a -> Parser s -> Parser [a]
-- sepBy p s = sepBy1 p s <|> return []

-- sepBy1 :: Parser a -> Parser s -> Parser [a]
-- sepBy1 p s = do
--   first <- p
--   rest <- many $ s >> p
--   return $ first : rest

-- space :: Parser Char
-- space = satisfy isSpace

-- spaces :: Parser String
-- spaces = many space
