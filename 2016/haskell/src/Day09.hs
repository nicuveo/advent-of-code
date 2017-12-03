-- module

module Day09 (day09_1, day09_2) where



-- import

import           Control.Monad
import           Data.Either.Utils
import           Text.Parsec

import           Common



-- solution

day09_1 :: Solution
day09_1 = show . parseData1 . join . lines


day09_2 :: Solution
day09_2 = show . parseData2 . join . lines



-- helpers

parseData1 :: String -> Int
parseData1 = sum . fromRight . parse (many text) ""
  where text = try repeated <|> expanded
        repeated = do
          (t, p) <- repeatedPattern
          return $ t * length p

parseData2 :: String -> Int
parseData2 = sum . fromRight . parse (many text) ""
  where text = try repeated <|> expanded
        repeated = do
          (t, p) <- repeatedPattern
          return $ t * parseData2 p

expanded :: Parsec String u Int
expanded = length <$> many1 upper

repeatedPattern :: Parsec String u (Int, String)
repeatedPattern = do
  void $ char '('
  n <- read <$> many1 digit
  void $ char 'x'
  t <- read <$> many1 digit
  void $ char ')'
  p <- count n anyChar
  return (t, p)
