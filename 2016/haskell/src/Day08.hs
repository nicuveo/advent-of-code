-- module

module Day08 where



-- import

import           Control.Monad
import           Data.Array
import           Data.Bool
import           Data.Either.Utils
import           Data.List
import           Data.Monoid
import           Text.Parsec

import           Common



-- solution

day08_1 :: Solution
day08_1 = show . getSum . foldMap (Sum . fromEnum) . foldl' step empty . map parseCommand . lines


day08_2 :: Solution
day08_2 = unlines . printScreen . foldl' step empty . map parseCommand . lines



-- helpers

type Screen = Array (Int, Int) Bool

data Command = Rect Int Int
             | Row  Int Int
             | Col  Int Int

empty :: Screen
empty = array ((0, 0), (screenH - 1, screenW - 1))
        [((r, c), False) | r <- rows screenH, c <- cols screenW]

step, (|>) :: Screen -> Command -> Screen
step screen (Rect w h) = screen // [((r, c), True) | r <- rows h, c <- cols w]
step screen (Row  r d) = screen // [((r, c), screen ! (r, mod (c - d) screenW)) | c <- cols screenW]
step screen (Col  c d) = screen // [((r, c), screen ! (mod (r - d) screenH, c)) | r <- rows screenH]
(|>) = step

parseCommand :: String -> Command
parseCommand = fromRight . parse line ""
  where line = rect <|> row <|> col
        rect = try $ do
          void $ string "rect "
          w <- read <$> many1 digit
          void $ char 'x'
          h <- read <$> many1 digit
          return $ Rect w h
        row = try $ do
          void $ string "rotate row y="
          r <- read <$> many1 digit
          void $ string " by "
          d <- read <$> many1 digit
          return $ Row r d
        col = try $ do
          void $ string "rotate column x="
          c <- read <$> many1 digit
          void $ string " by "
          d <- read <$> many1 digit
          return $ Col c d

screenW, screenH :: Int
screenW = 50
screenH = 6

rows, cols :: Int -> [Int]
rows r = [0 .. r-1]
cols c = [0 .. c-1]

printScreen :: Screen -> [String]
printScreen s = [ [bool '.' '#' $ s ! (r, c) | c <- cols screenW] | r <- rows screenH ]
