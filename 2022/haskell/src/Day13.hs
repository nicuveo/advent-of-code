{-# LANGUAGE OverloadedLists #-}

module Main where


-- import

import Control.Monad
import Data.List        qualified as L
import GHC.Exts         (IsList (..))
import Text.Parsec
import Text.Parsec.Char

import AOC


-- input

data Value
  = Leaf Int
  | List [Value]
  deriving (Eq)

instance Show Value where
  show = \case
    Leaf n -> show n
    List l -> show l

instance Ord Value where
  compare (Leaf x) (Leaf y) =
    compare x y
  compare (List x) (List y) =
    mconcat (zipWith compare x y) <> compare (length x) (length y)
  compare (Leaf x) (List y) =
    compare (List [Leaf x]) (List y)
  compare (List x) (Leaf y) =
    compare (List x) (List [Leaf y])

type Input = [Value]

parseInput :: String -> Input
parseInput = parseWith $ many1 value
  where
    value = list <|> leaf
    leaf  = Leaf <$> number
    list  = List <$> brackets (value `sepBy` symbol ",")


-- solution

part1 :: Input -> Int
part1 values = sum do
  (i, (x,y)) <- zip [1..] $ chunksOf2 values
  guard $ x < y
  pure i
  where
    chunksOf2 []      = []
    chunksOf2 (a:b:l) = (a,b) : chunksOf2 l
    chunksOf2 _       = error "odd number of values"

part2 :: Input -> Int
part2 values = getIndex divider1 * getIndex divider2
  where
    sorted = L.sort $ divider1 : divider2 : values
    divider1 = List [List [Leaf 2]]
    divider2 = List [List [Leaf 6]]
    getIndex n = maybe (error "divider not found") (+1) $ L.elemIndex n sorted


-- main

main :: IO ()
main = aocMain 13 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
