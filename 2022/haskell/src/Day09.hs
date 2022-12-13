module Main where


-- import

import Data.Function
import Data.List          qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import AOC.Parsing
import AOC.Runtime


-- input

data Direction
  = U
  | D
  | R
  | L
  deriving (Eq, Show)

type Input = [Direction]

parseInput :: String -> Input
parseInput = concat . parseLinesWith line
  where
    line = do
      d <- direction
      n <- number
      pure $ replicate n d
    direction = tryAll
      [ U <$ symbol "U"
      , D <$ symbol "D"
      , L <$ symbol "L"
      , R <$ symbol "R"
      ]


-- solution

data Point = Point
  { px :: Int
  , py :: Int
  }
  deriving (Eq)

instance Show Point where
  show (Point x y) = printf "{%2d, %2d}" x y

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) =
  (max `on` abs) (x1 - x2) (y1 - y2)

move :: Direction -> Point -> Point
move d p = case d of
  D -> p { py = py p - 1 }
  U -> p { py = py p + 1 }
  L -> p { px = px p - 1 }
  R -> p { px = px p + 1 }

follow :: Point -> Point -> Point
follow ref p
  | distance ref p <= 1 = p
  | otherwise = Point
      { px = px p + signum (px ref - px p)
      , py = py p + signum (py ref - py p)
      }

part1 :: Input -> Int
part1 = length
  . L.nub
  . map snd
  . L.scanl' step start
  where
    start :: (Point, Point)
    start = (Point 0 0, Point 0 0)
    step :: (Point, Point) -> Direction -> (Point, Point)
    step (h, t) d =
      let nh = move d h in (nh, follow nh t)

part2 :: Input -> Int
part2 = length
  . L.nub
  . map NE.last
  . L.scanl' step start
  where
    start :: NonEmpty Point
    start = Point 0 0 :| replicate 9 (Point 0 0)
    step :: NonEmpty Point -> Direction -> NonEmpty Point
    step (h :| ts) d =
      let nh = move d h
       in nh :| snd (L.mapAccumL followPrevious nh ts)
    followPrevious :: Point -> Point -> (Point, Point)
    followPrevious ref p =
      let np = follow ref p
       in (np, np)


-- main

main :: IO ()
main = aocMain 09 $ \rawData -> do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

example1 :: String
example1 = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

example2 :: String
example2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
