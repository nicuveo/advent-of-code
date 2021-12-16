-- import

import           Control.Monad
import           Data.Char
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC
import           AOC.Map.Flat


-- input

type Cave = FlatMap Int

parseCave :: String -> Cave
parseCave = from2DList . map toRow . lines
  where
    toRow = map digitToInt

parseLargeCave :: String -> Cave
parseLargeCave =
  from2DList . concat . take 5 . iterate (map increase) . map toRow . lines
  where
    toRow = concat . take 5 . iterate increase . map digitToInt
    increase = map $ \x -> (x `mod` 9) + 1


-- solution

edges
  :: Cave
  -> Point
  -> [(Int, Point)]
edges cave point = zip
  (fourMapNeighboursOf cave point)
  (fourNeighbouringPointsOf cave point)

heuristic
  :: Point
  -> Point
  -> Int
heuristic target point = manhattanNorm $ target - point

solve :: Cave -> Int
solve cave = fst $ last $ unsafeFindPathWith (edges cave) (heuristic end) start end
  where
    (start, end) = bounds cave


-- main

main :: IO ()
main = aocMain 15 $ \rawData -> do
  putStrLn "# Part 1"
  print $ solve $ parseCave example
  print $ solve $ parseCave rawData
  putStrLn "# Part 2"
  print $ solve $ parseLargeCave example
  print $ solve $ parseLargeCave rawData

example :: String
example = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
