module Day03 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Control.Applicative
import Data.Char           (digitToInt, isDigit)
import Data.List           (nub)


-- input

type Input = Grid Char

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

goLeft :: Point -> Point
goLeft (Point x y) = Point (x-1) y

goRight :: Point -> Point
goRight (Point x y) = Point (x+1) y

findAllSymbols :: Input -> [Point]
findAllSymbols grid = filter isSymbol $ allPoints grid
  where
    isSymbol p = let c = grid ! p in
      if | isDigit c -> False
         | c == '.'  -> False
         | otherwise -> True

findAllStars :: Input -> [Point]
findAllStars grid = filter isStar $ allPoints grid
  where
    isStar p = (grid ! p) == '*'

findNumbers :: Input -> Point -> [Point]
findNumbers grid = nub
  . mapMaybe findNumberStartingPoint
  . gridEightSurroundingPoints grid
  where
    findNumberStartingPoint p = case grid !? p of
      Nothing -> Nothing
      Just c  ->
        if | not (isDigit c) -> Nothing
           | otherwise       -> findNumberStartingPoint (goLeft p) <|> Just p

extractNumber :: Input -> Point -> Int
extractNumber grid = go 0
  where
    go result p = case grid !? p of
      Nothing -> result
      Just c  ->
        if | not (isDigit c) -> result
           | otherwise       -> go (result * 10 + digitToInt c) (goRight p)

gearRatio :: Input -> Point -> Maybe Int
gearRatio grid p = case findNumbers grid p of
  [p1, p2] -> Just $ extractNumber grid p1 * extractNumber grid p2
  _        -> Nothing

part1 :: Input -> Int
part1 grid = sum
  $ map (extractNumber grid)
  $ nub
  $ concatMap (findNumbers grid)
  $ findAllSymbols grid

part2 :: Input -> Int
part2 grid = sum
  $ mapMaybe (gearRatio grid)
  $ findAllStars grid


-- main

example :: String
example = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput
