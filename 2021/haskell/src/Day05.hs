-- import

import           Control.Monad
import           Data.Function       (on)
import           Data.Functor        ((<&>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Maybe
import           Text.Parsec         hiding (Line)
import           Text.Parsec.Char

import           AOC                 hiding (Point)


-- input

type Input = [Line]
type Point = (Int, Int)
type Line  = (Point, Point)

parseInput :: String -> Input
parseInput = parseLinesWith line
  where
    line = do
      p1 <- point
      symbol "->"
      p2 <- point
      pure (p1, p2)
    point = do
      x <- number
      symbol ","
      y <- number
      pure (x, y)


-- solution

isOrtholinear :: Line -> Bool
isOrtholinear ((x1,y1), (x2,y2)) = x1 == x2 || y1 == y2

toPointMap :: Line -> HashMap Point Int
toPointMap ((x1,y1), (x2,y2)) = M.fromList $ [0..steps] <&> \s ->
  ((x1 + unitX * s, y1 + unitY * s), 1)
  where
    xDiff = x2 - x1
    yDiff = y2 - y1
    steps = gcd xDiff yDiff
    unitX = xDiff `div` steps
    unitY = yDiff `div` steps

coalesce :: [HashMap Point Int] -> HashMap Point Int
coalesce = foldl' (M.unionWith (+)) M.empty

part1 :: Input -> Int
part1 = M.size
  . M.filter (>1)
  . coalesce
  . map toPointMap
  . filter isOrtholinear

part2 :: Input -> Int
part2 = M.size
  . M.filter (>1)
  . coalesce
  . map toPointMap


-- main

main :: IO ()
main = aocMain 5 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
