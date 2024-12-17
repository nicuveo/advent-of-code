module Day14 where


-- import

import AOC
import AOC.Debug.Animate
import "this" Prelude

import Data.HashMap.Strict qualified as M
import Data.List           (nub)


-- input

type Input = [Robot]
type Robot = (Point, Vector)

parseInput :: String -> Input
parseInput = parseLinesWith robot
  where
    robot = do
      symbol "p="
      px <- number
      symbol ","
      py <- number
      symbol "v="
      vx <- number
      symbol ","
      vy <- number
      pure (Point px py, Point vx vy)


-- solution

sortByQuadrant :: Int -> Int -> [Robot] -> [Int]
sortByQuadrant w h positions = M.elems $ M.fromListWith (+) do
  (Point x y, _) <- positions
  guard $ (2 * x + 1) /= w
  guard $ (2 * y + 1) /= h
  pure ((x < div w 2, y < div h 2), 1)

moveRobot :: Int -> Int -> Int -> Robot -> Robot
moveRobot w h times (Point px py, v@(Point vx vy)) =
  ( Point
    ((px + times * vx) `mod` w)
    ((py + times * vy) `mod` h)
  , v
  )

part1 :: Int -> Int -> Input -> Int
part1 w h = product . sortByQuadrant w h . map (moveRobot w h 100)

stepRobots :: (Int, [Robot]) -> Maybe ([String], (Int, [Robot]))
stepRobots (maxScore, robots) = Just ([], nextFrame robots)
  where
    nextFrame rs =
      let candidate = map (moveRobot 101 103 1) rs
          quadrants = sortByQuadrant 101 103 candidate
          score     = product quadrants
      in  if score > maxScore then (score, candidate) else nextFrame candidate

renderRobots :: (Int, [Robot]) -> String
renderRobots (_, robots) = unlines do
  row <- [0..102]
  pure do
    col <- [0..100]
    if Point col row `elem` map fst robots then "#" else "."


-- main

example :: String
example = "\
\p=0,4 v=3,-3\n\
\p=6,3 v=-1,-3\n\
\p=10,3 v=-1,2\n\
\p=2,0 v=2,-1\n\
\p=0,0 v=1,3\n\
\p=3,0 v=-2,-2\n\
\p=7,6 v=-1,-3\n\
\p=3,0 v=-1,-2\n\
\p=9,3 v=2,3\n\
\p=7,3 v=-1,2\n\
\p=2,4 v=2,-3\n\
\p=9,5 v=-3,-3"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1  11   7 testInput
  print $ part1 101 103 realInput
  animate 1000 resetCursor renderRobots stepRobots ([], (0, realInput))
