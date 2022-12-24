module Main where


-- import

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as M
import Data.HashSet        (HashSet)
import Data.HashSet        qualified as S
import Data.List           qualified as L
import Data.Maybe
import Safe

import AOC

import Debug.Trace


-- input

data Input = Input
  { blizzards   :: HashSet Blizzard
  , startPoint  :: Point
  , endPoint    :: Point
  , fieldWidth  :: Int
  , fieldHeight :: Int
  }

type Blizzard = (Point, Vector)

parseInput :: String -> Input
parseInput (lines -> input) = Input
  { blizzards   = blizs
  , startPoint  = Point (findDot (head input) - 1) (-1)
  , endPoint    = Point (findDot (last input) - 1) (length input - 2)
  , fieldWidth  = length (head input) - 2
  , fieldHeight = length input - 2
  }
  where
    findDot = fromJust . L.elemIndex '.'
    blizs = S.fromList do
      (y, r) <- zip [-1..] input
      (x, c) <- zip [-1..] r
      case c of
        '^' -> pure (Point x y, directionVector N)
        'v' -> pure (Point x y, directionVector S)
        '<' -> pure (Point x y, directionVector W)
        '>' -> pure (Point x y, directionVector E)
        _   -> mzero


-- bfs

moveExpedition :: HashSet Point -> Point -> Int -> Int -> Point -> HashSet Point
moveExpedition danger endPoint valleyW valleyH p = S.fromList do
  np@(Point x y) <- p : fourSurroundingPoints p
  guard $ np == p || np == endPoint || x >= 0 && x < valleyW && y >= 0 && y < valleyH
  guard $ not $ np `S.member` danger
  pure np

moveBlizzard :: Int -> Int -> Blizzard -> Blizzard
moveBlizzard valleyW valleyH (Point px py, d@(Point dx dy)) = (Point nx ny, d)
  where
    nx = mod (px + dx) valleyW
    ny = mod (py + dy) valleyH

step :: Point -> Int -> Int -> (HashSet Blizzard, HashSet Point) -> (HashSet Blizzard, HashSet Point)
step endPoint valleyW valleyH (!bliz, !ps) =
  (bliz', foldMap (moveExpedition danger endPoint valleyW valleyH) ps)
  where
    bliz' = S.map (moveBlizzard valleyW valleyH) bliz
    danger = S.map fst bliz'


-- solution

steps :: Point -> Int -> Int -> (HashSet Blizzard, HashSet Point) -> (Int, HashSet Blizzard)
steps goal valleyW valleyH = go 0
  where
    go !n s@(bs, ps)
      | goal `S.member` ps = (n, bs)
      | otherwise          = go (n+1) $ step goal valleyW valleyH s

part1 :: Input -> Int
part1 Input {..} = fst $ steps endPoint fieldWidth fieldHeight (blizzards, S.singleton startPoint)

part2 :: Input -> Int
part2 Input {..} = n1+n2+n3
  where
    (n1, b1) = steps   endPoint fieldWidth fieldHeight (blizzards, S.singleton startPoint)
    (n2, b2) = steps startPoint fieldWidth fieldHeight (       b1, S.singleton   endPoint)
    (n3,  _) = steps   endPoint fieldWidth fieldHeight (       b2, S.singleton startPoint)


-- main

main :: IO ()
main = aocMain 24 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"
