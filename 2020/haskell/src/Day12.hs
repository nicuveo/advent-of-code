{-# LANGUAGE LambdaCase #-}

-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Prelude          hiding (Left, Right)
import           Text.Parsec.Char

import           AOC



-- input

data Step
  = North   Int
  | West    Int
  | South   Int
  | East    Int
  | Left    Int
  | Right   Int
  | Forward Int
  deriving (Show, Eq)

type Path = [Step]

parseInput :: String -> Path
parseInput = parseLinesWith line
  where line = do
          inst  <- upper
          value <- intLiteral
          pure $ case inst of
            'N' -> North   value
            'W' -> West    value
            'S' -> South   value
            'E' -> East    value
            'L' -> Left    value
            'R' -> Right   value
            'F' -> Forward value
            _   -> error "wrong input format"



-- part 1

type Position = (Point, Direction)

step1 :: Position -> Step -> Position
step1 (p,d) s = case s of
  North   n -> (p + directionVector N *. n, d)
  West    n -> (p + directionVector W *. n, d)
  South   n -> (p + directionVector S *. n, d)
  East    n -> (p + directionVector E *. n, d)
  Forward n -> (p + directionVector d *. n, d)
  Left    n -> (p, rL n d)
  Right   n -> (p, rR n d)
  where rL 0 = id
        rL n = rL (n-90) . rotateL
        rR 0 = id
        rR n = rR (n-90) . rotateR

startingPos :: Position
startingPos = (Point 0 0, E)

part1 :: Path -> Int
part1 = manhattanNorm . fst . foldl' step1 startingPos



-- part 2

type State = (Vector, Point)

step2 :: State -> Step -> State
step2 (w,s) i = case i of
  North   n -> (w + directionVector N *. n, s)
  West    n -> (w + directionVector W *. n, s)
  South   n -> (w + directionVector S *. n, s)
  East    n -> (w + directionVector E *. n, s)
  Left    n -> (rL n w, s)
  Right   n -> (rR n w, s)
  Forward n -> (w, s + w *. n)
  where rL 0 = id
        rL n = rL (n-90) . rotate90L
        rR 0 = id
        rR n = rR (n-90) . rotate90R

startingState :: State
startingState = (Point (-1) 10, Point 0 0)

part2 :: Path -> Int
part2 = manhattanNorm . snd . foldl' step2 startingState



-- main

main :: IO ()
main = aocMain 12 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "F10\nN3\nF7\nR90\nF11"
