module Day02 where


-- import

import AOC
import "this" Prelude

import Text.Parsec


-- input

type Input = [Game]
type Game  = [Draw]
type Draw  = (Int, Int, Int)

data Color = R | G | B
  deriving Show

parseInput :: String -> Input
parseInput = parseLinesWith game
  where
    game = do
      symbol "Game"
      number
      symbol ":"
      draw `sepBy1` symbol ";"
    draw = do
      cubes <- cube `sepBy` symbol ","
      pure $ foldl' sumDraw (0, 0, 0) cubes
    cube = do
      amount <- number
      color  <- choice
        [ R <$ symbol "red"
        , G <$ symbol "green"
        , B <$ symbol "blue"
        ]
      pure $ case color of
        R -> (amount, 0, 0)
        G -> (0, amount, 0)
        B -> (0, 0, amount)
    sumDraw (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)


-- solution

part1 :: Input -> Int
part1 = sum . map fst . filter (all isValid . snd) . zip [1..]
  where
    isValid (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 :: Input -> Int
part2 = sum . map power
  where
    power game =
      let (maxR, maxG, maxB) = foldl' maxDraw (0, 0, 0) game
      in  maxR * maxG * maxB
    maxDraw (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)


-- main

example :: String
example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

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
