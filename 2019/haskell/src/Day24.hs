-- import

import           Data.Bool
import           Data.List
import           Data.Maybe

import           AOC
import           AOC.Map.Flat



-- game of life

type GameOfLife = FlatMap Bool

parseGame :: String -> GameOfLife
parseGame = from2DList . map (map (== '#')) . lines

step :: GameOfLife -> GameOfLife
step g = pmap f g
  where f p a = let c = countTrue $ fourMapNeighboursOf g p in
          if a then c == 1 else c == 1 || c == 2

rating :: GameOfLife -> Int
rating = sum . zipWith (*) (map (2^) [0..]) . map fromEnum . toList

render :: GameOfLife -> String
render = displayWith $ const $ bool "." "#"



-- solution

part1 :: GameOfLife -> Int
part1 = rating . head . snd . findCycle step

part2 :: GameOfLife -> String
part2 = undefined
  where f = undefined



-- main

main :: IO ()
main = aocMain 24 $ \rawInput -> do
  let input = parseGame rawInput
  print $ part1 input
  print $ part2 input
