-- import

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe

import           AOC



-- input

type Input = [Layer]
type Layer = [Int]

width, height :: Int
width  = 25
height = 6

parseInput :: String -> Input
parseInput = chunksOf (width * height) . map digitToInt



-- solution

part1 :: Input -> Int
part1 layers = count 1 l * count 2 l
  where l = minimumOn (count 0) layers

part2 :: Input -> String
part2 layers = unlines $ chunksOf width image
  where findPixel = fromMaybe 2 . find (/= 2)
        image     = map (toChar . findPixel) $ transpose layers
        toChar 0 = ' '
        toChar 1 = '#'
        toChar 2 = '.'
        toChar _ = error "wut"



-- main

main :: IO ()
main = aocMain 8 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  putStrLn $ part2 input
