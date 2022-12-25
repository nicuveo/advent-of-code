module Main where


-- import

import Data.Char

import AOC


-- snafu

toSNAFU :: Int -> String
toSNAFU = reverse . go
  where
    go 0 = []
    go n =
      let (x,y) = n `divMod` 5
          (d,c) = if y <= 2
                  then (x,   intToDigit y)
                  else (x+1, if y == 3 then '=' else '-')
       in c : go d

fromSNAFU :: String -> Int
fromSNAFU = sum . zipWith combine [0..] . map toDigit . reverse
  where
    toDigit = \case
      '2' -> 2
      '1' -> 1
      '0' -> 0
      '-' -> (-1)
      '=' -> (-2)
      _   -> error "unknown digit"
    combine :: Int -> Int -> Int
    combine f x = (5 ^ f) * x


-- solution

part1 :: String -> String
part1 = toSNAFU . sum . map fromSNAFU . lines


-- main

main :: IO ()
main = aocMain 25 $ \rawData -> do
  putStrLn "# Part 1"
  putStrLn $ toSNAFU 2022
  putStrLn $ toSNAFU 12345
  print $ fromSNAFU "1=11-2"
  print $ fromSNAFU "1-0---0"
  putStrLn $ part1 example
  putStrLn $ part1 rawData

example :: String
example = "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122"
