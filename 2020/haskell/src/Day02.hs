{-# LANGUAGE RecordWildCards #-}

-- import

import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Input = [Password]

data Password = PW { minAmount :: Int
                   , maxAmount :: Int
                   , requiredC :: Char
                   , password  :: String
                   } deriving Show

parseInput :: String -> Input
parseInput = parseLinesWith line
  where line = do
          minA <- intLiteral
          char '-'
          maxA <- intLiteral
          req  <- lower
          symbol ":"
          pw   <- many anyChar
          pure $ PW minA maxA req pw



-- solution

part1 :: Input -> Int
part1 = length . filter isValid
  where isValid PW{..} =
          let occurences = AOC.count requiredC password
          in  minAmount <= occurences && occurences <= maxAmount

part2 :: Input -> Int
part2 = length . filter isValid
  where isValid PW{..} =
          let firstIsChar  = requiredC == (password !! (minAmount-1))
              secondIsChar = requiredC == (password !! (maxAmount-1))
          in  firstIsChar /= secondIsChar



-- main

main :: IO ()
main = aocMain 2 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Given example"
  print $ part1 testInput
  print $ part2 testInput
  putStrLn "# Real input"
  print $ part1 realInput
  print $ part2 realInput

example :: String
example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
