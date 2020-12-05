{-# LANGUAGE ViewPatterns #-}


-- import

import           Control.Monad
import           Data.Function    (on)
import qualified Data.IntSet      as S
import           Data.List
import           Data.Maybe
import           Numeric
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Ticket = Int
type Input  = [Ticket]

parseInput :: String -> Input
parseInput = map readTicket . lines
  where readTicket = fst . head . readInt 2 (const True) (fromEnum . (`elem` "BR"))



-- solution

part1 :: Input -> Int
part1 = maximum

part2 :: Input -> Int
part2 = findSeat . sort
  where findSeat (x:y:tickets)
          | y - x == 2 = x + 1
          | otherwise  = findSeat (y:tickets)
        findSeat _ = error "missed my seat"



-- main

main :: IO ()
main = aocMain 5 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  -- print $ part2 testInput
  print $ part2 realInput

example :: String
example = "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL"
