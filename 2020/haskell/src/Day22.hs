{-# LANGUAGE MultiWayIf #-}

-- import

import           Control.Monad
import           Data.Function    (on)
import qualified Data.HashSet     as S
import           Data.List
import           Data.List.Split  (splitOn)
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Input = (Deck, Deck)
type Deck  = [Card]
type Card  = Int

parseInput :: String -> Input
parseInput input = (p1, p2)
  where [p1, p2] = map (parseWith intLiteral) . tail . lines  <$> splitOn "\n\n" input



-- part1

step1 :: (Deck, Deck) -> Either Deck (Deck, Deck)
step1 (c1:d1, c2:d2)
  | c1 > c2   = Right (d1 ++ [c1,c2], d2)
  | c1 < c2   = Right (d1, d2 ++ [c2,c1])
  | otherwise = error "duplicate card"
step1 (d1, d2) = Left $ if null d2 then d1 else d2

play1 :: Input -> Deck
play1 = either id play1 . step1




-- part 2

type History = S.HashSet (Deck, Deck)

step2 :: (History, Deck, Deck) -> Either (Int, Deck) (History, Deck, Deck)
step2 (h, p1@(c1:d1), p2@(c2:d2)) =
  if | S.member (p1, p2) h -> Left (1, p1)
     | c1 <= length d1 && c2 <= length d2 ->
         case play2 (take c1 d1, take c2 d2) of
           (1, _) -> Right (h', d1 ++ [c1,c2], d2)
           (2, _) -> Right (h', d1, d2 ++ [c2,c1])
           (p, _) -> error $ show p ++ " is not a valid player"
     | c1 > c2    -> Right (h', d1 ++ [c1,c2], d2)
     | c1 < c2    -> Right (h', d1, d2 ++ [c2,c1])
     | otherwise  -> error "duplicate card"
  where h' = S.insert (p1,p2) h
step2 (_, d1, []) = Left (1, d1)
step2 (_, [], d2) = Left (2, d2)

play2 :: Input -> (Int, Deck)
play2 (d1, d2) = p (S.empty, d1, d2)
  where p = either id p . step2



-- solution

solveWith p = sum . zipWith (*) [1..] . reverse . p

part1, part2 :: Input -> Int
part1 = solveWith play1
part2 = solveWith $ snd . play2



-- main

main :: IO ()
main = aocMain 22 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10"
