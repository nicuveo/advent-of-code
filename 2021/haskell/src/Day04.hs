-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.IntSet      (IntSet)
import qualified Data.IntSet      as S
import           Data.List
import           Data.Maybe
import           Text.Parsec      as P
import           Text.Parsec.Char

import           AOC


-- input

type Input = ([Int], [Board])

parseInput :: String -> Input
parseInput = parseWith $ do
  draws  <- number `sepBy` symbol ","
  boards <- many board
  pure (draws, map Board boards)
  where
    board = P.count 5 $ P.count 5 number


-- game

newtype Board = Board [[Int]]

type History = IntSet

instance Show Board where
  show (Board board) = unlines $ map show board

isWinner :: History -> Board -> Bool
isWinner history (Board b) =
  any wins b || any wins (transpose b)
  where
    wins = all (\x -> S.member x history)

unmarkedNumbers :: History -> Board -> [Int]
unmarkedNumbers h (Board b) = filter notSeen $ concat b
  where
    notSeen x = x `S.notMember` h


-- solution

part1 :: Input -> Int
part1 (draws, boards) = go S.empty draws
  where
    go :: History -> [Int] -> Int
    go h = \case
      []     -> error "reached end of input, did not find a winner"
      (d:ds) ->
        let
          newHistory = S.insert d h
        in
          case find (isWinner newHistory) boards of
            Just b  -> d * sum (unmarkedNumbers newHistory b)
            Nothing -> go newHistory ds

part2 :: Input -> Int
part2 = uncurry $ go S.empty
  where
    go :: History -> [Int] -> [Board] -> Int
    go history draws boards = case draws of
      []     -> error "reached end of input, did not find a result"
      (d:ds) ->
        let
          newHistory = S.insert d history
          (winners, losers) = partition (isWinner newHistory) boards
        in
          case (winners, losers) of
            ([b], []) -> d * sum (unmarkedNumbers newHistory b)
            (_,   l)  -> go newHistory ds l


-- main

main :: IO ()
main = aocMain 4 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"
