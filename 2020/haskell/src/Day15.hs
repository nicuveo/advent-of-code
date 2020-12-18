{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

-- import

import           Control.Monad
import           Data.Function      (on)
import           Data.Hashable
import qualified Data.IntMap.Strict as M
import           Data.List
import           Data.Maybe
import           Text.Parsec        hiding (State)
import           Text.Parsec.Char

import           AOC

import           Debug.Trace
import           Text.Printf



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = parseWith $ intLiteral `sepBy` symbol ","



-- solution

type History = M.IntMap Int

data State = State { history :: !History
                   , spoken  :: !Int
                   , turnN   :: !Int
                   } deriving (Show)

extractKey :: State -> [(Int, Int)]
extractKey (State h s t) =
  sort $ (s,0) : [(n,t-x) | (n,x) <- M.assocs h]


step :: State -> State
step (State h n t) = State (M.insert n t h) s (t+1)
  where s = maybe 0 (t-) $ M.lookup n h

mkState :: Input -> State
mkState (first:numbers) = foldl' go (State M.empty first 1) numbers
  where go (State h n t) x = State (M.insert n t h) x (t+1)

spokenAt :: Input -> Int -> Int
spokenAt input target = go $ mkState input
  where go !current
          | turnN current == target = spoken current
          | otherwise               = go $ step current

part1 :: Input -> Int
part1 input = spokenAt input 2020

part2 :: Input -> Int
part2 input = spokenAt input 30000000



-- main

main :: IO ()
main = aocMain 15 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "0,3,6"
