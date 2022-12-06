module Main where


-- import

import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.IntMap         qualified as M
import Data.List           (transpose)
import Data.Maybe
import Text.Parsec         as P hiding (State)
import Text.Parsec.Char

import AOC


-- input

type Crate  = Char
type Stack  = [Char]
type Stacks = M.IntMap Stack

type Instruction = (Int, Int, Int)

type Input = (Stacks, [Instruction])

parseInput :: String -> Input
parseInput = parseWith do
  rows <- try cratesRow `sepEndBy` newline
  startIndex <- spaces *> number <* (anyChar `manyTill` newline)
  newline
  instructions <- many instruction
  let stacksList = map catMaybes $ transpose rows
  pure (M.fromList $ zip [startIndex..] stacksList, instructions)
  where
    cratesRow = crate `sepBy` char ' '
    crate = emptyCrate <|> fullCrate
    emptyCrate = do
      try $ string "   "
      pure Nothing
    fullCrate = do
      char '['
      c <- letter
      char ']'
      pure $ Just c
    instruction = do
      symbol "move"
      n <- number
      symbol "from"
      f <- number
      symbol "to"
      t <- number
      pure (n, f, t)


-- solution

type Procedure = State Stacks

push :: Int -> Crate -> Procedure ()
push i c = modify $ M.adjust (c:) i

pushMultiple :: Int -> [Crate] -> Procedure ()
pushMultiple i cs = modify $ M.adjust (cs ++) i

pop :: Int -> Procedure Crate
pop i = do
  stacks <- get
  let crate = head $ stacks M.! i
      newStacks = M.adjust tail i stacks
  put newStacks
  pure crate

popMultiple :: Int -> Int -> Procedure [Crate]
popMultiple n i = do
  stacks <- get
  let crates = take n $ stacks M.! i
      newStacks = M.adjust (drop n) i stacks
  put newStacks
  pure crates

execute9000 :: Instruction -> Procedure ()
execute9000 (n, f, t) = sequence_ $ replicate n $ push t =<< pop f

execute9001 :: Instruction -> Procedure ()
execute9001 (n, f, t) = pushMultiple t =<< popMultiple n f

runProcedure :: Stacks -> Procedure () -> Stacks
runProcedure initialState action = execState action initialState

part1 :: Input -> String
part1 (stacks, instructions) = map head
  $ M.elems
  $ runProcedure stacks
  $ traverse_ execute9000 instructions

part2 :: Input -> String
part2 (stacks, instructions) = map head
  $ M.elems
  $ runProcedure stacks
  $ traverse_ execute9001 instructions


-- main

main :: IO ()
main = aocMain 05 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  putStrLn $ part1 testInput
  putStrLn $ part1 realInput
  putStrLn "# Part 2"
  putStrLn $ part2 testInput
  putStrLn $ part2 realInput

example :: String
example = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
