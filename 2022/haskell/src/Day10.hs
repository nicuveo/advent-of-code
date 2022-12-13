module Main where


-- import

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List.Split      (chunksOf)
import Prelude              hiding (cycle)
import Text.Parsec          hiding (State)
import Text.Parsec.Char

import AOC


-- input

data Command
  = Noop
  | AddX Int

type Input = [Command]

parseInput :: String -> Input
parseInput = parseLinesWith line
  where
    line = noop <|> addx
    noop = Noop <$ symbol "noop"
    addx = do
      symbol "addx"
      AddX <$> number


-- solution

data CPU = CPU
  { cycle :: Int
  , xReg  :: Int
  }

type Compute w = StateT CPU (Writer w)

runCompute :: Monoid w => Compute w a -> w
runCompute = execWriter . flip evalStateT startCPU
  where
    startCPU = CPU 0 1

exec :: Monoid w => Compute w () -> Command -> Compute w ()
exec step = \case
  Noop   -> step
  AddX n -> do
    step
    step
    modify \s -> s { xReg = xReg s + n }

part1 :: Input -> Int
part1 = sum . runCompute . traverse_ (exec step)
  where
    step :: Compute [Int] ()
    step = do
      modify \s -> s { cycle = cycle s + 1 }
      CPU currentCycle x <- get
      when (mod (currentCycle - 20) 40 == 0) $
        tell [x * currentCycle]

part2 :: Input -> [String]
part2 = chunksOf 80 . runCompute . traverse_ (exec step)
  where
    step :: Compute String ()
    step = do
      CPU currentCycle x <- get
      let d = x - mod currentCycle 40
      tell $ if abs d <= 1 then "▇▇" else "  "
      modify \s -> s { cycle = cycle s + 1 }


-- main

main :: IO ()
main = aocMain 10 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  traverse_ putStrLn $ part2 testInput
  putStrLn ""
  traverse_ putStrLn $ part2 realInput

example :: String
example = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"
