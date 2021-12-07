-- import

import           Control.Monad
import           Control.Monad.State
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Debug.Trace
import           Text.Parsec         (sepBy)

import           AOC


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = parseWith $ number `sepBy` symbol ","


-- solution

type Cache = HashMap Int Integer

fishes :: Int -> State Cache Integer
fishes day = do
  cache <- get
  case M.lookup day cache of
    Just result -> pure result
    Nothing     -> do
      allBranches <- traverse fishes [day-9,day-16..0]
      let result = sum allBranches + 1
      modify $ M.insert day result
      pure result

totalFishes :: Int -> Input -> Integer
totalFishes deadline input = flip evalState M.empty $ do
  result <- for input $ \fish ->
    fishes (deadline + 8 - fish)
  pure $ sum result

part1 :: Input -> Integer
part1 = totalFishes 80

part2 :: Input -> Integer
part2 = totalFishes 2021


-- main

main :: IO ()
main = aocMain 6 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "3,4,3,1,2"
