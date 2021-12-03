-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

data Command
  = Forward Int
  | Up      Int
  | Down    Int
  deriving (Show, Eq)

type Input = [Command]

parseInput :: String -> Input
parseInput = parseLinesWith $ choice
  [ Forward <$> command "forward"
  , Up      <$> command "up"
  , Down    <$> command "down"
  ]
  where
    command n = symbol n >> number


-- solution

data Submarine = Submarine
  { subPosition :: Int
  , subDepth    :: Int
  , subAim      :: Int
  }
  deriving (Show, Eq)

start :: Submarine
start = Submarine 0 0 0

apply1 :: Submarine -> Command -> Submarine
apply1 s@(Submarine p d _) c = case c of
  Forward n -> s { subPosition = p+n }
  Up      n -> s { subDepth    = d-n }
  Down    n -> s { subDepth    = d+n }

apply2 :: Submarine -> Command -> Submarine
apply2 s@(Submarine p d a) c = case c of
  Up      n -> s { subAim      = a-n }
  Down    n -> s { subAim      = a+n }
  Forward n -> s { subPosition = p+n
                 , subDepth    = d+n*a
                 }

translate :: Submarine -> Int
translate (Submarine p d _) = p * d

part1 :: Input -> Int
part1 = translate . foldl' apply1 start

part2 :: Input -> Int
part2 = translate . foldl' apply2 start


-- main

main :: IO ()
main = aocMain 2 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  print testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
