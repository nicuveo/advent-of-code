-- import

import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.Foldable
import           Data.Function       (on)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S
import           Data.List
import           Data.List
import           Data.Maybe
import           Text.Parsec         hiding (State)
import           Text.Parsec.Char

import           AOC
import           AOC.Map.Flat


-- input

type Input = FlatMap Int

parseInput :: String -> Input
parseInput = from2DList . parseWith heightmap
  where
    heightmap = many1 cell `sepEndBy1` newline
    cell      = fmap digitToInt digit


-- solution

lowPoints :: Input -> [Point]
lowPoints hm = filter lowerThanNeighbours $ allPoints hm
  where
    lowerThanNeighbours p = all (> (hm ! p)) $ fourMapNeighboursOf hm p

basin :: Input -> Point -> HashSet Point
basin hm = flip execState S.empty . go
  where
    go :: Point -> State (HashSet Point) ()
    go p = do
      cache <- get
      when (not (p `S.member` cache) && (hm ! p) < 9) $ do
        modify $ S.insert p
        traverse_ go $ fourNeighbouringPointsOf hm p

part1 :: Input -> Int
part1 hm = sum $ map ((+1) . (hm !)) $ lowPoints hm

part2 :: Input -> Int
part2 hm = product
  $ take 3
  $ sortOn negate
  $ map S.size
  $ nub
  $ map (basin hm)
  $ lowPoints hm


-- main

main :: IO ()
main = aocMain 9 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  -- putStrLn $ displayWith (const show) testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
