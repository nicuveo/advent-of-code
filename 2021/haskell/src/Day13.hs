-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.HashSet     (HashSet)
import qualified Data.HashSet     as S
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC.Parsing
import           AOC.Runtime


-- input

type Input = (HashSet Point, [Fold])

parseInput :: String -> Input
parseInput = parseWith $ do
  points <- many1 point
  folds  <- many1 fold
  eof
  pure (S.fromList points, folds)
  where
    point = do
      x <- number
      symbol ","
      y <- number
      pure (x, y)
    fold = tryAll
      [ foldX
      , foldY
      ]
    foldX = do
      symbol "fold along x="
      x <- number
      pure $ FoldX x
    foldY = do
      symbol "fold along y="
      y <- number
      pure $ FoldY y


-- solution

type Point = (Int, Int)
data Fold
  = FoldX Int
  | FoldY Int
  deriving (Show, Eq)

apply :: HashSet Point -> Fold -> HashSet Point
apply set fold = S.map (applyToPoint fold) set
  where
    applyToPoint fold (x,y) = case fold of
      FoldX l -> (l - abs (x - l), y)
      FoldY l -> (x, l - abs (y - l))

applyAll :: HashSet Point -> [Fold] -> HashSet Point
applyAll = foldl' apply

display :: HashSet Point -> String
display set = unlines $ do
  y <- [minY .. maxY]
  pure $ do
    x <- [minX .. maxX]
    pure $ if (x,y) `S.member` set then '#' else ' '
  where
    points = S.toList set
    allXs  = map fst points
    allYs  = map snd points
    minX   = minimum allXs
    minY   = minimum allYs
    maxX   = maximum allXs
    maxY   = maximum allYs

part1 :: Input -> Int
part1 (set, folds) = S.size $ apply set (head folds)

part2 :: Input -> String
part2 (set, folds) = display $ applyAll set folds


-- main

main :: IO ()
main = aocMain 13 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  putStrLn $ part2 testInput
  putStrLn $ part2 realInput

example :: String
example = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
