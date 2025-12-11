module Day10 where

import "this" Prelude

import AOC

import Control.Parallel.Strategies
import Data.Bits
import Data.List.Extra             qualified as L
import Data.Maybe
import Data.SBV                    as SBV
import Data.SBV.List               qualified as SBV
import Text.Parsec


type Input = [Machine]

data Machine = Machine
  { desiredLights  :: Int
  , desiredJoltage :: [Int]
  , buttons        :: [[Int]]
  }
  deriving Show

parseInput :: String -> Input
parseInput = parseLinesWith do
  l <- lights
  b <- many button
  j <- joltage
  pure $ Machine l j b
  where
    lights = do
      symbol "["
      bits <- many1 light
      symbol "]"
      pure $ sum do
        (index, b) <- zip [0..] bits
        pure $ b * 2 ^ (index :: Int)
    light = choice
      [ 1 <$ char '#'
      , 0 <$ char '.'
      ]
    button = do
      symbol "("
      indices <- number `sepBy` symbol ","
      symbol ")"
      pure indices
    joltage = do
      symbol "{"
      indices <- number `sepBy` symbol ","
      symbol "}"
      pure indices


findShortestLightsSequence :: Machine -> Int
findShortestLightsSequence Machine {..} = go 0 [0]
  where
    buttonValues :: [Int]
    buttonValues = map (sum . map (2 ^)) buttons

    go :: Int -> [Int] -> Int
    go !steps values
      | desiredLights `elem` values = steps
      | otherwise =
          go (steps + 1) $ L.nubOrd do
            v <- values
            b <- buttonValues
            pure $ v `xor` b

findShortestJoltageSequence :: Machine -> IO Word16
findShortestJoltageSequence Machine {..} = do
  model <- optLexicographic do
    (buttonVariables, columns) <-
      unzip <$> for buttons \button -> do
        buttonVar <- sWord16_
        let column = do
              index <- [0..length desiredJoltage-1]
              pure $ if index `elem` button
                     then Just buttonVar
                     else Nothing
        constrain $ buttonVar .<= fromIntegral maxPresses
        pure (buttonVar, column)
    let rows = map (SBV.implode . catMaybes) $ L.transpose columns
    for_ (zip rows desiredJoltage) \(row, value) -> do
      SBV.constrain $ varSum row .== fromIntegral value
    minimize resultName $ varSum $ SBV.implode buttonVariables
  pure $ fromJust $ getModelValue resultName model
  where
    maxPresses = maximum desiredJoltage
    resultName = "fewest presses"
    varSum = SBV.foldl (+) 0

part1 :: Input -> Int
part1 = sum . parMap rpar findShortestLightsSequence

part2 :: Input -> IO Word16
part2 = fmap (sum . withStrategy (evalList rpar)) . traverse findShortestJoltageSequence


example :: String
example = "\
  \[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
  \[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
  \[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print =<< part2 testInput
  print =<< part2 realInput
