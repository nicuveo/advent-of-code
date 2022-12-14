module Main where


-- import

import Control.Applicative
import Data.Functor
import Data.HashMap.Strict (HashMap, (!?))
import Data.HashMap.Strict qualified as M
import Text.Parsec         hiding ((<|>))
import Text.Parsec.Char

import AOC
import AOC.Debug.Animate
import AOC.Debug.Color


-- input

data Input = Input
  { inputGrid :: HashMap Point Cell
  , mapBottom :: Int
  }

data Cell
  = Rock
  | Sand
  deriving (Eq)

parseInput :: String -> Input
parseInput = createInput . concatMap expand . parseLinesWith path
  where
    path = point `sepBy` symbol "->"
    point = do
      x <- number
      symbol ","
      y <- number
      pure $ Point x y

expand :: [Point] -> [Point]
expand path = head path : concat (zipWith construct path (tail path))
  where
    construct start end =
      let diff = end - start
          incv = signum diff
       in take (manhattanNorm diff) $ tail $ iterate (+incv) start

createInput :: [Point] -> Input
createInput points = Input
  { inputGrid = M.fromList $ map (,Rock) points
  , mapBottom = maximum $ map py points
  }


-- solution

data SimulationState = SimulationState
  { gridState     :: HashMap Point Cell
  , threshold     :: Int
  , currentGrain  :: Point
  , settledGrains :: Int
  }

startPoint :: Point
startPoint = Point 500 0

initState :: Input -> SimulationState
initState Input {..} = SimulationState
  { gridState = inputGrid
  , threshold = mapBottom
  , currentGrain = startPoint
  , settledGrains = 0
  }

moveGrain :: HashMap Point Cell -> Point -> Maybe Point
moveGrain grid grain = down <|> downLeft <|> downRight
  where
    available p = if M.member p grid then Nothing else Just p
    down      = available $ grain + Point 0    1
    downLeft  = available $ grain + Point (-1) 1
    downRight = available $ grain + Point 1    1

step1 :: SimulationState -> Maybe SimulationState
step1 s@SimulationState {..} =
  case moveGrain gridState currentGrain of
    Just nextPos
      | py nextPos > threshold -> Nothing
      | otherwise -> Just $ s { currentGrain = nextPos }
    Nothing -> Just $ s
      { currentGrain  = startPoint
      , gridState     = M.insert currentGrain Sand gridState
      , settledGrains = settledGrains + 1
      }

step2 :: SimulationState -> Maybe SimulationState
step2 s@SimulationState {..}
  | M.member currentGrain gridState = Nothing
  | py currentGrain == threshold + 1 = reset
  | otherwise = case moveGrain gridState currentGrain of
      Just nextPos -> Just $ s { currentGrain = nextPos }
      Nothing      -> reset
  where
    reset = Just $ s
      { currentGrain  = startPoint
      , gridState     = M.insert currentGrain Sand gridState
      , settledGrains = settledGrains + 1
      }


-- solution

part1 :: Input -> Int
part1 = settledGrains . run . initState
  where
    run s = maybe s run $ step1 s

part2 :: Input -> Int
part2 = settledGrains . run . initState
  where
    run s = maybe s run $ step2 s


-- main

nextState :: SimulationState -> Maybe ([String], SimulationState)
nextState = fmap ([],) . step2

displayState :: SimulationState -> String
displayState SimulationState {..} = unlines $
  [yMin..yMax] <&> \y -> concat $
    [xMin..xMax] <&> \x ->
      case gridState !? Point x y of
        Nothing   -> if Point x y == currentGrain
          then fgColor (RGB 100 220   0) "o"
          else " "
        Just Sand -> fgColor (RGB 200 200   0) "@"
        Just Rock -> fgColor (RGB 200 200 200) "#"
  where
    xMin = minimum $ map px $ startPoint : M.keys gridState
    xMax = maximum $ map px $ startPoint : M.keys gridState
    yMin = minimum $ map py $ startPoint : M.keys gridState
    yMax = maximum $ map py $ startPoint : M.keys gridState

main :: IO ()
main = aocMain 14 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  -- animate 100 resetCursor displayState nextState ([], initState testInput)
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
