-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Input = ((Int, Int), (Int, Int))

parseInput :: String -> Input
parseInput = parseWith do
  symbol "target area:"
  symbol "x="
  x1 <- abs <$> number
  symbol ".."
  x2 <- abs <$> number
  symbol ","
  symbol "y="
  yMin <- number
  symbol ".."
  yMax <- number
  pure ((min x1 x2, max x1 x2), (yMin, yMax))


-- solution

triangular :: Int -> Int
triangular x = (x * (x+1)) `div` 2

part1 :: Input -> Int
part1 ((_xMin, _xMax), (yMin, yMax)) =
  if yMax > 0
  then triangular yMax
  else triangular (abs yMin - 1)

smallestVelocityForTarget :: Int -> Int
smallestVelocityForTarget target =
  head $ filter (\x -> triangular x >= target) [0..]

data Probe = Probe
  { xPos :: Int
  , yPos :: Int
  , xVel :: Int
  , yVel :: Int
  }

part2 :: Input -> Int
part2 ((xMin, xMax), (yMin, yMax)) = length $ do
  vy <- [vyMin..vyMax]
  vx <- [vxMin..vxMax]
  guard $ head $ mapMaybe getResult $ iterate step $ Probe 0 0 vx vy
  pure ()
  where
    vxMin = smallestVelocityForTarget xMin
    vxMax = xMax
    (vyMin, vyMax) = if yMin < 0
      then (yMin, abs yMin - 1)
      else (smallestVelocityForTarget yMin, yMax)
    getResult (Probe xp yp _ yv)
      | xp >= xMin && xp <= xMax && yp >= yMin && yp <= yMax = Just True
      | xp >= xMax = Just False
      | yv < 0 && yp < yMin = Just False
      | otherwise = Nothing
    step (Probe xp yp xv yv) =
      Probe (xp + xv) (yp + yv) (xv - signum xv) (yv - 1)


-- main

main :: IO ()
main = aocMain 17 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "target area: x=20..30, y=-10..-5"
