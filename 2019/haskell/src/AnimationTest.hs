{-# LANGUAGE TupleSections #-}

import           Data.Bool
import           Data.Ratio
import           System.Random

import           AOC
import           AOC.Debug
import           AOC.Map.Flat


type GameOfLife = FlatMap Bool


tlColor = interpolate (2%3) white red
trColor = interpolate (2%3) white yellow
blColor = interpolate (2%3) white green
brColor = interpolate (2%3) white blue

w = 50
h = 50


mkGame :: StdGen -> GameOfLife
mkGame = makeFlatMapFromList w h . take (w*h) . randoms

display :: GameOfLife -> String
display = displayWith $ \p ->  bool "  " $ fgColor (colorOf p) "##"
  where colorOf (Point y x) =
          let c1 = interpolate (x % (w-1)) tlColor trColor
              c2 = interpolate (x % (w-1)) blColor brColor
          in interpolate (y % (h-1)) c1 c2

step :: GameOfLife -> GameOfLife
step g = flip pmap g $ \p b -> status b $ countTrue $ eightMapNeighboursOf g p
  where status True  x = x `elem` [2,3]
        status False x = x == 3


main :: IO ()
main = do
  n <- getStdGen
  let s = return ([], mkGame n)
      r = display
      u = return . Just . ([],) . step
  animate defaultDelay r u s
