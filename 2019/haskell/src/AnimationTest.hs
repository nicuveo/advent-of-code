{-# LANGUAGE TupleSections #-}

import           Data.Bool
import           Data.Ratio
import           System.Random

import           AOC
import           AOC.Debug
import           AOC.Map.Flat


type GameOfLife = FlatMap Bool


mkGame :: StdGen -> GameOfLife
mkGame = makeFlatMapFromList w h . take (w*h) . randoms
  where w = 50
        h = 50

display :: GameOfLife -> String
display = displayWith $ \p ->  bool "  " $ fgColor (colorOf p) "##"
  where colorOf (Point y x) =
          let c1 = interpolate (x % 49) red   yellow
              c2 = interpolate (x % 49) green blue
          in interpolate (y % 49) c1 c2

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
