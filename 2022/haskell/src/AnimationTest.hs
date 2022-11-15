{-# LANGUAGE TupleSections #-}

import           Data.Bool
import           Data.Ratio
import           System.Random

import           AOC
import           AOC.Debug
import           AOC.Grid.Flat


type GameOfLife = Grid Bool


tlColor = interpolate (2%3) white red
trColor = interpolate (2%3) white yellow
blColor = interpolate (2%3) white green
brColor = interpolate (2%3) white blue

w = 50
h = 50


mkGame :: StdGen -> GameOfLife
mkGame = fromList w h . take (w*h) . randoms

display :: GameOfLife -> String
display = displayWith $ \p -> bool "  " $ bgColor white $ fgColor (colorOf p) "▇▇"
  where colorOf (Point y x) =
          let c1 = interpolate (x % (w-1)) tlColor trColor
              c2 = interpolate (x % (w-1)) blColor brColor
          in interpolate (y % (h-1)) c1 c2

step :: GameOfLife -> GameOfLife
step g = flip pmap g $ \p b -> status b $ countTrue $ gridEightNeighbours g p
  where
    status True  x = x `elem` [2,3]
    status False x = x == 3


main :: IO ()
main = do
  n <- getStdGen
  let s = ([], mkGame n)
      r = display
      u = Just . ([],) . step
  animate 100 resetCursor r u s
