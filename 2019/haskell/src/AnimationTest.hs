{-# LANGUAGE TupleSections #-}

import           Data.Bool
import           System.Random

import           AOC
import           AOC.Debug.Animate
import           AOC.Map.Flat


type GameOfLife = FlatMap Bool


mkGame :: StdGen -> GameOfLife
mkGame = makeFlatMapFromList w h . take (w*h) . randoms
  where w = 50
        h = 50

display :: GameOfLife -> String
display g =
  unlines [ concat [ bool "  " "##" $ g ! Point y x
                   | x <- mapXs g
                   ]
          | y <- mapYs g
          ]

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
