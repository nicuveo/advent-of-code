-- module

module Day11 (day11_1, day11_2) where



-- import

import           Data.Array

import           Common



-- solution

day11_1 :: Solution
day11_1 input = snd $ maximum [ ( grid !? (y-1,x-1) +
                                  grid !? (y+2,x+2) -
                                  grid !? (y+2,x-1) -
                                  grid !? (y-1,x+2)
                                , show x ++ "," ++ show y
                                )
                              | y <- [1..298]
                              , x <- [1..298]
                              ]
  where serial = read input
        grid   = summedAreaTable $ makeGrid serial

day11_2 :: Solution
day11_2 input = snd $ maximum [ ( grid !? (y  -1,x  -1)   +
                                  grid !? (y+s-1,x+s-1) -
                                  grid !? (y+s-1,x  -1)   -
                                  grid !? (y  -1,x+s-1)
                                , show x ++ "," ++ show y ++ "," ++ show s
                                )
                              | s <- [1..300]
                              , y <- [1..301-s]
                              , x <- [1..301-s]
                              ]
  where serial = read input
        grid   = summedAreaTable $ makeGrid serial



-- helpers

type Grid = Array (Int,Int) Int

(!?) :: Grid -> (Int,Int) -> Int
grid !? p@(y,x)
  | y < 1 || x < 1 = 0
  | otherwise      = grid ! p


gridBounds :: ((Int,Int), (Int,Int))
gridBounds = ((1,1), (300,300))

makeGrid :: Int -> Grid
makeGrid serial = array gridBounds [ ((y,x), powerLevel serial y x)
                                   | y <- [1..300]
                                   , x <- [1..300]
                                   ]

summedAreaTable :: Grid -> Grid
summedAreaTable grid = result
  where result = array b [ ((y,x), summedPowerLevel (y,x))
                         | y <- [ymin..ymax]
                         , x <- [xmin..xmax]
                         ]
        summedPowerLevel (1,1) = grid ! (1,1)
        summedPowerLevel (y,1) = grid ! (y,1) + result ! (y-1,1)
        summedPowerLevel (1,x) = grid ! (1,x) + result ! (1,x-1)
        summedPowerLevel (y,x) = grid ! (y,x) +
                                 result ! (y-1,x) +
                                 result ! (y,x-1) -
                                 result ! (y-1,x-1)
        b@((ymin,xmin),(ymax,xmax)) = bounds grid


powerLevel :: Int -> Int -> Int -> Int
powerLevel serial y x = mod (div ((y * rackId + serial) * rackId) 100) 10 - 5
  where rackId = x + 10
