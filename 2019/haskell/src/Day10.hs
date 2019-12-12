-- import

import           Data.Function (on)
import           Data.List

import           AOC



-- input

type Asteroids = [Point]

parseInput :: String -> Asteroids
parseInput input = [ Point y x
                   | (y, row) <- zip [0..] $ lines input
                   , (x, c)   <- zip [0..] row
                   , c == '#'
                   ]



-- solution

reduce :: Point -> Vector
reduce (Point 0 0) = error "overlapping asteroids"
reduce (Point 0 x) = Point 0 $ signum x
reduce (Point y 0) = Point (signum y) 0
reduce (Point y x) = Point (div y d) (div x d)
  where d = gcd y x


part1 :: Asteroids -> (Int, Point)
part1 asteroids = maximum $ zip (map canView asteroids) asteroids
  where canView a = length $ nub [ getVector a b
                                 | b <- asteroids
                                 , b /= a
                                 ]
        getVector p q = reduce $ q - p


part2 :: Asteroids -> Point -> (Double, Int, Point)
part2 asteroids station = (!! 199)      $
                          concat        $
                          transpose     $
                          groupOn angle $
                          sort          $
                          map addInfo   $
                          filter (/= station) asteroids
  where addInfo :: Point -> (Double, Int, Point)
        addInfo p =
          let r = p - station
              Point y x = reduce r
          in (-(atan2 `on` fromIntegral) x y, sqNorm r, p)
        angle (a, _, _) = a



-- main

main :: IO ()
main = aocMain 10 $ \rawInput -> do
  let input = parseInput rawInput
      (part1Result, station) = part1 input
  print part1Result
  print $ part2 input station
