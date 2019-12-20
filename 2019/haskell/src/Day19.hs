{-# LANGUAGE MultiWayIf #-}


-- import

import           Control.Arrow         ((&&&))
import           Data.Function.Memoize
import           Data.List.Split
import           Data.Ratio
import           Data.Tuple.Extra      (both)
import qualified Data.Vector.Unboxed   as V
import           Text.Printf

import           AOC
import           AOC.Debug.Color
import           AOC.Map.Sparse
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- probing

type Samples = SparseMap Bool
type Prober  = Point -> Bool

probe :: Program -> Int -> Int -> Bool
probe v x y = toEnum $ head $ snd $ run v [x, y]

getRange :: Prober -> [Point] -> (Point, Point)
getRange f = (head &&& last) . takeWhile f . dropWhile (not . f)

getXRange :: Prober -> (Int, Int)
getXRange f = both px $ getRange f [Point 200 x | x <- [0..]]

getYRange :: Prober -> (Int, Int)
getYRange f = both py $ getRange f [Point y 200 | y <- [0..]]



-- solution

guessSquare :: Prober -> Point
guessSquare prob = guess start
  where (xmin, xmax) = getXRange prob
        (ymin, ymax) = getYRange prob
        start = if xmin > ymin
                then Point 100 (div (xmin+xmax) 2)
                else Point (div (ymin+ymax) 2) 100
        extrapolate (Point y x) = [ floor   $ xmin * y % 200
                                  , ceiling $ xmax * y % 200
                                  , floor   $ ymin * x % 200
                                  , ceiling $ ymax * x % 200
                                  ]
        guess p@(Point y x) = let [a,b,c,d] = extrapolate p in
          if | b - a < 99 -> guess $ below p
             | d - c < 99 -> guess $ rightOf p
             | b - x < 99 -> guess $ below p
             | d - y < 99 -> guess $ rightOf p
             | otherwise   -> p

check :: Prober -> Point -> Bool
check prob (Point y x) = all prob [ Point  y     x
                                  , Point (y+99) x
                                  , Point  y    (x+99)
                                  ]

view :: Prober -> (Int, Int, Int, Int) -> Point -> String
view prob (xmin, xmax, ymin, ymax) p =
  unlines [ concat [ if Point y x == p
                     then bgColor white $ fgColor blue "@"
                     else case (prob (Point y x), by && bx) of
                       (False, False) -> "."
                       (True,  False) -> "#"
                       (False, True ) -> fgColor red   "X"
                       (True,  True ) -> fgColor green "O"
                   | x <- [xmin - 20 .. xmax + 20]
                   , let bx = x >= xmin && x <= xmax
                   ]
          | y <- [ymin - 20 .. ymax + 20]
          , let by = y >= ymin && y <= ymax
          ]



-- main

main :: IO ()
main = aocMain 19 $ \rawInput -> do
  let program  = parseInput rawInput
      prober p = (memoize2 $ probe program) (px p) (py p)
      guess    = guessSquare prober
      corner   = minimumOn manhattanNorm $ filter (check prober)
                 [ Point y x
                 | y <- [py guess - 10 .. py guess + 10]
                 , x <- [px guess - 10 .. px guess + 10]
                 ]

  printf "Part1: %d\n" $ countIf prober [Point y x | y <- [0..49], x <- [0..49]]
  printf "Part2: %d\n" $ 10000 * px corner + py corner

  {-
  let (xmin, xmax) = getXRange prober
      (ymin, ymax) = getYRange prober
      [lp,rp,ab,bp]= [ floor   $ xmin * py corner % 200
                     , ceiling $ xmax * py corner % 200
                     , floor   $ ymin * px corner % 200
                     , ceiling $ ymax * px corner % 200
                     ]
  putStrLn $ view prober (lp,rp,ab,bp) corner
  -}
