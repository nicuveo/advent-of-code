-- module

module Day13 (day13_1, day13_2) where



-- import

import           Common

import           Data.List
import           Safe
import           Text.Parsec
import           Text.Printf



-- solution

day13_1 :: Solution
day13_1 input  = show $ sum [severity l | l <- layers, at (scanner l) (depth l) == 0]
  where layers = parseLayer <$> lines input

day13_2 :: Solution
day13_2 input  = show $ head [delay | (delay, plan) <- zip [0..] plans, 0 `notElem` plan]
  where layers = parseLayer <$> lines input
        plans  = transpose [drop (depth l) $ scanner l | l <- layers]



-- helpers

data Layer = Layer { depth   :: Int
                   , range   :: Int
                   , scanner :: [Int]
                   }

instance Show Layer where
  show l = printf "%2d:%s" (depth l) $ show $ take 30 $ scanner l

severity :: Layer -> Int
severity l = depth l * range l

parseLayer :: String -> Layer
parseLayer = parseWith line
  where line = do
          d <- intParser
          string ": "
          r <- intParser
          return $ Layer d r $ cycle $ [0..r-1] ++ [r-2,r-3..1]
