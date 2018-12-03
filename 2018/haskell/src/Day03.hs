-- module

module Day03 (day03_1, day03_2) where



-- import

import           Data.Function
import           Data.Set      as S
import           Text.Parsec

import           Common



-- solution

day03_1 :: Solution
day03_1 input = show $ S.size $ overlap claims
  where claims = parseInput input
        overlap [] = S.empty
        overlap (claim : others) =
          let !accumulator = overlap others
          in  unions $ accumulator : [ fromList $ overlappingPoints claim other
                                     | other <- others
                                     ]


day03_2 :: Solution
day03_2 input = show $ head [ cid claim
                            | claim <- claims
                            , all (not . overlapping claim) claims
                            ]
  where claims = parseInput input


-- helpers

type Point = Int
data Claim = Claim { cid  :: Int
                   , xmin :: Int
                   , ymin :: Int
                   , xmax :: Int
                   , ymax :: Int
                   } deriving (Show, Eq, Ord)

toPoint :: Int -> Int -> Point
toPoint x y = y * 1000 + x

overlapping :: Claim -> Claim -> Bool
overlapping claim1 claim2
  | cid claim1 == cid claim2 = False
  | otherwise =
      max (xmin claim1) (xmin claim2) < min (xmax claim1) (xmax claim2) &&
      max (ymin claim1) (ymin claim2) < min (ymax claim1) (ymax claim2)

overlappingPoints :: Claim -> Claim -> [Point]
overlappingPoints claim1 claim2
  | not $ overlapping claim1 claim2 = []
  | otherwise = [ toPoint x y
                | x <- [oxmin .. oxmax-1]
                , y <- [oymin .. oymax-1]
                ]
      where oxmin = (max `on` xmin) claim1 claim2
            oxmax = (min `on` xmax) claim1 claim2
            oymin = (max `on` ymin) claim1 claim2
            oymax = (min `on` ymax) claim1 claim2

parseInput :: String -> [Claim]
parseInput = parseWith $ claim `sepBy` newline
  where claim = do
          symbol "#"
          i <- intParser
          symbol "@"
          x <- intParser
          symbol ","
          y <- intParser
          symbol ":"
          w <- intParser
          symbol "x"
          h <- intParser
          return $ Claim i x y (x + w) (y + h)
