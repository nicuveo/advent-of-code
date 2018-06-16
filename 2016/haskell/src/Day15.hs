-- module

module Day15 (day15_1, day15_2) where



-- import

import           Text.Parsec

import           Common



-- solution

day15_1 :: Solution
day15_1 input = show $ head $ filter check [0..]
  where discs = parseInput input
        check index = and [ mod (start + number + index) positions == 0
                          | (number, positions, start) <- discs
                          ]


day15_2 :: Solution
day15_2 input = show $ head $ filter check [0..]
  where discs = parseInput input ++ [(7, 11, 0)]
        check index = and [ mod (start + number + index) positions == 0
                          | (number, positions, start) <- discs
                          ]



-- parsing

type Disc = (Int, Int, Int)

parseInput :: String -> [Disc]
parseInput = parseWith discs
  where discs = disc `sepBy1` many1 newline
        disc  = do
          symbol "Disc #"
          number <- intParser
          symbol "has"
          positions <- intParser
          symbol "positions; at time=0, it is at position"
          start <- intParser
          symbol "."
          return (number, positions, start)
