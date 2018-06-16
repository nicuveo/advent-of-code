-- module

module Day18 (day18_1, day18_2, prettyPrint) where



-- import

import           Data.Maybe
import qualified Data.Vector.Unboxed as V

import           Common



-- solution

day18_1 :: Solution
day18_1 = show . sum . map countSafe . take 40 . iterate nextRow . parseInput


day18_2 :: Solution
day18_2 = show . sum . map countSafe . take 400000 . iterate nextRow . parseInput



-- helpers

type Row = V.Vector Bool

countSafe :: Row -> Int
countSafe = V.length . V.filter id

nextRow :: Row -> Row
nextRow r = V.imap nextTile r
  where nextTile i _ = let left  = fromMaybe True $ r V.!? (i-1)
                           right = fromMaybe True $ r V.!? (i+1)
                       in  left == right

parseInput :: String -> Row
parseInput = V.fromList . map isSafe
  where isSafe '.' = True
        isSafe '^' = False
        isSafe _   = error "parseInput: malformed input!"

prettyPrint :: Row -> String
prettyPrint = map toChar . V.toList
  where toChar True  = '.'
        toChar False = '^'
