-- module

module Day08 (day08_1, day08_2) where



-- import

import           Data.Tree
import           Safe
import           Text.Parsec

import           Common



-- solution

day08_1 :: Solution
day08_1 = show . sum . concat . flatten . parseTree


day08_2 :: Solution
day08_2 = show . foldTree value . parseTree
  where value metadata childrenValue
          | null childrenValue = sum metadata
          | otherwise          = sum [ atDef 0 childrenValue $ index - 1
                                     | index <- metadata
                                     ]


-- helpers

parseTree :: String -> Tree [Int]
parseTree = parseWith tree
  where tree = do
          n <- intParser
          m <- intParser
          subTrees <- count n tree
          metadata <- count m intParser
          return $ Node metadata subTrees
