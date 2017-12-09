-- module

module Day09 (day09_1, day09_2) where



-- import

import           Text.Parsec

import           Common



-- solution

day09_1 :: Solution
day09_1 = show . score 1 . parseGroup
  where score _ (Garbage _) = 0
        score x (Group sub) = x + sum [score (x+1) g | g <- sub]


day09_2 :: Solution
day09_2 = show . collect . parseGroup
  where collect (Garbage x) = x
        collect (Group sub) = sum $ collect <$> sub



-- helpers

data Group = Group   [Group]
           | Garbage Int


parseGroup :: String -> Group
parseGroup = parseWith group
  where group  = try ok <|> ko
        ok     = fmap Group $ betweenBraces $ group `sepBy` char ','
        ko     = fmap (Garbage . sum) $ between (char '<') (char '>') $ many $ try escape <|> garb
        escape = char '!' >> anyChar >> return 0
        garb   = noneOf ">" >> return 1
