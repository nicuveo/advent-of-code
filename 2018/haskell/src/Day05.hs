-- module

module Day05 (day05_1, day05_2) where



-- import

import           Data.Char
import           Data.Function

import           Common



-- solution

day05_1 :: Solution
day05_1 = show . length . foldr react []


day05_2 :: Solution
day05_2 input = show $ minimum
                [ length $ foldr react [] $ removeUnit x firstPass
                | x <- ['a'..'z']
                ]
  where firstPass = foldr react [] input



-- helpers

react :: Char -> String -> String
react unit [] = [unit]
react unit (otherUnit : reacted) =
  if unit `reactsWith` otherUnit
  then                    reacted
  else unit : otherUnit : reacted

removeUnit :: Char -> String -> String
removeUnit c = filter $ isNot c
  where isNot y x = y /= toLower x

reactsWith :: Char -> Char -> Bool
reactsWith a b = on (==) toLower a b && a /= b
