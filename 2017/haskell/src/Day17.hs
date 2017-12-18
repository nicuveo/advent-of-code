{-# LANGUAGE BangPatterns #-}



-- module

module Day17 ( day17_1
             , day17_2
             , day17_tests
             ) where



-- import

import           Data.List
import           Test.QuickCheck
import           Text.Printf

import           Common



-- solution

day17_1 :: Solution
day17_1 input = prettify 2017 $ buffer $ foldl' (flip step) mkBuffer operations
  where steps = read input :: Int
        operations = steps <$ [1 .. 2017]


day17_2 :: Solution
day17_2 input = show $ fakeStep steps 0 0 1
  where steps = read input :: Int



-- helpers

data Buffer = Buffer { buffer :: [Int]
                     , pos    :: Int
                     } deriving Show

prettify :: Int -> [Int] -> String
prettify key buf = printf "... %d %d (%d) %d ..." a b c d
  where padded = buf ++ buf ++ buf
        i = elemIndices key padded !! 1
        [a,b,c,d] = take 4 $ drop (i-1) padded

mkBuffer :: Buffer
mkBuffer = Buffer [0] 0

step :: Int -> Buffer -> Buffer
step steps b = Buffer (beginning ++ [len] ++ end) newPos
  where len = length $ buffer b
        newPos = mod (pos b + steps) len + 1
        (beginning, end) = splitAt newPos $ buffer b

fakeStep :: Int -> Int -> Int -> Int -> Int
fakeStep !steps !currentPos !res !size =
  if size == 50000001
  then res
  else fakeStep steps newPos newRes $ size + 1
  where newPos = mod (currentPos + steps) size + 1
        newRes = if newPos == 1 then size else res



-- tests

testPart1 :: Positive Int -> Positive Int -> Bool
testPart1 (Positive steps) (Positive operations) = sort l == [0 .. operations]
  where l = buffer $ foldl' (flip step) mkBuffer $ steps <$ [1 .. operations]

day17_tests :: IO ()
day17_tests = quickCheck testPart1
