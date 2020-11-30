{-# LANGUAGE ParallelListComp #-}


-- import

import           Data.Function               (on, (&))
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V
import           Text.Parsec

import           Debug.Trace

import           AOC



-- input

type Operation = Cards -> Cards

parseOperation :: String -> [Operation]
parseOperation = map parseLine . lines
  where parseLine = parseWith line
        line  = tryAll [newI, cutI, dealI]
        newI  = string "deal into new stack" >> return new
        cutI  = string "cut" >> cut <$> intLiteral
        dealI = string "deal with increment" >> deal <$> intLiteral

type Op = Int -> Int

parseOp :: String -> [Op]
parseOp = map parseLine . lines
  where parseLine = parseWith line
        line  = tryAll [newI, cutI, dealI]
        newI  = string "deal into new stack" >> return rNew
        cutI  = string "cut" >> rCut <$> intLiteral
        dealI = string "deal with increment" >> rDeal <$> intLiteral



-- part 1

type Cards = [Int]

new = reverse
cut d s
  | d >= 0    = let (a,b) = splitAt d            s in b ++ a
  | otherwise = let (a,b) = splitAt (length s+d) s in b ++ a
deal d s = snd <$> sort [(mod i n, c) | c <- s | i <- [0,d..]]
  where n = length s



-- part 2

m = 119315717514047

rNew x = m - x - 1
rCut d x
  | d >= 0    = if x < m - d then x + d else x - (m - d)
  | otherwise = if x < abs d then x + (m + d) else x + d
rDeal d x = head [div (m * z + x) d | z <- [0..], mod (z * m + x) d == 0]



-- main

testInput = "deal with increment 7"

main :: IO ()
main = aocMain 22 $ \rawInput -> do
  print $ elemIndex 2019 $ foldl' (&) [0..10006] $ parseOperation rawInput
  let f x = foldr ($) x $ parseOp rawInput
  mapM_ print $ iterate f 2020
  print $ iterateN 101741582076661 f 2020
  -- print $ foldl' (&) [0..9] $ parseOperation testInput
  -- print $ flip map [0..9] $ \n -> foldr ($) n $ parseOp testInput
  -- 101741582076661
