{-# LANGUAGE FlexibleContexts #-}


-- module

module Day19 (day19_1, day19_2) where



-- import

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Map.Strict             as M
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           Text.Parsec                 as P

import           Assembly
import           Common



-- solution

day19_1 :: Solution
day19_1 = show . sumOfDivisors . run 0


day19_2 :: Solution
day19_2 = show . sumOfDivisors . run 1



-- rewritten equivalent

sumOfDivisors :: Int -> Int
sumOfDivisors n = sum [x | x <- [1..n], n `mod` x == 0]



-- program

type Registers s = MV.MVector s Int
type Function  s = Registers s -> ST s ()
type Program   s = V.Vector (Function s)

incrementIP :: Int -> Registers s -> ST s ()
incrementIP ipr r = do
  p <- regRM ipr r
  regWM ipr (p+1) r

run :: Int -> String -> Int
run x input = runST $ do
  r <- UV.unsafeThaw (UV.fromList [x,0,0,0,0,0])
  run_ r
  -- register 1 is the one in which the number to analyze is stored
  MV.read r 1
  where (ipr,p) = parseInput input
        run_ r = do
          ip <- regRM ipr r
          -- instruction 1 is where the double loops begin
          unless (ip == 1) $ do
            p V.! ip $ r
            incrementIP ipr r
            run_ r



-- parsing

parseInput ::  String -> (Int, Program s)
parseInput = parseWith $ do
  string "#ip"
  ipr <- intParser
  newline
  prg <- inst `sepEndBy` newline
  return (ipr, V.fromList prg)
  where inst = do
          i       <- many1 lower
          [a,b,c] <- P.count 3 intParser
          optional $ do
            spaces
            char '#'
            manyTill (noneOf "\n") $ lookAhead (void newline <|> eof)
          return $ (instructionsNamesM M.! i) a b c



-- debug

testData :: String
testData = "#ip 0\n\
           \seti 5 0 1\n\
           \seti 6 0 2\n\
           \addi 0 1 0\n\
           \addr 1 2 3\n\
           \setr 1 0 0\n\
           \seti 8 0 4\n\
           \seti 9 0 5\n"
