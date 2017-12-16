{-# LANGUAGE TupleSections #-}



-- module

module Day16 (day16_1, day16_2) where



-- import

import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Maybe
import           Data.Vector.Unboxed         hiding (foldM)
import           Data.Vector.Unboxed.Mutable hiding (splitAt)
import           Prelude                     hiding (splitAt, (++))
import           Text.Parsec
import           Text.Printf

import           Common



-- solution

day16_1 :: Solution
day16_1 = dance 16 1 . parseInstructions


day16_2 :: Solution
day16_2 = dance 16 1000000000 . parseInstructions



-- helpers

type Name = Char
type Dancers s = MVector s Name

data Instruction = Spin     Int
                 | Exchange Int  Int
                 | Partner  Name Name

dance :: Int -> Int -> [Instruction] -> String
dance n times insts = printf "after %3d dance(s): %s " steps $ toList res
  where (steps, res) = runST $ do
          origin <- mkDancers n
          backup <- freeze origin
          _dance times 0 backup origin
        _dance t i b dancers = do
          current <- unsafeFreeze dancers
          let nt = if t < times && current == b
                   then mod times (times - t)
                   else t
          if nt == 0
          then return (i :: Int, current)
          else do
            ds <- unsafeThaw current
            _dance (nt-1) (i+1) b =<< foldM (execute n) ds insts

mkDancers :: Int -> ST s (Dancers s)
mkDancers x = thaw $ fromList $ chr <$> [start .. start + x - 1]
  where start = ord 'a'

execute :: Int -> Dancers s -> Instruction -> ST s (Dancers s)
execute n dancers (Spin x) = do
  names <- unsafeFreeze dancers
  let (beginning, end) = splitAt (n - x) names
  unsafeThaw $ end ++ beginning
execute _ dancers (Exchange a b) = do
  unsafeSwap dancers a b
  return dancers
execute _ dancers (Partner x y) = do
  names <- unsafeFreeze dancers
  let a = fromJust $ elemIndex x names
      b = fromJust $ elemIndex y names
  newDancers <- unsafeThaw names
  unsafeSwap newDancers a b
  return newDancers

parseInstructions :: String -> [Instruction]
parseInstructions = parseWith $ inst `sepBy` char ','
  where inst = tryAll [spin, exch, part]
        spin = do
          char 's'
          Spin <$> intParser
        exch = do
          char 'x'
          a <- intParser
          char '/'
          b <- intParser
          return $ Exchange a b
        part = do
          char 'p'
          x <- lower
          char '/'
          y <- lower
          return $ Partner x y
