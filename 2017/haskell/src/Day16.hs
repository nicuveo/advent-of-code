{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}



-- module

module Day16 ( day16_1,     day16_2
             , day16_MV1_1, day16_MV1_2
             , day16_MV2_1, day16_MV2_2
             , day16_IV1_1, day16_IV1_2
             , day16_IV2_1, day16_IV2_2
             , day16_tests
             ) where



-- import

import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           GHC.TypeLits
import           Test.QuickCheck
import           Text.Parsec
import           Text.Printf

import           Common



-- short solutions
-- easier to read, lists only, instruction packing done at parsing time
-- independent from the rest of the file

day16_1 :: Solution
day16_1 = dance 1 . parseDance

day16_2 :: Solution
day16_2 = dance (10^9) . parseDance


allDancers :: String
allDancers = ['a' .. 'p']

dance :: Int -> ([Int], String) -> String
dance times moves = dance_ times allDancers
  where dance_ t d = let nt = if t < times && d == allDancers
                              then mod times $ times - t
                              else t
                     in  if nt == 0
                         then d
                         else dance_ (nt-1) $ applyDance moves d


applyDance :: ([Int], String) -> String -> String
applyDance (is, ns) dancers = map pick is
  where pick index = let n = dancers !! index in ns !! (ord n - ord 'a')


applySpin :: Int -> [a] -> [a]
applySpin x dancers = end ++ beginning
  where (beginning, end) = splitAt (16 - x) dancers

applyExchange :: Int -> Int -> [a] -> [a]
applyExchange a b dancers = res
  where (res, x, y) = foldr step ([], x, y) $ zip [0..] dancers
        step (index, d) (result, xval, yval)
          | index == a = (y : result,    d, yval)
          | index == b = (x : result, xval,    d)
          | otherwise  = (d : result, xval, yval)

applyPartners :: Char -> Char -> String -> String
applyPartners x y = foldr step []
  where step d res
          | d == x    = y : res
          | d == y    = x : res
          | otherwise = d : res


parseDance :: String -> ([Int], String)
parseDance = parseWith $ inst ([0..15], allDancers)
  where inst s = tryAll [spin s, exch s, part s]
        step s = (char ',' >> inst s) <|> (eof >> return s)
        spin (i, n) = do
          char 's'
          x <- intParser
          step (applySpin x i, n)
        exch (i, n) = do
          char 'x'
          a <- intParser
          char '/'
          b <- intParser
          step (applyExchange a b i, n)
        part (i, n) = do
          char 'p'
          x <- lower
          char '/'
          y <- lower
          step (i, applyPartners x y n)



-- solutions MV1: fast but ugly, using mutable unboxed vectors in the ST Monad
-- solutions MV2: same, but with instruction packing

day16_MV1_1 :: Solution
day16_MV1_1 = danceMV 1 . parseInstructions

day16_MV1_2 :: Solution
day16_MV1_2 = danceMV (10^9) . parseInstructions

day16_MV2_1 :: Solution
day16_MV2_1 = danceMV 1 . pack . parseInstructions

day16_MV2_2 :: Solution
day16_MV2_2 = danceMV (10^9) . pack . parseInstructions


danceMV :: KnownNat n => Int -> [Instruction n] -> String
danceMV times insts =
  printf "MV, after %3d dance(s) of %5d instructions: %s " s (length insts) $ V.toList res
  where n        = instSize $ head insts
        start    = V.fromList $ mkDancers n
        (s, res) = runST $ dance_ times 0 =<< V.thaw start
        dance_ t i dancers = do
          current <- V.unsafeFreeze dancers
          let nt = if t < times && current == start
                   then mod times $ times - t
                   else t
          if nt == 0
          then return (i :: Int, current)
          else do
            ds <- V.unsafeThaw current
            dance_ (nt-1) (i+1) =<< foldM executeMV ds insts

executeMV :: KnownNat n => V.MVector s Name -> Instruction n -> ST s (V.MVector s Name)
executeMV dancers i@(Spin x) = doSpin (instSize i) x <$> V.unsafeFreeze dancers >>= V.unsafeThaw
executeMV dancers (Exchange a b) = VM.unsafeSwap dancers a b >> return dancers
executeMV dancers (Partner x y) = do
  names <- V.unsafeFreeze dancers
  let !a = fromJust $ V.elemIndex x names
      !b = fromJust $ V.elemIndex y names
  newDancers <- V.unsafeThaw names
  VM.unsafeSwap newDancers a b
  return newDancers
executeMV dancers i@(UpdateI l) = do
  let n = instSize i
  result <- VM.new n
  old    <- V.unsafeFreeze dancers
  V.forM_ (V.indexed l) $ \(newi, oldi) -> VM.unsafeWrite result newi $ V.unsafeIndex old oldi
  return result
executeMV dancers i@(UpdateN l) = do
  let n = instSize i
  forM_ [0..n-1] $ \x -> do
    c <- VM.unsafeRead dancers x
    VM.unsafeWrite dancers x $ V.unsafeIndex l $ ord c - ord 'a'
  return dancers



-- solutions IV1: pure and more readable, using immutable unboxed vectors
-- solutions IV2: same, but with instruction packing

day16_IV1_1 :: Solution
day16_IV1_1 = danceIV 1 . parseInstructions

day16_IV1_2 :: Solution
day16_IV1_2 = danceIV (10^9) . parseInstructions

day16_IV2_1 :: Solution
day16_IV2_1 = danceIV 1 . pack . parseInstructions

day16_IV2_2 :: Solution
day16_IV2_2 = danceIV (10^9) . pack . parseInstructions


danceIV :: KnownNat n => Int -> [Instruction n] -> String
danceIV times insts =
  printf "IV, after %3d dance(s) of %5d instructions: %s " s (length insts) $ V.toList res
  where n        = instSize $ head insts
        start    = V.fromList $ mkDancers n
        (s, res) = dance_ times 0 start
        dance_ t i dancers =
          let nt = if t < times && start == dancers
                   then mod times $ times - t
                   else t
          in  if nt == 0
              then (i :: Int, dancers)
              else dance_ (nt-1) (i+1) $ foldl' executeIV dancers insts

executeIV :: KnownNat n => V.Vector Name -> Instruction n -> V.Vector Name
executeIV dancers i@(Spin x)     = doSpin (instSize i) x dancers
executeIV dancers (Exchange a b) = doExchange a b dancers
executeIV dancers (Partner x y)  = doPartners x y dancers
executeIV dancers (UpdateI l)    = V.map (V.unsafeIndex dancers) l
executeIV dancers (UpdateN l)    = flip V.map dancers $ \c -> V.unsafeIndex l $ ord c - ord 'a'



-- common helpers

type Name = Char

data Instruction (n :: Nat)
  -- unpacked
  = Spin     Int
  | Exchange Int  Int
  | Partner  Name Name
  -- packed
  | UpdateI  (V.Vector Int)
  | UpdateN  (V.Vector Name)
  deriving Show


instSize :: KnownNat n => Instruction n -> Int
instSize = fromInteger . natVal

noop :: KnownNat n => Instruction n
noop = i
  where i = UpdateI $ V.enumFromN 0 n
        n = instSize i

doSpin :: V.Unbox a => Int -> Int -> V.Vector a -> V.Vector a
doSpin n x v = end V.++ beginning
  where (beginning, end) = V.splitAt (n - x) v

doExchange :: V.Unbox a => Int -> Int -> V.Vector a -> V.Vector a
doExchange a b v = V.unsafeUpd v [(a, y), (b, x)]
  where x = V.unsafeIndex v a
        y = V.unsafeIndex v b

doPartners :: Name -> Name -> V.Vector Name -> V.Vector Name
doPartners x y v = V.unsafeUpd v [(a, y), (b, x)]
  where a = fromJust $ V.elemIndex x v
        b = fromJust $ V.elemIndex y v

mkDancers :: Int -> [Name]
mkDancers n = chr <$> [start .. start + n - 1]
  where start = ord 'a'

pack :: KnownNat n => [Instruction n] -> [Instruction n]
pack insts = [indexInst, namesInst]
  where dancers = mkDancers n
        (onNames, onIndex) = partition isPartner insts
        isPartner (Partner _ _) = True
        isPartner  _            = False
        combineI (UpdateI l) (Spin x)       = UpdateI $ doSpin n x l
        combineI (UpdateI l) (Exchange a b) = UpdateI $ doExchange a b l
        combineI _           _              = error "day16: unexpected instructions when packing"
        combineP d           (Partner x y)  = doPartners x y d
        combineP _           _              = error "day16: unexpected instructions when packing"
        indexInst = foldl' combineI noop onIndex
        namesInst = UpdateN $ foldl' combineP (V.fromList dancers) onNames
        n = instSize indexInst

parseInstructions :: String -> [Instruction 16]
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



-- tests

instance Arbitrary (Instruction 16) where
  arbitrary = oneof [arbitraryS, arbitraryE, arbitraryP]
    where arbitraryI = elements [0..15]
          arbitraryN = elements $ mkDancers 16
          arbitraryS = Spin <$> arbitraryI
          arbitraryP = Partner <$> arbitraryN <*> arbitraryN
          arbitraryE = Exchange <$> arbitraryI <*> arbitraryI

day16_tests :: IO ()
day16_tests = quickCheck $ \l -> solve l == solve (pack l)
  where solve :: [Instruction 16] -> V.Vector Name
        solve = foldl' executeIV $ V.fromList $ mkDancers 16
