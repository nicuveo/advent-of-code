{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}



-- module

module Day16 ( day16_MV1_1, day16_MV1_2
             , day16_MV2_1, day16_MV2_2
             , day16_IV1_1, day16_IV1_2
             , day16_IV2_1, day16_IV2_2
             , day16_tests
             ) where



-- import

import           Control.Monad               as M
import           Control.Monad.ST
import           Data.Char
import           Data.List                   as L hiding (elemIndex, splitAt,
                                                   (++))
import           Data.Maybe
import           Data.Vector.Unboxed         as V hiding (foldM)
import           Data.Vector.Unboxed.Mutable as V hiding (splitAt)
import           GHC.TypeLits
import           Prelude                     hiding (splitAt, (++))
import           Test.QuickCheck
import           Text.Parsec
import           Text.Printf

import           Common



-- solutions MV1: fast but ugly, using mutable unboxed vectors in the ST Monad
-- solutions MV2: same, but with instruction packing

day16_MV1_1 :: Solution
day16_MV1_1 = danceMV 1 . parseInstructions

day16_MV1_2 :: Solution
day16_MV1_2 = danceMV 1000000000 . parseInstructions

day16_MV2_1 :: Solution
day16_MV2_1 = danceMV 1 . pack . parseInstructions

day16_MV2_2 :: Solution
day16_MV2_2 = danceMV 1000000000 . pack . parseInstructions


danceMV :: KnownNat n => Int -> [Instruction n] -> String
danceMV times insts =
  printf "MV, after %3d dance(s) of %5d instructions: %s " steps (L.length insts) $ toList res
  where n = instSize $ L.head insts
        (steps, res) = runST $ do
          origin <- thaw $ fromList $ mkDancers n
          backup <- freeze origin
          _dance times 0 backup origin
        _dance t i b dancers = do
          current <- unsafeFreeze dancers
          let nt = if t < times && current == b
                   then mod times $ times - t
                   else t
          if nt == 0
          then return (i :: Int, current)
          else do
            ds <- unsafeThaw current
            _dance (nt-1) (i+1) b =<< foldM executeMV ds insts

executeMV :: KnownNat n => MVector s Name -> Instruction n -> ST s (MVector s Name)
executeMV dancers i@(Spin x) = doSpin (instSize i) x <$> unsafeFreeze dancers >>= unsafeThaw
executeMV dancers (Exchange a b) = unsafeSwap dancers a b >> return dancers
executeMV dancers (Partner x y) = do
  names <- unsafeFreeze dancers
  let a = fromJust $ elemIndex x names
      b = fromJust $ elemIndex y names
  newDancers <- unsafeThaw names
  unsafeSwap newDancers a b
  return newDancers
executeMV dancers i@(UpdateI l) = do
  let n = instSize i
  result <- new n
  old    <- unsafeFreeze dancers
  V.forM_ (indexed l) $ \(newi, oldi) -> unsafeWrite result newi $ unsafeIndex old oldi
  return result
executeMV dancers i@(UpdateN l) = do
  let n = instSize i
  M.forM_ [0..n-1] $ \x -> do
    c <- unsafeRead dancers x
    unsafeWrite dancers x $ unsafeIndex l $ ord c - ord 'a'
  return dancers



-- solutions IV1: pure and more readable, using immutable unboxed vectors
-- solutions IV2: same, but with instruction packing

day16_IV1_1 :: Solution
day16_IV1_1 = danceIV 1 . parseInstructions

day16_IV1_2 :: Solution
day16_IV1_2 = danceIV 1000000000 . parseInstructions

day16_IV2_1 :: Solution
day16_IV2_1 = danceIV 1 . pack . parseInstructions

day16_IV2_2 :: Solution
day16_IV2_2 = danceIV 1000000000 . pack . parseInstructions


danceIV :: KnownNat n => Int -> [Instruction n] -> String
danceIV times insts =
  printf "IV, after %3d dance(s) of %5d instructions: %s " steps (L.length insts) $ toList res
  where n = instSize $ L.head insts
        start = fromList $ mkDancers n
        (steps, res) = _dance times 0 start
        _dance t i dancers =
          let nt = if t < times && start == dancers
                   then mod times $ times - t
                   else t
          in  if nt == 0
              then (i :: Int, dancers)
              else _dance (nt-1) (i+1) $ L.foldl' executeIV dancers insts

executeIV :: KnownNat n => Vector Name -> Instruction n -> Vector Name
executeIV dancers i@(Spin x)     = doSpin (instSize i) x dancers
executeIV dancers (Exchange a b) = doExchange a b dancers
executeIV dancers (Partner x y)  = doPartners x y dancers
executeIV dancers (UpdateI l)    = V.map (unsafeIndex dancers) l
executeIV dancers (UpdateN l)    = flip V.map dancers $ \c -> unsafeIndex l $ ord c - ord 'a'



-- common helpers

type Name = Char

data Instruction (n :: Nat)
  -- unpacked
  = Spin     Int
  | Exchange Int  Int
  | Partner  Name Name
  -- packed
  | UpdateI  (Vector Int)
  | UpdateN  (Vector Name)
  deriving Show


instSize :: KnownNat n => Instruction n -> Int
instSize = fromInteger . natVal

noop :: KnownNat n => Instruction n
noop = i
  where i = UpdateI $ enumFromN 0 n
        n = instSize i

doSpin :: Unbox a => Int -> Int -> Vector a -> Vector a
doSpin n x v = end ++ beginning
  where (beginning, end) = splitAt (n - x) v

doExchange :: Unbox a => Int -> Int -> Vector a -> Vector a
doExchange a b v = unsafeUpd v [(a, y), (b, x)]
  where x = unsafeIndex v a
        y = unsafeIndex v b

doPartners :: Name -> Name -> Vector Name -> Vector Name
doPartners x y v = unsafeUpd v [(a, y), (b, x)]
  where a = fromJust $ elemIndex x v
        b = fromJust $ elemIndex y v

mkDancers :: Int -> [Name]
mkDancers n = chr <$> [start .. start + n - 1]
  where start = ord 'a'

pack :: KnownNat n => [Instruction n] -> [Instruction n]
pack insts = [indexInst, namesInst]
  where dancers = mkDancers n
        (onNames, onIndex) = L.partition isPartner insts
        isPartner (Partner _ _) = True
        isPartner  _            = False
        combineI (UpdateI l) (Spin x)       = UpdateI $ doSpin n x l
        combineI (UpdateI l) (Exchange a b) = UpdateI $ doExchange a b l
        combineI _           _              = error "day16: unexpected instructions when packing"
        combineP d           (Partner x y)  = doPartners x y d
        combineP _           _              = error "day16: unexpected instructions when packing"
        indexInst = L.foldl' combineI noop onIndex
        namesInst = UpdateN $ L.foldl' combineP (fromList dancers) onNames
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
  where solve :: [Instruction 16] -> Vector Name
        solve = L.foldl' executeIV $ fromList $ mkDancers 16
