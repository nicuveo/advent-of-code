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
import           Prelude                     hiding (splitAt, (++))
import           Test.QuickCheck
import           Text.Parsec
import           Text.Printf

import           Common



-- solutions MV1: fast but ugly, using mutable unboxed vectors in the ST Monad
-- solutions MV2: same, but with instruction packing

day16_MV1_1 :: Solution
day16_MV1_1 = danceMV 16 1 . parseInstructions

day16_MV1_2 :: Solution
day16_MV1_2 = danceMV 16 1000000000 . parseInstructions

day16_MV2_1 :: Solution
day16_MV2_1 = danceMV 16 1 . pack 16 . parseInstructions

day16_MV2_2 :: Solution
day16_MV2_2 = danceMV 16 1000000000 . pack 16 . parseInstructions


danceMV :: Int -> Int -> [Instruction] -> String
danceMV n times insts =
  printf "MV, after %3d dance(s) of %5d instructions: %s " steps (L.length insts) $ toList res
  where (steps, res) = runST $ do
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
            _dance (nt-1) (i+1) b =<< foldM (executeMV n) ds insts

executeMV :: Int -> MVector s Name -> Instruction -> ST s (MVector s Name)
executeMV n dancers (Spin x) = doSpin n x <$> unsafeFreeze dancers >>= unsafeThaw
executeMV _ dancers (Exchange a b) = unsafeSwap dancers a b >> return dancers
executeMV _ dancers (Partner x y) = do
  names <- unsafeFreeze dancers
  let a = fromJust $ elemIndex x names
      b = fromJust $ elemIndex y names
  newDancers <- unsafeThaw names
  unsafeSwap newDancers a b
  return newDancers
executeMV n dancers (UpdateI l) = do
  result <- new n
  old    <- unsafeFreeze dancers
  V.forM_ (indexed l) $ \(newi, oldi) -> unsafeWrite result newi $ unsafeIndex old oldi
  return result
executeMV n dancers (UpdateN l) = do
  M.forM_ [0..n-1] $ \i -> do
    c <- unsafeRead dancers i
    unsafeWrite dancers i $ unsafeIndex l $ ord c - ord 'a'
  return dancers



-- solutions IV1: pure and more readable, using immutable unboxed vectors
-- solutions IV2: same, but with instruction packing

day16_IV1_1 :: Solution
day16_IV1_1 = danceIV 16 1 . parseInstructions

day16_IV1_2 :: Solution
day16_IV1_2 = danceIV 16 1000000000 . parseInstructions

day16_IV2_1 :: Solution
day16_IV2_1 = danceIV 16 1 . pack 16 . parseInstructions

day16_IV2_2 :: Solution
day16_IV2_2 = danceIV 16 1000000000 . pack 16 . parseInstructions


danceIV :: Int -> Int -> [Instruction] -> String
danceIV n times insts =
  printf "IV, after %3d dance(s) of %5d instructions: %s " steps (L.length insts) $ toList res
  where start = fromList $ mkDancers n
        (steps, res) = _dance times 0 start
        _dance t i dancers =
          let nt = if t < times && start == dancers
                   then mod times $ times - t
                   else t
          in  if nt == 0
              then (i :: Int, dancers)
              else _dance (nt-1) (i+1) $ L.foldl' (executeIV n) dancers insts

executeIV :: Int -> Vector Name -> Instruction -> Vector Name
executeIV n dancers (Spin x)       = doSpin n x dancers
executeIV _ dancers (Exchange a b) = doExchange a b dancers
executeIV _ dancers (Partner x y)  = doPartners x y dancers
executeIV _ dancers (UpdateI l)    = V.map (unsafeIndex dancers) l
executeIV _ dancers (UpdateN l)    = flip V.map dancers $ \c -> unsafeIndex l $ ord c - ord 'a'



-- common helpers

type Name = Char

data Instruction
  -- unpacked
  = Spin     Int
  | Exchange Int  Int
  | Partner  Name Name
  -- packed
  | UpdateI  (Vector Int)
  | UpdateN  (Vector Name)
  deriving Show


noop :: Int -> Instruction
noop = UpdateI . enumFromN 0

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

pack :: Int -> [Instruction] -> [Instruction]
pack n insts = [indexInst, namesInst]
  where dancers = mkDancers n
        (onNames, onIndex) = L.partition isPartner insts
        isPartner (Partner _ _) = True
        isPartner  _            = False
        combineI (UpdateI l) (Spin x)       = UpdateI $ doSpin n x l
        combineI (UpdateI l) (Exchange a b) = UpdateI $ doExchange a b l
        combineI _           _              = error "day16: unexpected instructions when packing"
        combineP d           (Partner x y)  = doPartners x y d
        combineP _           _              = error "day16: unexpected instructions when packing"
        indexInst = L.foldl' combineI (noop n) onIndex
        namesInst = UpdateN $ L.foldl' combineP (fromList dancers) onNames

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



-- tests

instance Arbitrary Instruction where
  arbitrary = oneof [arbitraryS, arbitraryE, arbitraryP]
    where arbitraryI = elements [0..15]
          arbitraryN = elements $ mkDancers 16
          arbitraryS = Spin <$> arbitraryI
          arbitraryP = Partner <$> arbitraryN <*> arbitraryN
          arbitraryE = Exchange <$> arbitraryI <*> arbitraryI

day16_tests :: IO ()
day16_tests = quickCheck $ \l -> solve l == solve (pack 16 l)
  where solve = L.foldl' (executeIV 16) $ fromList $ mkDancers 16
