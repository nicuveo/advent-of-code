{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.Hashable
import qualified Data.HashSet     as S
import           Data.List
import           Data.List.Extra  (nubOrd)
import           Data.Maybe
import           Data.Proxy
import           GHC.Natural
import           GHC.TypeNats
import           Text.Parsec
import           Text.Parsec.Char

import           AOC              hiding (Point)



-- dimension

data Dimension (n :: Nat) where
  D1 :: Dimension 1
  D2 :: Dimension 2
  D3 :: Dimension 3
  D4 :: Dimension 4

deriving instance Show (Dimension n)

dimension :: Dimension n -> Int
dimension D1 = 1
dimension D2 = 2
dimension D3 = 3
dimension D4 = 4

p2l :: Dimension n -> Point n -> [Int]
p2l D1  x        = [x]
p2l D2 (x,y)     = [x,y]
p2l D3 (x,y,z)   = [x,y,z]
p2l D4 (x,y,z,w) = [x,y,z,w]

l2p :: Dimension n -> [Int] -> Point n
l2p D1 [x]       = x
l2p D2 [x,y]     = (x,y)
l2p D3 [x,y,z]   = (x,y,z)
l2p D4 [x,y,z,w] = (x,y,z,w)
l2p d l = error $ "l2p: wrong number of elements for " ++ show d ++ ": " ++ show l



-- points

type family Point (n :: Nat) where
  Point 1 = Int
  Point 2 = (Int, Int)
  Point 3 = (Int, Int, Int)
  Point 4 = (Int, Int, Int, Int)

neighbours :: Eq (Point n) => Dimension n -> Point n -> [Point n]
neighbours d p = delete p $ neighbourhood d p

neighbourhood :: Dimension n -> Point n -> [Point n]
neighbourhood d p =
  l2p d . zipWith (+) l <$> replicateM (dimension d) [-1,0,1]
  where l = p2l d p



-- grid

type Key p = (Eq p, Ord p, Hashable p)

data Grid (n :: Nat) =
  Grid { gDim  :: Dimension n
       , gData :: S.HashSet (Point n)
       }

mkGrid :: Key (Point n) => Dimension n -> [Point n] -> Grid n
mkGrid d ps = Grid d $ S.fromList ps

isActive :: Key (Point n) => Grid n -> Point n -> Bool
isActive g p = S.member p $ gData g

activeNeighbours :: Key (Point n) => Grid n -> Point n -> Int
activeNeighbours g@(Grid d _) p =
  countTrue $ isActive g <$> neighbours d p

step :: Key (Point n) => Grid n -> Grid n
step grid@(Grid d !g) = Grid d
  $ S.fromList
  $ filter nowActive
  $ nubOrd
  $ neighbourhood d =<< S.toList g
  where
    nowActive p = case (isActive grid p, activeNeighbours grid p) of
                    (False, 3) -> True
                    (True,  2) -> True
                    (True,  3) -> True
                    _          -> False



-- debug

display :: Key (Point n) => Grid n -> IO ()
display grid@(Grid d g) = do
  let allPoints = S.toList g
      coordinates = transpose $ p2l d <$> allPoints
      bounds = [(minimum l, maximum l) | l <- coordinates]
      [(xMin,xMax),(yMin,yMax)] = take 2 bounds
      ranges = [[a..b] | (a,b) <- drop 2 bounds]
  forM_ (sequence ranges) $ \coords -> do
    putStrLn $ intercalate ", " $ do
      (name, value) <- zip "zwqut" coords
      pure $ name : '=' : show value
    forM_ [yMin .. yMax] $ \y -> do
      putStrLn $ do
        x <- [xMin .. xMax]
        pure $ toC $ isActive grid $ l2p d $ x:y:coords
    putStrLn ""
  where toC False = '.'
        toC True  = '#'



-- input

parseInput :: Key (Point n) => Dimension n -> String -> Grid n
parseInput d s = mkGrid d $ do
  (y, line) <- zip [0..] $ lines s
  (x, c)    <- zip [0..] line
  if c == '#'
    then pure $ l2p d $ take (dimension d) $ x : y : repeat 0
    else mempty



-- solution

(^.) :: (a -> a) -> Int -> (a -> a)
(^.) _ 0  = id
(^.) f 1  = f
(^.) f !n = f . (f ^. (n-1))

part1 :: Grid 3 -> Int
part1 = S.size . gData . (step ^. 6)

part2 :: Grid 4 -> Int
part2 = S.size . gData . (step ^. 6)



-- main

main :: IO ()
main = aocMain 17 $ \rawData -> do
  let testInput3 = parseInput D3 example
      testInput4 = parseInput D4 example
      realInput3 = parseInput D3 rawData
      realInput4 = parseInput D4 rawData
  putStrLn "# Part 1"
  print $ part1 testInput3
  print $ part1 realInput3
  putStrLn "# Part 2"
  print $ part2 testInput4
  print $ part2 realInput4

example :: String
example = ".#.\n..#\n###"
