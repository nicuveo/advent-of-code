{-# LANGUAGE RecursiveDo #-}

module Main where


-- import

import Control.Monad.Tardis
import Data.Char            (digitToInt)
import Data.IntMap          as M
import Debug.Trace

import AOC


-- solution

part1 :: String -> Int
part1 = countTrue
  . flip evalTardis ((mempty, mempty), (mempty, mempty, 0, 0))
  . traverse solve
  where
    solve '\n' = do
      (down, right, row, _col) <- getPast
      sendFuture (down, right, row+1, 0)
      pure False
    solve (digitToInt -> tree) = mdo
      (downMap, rightMap, row, col) <- getPast
      sendPast
        ( M.insert col (max tree up)   upMap
        , M.insert row (max tree left) leftMap
        )
      let
        down   = M.findWithDefault (-1) col downMap
        up     = M.findWithDefault (-1) col upMap
        left   = M.findWithDefault (-1) row leftMap
        right  = M.findWithDefault (-1) row rightMap
      sendFuture
        ( M.insert col (max tree down)  downMap
        , M.insert row (max tree right) rightMap
        , row
        , col+1
        )
      ~(upMap, leftMap) <- getFuture
      pure $ tree > down || tree > right || tree > up || tree > left

part2 :: String -> Int
part2 = maximum
  . flip evalTardis ((mempty, mempty), (mempty, mempty, 0, 0))
  . traverse solve
  where
    solve '\n' = do
      (down, right, row, _col) <- getPast
      sendFuture (down, right, row+1, 0)
      pure 0
    solve (digitToInt -> tree) = mdo
      (downMap, rightMap, row, col) <- getPast
      sendPast
        ( M.insertWith (++) col [tree] upMap
        , M.insertWith (++) row [tree] leftMap
        )
      let
        down   = M.findWithDefault [] col downMap
        up     = M.findWithDefault [] col upMap
        left   = M.findWithDefault [] row leftMap
        right  = M.findWithDefault [] row rightMap
      sendFuture
        ( M.insertWith (++) col [tree] downMap
        , M.insertWith (++) row [tree] rightMap
        , row
        , col+1
        )
      ~(upMap, leftMap) <- getFuture
      pure $
        countTrees tree down * countTrees tree up *
        countTrees tree left * countTrees tree right
    countTrees _ [] = 0
    countTrees tree (t:ts)
      | t < tree  = 1 + countTrees tree ts
      | otherwise = 1


-- main

main :: IO ()
main = aocMain 08 $ \rawData -> do
  putStrLn "# Part 1"
  print $ part1 example
  print $ part1 rawData
  putStrLn "# Part 2"
  print $ part2 example
  print $ part2 rawData

example :: String
example = "30373\n25512\n65332\n33549\n35390"
