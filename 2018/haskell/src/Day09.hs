{-# LANGUAGE BangPatterns #-}
-- module

module Day09 (day09_1, day09_2) where



-- import

import           Control.Monad
import           Data.Foldable as F (toList)
import           Data.List     as L
import qualified Data.Map      as M
import qualified Data.Sequence as S
import qualified Text.Parsec   as P

import           Common



-- solution

day09_1 :: Solution
day09_1 input = show $ maximum $ M.elems $ countPoints players ps
  where (players, marbles) = parseInput input
        ps = points $ L.foldl' playTurn originalState [1..marbles]


day09_2 :: Solution
day09_2 input = show $ maximum $ M.elems $ countPoints players ps
  where (players, marbles) = parseInput input
        ps = points $ L.foldl' playTurn originalState [1..marbles * 100]



-- helpers

data State = State { circle       :: S.Seq Int
                   , currentIndex :: Int
                   , points       :: [[Int]]
                   }

originalState :: State
originalState = State (S.singleton 0) 0 []

instance Show State where
  show (State c i p) = "[" ++ render (F.toList c) i ++ "] " ++ show p
    where render c ci = concat [ if index == ci
                                 then '(' : show marble ++ ")"
                                 else ' ' : show marble ++ " "
                               | (index, marble) <- L.zip [0..] c
                               ]

playTurn :: State -> Int -> State
playTurn (State !circle index points) marble
  | marble `mod` 23 /= 0 =
    let newIndex = mod (index + 2) $ S.length circle
    in State (S.insertAt newIndex marble circle) newIndex points
  | otherwise =
    let newIndex = mod (index - 7) $ S.length circle
    in State (S.deleteAt newIndex circle) (newIndex `mod` S.length circle) $ [marble, S.index circle newIndex] : points

countPoints :: Int -> [[Int]] -> M.Map Int Int
countPoints players = L.foldl' insertPoint M.empty
  where insertPoint m [m1, m2] = M.insertWith (+) (mod m1 players) (m1 + m2) m

parseInput :: String -> (Int, Int)
parseInput = parseWith gameData
  where gameData = do
          player <- intParser
          void $ P.string " players; last marble is worth"
          marble <- intParser
          void $ P.string " points"
          return (player, marble)
