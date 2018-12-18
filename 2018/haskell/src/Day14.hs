{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}



-- module

module Day14 (day14_1, day14_2) where



-- import

import           Data.Char
import           Data.Foldable as F (toList)
import           Data.Function
import           Data.List     as L
import           Data.Sequence as S

import           Common



-- solution

day14_1 :: Solution
day14_1 input = nNextRecipesAfterMFirst 10 (read input) $ initialState "37"


day14_2 :: Solution
day14_2 input = show $ recipesBefore (toScoreBoard input) $ initialState "37"



-- helpers

type Scores = S.Seq Int

data State = State { scores :: Scores
                   , elf1   :: Int
                   , elf2   :: Int
                   } deriving Show


initialState :: String -> State
initialState s = State (toScoreBoard s) 0 1


step :: State -> State
step (State s e1 e2) = State newScores newElf1 newElf2
  where score1     = index s e1
        score2     = index s e2
        newScore   = score1 + score2
        newRecipes = [newScore `div` 10 | newScore > 9] ++ [newScore `mod` 10]
        newScores  = s >< S.fromList newRecipes
        newElf1    = mod (e1 + score1 + 1) $ S.length newScores
        newElf2    = mod (e2 + score2 + 1) $ S.length newScores

nNextRecipesAfterMFirst :: Int -> Int -> State -> String
nNextRecipesAfterMFirst n m = map intToDigit . L.take n . L.drop m . F.toList . scores . head . dropWhile notBigEnough . iterate step
  where notBigEnough s = S.length (scores s) < n + m

recipesBefore :: Scores -> State -> Int
recipesBefore suffix !state
  | suffix `isSeqSuffixOf` allScores     = S.length allScores - S.length suffix
  | suffix `isSeqSuffixOf` allButTheLast = S.length allScores - S.length suffix - 1
  | otherwise                            = recipesBefore suffix $ step state
  where allScores     = scores state
        allButTheLast = dropAtEnd allScores


isSeqSuffixOf :: Seq Int -> Seq Int -> Bool
isSeqSuffixOf = isSeqSuffixOf_ `on` viewr
  where isSeqSuffixOf_ EmptyR   _        = True
        isSeqSuffixOf_ _        EmptyR   = False
        isSeqSuffixOf_ (l :> x) (r :> y) = x == y && isSeqSuffixOf l r

dropAtEnd :: Seq a -> Seq a
dropAtEnd (viewr -> s :> _) = s
dropAtEnd _ = error "dropAtEnd of empty sequence"



-- parsing

toScoreBoard :: String -> Scores
toScoreBoard = S.fromList . map digitToInt
