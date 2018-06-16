{-# LANGUAGE BangPatterns #-}



-- module

module Day19 (day19_1, day19_2) where



-- import

import           Data.Sequence (ViewL (..), deleteAt, fromList, viewl, (|>))
import qualified Data.Sequence as S

import           Common



-- solution

day19_1 :: Solution
day19_1 input = show $ eliminate1 [1 .. read input]


day19_2 :: Solution
day19_2 input = show $ eliminate2 [1 .. read input]



-- helpers

eliminate1 :: [Int] -> Int
eliminate1 []    = error "eliminate: killed all of them!"
eliminate1 [elf] = elf
eliminate1 !elves
  | even (length elves) = eliminate1 next
  | otherwise           = eliminate1 $ tail next
  where next = [ elf
               | (index, elf) <- zip [0..] elves
               , even index
               ]

eliminate2 :: [Int] -> Int
eliminate2 = eliminate_ . viewl . fromList
  where eliminate_ EmptyL         = error "eliminate: killed all of them!"
        eliminate_ (elf :< elves) =
          case viewl elves of
            EmptyL -> elf
            _      -> eliminate_ $ viewl $ deleteAt killedIndex elves |> elf
          where killedIndex = div (S.length elves - 1) 2
