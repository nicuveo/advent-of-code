-- import

import           Data.Foldable (toList)
import           Data.Function ((&))
import qualified Data.IntSet   as Set
import           Data.List
import           Data.Maybe
import           Data.Sequence (ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence as Seq

import           AOC



-- input

type Input = [Int]

parseInput :: String -> [Int]
parseInput = map read . lines



-- part 1

part1 :: Input -> Int -> Int
part1 input preambleSize =
  let (preamble, rest) = splitAt preambleSize input
      initialRange     = Seq.fromList preamble
      initialSet       = Set.fromList preamble
  in go initialRange initialSet rest
  where go _ _ [] = error "reached the end, didn't find the number"
        go range set (newest:xs)
          | any (canSumTo newest set) $ Set.elems set =
            let (oldest :< range') = Seq.viewl range
                newRange           = range' |> newest
                newSet             = Set.insert newest $ Set.delete oldest $ set
            in  go newRange newSet xs
          | otherwise = newest
        canSumTo n set a = Set.member (n - a) set

part1ButSlower :: Input -> Int -> Int
part1ButSlower input preambleSize =
    reverse input
  & tails
  & map (take $ 1 + preambleSize)
  & filter (\l -> length l == 1 + preambleSize)
  & filter (\(x:l) -> x `notElem` [a + b | (a:s) <- tails l, b <- s])
  & head
  & head



-- part 2

part2 :: Input -> Int -> Int
part2 input target = minimum result + maximum result
  where
    (preamble, rest) = splitAt 2 input
    initialRange     = (Seq.fromList preamble, sum preamble, Seq.fromList rest)
    result           = findAsc target initialRange

part2ButSlower :: Input -> Int -> Int
part2ButSlower input target = minimum result + maximum result
  where result = tails input
          & concatMap inits
          & filter bigEnough
          & find sumsToTarget
          & fromJust
        bigEnough (_:_:_) = True
        bigEnough _       = False
        sumsToTarget l = sum l == target


type Range = (Seq.Seq Int, Int, Seq.Seq Int)

incLeft, incRight, decRight :: Range -> Range
incLeft  (selection, current, rest) = case Seq.viewl selection of
  EmptyL   -> error "tried to incLeft an empty range"
  (x :< s) -> (s, current - x, rest)
incRight (selection, current, rest) = case Seq.viewl rest of
  EmptyL   -> error "tried to incRight an empty range"
  (x :< s) -> (selection |> x, current + x, s)
decRight (selection, current, rest) = case Seq.viewr selection of
  EmptyR   -> error "tried to decRight an empty range"
  (s :> x) -> (s, current - x, x <| rest)


findAsc :: Int -> Range -> [Int]
findAsc target range@(selection, current, _) =
  case compare current target of
    EQ -> toList selection
    GT -> findDesc target $ incLeft  range
    LT -> findAsc  target $ incRight range

findDesc :: Int -> Range -> [Int]
findDesc target range@(selection, current, _) =
  case compare current target of
    EQ -> toList selection
    LT -> findAsc  target $ incLeft  range
    GT -> findDesc target $ decRight range

{-

 0 2 4 6 2 3 6 7 2 3
[   ]        too small -> start asc
[     ]
[       ]    too big   -> increase left
  [     ]    too big   -> start desc
  [   ]      too small -> increase left
    [ ]      too small -> start asc
    [   ]
    [     ]

-}



-- main

main :: IO ()
main = aocMain 09 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  let testResult = part1 testInput  5
  let realResult = part1 realInput 25
  print testResult
  print realResult
  print $ part1ButSlower testInput  5
  print $ part1ButSlower realInput 25
  putStrLn "# Part 2"
  print $ part2 testInput testResult
  print $ part2 realInput realResult
  print $ part2ButSlower testInput testResult
  print $ part2ButSlower realInput realResult


example :: String
example = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"
