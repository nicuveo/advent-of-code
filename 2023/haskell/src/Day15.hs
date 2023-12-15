module Day15 where


-- import

import AOC
import "this" Prelude

import Data.Char
import Data.HashMap.Strict.InsOrd qualified as M
import Data.List                  (groupBy)
import Data.List.Split
import Text.Parsec                hiding (label)


-- input

type Input = [String]

parseInput :: String -> Input
parseInput = wordsBy (== ',')


-- solution

hash :: String -> Int
hash = foldl' step 0
  where
    step accum c = mod (17 * (accum + ord c)) 256

part1 :: Input -> Int
part1 = sum . map hash

data Instruction
  = Set    String Int
  | Delete String

part2 :: Input -> Int
part2 = computePower . foldl' exec mempty . map (parseWith instruction)
  where
    instruction = do
      label <- many1 letter
      sign  <- char '=' <|> char '-'
      case sign of
        '=' -> do
          arg <- number
          pure $ Set label arg
        '-' -> do
          pure $ Delete label
        c   -> error $ "unexpected character " ++ [c]

    exec = flip \case
      Delete label    -> M.delete (hash label, label)
      Set label value -> M.insert (hash label, label) value

    computePower lenses = sum do
      boxLenses <- groupBy ((==) `on` (fst . fst)) $ sortOn (fst . fst) $ M.toList lenses
      (slotIndex, ((boxIndex, _), focalLength)) <- zip [1..] boxLenses
      pure $ (boxIndex + 1) * slotIndex * focalLength


-- main

example :: String
example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput
