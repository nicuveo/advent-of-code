-- import

import           Control.Monad
import           Data.Char
import           Data.Function    (on)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Group  = String
type Groups = [Group]

parseGroups :: String -> Groups
parseGroups = splitOn "\n\n"



-- solution

part1 :: Groups -> Int
part1 = sum . map (length . nub . filter isAsciiLower)

part2 :: Groups -> Int
part2 = sum . map countAnswers
  where countAnswers = length . foldl1' intersect . map clean . lines
        clean = nub . filter isAsciiLower



-- main

main :: IO ()
main = aocMain 6 $ \rawData -> do
  let testGroups = parseGroups example
      realGroups = parseGroups rawData
  putStrLn "# Part 1"
  print $ part1 testGroups
  print $ part1 realGroups
  putStrLn "# Part 2"
  print $ part2 testGroups
  print $ part2 realGroups

example :: String
example = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
