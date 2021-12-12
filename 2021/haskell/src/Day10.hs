-- import

import           Control.Monad
import           Data.Either   (lefts, rights)
import           Data.Function (on)
import           Data.List
import           Data.Maybe

import           AOC


-- input

type Input = [String]

parseInput :: String -> Input
parseInput = lines


-- solution

validate :: String -> Either String Char
validate = go []
  where
    go :: [Char] -> String -> Either String Char
    go s []     = Left $ map closing s
    go s (c:cs) = case c of
      '(' -> go (c:s) cs
      '[' -> go (c:s) cs
      '{' -> go (c:s) cs
      '<' -> go (c:s) cs
      ')' -> matchClosing c '(' s cs
      ']' -> matchClosing c '[' s cs
      '}' -> matchClosing c '{' s cs
      '>' -> matchClosing c '<' s cs
      w   -> error $ "unexpected character: '" ++ [w] ++ "'"
    matchClosing :: Char -> Char -> [Char] -> [Char] -> Either String Char
    matchClosing given expected s cs = case uncons s of
      Nothing      -> error "encoutered closing tag with empty stack"
      Just (c, ns) -> if c == expected
        then go ns cs
        else Right given

closing :: Char -> Char
closing c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  c   -> error $ "unexpected opening character: '" ++ [c] ++ "'"

errorScore :: Char -> Int
errorScore c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  c   -> error $ "unexpected error score character: '" ++ [c] ++ "'"

incompleteScore :: String -> Int
incompleteScore = foldl' step 0
  where
    step t c = 5 * t + case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      w   -> error $ "unexpected incomplete score character: '" ++ [w] ++ "'"

part1 :: Input -> Int
part1 = sum . map errorScore . rights . map validate

part2 :: Input -> Int
part2 = middle . sort . map incompleteScore . lefts . map validate
  where
    middle :: [a] -> a
    middle l = l !! (length l `div` 2)


-- main

main :: IO ()
main = aocMain 10 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[][\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
