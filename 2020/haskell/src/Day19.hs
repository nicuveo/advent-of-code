{-# LANGUAGE FlexibleContexts #-}

-- import

import           Control.Monad
import           Data.Either
import           Data.Function      (on)
import qualified Data.IntMap        as M
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

import           AOC

import           Debug.Trace



-- input

data Step  = MatchChar Char | CallRule Int deriving (Show, Eq)
type Rule  = [Step]
type Rules = M.IntMap [Rule]
type Input = (Rules, [String])

parseInput :: String -> Input
parseInput s = (M.fromList $ parseLinesWith rule rules, lines strings)
  where [rules, strings] = splitOn "\n\n" s
        rule = do
          n <- intLiteral
          symbol ":"
          r <- steps `sepBy` symbol "|"
          pure (n, r)
        steps = many1 $ choice [match, call]
        match = fmap MatchChar $ char '"' *> lower <* symbol "\""
        call  = CallRule <$> intLiteral



-- solution

mkParsers :: Rules -> M.IntMap (Parser String -> Parser String)
mkParsers rs = ps
  where ps = mkParser <$> rs
        mkParser rules cont = tryAll [foldr fromRule cont rule | rule <- rules]
        fromRule (MatchChar c) cont = (:) <$> char c <*> cont
        fromRule (CallRule  i) cont = ps M.! i $ cont

getRootParser :: Rules -> Parser String
getRootParser rs = mkParsers rs M.! 0 $ "" <$ eof

runWith :: Rules -> Input -> Int
runWith specialRules (rules, strings) = countIf isRight $ parse parser "" <$> strings
  where parser = getRootParser $ M.union specialRules rules



part1 :: Input -> Int
part1 = runWith M.empty

part2 :: Input -> Int
part2 = runWith $ M.fromList
  [ ( 8, map CallRule <$> [[42], [42,8]])
  , (11, map CallRule <$> [[42,31], [42,11,31]])
  ]



-- main

main :: IO ()
main = aocMain 19 $ \rawData -> do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

example1, example2 :: String
example1 = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
example2 = "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
