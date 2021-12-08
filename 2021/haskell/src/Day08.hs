-- import

import           Control.Monad
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC                 hiding (count)


-- input

type Input = [Pattern]
type Pattern = ([HashSet Char], [HashSet Char])

parseInput :: String -> Input
parseInput = parseLinesWith $ do
  w <- count 10 word
  symbol "|"
  d <- count 4 word
  pure (sortOn S.size w, d)
  where
    word = fmap S.fromList $ lexeme $ many1 letter


-- solution

solve :: Pattern -> [Int]
solve (wires, digits) = map (mapping !) digits
  where
    [one, seven, four, f1, f2, f3, s1, s2, s3, eight] = wires
    bd = S.difference four one
    ([five], [tt1, tt2]) = partition (S.isSubsetOf bd) [f1, f2, f3]
    ([sn1, sn2], [zero]) = partition (S.isSubsetOf bd) [s1, s2, s3]
    (two, three) = if one `S.isSubsetOf` tt1
      then (tt2, tt1)
      else (tt1, tt2)
    (six, nine) = if one `S.isSubsetOf` sn1
      then (sn2, sn1)
      else (sn1, sn2)
    mapping = M.fromList $
      zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]

part1 :: [[Int]] -> Int
part1 = sum . map (countIf (`elem` [1,4,7,8]))

part2 :: [[Int]] -> Int
part2 = sum . map toInt
  where
    toInt = foldl1 (\a x -> 10 * a + x)


-- main

main :: IO ()
main = aocMain 8 $ \rawData -> do
  let testInput = map solve $ parseInput example
      realInput = map solve $ parseInput rawData
  print testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example =
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |\
  \fdgacbe cefdb cefbgd gcbe\n\
  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |\
  \fcgedb cgb dgebacf gc\n\
  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |\
  \cg cg fdcagb cbg\n\
  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |\
  \efabcd cedba gadfec cb\n\
  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |\
  \gecf egdcabf bgf bfgea\n\
  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |\
  \gebdcfa ecba ca fadegcb\n\
  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |\
  \cefg dcbef fcge gbcadfe\n\
  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |\
  \ed bcgafe cdgba cbgef\n\
  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |\
  \gbdfcae bgc cg cgb\n\
  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |\
  \fgae cfgab fg bagce"
