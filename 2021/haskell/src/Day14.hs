-- import

import           Control.Monad
import           Data.Function       (on)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Strict.These
import           Text.Parsec
import           Text.Parsec.Char

import           AOC.Parsing
import           AOC.Runtime


-- input

type Input = (String, HashMap Pair Char)
type Pair  = (Char, Char)

parseInput :: String -> Input
parseInput = parseWith $ do
  polymer <- lexeme $ many1 letter
  rules   <- many1 rule
  pure (polymer, M.fromList rules)
  where
    rule = lexeme $ do
      pl <- letter
      pr <- letter
      symbol " -> "
      c  <- letter
      pure ((pl,pr),c)


-- solution

addN :: (Eq k, Hashable k) => k -> Int -> HashMap k Int -> HashMap k Int
addN = M.insertWith (+)

fromPairCountToCharCount :: HashMap Pair Int -> HashMap Char Int
fromPairCountToCharCount = M.foldrWithKey' step M.empty
  where
    step :: Pair -> Int -> HashMap Char Int -> HashMap Char Int
    step (_,r) n = addN r n

countRightChars :: Int -> HashMap Pair Char -> [Pair] -> HashMap Char Int
countRightChars iteration rules start =
  fromPairCountToCharCount $ iterate step startCount !! iteration
  where
    startCount = M.fromListWith (+) $ map (,1) start
    step = M.foldrWithKey' transform M.empty
    transform p@(l,r) n =
      let
        c = rules M.! p
      in
        addN (l,c) n . addN (c,r) n

solve :: Int -> String -> HashMap Pair Char -> Int
solve iteration start rules = maximum charCount - minimum charCount
  where
    pairs l = zip l $ tail l
    charCount = M.elems $
      addN (head start) 1 $ countRightChars iteration rules $ pairs start

part1 :: Input -> Int
part1 = uncurry $ solve 10

part2 :: Input -> Int
part2 = uncurry $ solve 40


-- main

main :: IO ()
main = aocMain 14 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
