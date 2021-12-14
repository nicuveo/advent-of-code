-- import

import           Control.Monad
import           Control.Monad.State
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import           Data.Hashable       (Hashable)
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Strict.These
import           Data.Traversable    (for)
import           Text.Parsec         hiding (State)
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

data PolymerTree = PolymerTree Pair ~PolymerTree ~PolymerTree

mkTrees :: HashMap Pair Char -> HashMap Pair PolymerTree
mkTrees rules = trees
  where
    trees = M.mapWithKey mkTree rules
    mkTree p@(l,r) c = PolymerTree p (trees ! (l,c)) (trees ! (c,r))

memoize
  :: (Eq k, Hashable k, MonadState (HashMap k a) m)
  => k    -- ^ key
  -> m a  -- ^ action to generate the value
  -> m a  -- ^ resulting value
memoize key action = do
  cache <- get
  case M.lookup key cache of
    Just value -> pure value
    Nothing    -> do
      result <- action
      modify $ M.insert key result
      pure result

countRightChars
  :: Int
  -> PolymerTree
  -> State (HashMap (Pair, Int) (HashMap Char Int)) (HashMap Char Int)
countRightChars depth (PolymerTree p@(_, pr) tl tr)
  | depth == 0 = pure $ M.singleton pr 1
  | otherwise  = memoize (p, depth) $ do
      countL <- countRightChars (depth-1) tl
      countR <- countRightChars (depth-1) tr
      pure $ M.unionWith (+) countL countR

solve :: Int -> String -> HashMap Pair Char -> Int
solve depth start rules = flip evalState M.empty $ do
  let
    pairs = zip start $ tail start
    trees = mkTrees rules
  counts <- for pairs $ \p ->
    countRightChars depth $ trees ! p
  let
    charCount =
      M.elems $
      M.insertWith (+) (head start) 1 $
      foldl' (M.unionWith (+)) M.empty counts
  pure $ maximum charCount - minimum charCount

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
