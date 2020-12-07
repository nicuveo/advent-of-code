{-# LANGUAGE BangPatterns #-}

-- import

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (for_)
import           Data.Function        (on)
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import           Data.List
import           Data.Maybe
import           Data.Traversable     (for)
import           Text.Parsec          hiding (State)
import           Text.Parsec.Char

import           AOC



-- input

type Bag  = String
type Rule = (String, [(Int, String)])

parseInput :: String -> [Rule]
parseInput = parseLinesWith rule
  where rule = do
          container  <- bag
          symbol "contain"
          containees <- noOtherBags <|> otherBags
          symbol "."
          pure (container, containees)
        bag = do
          adjective <- many1 lower
          space
          color     <- many1 lower
          string " bag"
          optional $ symbol "s"
          pure (adjective <> " " <> color)
        noOtherBags = [] <$ string "no other bags"
        otherBags   = otherBag `sepBy` symbol ","
        otherBag = do
          amount    <- intLiteral
          containee <- bag
          pure (amount, containee)



-- graph1

type Graph1 = M.HashMap Bag [Bag]

makeGraph1 :: [Rule] -> Graph1
makeGraph1 = foldl' insertEdges M.empty
  where insertEdges graph (target, bags) =
          foldl' (insertEdge target) graph bags
        insertEdge target graph (_, key) =
          M.insertWith (++) key [target] graph

reachable :: Bag -> Graph1 -> [Bag]
reachable root graph = S.toList $ visit S.empty root
  where visit !seen bag
          | bag `S.member` seen = seen
          | otherwise           =
            let startSet = S.insert bag seen
                nextBags = M.lookupDefault [] bag graph
            in foldl' visit startSet nextBags



-- graph2

type Graph2 = M.HashMap Bag [(Int, Bag)]

makeGraph2 :: [Rule] -> Graph2
makeGraph2 = foldl' insertEdges M.empty
  where insertEdges graph (container, containees) =
          M.insertWith (++) container containees graph


type Amount  = M.HashMap Bag Int
type Amounts = M.HashMap Bag Amount

type AmountMonad = ReaderT Graph2 (State Amounts)

totalBags :: Bag -> Graph2 -> Amount
totalBags root graph =
  flip evalState M.empty $
  flip runReaderT graph $
  computeBagAmount (1, root)

computeBagAmount :: (Int, Bag) -> AmountMonad Amount
computeBagAmount (quantity, bag) = do
  knownAmounts <- get
  case M.lookup bag knownAmounts of
    Just amount -> return $ (quantity *) <$> amount
    Nothing     -> do
      subBags    <- asks $ M.lookupDefault [] bag
      subAmounts <- for subBags $ \sub@(q,b) -> do
        subAmount <- computeBagAmount sub
        pure $ M.insertWith (+) b q subAmount
      let bagAmount = foldl' (M.unionWith (+)) M.empty subAmounts
      modify $ M.insert bag bagAmount
      return $ (quantity *) <$> bagAmount



-- solution

part1 :: [Rule] -> Int
part1 = subtract 1 . length . reachable "shiny gold" . makeGraph1

part2 :: [Rule] -> Int
part2 = sum . M.elems . totalBags "shiny gold" . makeGraph2



-- main

main :: IO ()
main = aocMain 7 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."
