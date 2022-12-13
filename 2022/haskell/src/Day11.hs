{-# LANGUAGE TemplateHaskell #-}

module Main where


-- import

import Control.Lens.At
import Control.Lens.Each
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad.State.Strict
import Data.Foldable
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as M
import Data.List                  qualified as L
import Data.Sequence              (Seq, (|>))
import Data.Sequence              qualified as S
import Prelude                    hiding (round)
import Text.Parsec                hiding (State)
import Text.Parsec.Char
import Text.Printf

import AOC


-- input

data Expr
  = Product Value Value
  | Sum Value Value
  deriving (Show)

data Value
  = IntValue Int
  | OldValue
  deriving (Show)

data Monkey = Monkey
  { _mItems     :: Seq Int
  , _mOperation :: Expr
  , _mTest      :: Int
  , _mSuccess   :: Int
  , _mFailure   :: Int
  }
  deriving (Show)

makeLenses ''Monkey

type MonkeyMap = IntMap Monkey

parseInput :: String -> MonkeyMap
parseInput = parseWith $ M.fromList <$> many1 monkey
  where
    monkey = do
      symbol "Monkey"
      index <- number
      symbol ":"
      symbol "Starting items: "
      items <- fmap S.fromList $ number `sepBy` symbol ","
      symbol "Operation: new ="
      operation <- expression
      symbol "Test:"
      symbol "divisible by"
      test <- number
      symbol "If true: throw to monkey"
      success <- number
      symbol "If false: throw to monkey"
      failure <- number
      pure (index, Monkey items operation test success failure)
    expression = do
      v1 <- value
      op <- tryAll
        [ Product <$ symbol "*"
        , Sum     <$ symbol "+"
        ]
      v2 <- value
      pure $ op v1 v2
    value = tryAll
      [ OldValue <$  symbol "old"
      , IntValue <$> number
      ]


-- solution

data MonkeyState = MonkeyState
  { _monkeyMap   :: MonkeyMap
  , _monkeyCount :: IntMap Int
  }
  deriving (Show)

makeLenses ''MonkeyState

round :: Int -> MonadState MonkeyState m => m ()
round worry = traverse_ (step worry) =<< uses monkeyMap M.keys

step :: MonadState MonkeyState m => Int -> Int -> m ()
step worry index = do
  det <- uses monkeyMap $ product . map _mTest . M.elems
  Monkey {..} <- uses monkeyMap (M.! index)
  monkeyMap . at index . each . mItems .= S.empty
  monkeyCount . at index . each += length _mItems
  for_ _mItems \item -> do
    let item' = mod (eval _mOperation item `div` worry) det
    if mod item' _mTest == 0
    then monkeyMap . at _mSuccess . each . mItems %= (|> item')
    else monkeyMap . at _mFailure . each . mItems %= (|> item')

eval :: Expr -> Int -> Int
eval e old = case e of
  Product v1 v2 -> go v1 * go v2
  Sum     v1 v2 -> go v1 + go v2
  where
    go = \case
      OldValue   -> old
      IntValue n -> n

part1 :: MonkeyMap -> Int
part1 monkeys =
  flip evalState (MonkeyState monkeys $ 0 <$ monkeys) do
    sequence_ $ replicate 20 $ round 3
    counts <- uses monkeyCount M.elems
    pure $ product $ take 2 $ L.sortOn negate counts

part2 :: MonkeyMap -> Int
part2 monkeys =
  flip evalState (MonkeyState monkeys $ 0 <$ monkeys) do
    sequence_ $ replicate 10000 $ round 1
    counts <- uses monkeyCount M.elems
    pure $ product $ take 2 $ L.sortOn negate counts


-- main

main :: IO ()
main = aocMain 11 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
