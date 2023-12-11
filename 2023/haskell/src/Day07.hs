module Day07 where


-- import

import AOC            as AOC
import "this" Prelude

import Data.Char      (digitToInt)
import Text.Parsec    as P


-- input

type Input = [Hand]
type Hand  = ([Card], Int)

data Card
  = Number Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

parseInput :: String -> Input
parseInput = parseLinesWith do
  cards <- P.count 5 card
  spaces
  score <- number
  pure (cards, score)
  where
    card = choice
      [ Ace       <$ char 'A'
      , King      <$ char 'K'
      , Queen     <$ char 'Q'
      , Jack      <$ char 'J'
      , Number 10 <$ char 'T'
      , Number . digitToInt <$> digit
      ]


-- solution

data HandType
  = HighCard
  | Pair
  | TwoPairs
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord, Bounded, Enum)

handType1 :: [Card] -> HandType
handType1 cards = case sort $ map length $ group $ sort cards of
  [5]         -> FiveOfAKind
  [1,4]       -> FourOfAKind
  [2,3]       -> FullHouse
  [1,1,3]     -> ThreeOfAKind
  [1,2,2]     -> TwoPairs
  [1,1,1,2]   -> Pair
  [1,1,1,1,1] -> HighCard
  l           -> error $ "encountered unexpected hand: " ++ show cards ++ " => " ++ show l

strength1 :: Hand -> Hand -> Ordering
strength1 (leftCards, _) (rightCards, _) =
  case compare leftType rightType of
    EQ -> compare leftCards rightCards
    o  -> o
  where
    leftType  = handType1 leftCards
    rightType = handType1 rightCards

handType2 :: [Card] -> HandType
handType2 cards = handType1 $ otherCards ++ replicate nbJokers mostCommon
  where
    nbJokers = AOC.count Jack cards
    otherCards = filter (/= Jack) cards
    mostCommon = case sortOn length $ group $ sort otherCards of
      [] -> Ace
      gs -> head $ last gs

strength2 :: Hand -> Hand -> Ordering
strength2 (leftCards, _) (rightCards, _) =
  case compare leftType rightType of
    EQ -> compare (map replaceJoker leftCards) (map replaceJoker rightCards)
    o  -> o
  where
    leftType  = handType2 leftCards
    rightType = handType2 rightCards
    replaceJoker = \case
      Jack -> Number 0
      card -> card

part1 :: Input -> Int
part1 = sum . zipWith (*) [1..] . map snd . sortBy strength1

part2 :: Input -> Int
part2 = sum . zipWith (*) [1..] . map snd . sortBy strength2


-- main

example :: String
example = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

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
