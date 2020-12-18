{-# LANGUAGE ViewPatterns #-}

-- import

import           Control.Monad
import           Data.Function       (on)
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Printf

import           AOC



-- input

type Range       = (Int, Int)
type Ranges      = [Range]
type Constraints = HM.HashMap String Ranges
type Ticket      = [Int]

type Input = (Constraints, [Ticket])

parseInput :: String -> Input
parseInput = parseWith input
  where input = do
          cs     <- constraints
          newline
          mine   <- myTicket
          newline >> newline
          others <- otherTickets
          pure (cs, mine : others)
        constraints = fmap HM.fromList $ constraint `sepEndBy` newline
        constraint = do
          key <- many1 $ lower <|> char ' '
          string ": "
          ranges <- range `sepBy` string " or "
          pure (key, ranges)
        range = do
          a <- read <$> many1 digit
          char '-'
          b <- read <$> many1 digit
          guard $ a <= b
          pure (a,b)
        myTicket = do
          symbol "your ticket:"
          ticket
        otherTickets = do
          symbol "nearby tickets:"
          ticket `sepBy` newline
        ticket = (fmap read $ many1 digit) `sepBy` symbol ","



-- solution

part1 :: Input -> Int
part1 (constraints, tickets) =
  sum $ mapMaybe (findError constraints) tickets

part2 :: Input -> Int
part2 (constraints, tickets) = product $ catMaybes $ do
  (index, value) <- zip [0..] myTicket
  pure $ if "departure" `isPrefixOf` (fieldMap HM.! index)
         then Just value
         else Nothing
  where myTicket = head tickets
        fieldMap = solve $ findMatches constraints tickets


valid :: Constraints -> Ticket -> Bool
valid = isNothing ... findError

findError :: Constraints -> Ticket -> Maybe Int
findError constraints = find wrong
  where wrong x = all (outside x) $ concat $ HM.elems constraints
        x `outside` (a,b) = x < a || x > b

findMatches :: Constraints -> [Ticket] -> HM.HashMap Int [String]
findMatches constraints tickets = HM.fromList $ do
  (index, values) <-
    zip [0..] $ transpose $ filter (valid constraints) tickets
  pure (index, matches values)
  where
    within x (a,b) = x >= a && x <= b
    matches values = do
      (name, ranges) <- HM.toList constraints
      guard $ all (\x -> any (x `within`) ranges) values
      pure name

solve :: HM.HashMap Int [String] -> HM.HashMap Int String
solve = HM.fromList . go . HM.toList
  where
    go l = case find (\(_, ms) -> length ms == 1) l of
      Just (i, [match]) -> (i, match) : go (mapMaybe (remove match) l)
      _                 -> if null l then [] else error "could not solve"
    remove m (i, ms) = case delete m ms of
      []  -> Nothing
      ms' -> Just (i, ms')



-- main

main :: IO ()
main = aocMain 16 $ \rawData -> do
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
example1 = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"
example2 = "class: 0-1 or 4-19\nrow: 0-5 or 8-19\ndeparture seat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"
