module Main where


-- import

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as M
import Data.List           qualified as L
import Data.Maybe
import Data.Traversable
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import AOC

import Debug.Trace


-- input

data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Bounded, Enum, Eq, Ord)

instance Show Resource where
  show Ore      = "ore"
  show Clay     = "clay"
  show Obsidian = "obsidian"
  show Geode    = "geode"

instance Hashable Resource where
  hashWithSalt s = hashWithSalt s . fromEnum

type Input = [Blueprint]

type Blueprint = HashMap Resource Cost
type Cost      = HashMap Resource Int
type Robots    = HashMap Resource Int
type Stock     = HashMap Resource Int


none :: HashMap Resource Int
none = M.fromList [(r, 0) | r <- enumerate]

parseInput :: String -> Input
parseInput = parseWith $ many1 blueprint
  where
    blueprint = do
      symbol "Blueprint"
      number
      symbol ":"
      M.fromList <$> many1 robot
    robot = do
      symbol "Each"
      target <- resource
      symbol "robot costs"
      costList <- fmap M.fromList $ cost `sepBy` symbol "and"
      symbol "."
      pure (target, costList)
    cost = do
      n <- number
      r <- resource
      pure (r, n)
    resource = tryAll
      [ Ore      <$ symbol "ore"
      , Clay     <$ symbol "clay"
      , Obsidian <$ symbol "obsidian"
      , Geode    <$ symbol "geode"
      ]

render :: HashMap Resource Int -> String
render m = L.intercalate ", " do
  r <- enumerate
  let x = m ! r
  guard $ x > 0
  pure $ printf "%d %s" x (show r)


-- build

pay :: Stock -> Cost -> Stock
pay = M.unionWith (-)

collect :: Stock -> Robots -> Stock
collect = M.unionWith (+)

maxRobots :: Blueprint -> HashMap Resource Int
maxRobots = L.foldl1' (M.unionWith max) . M.elems

maxGeodes :: Int -> Blueprint -> Int
maxGeodes totalTime bp = flip evalState mempty $ go totalTime (M.insert Ore 1 none) none
  where
    maxNeeded = maxRobots bp
    recipes = M.toList bp
    go timeLeft robots bag = do
      let delays = flip mapMaybe recipes $ \(target, cost) -> do
            waitTimes <- flip M.traverseWithKey cost \resource amountNeeded ->
              case robots ! resource of
                0 -> Nothing
                r -> Just $ case divMod (max 0 $ amountNeeded - bag ! resource) r of
                  (turns, 0) -> turns
                  (turns, _) -> turns+1
            let delay = maximum waitTimes + 1
            guard $ timeLeft > delay
            when (target /= Geode) $
              guard $ (maxNeeded ! target - robots ! target) * timeLeft > bag ! target
            pure (target, cost, delay)
          justWait = timeLeft * robots ! Geode + bag ! Geode
      bestSoFar <- gets $ M.findWithDefault 0 robots
      if bestSoFar > timeLeft
      then pure 0
      else do
        modify $ M.insert robots timeLeft
        paths <- for delays \(target, cost, delay) -> do
          go
            (timeLeft - delay)
            (M.insertWith (+) target 1 robots)
            (pay (collect bag $ fmap (delay*) robots) cost)
        pure $ maximum $ justWait : paths


-- solution

part1 :: Input -> Int
part1 = sum . zipWith (*) [1..] . map (maxGeodes 24)

part2 :: Input -> Int
part2 = product . map (maxGeodes 32) . take 3


-- main

main :: IO ()
main = aocMain 19 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Blueprint 1:\n  Each ore robot costs 4 ore.\n  Each clay robot costs 2 ore.\n  Each obsidian robot costs 3 ore and 14 clay.\n  Each geode robot costs 2 ore and 7 obsidian.\n\nBlueprint 2:\n  Each ore robot costs 2 ore.\n  Each clay robot costs 3 ore.\n  Each obsidian robot costs 3 ore and 8 clay.\n  Each geode robot costs 3 ore and 12 obsidian."
