module Day08 where


-- import

import AOC
import "this" Prelude

import Data.Hashable
import Data.HashMap.Strict qualified as M
import Data.IntSet         qualified as S
import Data.List           (foldl1', isSuffixOf, nub)
import Text.Parsec


-- input

type Input   = ([Instruction], Network)
type Network = HashMap (String, Instruction) String

data Instruction = L | R
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Hashable Instruction where
  hashWithSalt salt = hashWithSalt salt . fromEnum

parseInput :: String -> Input
parseInput = parseWith do
  instructions <- many1 instruction
  spaces
  nodes <- many1 nodeInfo
  pure (instructions, M.fromList $ concat nodes)
  where
    instruction = choice
      [ L <$ char 'L'
      , R <$ char 'R'
      ]
    name = many1 alphaNum
    nodeInfo = do
      nodeName <- name
      spaces
      symbol "="
      symbol "("
      leftName <- name
      symbol ","
      rightName <- name
      symbol ")"
      pure
        [ ((nodeName, L), leftName)
        , ((nodeName, R), rightName)
        ]


-- solution

step :: Network -> String -> Instruction -> String
step network node instruction =
  case M.lookup (node, instruction) network of
    Just n  -> n
    Nothing -> error $ "path not found: " ++ show (node, instruction)

part1 :: Input -> Int
part1 (instructions, network) = go 0 "AAA" $ cycle instructions
  where
    go !steps "ZZZ" _     = steps
    go !steps node (i:is) = go (steps+1) (step network node i) is
    go _      _    []     = error "empty list of instructions"


findLoop :: Input -> String -> (Int, Int, [Int])
findLoop (instructions, network) start = go mempty 0 start $ cycle $ zip [0..] instructions
  where
    go
      :: HashMap (String, Int) Int
      -> Int
      -> String
      -> [(Int, Instruction)]
      -> (Int, Int, [Int])
    go _ _ _ [] = error "empty list of instructions"
    go !cache !stepCount node ((index,i):is) = case M.lookup (node, index) cache of
      Nothing       -> go
        (M.insert (node, index) stepCount cache)
        (stepCount + 1)
        (step network node i)
        is
      Just loopStep ->
        let zs = findZs node 0 (stepCount - loopStep) ((index,i):is)
            smallestZ = head zs
        in  (loopStep + smallestZ, stepCount - loopStep, map (subtract smallestZ) zs)
    findZs _ _ _ [] = error "empty list of instructions"
    findZs node !stepCount duration ((_,i):is)
      | stepCount == duration = []
      | "Z" `isSuffixOf` node =
        stepCount : findZs (step network node i) (stepCount+1) duration is
      | otherwise =
        findZs (step network node i) (stepCount+1) duration is

normalize :: [(Int, Int, [Int])] -> (Int, [(Int, S.IntSet)])
normalize loops = (maxStartingPoint,) do
  (startingPoint, loopSize, zs) <- loops
  let index = mod (maxStartingPoint - startingPoint) loopSize
      newZs = [mod (z - index) loopSize | z <- zs]
  pure (loopSize, S.fromList newZs)
  where
    maxStartingPoint = maximum [sp | (sp, _, _) <- loops]

part2 :: Input -> Int
part2 input@(_, network) = foldl1' lcm $ map fst normalizedLoops
  where
    startingPoints = filter ("A" `isSuffixOf`) $ nub $ map fst $ M.keys network
    loopsInfo = map (findLoop input) startingPoints
    (_, normalizedLoops) = normalize loopsInfo



-- main

example :: String
example = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput
