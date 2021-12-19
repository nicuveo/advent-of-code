-- import

import           Control.Monad
import           Data.Char        (digitToInt)
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Input = [SnailfishNumber]

type SnailfishNumber = [Node]

data Step = L | R deriving Show
type Path = [Step]

type Value = Int
type Depth = Int
data Node  = Node
  { nodeValue :: Value
  , nodePath  :: Path
  , nodeDepth :: Depth
  }

instance Show Node where
  show Node{..} = "{" ++ show nodePath ++ ":" ++ show nodeValue ++ "}"

parseInput :: String -> Input
parseInput = parseLinesWith (snailfishNumber 0 [])
  where
    snailfishNumber d p = pair d p <|> leaf d p
    pair d p = do
      char '['
      l <- snailfishNumber (d+1) (L:p)
      char ','
      r <- snailfishNumber (d+1) (R:p)
      char ']'
      pure $ l ++ r
    leaf d p = do
      n <- digitToInt <$> digit
      pure [Node n p d]


-- solution

tooDeep :: Node -> Bool
tooDeep = (>4) . nodeDepth

addValue :: Node -> Value -> Node
addValue n v = n { nodeValue = nodeValue n + v }

explode :: SnailfishNumber -> SnailfishNumber
explode = \case
  (v1:v2:v3:rest)
    -- left-most explosion
    | tooDeep v1 && tooDeep v2 ->
      let
        newV12 = Node 0 (tail $ nodePath v1) 4
        newV3  = addValue v3 (nodeValue v2)
      in
        explode $ newV12:newV3:rest
  (v1:v2:v3:v4:rest)
    -- normal explosion
    | tooDeep v2 && tooDeep v3 ->
      let
        newV1  = addValue v1 (nodeValue v2)
        newV23 = Node 0 (tail $ nodePath v2) 4
        newV4  = addValue v4 (nodeValue v3)
      in
        explode $ newV1:newV23:newV4:rest
  [v1,v2,v3]
    -- right-most explosion
    | tooDeep v2 && tooDeep v3 ->
      let
        newV1  = addValue v1 (nodeValue v2)
        newV23 = Node 0 (tail $ nodePath v2) 4
      in
        [newV1,newV23]
  -- no explosion
  (x:rest) -> x : explode rest
  []       -> []

split :: SnailfishNumber -> Maybe SnailfishNumber
split = \case
  [] -> Nothing
  (n@Node{..}:rest)
    | nodeValue < 10 -> (n:) <$> split rest
    | otherwise      ->
      let
        newL = n
          { nodeValue = nodeValue `div` 2
          , nodeDepth = nodeDepth + 1
          , nodePath  = L : nodePath
          }
        newR = n
          { nodeValue = (nodeValue + 1) `div` 2
          , nodeDepth = nodeDepth + 1
          , nodePath  = R : nodePath
          }
      in
        Just $ newL : newR : rest

reduce :: SnailfishNumber -> SnailfishNumber
reduce (explode -> n) = case split n of
  Nothing -> n
  Just n' -> reduce n'

joinNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
joinNumbers n1 n2 = reduce $ map (incDepth L) n1 ++ map (incDepth R) n2
  where
    incDepth s n@Node{..} = n
      { nodeDepth = nodeDepth + 1
      , nodePath  = nodePath ++ [s]
      }

magnitude :: SnailfishNumber -> Int
magnitude = sum . map nodeMagnitude
  where
    nodeMagnitude (Node v p _) = product $ v : map toCoefficient p
    toCoefficient = \case
      L -> 3
      R -> 2

part1 :: Input -> Int
part1 = magnitude . foldl1 joinNumbers

part2 :: Input -> Int
part2 numbers = maximum $ do
  (n1:rest) <- tails numbers
  n2 <- rest
  [magnitude $ joinNumbers n1 n2, magnitude $ joinNumbers n2 n1]


-- main

main :: IO ()
main = aocMain 18 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
