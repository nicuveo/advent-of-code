{-# LANGUAGE OverloadedLists #-}

-- import

import           Control.Monad
import           Data.Char        (digitToInt)
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           GHC.Exts
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Input = [SnailfishNumber]

data SnailfishNumber
  = Leaf Int
  | Node SnailfishNumber SnailfishNumber

instance Show SnailfishNumber where
  show (Leaf x)   = show x
  show (Node l r) = "[" ++ show l ++ "," ++ show r ++ "]"

parseInput :: String -> Input
parseInput = parseLinesWith snailfishNumber
  where
    snailfishNumber = pair <|> leaf
    pair = do
      char '['
      l <- snailfishNumber
      char ','
      r <- snailfishNumber
      char ']'
      pure $ Node l r
    leaf = do
      n <- digitToInt <$> digit
      pure $ Leaf n


-- solution

explode :: SnailfishNumber -> SnailfishNumber
explode sn = let (_, _, result) = go 0 Nothing sn in result
  where
    getLeaf (Leaf n) = n
    getLeaf _        = error "encountered node where leaf expected"

    adjustRightMost x = \case
      Leaf n   -> Leaf (n+x)
      Node l r -> Node l $ adjustRightMost x r

    go :: Int -> Maybe Int -> SnailfishNumber -> (Maybe Int, Maybe Int, SnailfishNumber)
    go _ accum (Leaf n) = case accum of
      Nothing -> (Nothing, Nothing, Leaf n)
      Just a  -> (Nothing, Nothing, Leaf (n + a))
    go depth accum (Node l r)
      | depth < 4 =
        let
          (lv1, rv1, newL) = go (depth+1) accum l
          (lv2, rv2, newR) = go (depth+1) rv1   r
          actualNewL = case lv2 of
            Nothing -> newL
            Just x  -> adjustRightMost x newL
        in
          (lv1, rv2, Node actualNewL newR)
      | otherwise =
        let
          vl = getLeaf l + fromMaybe 0 accum
          vr = getLeaf r
        in
          (Just vl, Just vr, Leaf 0)

split :: SnailfishNumber -> Maybe SnailfishNumber
split sn =
  let
    (splitHappened, result) = go sn
  in
    if splitHappened
    then Just result
    else Nothing
  where
    go :: SnailfishNumber -> (Bool, SnailfishNumber)
    go = \case
      Leaf n
        | n >= 10   -> (True, Node (Leaf $ n `div` 2) (Leaf $ succ n `div` 2))
        | otherwise -> (False, Leaf n)
      Node l r ->
        let
          (splitL, newL) = go l
          (splitR, newR) = go r
        in
          if splitL
            then (True,   Node newL r)
            else (splitR, Node newL newR)

reduce :: SnailfishNumber -> SnailfishNumber
reduce (explode -> n) = case split n of
  Nothing -> n
  Just n' -> reduce n'

joinNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
joinNumbers = reduce ... Node

magnitude :: SnailfishNumber -> Int
magnitude = \case
  Leaf n   -> n
  Node l r -> 3 * magnitude l + 2 * magnitude r

part1 :: Input -> Int
part1 = magnitude . foldl1 joinNumbers

part2 :: Input -> Int
part2 numbers = maximum $ do
  (n1:rest) <- tails numbers
  n2 <- rest
  [magnitude $ joinNumbers n1 n2, magnitude $ joinNumbers n2 n1]


-- debug horribleness do not use cursed bad
-- THIS IS NOT A PLACE OF HONOUR

instance Num SnailfishNumber where
  (+) = joinNumbers
  fromInteger n = Leaf $ fromInteger n

instance IsList SnailfishNumber where
  type Item SnailfishNumber = SnailfishNumber
  fromList [x,y] = Node x y


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
