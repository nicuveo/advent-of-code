module Main where


-- import

import Data.HashMap.Lazy (HashMap, (!))
import Data.HashMap.Lazy qualified as M

import Text.Parsec
import Text.Parsec.Char

import AOC


-- input

type Input = HashMap Name Expr

type Name = String
data Expr
  = Value Int
  | Name :+: Name
  | Name :-: Name
  | Name :*: Name
  | Name :/: Name

instance Show Expr where
  show (Value x) = show x
  show (x :+: y) = x ++ " + " ++ y
  show (x :-: y) = x ++ " - " ++ y
  show (x :*: y) = x ++ " * " ++ y
  show (x :/: y) = x ++ " / " ++ y

parseInput :: String -> Input
parseInput = M.fromList . parseLinesWith do
  monkey <- identifier
  symbol ":"
  shout <- expr
  pure (monkey, shout)
  where
    expr = value <|> operation
    value = Value <$> number
    operation = do
      m1 <- identifier
      op <- tryAll
        [ (:+:) <$ symbol "+"
        , (:-:) <$ symbol "-"
        , (:*:) <$ symbol "*"
        , (:/:) <$ symbol "/"
        ]
      m2 <- identifier
      pure $ op m1 m2


-- solution

resolve :: HashMap Name Expr -> HashMap Name Int
resolve exprs = result
  where
    result = flip M.map exprs \case
      Value x   -> x
      n1 :+: n2 -> (result ! n1) + (result ! n2)
      n1 :-: n2 -> (result ! n1) - (result ! n2)
      n1 :*: n2 -> (result ! n1) * (result ! n2)
      n1 :/: n2 -> (result ! n1) `div` (result ! n2)

partialEval :: HashMap Name Expr -> Name -> Either (Int -> Int) Int
partialEval exprs = go
  where
    go "humn" = Left id
    go name   = case exprs ! name of
      Value x   -> Right x
      n1 :+: n2 -> case (go n1, go n2) of
        (Right v1, Right v2) -> Right $ v1 + v2
        (Left  f1, Right v2) -> Left \t -> f1 (t - v2)
        (Right v1, Left  f2) -> Left \t -> f2 (t - v1)
        (Left   _, Left   _) -> error "too many humans"
      n1 :*: n2 -> case (go n1, go n2) of
        (Right v1, Right v2) -> Right $ v1 * v2
        (Left  f1, Right v2) -> Left \t -> f1 (t `div` v2)
        (Right v1, Left  f2) -> Left \t -> f2 (t `div` v1)
        (Left   _, Left   _) -> error "too many humans"
      n1 :-: n2 -> case (go n1, go n2) of
        (Right v1, Right v2) -> Right $ v1 - v2
        (Left  f1, Right v2) -> Left \t -> f1 (v2 + t)
        (Right v1, Left  f2) -> Left \t -> f2 (v1 - t)
        (Left   _, Left   _) -> error "too many humans"
      n1 :/: n2 -> case (go n1, go n2) of
        (Right v1, Right v2) -> Right $ v1 `div` v2
        (Left  f1, Right v2) -> Left \t -> f1 (v2 * t)
        (Right v1, Left  f2) -> Left \t -> f2 (v1 `div` t)
        (Left   _, Left   _) -> error "too many humans"

part1 :: Input -> Int
part1 = (! "root") . resolve

part2 :: Input -> Int
part2 m = case (partialEval m l, partialEval m r) of
  (Left f, Right t) -> f t
  (Right t, Left f) -> f t
  _                 -> error "wrong input"
  where
    (l,r) = case m ! "root" of
      n1 :+: n2 -> (n1, n2)
      n1 :-: n2 -> (n1, n2)
      n1 :*: n2 -> (n1, n2)
      n1 :/: n2 -> (n1, n2)
      _         -> error "wrong input"


-- main

main :: IO ()
main = aocMain 21 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
