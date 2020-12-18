{-# LANGUAGE DeriveFunctor #-}

-- import

import           Control.Monad
import           Data.Function      (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Printf

import           AOC



-- AST

data Expr a = Value a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            deriving (Functor)

instance Show a => Show (Expr a) where
  show (Value a) = show a
  show (Add l r) = printf "(%s + %s)" (show l) (show r)
  show (Mul l r) = printf "(%s * %s)" (show l) (show r)


eval :: Num a => Expr a -> a
eval (Value x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r



-- input

exprP1 :: Parser (Expr Int)
exprP1 = chainl1 term binOp
  where term  = value <|> parens exprP1
        value = Value <$> intLiteral
        binOp = addOp <|> mulOp
        addOp = Add <$ symbol "+"
        mulOp = Mul <$ symbol "*"

evalP1 :: Parser Int
evalP1 = chainl1 term binOp
  where term  = value <|> parens evalP1
        value = intLiteral
        binOp = addOp <|> mulOp
        addOp = (+) <$ symbol "+"
        mulOp = (*) <$ symbol "*"

exprP2 :: Parser (Expr Int)
exprP2 = chainl1 factor mulOp
  where factor = chainl1 term addOp
        term   = value <|> parens exprP2
        value  = Value <$> intLiteral
        addOp  = Add <$ symbol "+"
        mulOp  = Mul <$ symbol "*"

parseInput1 :: String -> [Expr Int]
parseInput1 = parseLinesWith exprP1

parseInput2 :: String -> [Expr Int]
parseInput2 = parseLinesWith exprP2



-- solution

evalSum :: [Expr Int] -> Int
evalSum = sum . map eval



-- main

main :: IO ()
main = aocMain 18 $ \rawData -> do
  let testInput1 = parseInput1 example
      realInput1 = parseInput1 rawData
      testInput2 = parseInput2 example
      realInput2 = parseInput2 rawData
  putStrLn "# Part 1"
  print $ eval <$> testInput1
  print $ parseLinesWith evalP1 example
  print $ evalSum realInput1
  putStrLn "# Part 2"
  print $ eval <$> testInput2
  print $ evalSum realInput2

example :: String
example = "2 * 3 + (4 * 5)\n5 + (8 * 3 + 9 + 3 * 4 * 3)\n5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
