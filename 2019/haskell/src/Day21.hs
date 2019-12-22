{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}


-- import

import           Data.Char
import           Data.List.Split
import qualified Data.Vector.Unboxed as V
import           Text.Printf

import           AOC                 hiding (E)
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- springscript

class Show a => Register a

data IRegister = A | B | C | D | E | F | G | H | I
  deriving Show

data IORegister = T | J
  deriving Show

instance Register IRegister
instance Register IORegister

data Instruction where
  AND :: Register a => a -> IORegister -> Instruction
  NOT :: Register a => a -> IORegister -> Instruction
  OR  :: Register a => a -> IORegister -> Instruction

instance Show Instruction where
  show (AND a b) = printf "AND %s %s" (show a) (show b)
  show (NOT a b) = printf "NOT %s %s" (show a) (show b)
  show (OR  a b) = printf "OR %s %s"  (show a) (show b)

compileW :: [Instruction] -> String
compileW = unlines . (++ ["WALK"]) . map show

compileR :: [Instruction] -> String
compileR = unlines . (++ ["RUN"]) . map show



-- running

jumpAway :: Program -> String -> Either String Int
jumpAway v s = if any (> ord maxBound) output
               then Right $ last output
               else Left  $ chr <$> output
  where output = snd $ run v $ map ord s


walkAndJump :: Program -> [Instruction] -> Either String Int
walkAndJump v i = jumpAway v $ compileW i

runAndJump :: Program -> [Instruction] -> Either String Int
runAndJump v i = jumpAway v $ compileR i




-- solution

script1 = [ OR  A T
          , AND B T
          , AND C T
          , OR  D J
          , NOT T T
          , AND T J
          ]

script2 = [ OR  A T
          , AND B T
          , AND C T
          , NOT T T
          , AND D T
          , OR  E J
          , OR  H J
          , AND T J
          ]



-- main

printResult = either putStrLn print

main :: IO ()
main = aocMain 21 $ \rawInput -> do
  let program  = parseInput rawInput
  printResult $ walkAndJump program script1
  printResult $ runAndJump  program script2
