{-# LANGUAGE TemplateHaskell #-}

module Day17 where


-- import

import AOC
import "this" Prelude

import Control.Lens             hiding (op)
import Control.Monad.Extra      (whileM)
import Control.Monad.RWS.Strict
import Data.Bits
import Data.Vector.Unboxed      qualified as V
import Text.Parsec              hiding (State)


-- input

type Input     = (Registers, Program)
type Registers = V.Vector Int
type Program   = V.Vector Int

parseInput :: String -> Input
parseInput = parseWith do
  registers <- many1 register
  prog      <- program
  pure (V.fromList registers, V.fromList prog)
  where
    register = do
      symbol "Register"
      many1 alphaNum
      symbol ":"
      number
    program = do
      symbol "Program:"
      number `sepBy` symbol ","


-- part 1

data Register = A | B | C

data MachineState = MachineState
  { _instructionPointer :: Int
  , _registerA          :: Int
  , _registerB          :: Int
  , _registerC          :: Int
  }
  deriving Show

makeLenses ''MachineState

type MachineMonad = RWS Program [Int] MachineState

readRegister :: Register -> MachineMonad Int
readRegister = \case
  A -> use registerA
  B -> use registerB
  C -> use registerC

writeRegister :: Register -> Int -> MachineMonad ()
writeRegister register value = case register of
  A -> registerA .= value
  B -> registerB .= value
  C -> registerC .= value

currentInstruction :: MachineMonad (Maybe (Int, Int))
currentInstruction = do
  ip <- use instructionPointer
  program <- ask
  pure $ liftA2 (,) (program V.!? ip) (program V.!? succ ip)

evaluateComboOperand :: Int -> MachineMonad Int
evaluateComboOperand = \case
    0 -> pure 0
    1 -> pure 1
    2 -> pure 2
    3 -> pure 3
    4 -> readRegister A
    5 -> readRegister B
    6 -> readRegister C
    7 -> error $ "unexpected reserved 7 combo operand"
    x -> error $ "unrecognized combo operand: " ++ show x

step :: MachineMonad Bool
step = currentInstruction >>= \case
  Nothing -> pure False
  Just (opcode, operand) -> do
    case opcode of
      0 -> adv operand
      1 -> bxl operand
      2 -> bst operand
      3 -> jnz operand
      4 -> bxc operand
      5 -> out operand
      6 -> bdv operand
      7 -> cdv operand
      x -> error $ "unrecognized opcode: " ++ show x
    pure True
  where
    adv arg = do
      n <- readRegister A
      d <- evaluateComboOperand arg
      writeRegister A $ n `div` (2 ^ d)
      instructionPointer += 2
    bdv arg = do
      n <- readRegister A
      d <- evaluateComboOperand arg
      writeRegister B $ n `div` (2 ^ d)
      instructionPointer += 2
    cdv arg = do
      n <- readRegister A
      d <- evaluateComboOperand arg
      writeRegister C $ n `div` (2 ^ d)
      instructionPointer += 2
    bxl arg = do
      b <- readRegister B
      writeRegister B $ xor b arg
      instructionPointer += 2
    bxc _ = do
      b <- readRegister B
      c <- readRegister C
      writeRegister B $ xor b c
      instructionPointer += 2
    bst arg = do
      v <- evaluateComboOperand arg
      writeRegister B $ v `mod` 8
      instructionPointer += 2
    out arg = do
      v <- evaluateComboOperand arg
      tell [v `mod` 8]
      instructionPointer += 2
    jnz arg = do
      a <- readRegister A
      if a /= 0
        then instructionPointer .= arg
        else instructionPointer += 2

runProgram :: MachineMonad ()
runProgram = whileM step

evaluateMachine :: Program -> Registers -> MachineMonad a -> (a, MachineState, [Int])
evaluateMachine program registers action = runRWS action program $ MachineState 0 a b c
  where
    a = registers V.! 0
    b = registers V.! 1
    c = registers V.! 2

part1 :: Input -> [Int]
part1 (registers, program) = view _3 $ evaluateMachine program registers runProgram


-- part 2

findA :: (Int -> Int) -> Int -> [Int] -> State (Maybe Int) ()
findA output a = \case
  [] -> modify \case
    Nothing  -> Just a
    Just old -> Just $ min a old
  (x:xs) -> for_ [8*a .. 8*a+7] \candidate -> do
    currentBest <- get
    when (output candidate == x && maybe True (candidate <) currentBest) do
      findA output candidate xs

part2 :: Input -> Maybe Int
part2 (_, V.toList -> program) =
  flip execState Nothing $ findA output 0 $ reverse program
  where
    output :: Int -> Int
    output a = head $ view _3 $ evaluateMachine
      (V.fromList $ init program)
      (V.fromList [a, 0, 0])
      runProgram


-- main

example1 :: String
example1 = "\
\Register A: 729\n\
\Register B: 0\n\
\Register C: 0\n\
\\n\
\Program: 0,1,5,4,3,0"

example2 :: String
example2 = "\
\Register A: 2024\n\
\Register B: 0\n\
\Register C: 0\n\
\\n\
\Program: 0,3,5,4,3,0"

main :: String -> IO ()
main rawData = do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput
