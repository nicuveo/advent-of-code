{-# LANGUAGE BangPatterns #-}



-- module

module Day12 (day12_1, day12_2, runTests) where



-- import

import           Control.Monad
import           Data.Char
import qualified Data.Vector                 as BV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UV (write)
import           Text.Parsec                 hiding (State)

import           Test.Tasty                  as Test
import           Test.Tasty.QuickCheck

import           Common



-- solution

day12_1 :: Solution
day12_1 input     = "a: " ++ show (get 0 regs)
  where (_, regs) = exec (parseInput input) baseState


day12_2 :: Solution
day12_2 input    = "a: " ++ show (get 0 rs)
  where (_, rs)  = exec (parseInput input) newState
        newState = let (pc, regs) = baseState in (pc, regs UV.// [(2, 1)])


-- program

type Program     = BV.Vector Instruction
type Registers   = UV.Vector Int
type Name        = Int

data Value       = Literal  Int
                 | Variable Name

data Instruction = Inc Name
                 | Dec Name
                 | Cpy Value Name
                 | Jnz Value Int

type State       = (Int, Registers)




-- eval

get :: Name -> Registers -> Int
get = flip (UV.!)

set :: Name -> Int -> Registers -> Registers
set reg val = UV.modify $ \v -> UV.write v reg val

update :: Name -> (Int -> Int) -> Registers -> Registers
update reg transform regs = set reg (transform current) regs
  where current = get reg regs

eval :: Value -> Registers -> Int
eval (Literal  x) _ = x
eval (Variable n) r = get n r


baseState :: State
baseState = (0, UV.replicate 10 0)


isValid :: Program -> State -> Bool
isValid p (pc, _) = pc >= 0 && pc < BV.length p

step :: Program -> State -> State
step prog state@(!pc, !regs)
  | isValid prog state = case prog BV.! pc of
      Inc n   -> (pc+1, update n succ regs)
      Dec n   -> (pc+1, update n pred regs)
      Cpy v n -> (pc+1, set n (eval v regs) regs)
      Jnz v o -> if eval v regs == 0
                 then (pc+1, regs)
                 else (pc+o, regs)
  | otherwise   = error "step: out of bounds"

exec :: Program -> State -> State
exec prog origin = head $ dropWhile (isValid prog) $ iterate (step prog) origin



-- parsing

parseInput :: String -> Program
parseInput = BV.fromList . parseWith program
  where program     = instruction `sepBy1` many1 newline
        instruction = tryAll [inc, dec, cpy, jnz]
        value       = tryAll [literal, variable]
        literal     = Literal  <$> intParser
        variable    = Variable <$> name
        name        = do
          spaces
          reg <- lower
          when (reg > 'j') $ fail "parsing: unexpected register"
          return $ ord reg - 97
        inc         = do
          symbol "inc"
          reg <- name
          return $ Inc reg
        dec         = do
          symbol "dec"
          reg <- name
          return $ Dec reg
        cpy         = do
          symbol "cpy"
          val <- value
          reg <- name
          return $ Cpy val reg
        jnz         = do
          symbol "jnz"
          val <- value
          dif <- intParser
          return $ Jnz val dif



-- tests

newtype TName = TN Name      deriving (Show, Eq)
newtype TRegs = TR Registers deriving (Show, Eq)

instance Arbitrary TName where
  arbitrary = TN <$> elements [0..9]

instance Arbitrary TRegs where
  arbitrary = TR . UV.fromList <$> vectorOf 10 arbitrary


testGetAndSet :: TestTree
testGetAndSet = testGroup "get and set" [ setThenGet
                                        ]
  where setThenGet = testProperty "set then get" setThenGetPred
        setThenGetPred (TR r) (TN n) v = get n (set n v r) == v

runTests :: IO ()
runTests = Test.defaultMain $ testGroup "Day12" tests
  where tests = [ testGetAndSet
                ]
