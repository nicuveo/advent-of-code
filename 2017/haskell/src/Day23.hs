{-# LANGUAGE BangPatterns #-}



-- module

module Day23 (day23_1, day23_2) where



-- import

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import           Text.Parsec

import           Common



-- solution

day23_1 :: Solution
day23_1 = show . mulCount . execute initialStatus . parseInstructions


day23_2 :: Solution
day23_2 _ = show $ countTrue [not $ isPrime b | b <- [109900, 109917 .. 126900]]
  where
    isPrime n = and [n `mod` d > 0 | d <- [2..n-1]]

--day23_2 = show . getR 'h' . execute (setR (const 1) 'a' initialStatus) . parseInstructions



-- helper types

type Name         = Char
type Registers    = M.Map Name Int
type Argument     = Either Name Int
type Instructions = V.Vector Instruction

data Status       = Status { programCounter :: Int
                           , registers      :: Registers
                           , mulCount       :: Int
                           } deriving Show

data Instruction  = Set Name     Argument
                  | Sub Name     Argument
                  | Mul Name     Argument
                  | Jnz Argument Int
                  deriving Show



-- basic operations on those types

initialStatus :: Status
initialStatus = Status 0 M.empty 0

getR :: Name -> Status -> Int
getR n s = M.findWithDefault 0 n $ registers s

setR :: (Int -> Int) -> Name -> Status -> Status
setR f n s = s { registers = M.insert n (f $ getR n s) $ registers s }



-- execute

outOfBounds :: Instructions -> Status -> Bool
outOfBounds is s = programCounter s < 0 || programCounter s >= V.length is

step :: Instructions -> Status -> Status
step insts status =
  case insts V.! programCounter status of
    Set n a -> pc 1 $ setR (const    $ getArg a) n status
    Sub n a -> pc 1 $ setR (subtract $ getArg a) n status
    Mul n a -> pc 1 $ setR ((*)      $ getArg a) n $ status { mulCount = mulCount status + 1}
    Jnz a j -> pc (if getArg a /= 0 then j else 1) status
  where pc x s = s { programCounter = programCounter s + x }
        getArg = either (`getR` status) id

execute :: Status -> Instructions -> Status
execute !status insts
  | outOfBounds insts status = status
  | otherwise                = execute (step insts status) insts



-- parsing

parseInstructions :: String -> V.Vector Instruction
parseInstructions = V.fromList . map (parseWith inst) . lines
  where inst = do
          i <- tryAll [seti, subi, muli, jnzi] <?> "instruction"
          optional comm
          return i
        comm = symbol "#" >> many anyChar
        name = spaces >> lower
        seti = symbol "set" >> Set <$> name <*> arg
        subi = symbol "sub" >> Sub <$> name <*> arg
        muli = symbol "mul" >> Mul <$> name <*> arg
        jnzi = symbol "jnz" >> Jnz <$> arg  <*> intParser
        arg  = tryAll [Left <$> name, Right <$> intParser] <?> "arg"
