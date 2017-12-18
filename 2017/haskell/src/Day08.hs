-- module

module Day08 (day08_1, day08_2) where



-- import

import           Data.List
import           Data.Map.Strict hiding (foldl', map)
import           Text.Parsec

import           Common



-- solution

day08_1 :: Solution
day08_1 = show . regMax . foldl' (flip execute) empty . map compile . lines

day08_2 :: Solution
day08_2 = show . maximum . map regMax . scanl' (flip execute) empty . map compile . lines



-- helpers

type Registers = Map String Int

data Instruction = Inst { _iReg   :: String
                        , _iDelta :: Int
                        , _iTest  :: String
                        , _iCond  :: Int -> Bool
                        }

compile :: String -> Instruction
compile = parseWith line
  where name = nameParser
        val  = intParser
        mul  = tryAll [inc, dec] <?> "inc or dec"
        op   = tryAll [eq, ne, le, ge, lt, gt] <?> "operator"
        inc  = symbol "inc" >> return 1
        dec  = symbol "dec" >> return (-1)
        eq   = symbol "=="  >> return (==)
        ne   = symbol "!="  >> return (/=)
        le   = symbol "<="  >> return (<=)
        ge   = symbol ">="  >> return (>=)
        lt   = symbol "<"   >> return (<)
        gt   = symbol ">"   >> return (>)
        line = do
          reg      <- name
          coeff    <- mul
          delta    <- val
          symbol "if"
          comp     <- name
          operator <- op
          value    <- val
          return $ Inst reg (coeff * delta) comp (`operator` value)

regMax :: Registers -> Int
regMax = foldl' max 0 . elems

regGet :: String -> Registers -> Int
regGet = findWithDefault 0

regSet :: String -> Int -> Registers -> Registers
regSet = insertWith (+)

execute :: Instruction -> Registers -> Registers
execute (Inst reg delta test cond) regs = if cond $ regGet test regs
                                          then regSet reg delta regs
                                          else regs
