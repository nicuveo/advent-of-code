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
compile = either (error . show) id . parse line ""
  where name = nameParser
        val  = intParser
        mul  = tryAll [inc, dec]
        op   = tryAll [eq, ne, le, ge, lt, gt]
        inc  = string "inc" >> return 1
        dec  = string "dec" >> return (-1)
        eq   = string "=="  >> return (==)
        ne   = string "!="  >> return (/=)
        le   = string "<="  >> return (<=)
        ge   = string ">="  >> return (>=)
        lt   = string "<"   >> return (<)
        gt   = string ">"   >> return (>)
        line = do
          reg <- name
          spaces
          coeff <- mul
          spaces
          delta <- val
          spaces
          string "if"
          spaces
          comp <- name
          spaces
          operator <- op
          spaces
          value <- val
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
