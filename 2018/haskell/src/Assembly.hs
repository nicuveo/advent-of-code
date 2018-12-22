{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Assembly where



-- imports

import           Control.Monad.State
import           Data.Bits
import qualified Data.Map.Strict             as M
import qualified Data.Vector.Unboxed         as V (thaw)
import qualified Data.Vector.Unboxed.Mutable as V

import           Common



-- registers

class Register r where
  regR :: Int -> r -> Int
  regW :: Int -> Int -> r -> r

class Monad m => RegisterM r m where
  regRM :: Int -> r -> m Int
  regWM :: Int -> Int -> r -> m r


type Instruction  r   = Int -> Int -> Int -> r ->   r
type InstructionM r m = Int -> Int -> Int -> r -> m r



-- register list

instance Register [Int] where
  regR i l   = l !! i
  regW i v l = [if j /= i then x else v | (j,x) <- zip [0..] l]



-- instructions helpers

rrInst, riInst, irInst :: Register r => (Int -> Int -> Int) -> Instruction r
rrInst (<#>) a b c r = regW c (regR a r <#> regR b r) r
riInst (<#>) a b c r = regW c (regR a r <#>      b  ) r
irInst (<#>) a b c r = regW c (     a   <#> regR b r) r

rrInstM, riInstM, irInstM :: RegisterM r m => (Int -> Int -> Int) -> InstructionM r m
rrInstM (<#>) a b c r = do { x <- regRM a r; y <- regRM b r; regWM c (x <#> y) r }
riInstM (<#>) a b c r = do { x <- regRM a r;                 regWM c (x <#> b) r }
irInstM (<#>) a b c r = do {                 y <- regRM b r; regWM c (a <#> y) r }



-- instructions

instructions :: Register r => [Instruction r]
instructions = [ addr, addi
               , mulr, muli
               , banr, bani
               , borr, bori
               , setr, seti
               , gtir, gtri, gtrr
               , eqir, eqri, eqrr
               ]

instructionsNames :: Register r => M.Map String (Instruction r)
instructionsNames = M.fromList [ ("addr", addr), ("addi", addi)
                               , ("mulr", mulr), ("muli", muli)
                               , ("banr", banr), ("bani", bani)
                               , ("borr", borr), ("bori", bori)
                               , ("setr", setr), ("seti", seti)
                               , ("gtir", gtir), ("gtri", gtri), ("gtrr", gtrr)
                               , ("eqir", eqir), ("eqri", eqri), ("eqrr", eqrr)
                               ]

addr, addi :: Register r => Instruction r
mulr, muli :: Register r => Instruction r
banr, bani :: Register r => Instruction r
borr, bori :: Register r => Instruction r
setr, seti :: Register r => Instruction r
addrM, addiM :: RegisterM r m => InstructionM r m
mulrM, muliM :: RegisterM r m => InstructionM r m
banrM, baniM :: RegisterM r m => InstructionM r m
borrM, boriM :: RegisterM r m => InstructionM r m
setrM, setiM :: RegisterM r m => InstructionM r m
addr  = rrInst  (+)
addi  = riInst  (+)
addrM = rrInstM (+)
addiM = riInstM (+)
mulr  = rrInst  (*)
muli  = riInst  (*)
mulrM = rrInstM (*)
muliM = riInstM (*)
banr  = rrInst  (.&.)
bani  = riInst  (.&.)
banrM = rrInstM (.&.)
baniM = riInstM (.&.)
borr  = rrInst  (.|.)
bori  = riInst  (.|.)
borrM = rrInstM (.|.)
boriM = riInstM (.|.)
setr  = rrInst  const
seti  = irInst  const
setrM = rrInstM const
setiM = irInstM const

gtir, gtri, gtrr :: Register r => Instruction r
eqir, eqri, eqrr :: Register r => Instruction r
gtirM, gtriM, gtrrM :: RegisterM r m => InstructionM r m
eqirM, eqriM, eqrrM :: RegisterM r m => InstructionM r m
gtir  = irInst  $ fromEnum ... (>)
gtirM = irInstM $ fromEnum ... (>)
gtri  = riInst  $ fromEnum ... (>)
gtriM = riInstM $ fromEnum ... (>)
gtrr  = rrInst  $ fromEnum ... (>)
gtrrM = rrInstM $ fromEnum ... (>)
eqir  = irInst  $ fromEnum ... (==)
eqirM = irInstM $ fromEnum ... (==)
eqri  = riInst  $ fromEnum ... (==)
eqriM = riInstM $ fromEnum ... (==)
eqrr  = rrInst  $ fromEnum ... (==)
eqrrM = rrInstM $ fromEnum ... (==)
