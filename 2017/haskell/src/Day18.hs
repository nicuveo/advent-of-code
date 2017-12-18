-- module

module Day18 (day18_1, day18_2) where



-- import

import           Control.Monad
import           Data.List       as L
import           Data.Map.Strict as M
import           Data.Vector     as V
import           Text.Parsec

import           Common



-- solution

day18_1 :: Solution
day18_1 = show . L.head . (received <=< execute1 . parseInstructions)


day18_2 :: Solution
day18_2 = show . totalSent . (V.! 1) . execute2 . parseInstructions



-- helper types

type Name        = Char
type Registers   = Map Name Int
type Argument    = Either Name Int

data Status      = Status { programCounter :: Int
                          , registers      :: Registers
                          , received       :: [Int]
                          , lastSent       :: Int
                          , totalSent      :: Int
                          } deriving Show

data Instruction = Rcv Name
                 | Snd Argument
                 | Set Name Argument
                 | Add Name Argument
                 | Mul Name Argument
                 | Mod Name Argument
                 | Jgz Argument Argument
                 deriving Show



-- basic operations on those types

initialStatus :: Status
initialStatus = Status 0 M.empty [] 0 0

getR :: Name -> Status -> Int
getR n s = findWithDefault 0 n $ registers s

setR :: (Int -> Int) -> Name -> Status -> Status
setR f n s = s { registers = M.insert n (f $ getR n s) $ registers s }



-- part 1: execute one (sound)

step1 :: Vector Instruction -> Status -> Status
step1 insts status =
  case insts V.! programCounter status of
    Rcv n   -> pc 1 $ if getR n status /= 0
                      then status { received = lastSent status : received status }
                      else status
    Snd a   -> pc 1 $ status { lastSent = getArg a }
    Set n a -> pc 1 $ setR (const $ getArg a) n status
    Add n a -> pc 1 $ setR ((+)   $ getArg a) n status
    Mul n a -> pc 1 $ setR ((*)   $ getArg a) n status
    Mod n a -> pc 1 $ setR (`mod`   getArg a) n status
    Jgz a j -> pc (if getArg a > 0 then getArg j else 1) status
  where pc x s = s { programCounter = programCounter s + x }
        getArg = either (`getR` status) id

execute1 :: Vector Instruction -> [Status]
execute1 insts = L.iterate (step1 insts) initialStatus



-- part 2: execute two (duet)

isBlocked :: Vector Instruction -> Status -> Bool
isBlocked insts status =
  case insts !? programCounter status of
    Nothing      -> True
    Just (Rcv _) -> L.null $ received status
    _            -> False

step2 :: Vector Instruction -> Int -> Vector Status -> Vector Status
step2 insts i statuses = statuses // [(i, ncs), (1-i, nos)]
  where pc x s = s { programCounter = programCounter s + x }
        getArg = either (`getR` cs) id
        cs = statuses V.! i
        os = statuses V.! (1 - i)
        (nos, ncs) = case insts V.! programCounter cs of
          Snd a   -> ( os {received = received os L.++ [getArg a] }
                     , pc 1 $ cs { totalSent = totalSent cs + 1 }
                     )
          Rcv n   -> let (r:rs) = received cs
                     in  (os, pc 1 $ setR (const r) n $ cs { received = rs })
          Set n a -> (os, pc 1 $ setR (const $ getArg a) n cs)
          Add n a -> (os, pc 1 $ setR ((+)   $ getArg a) n cs)
          Mul n a -> (os, pc 1 $ setR ((*)   $ getArg a) n cs)
          Mod n a -> (os, pc 1 $ setR (`mod`   getArg a) n cs)
          Jgz a j -> (os, pc (if getArg a > 0 then getArg j else 1) cs)

execute2 :: Vector Instruction -> Vector Status
execute2 insts = execute_ 0 $ V.fromList [setR (const i) 'p' initialStatus | i <- [0,1]]
  where execute_ i s
          | not $ isBlocked insts $ s V.! i     = execute_ (1-i) $ step2 insts i     s
          | not $ isBlocked insts $ s V.! (1-i) = execute_ i     $ step2 insts (1-i) s
          | otherwise                           = s



-- parsing

parseInstructions :: String -> Vector Instruction
parseInstructions = V.fromList . L.map (parseWith inst) . lines
  where inst = tryAll [rcvi, sndi, seti, addi, muli, modi, jgzi] <?> "instruction"
        name = spaces >> lower
        rcvi = string "rcv" >> Rcv <$> name
        sndi = string "snd" >> Snd <$> arg
        seti = string "set" >> Set <$> name <*> arg
        addi = string "add" >> Add <$> name <*> arg
        muli = string "mul" >> Mul <$> name <*> arg
        modi = string "mod" >> Mod <$> name <*> arg
        jgzi = string "jgz" >> Jgz <$> arg  <*> arg
        arg  = tryAll [Right <$> intParser, Left <$> name] <?> "arg"
