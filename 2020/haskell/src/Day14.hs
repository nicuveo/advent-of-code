-- import

import           Control.Monad
import           Data.Bits
import           Data.Function      (on)
import qualified Data.IntMap.Strict as M
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

data Instruction
  = Mask String
  | MemSet Int Int
  deriving (Show, Eq)

type Input = [Instruction]

parseInput :: String -> Input
parseInput = parseLinesWith line
  where line = mask <|> memset
        mask = do
          try $ symbol "mask"
          symbol "="
          fmap Mask $ many1 $ choice [char 'X', char '0', char '1']
        memset = do
          try $ string "mem["
          address <- intLiteral
          symbol "]"
          symbol "="
          value <- intLiteral
          pure $ MemSet address value



-- part1

type Memory = M.IntMap Int
type Mask1  = Int -> Int

part1 :: Input -> Int
part1 = sum . M.elems . fst . foldl' step1 (M.empty, id)

step1 :: (Memory, Mask1) -> Instruction -> (Memory, Mask1)
step1 (mem, _) (Mask m) = (mem, mkMask1 m)
step1 (mem, m) (MemSet addr value) =
  ( M.insert addr (m value) mem
  , m
  )

mkMask1 :: String -> Mask1
mkMask1 mask = appEndo $ mconcat $ do
  (i, c) <- zip [0..] $ reverse mask
  guard $ c /= 'X'
  pure $ Endo $ case c of
    '0' -> flip clearBit i
    '1' -> flip setBit   i
    _   -> error "should never happen"



-- part2

type Mask2 = Int -> [Int]

part2 :: Input -> Int
part2 = sum . M.elems . fst . foldl' step2 (M.empty, pure)

step2 :: (Memory, Mask2) -> Instruction -> (Memory, Mask2)
step2 (mem, _)    (Mask m) = (mem, mkMask2 m)
step2 (mem, mask) (MemSet addr value) =
  ( foldl' (\m a -> M.insert a value m) mem $ mask addr
  , mask
  )

mkMask2 :: String -> Mask2
mkMask2 mask original = go $ zip [0..] $ reverse mask
  where go [] = [0]
        go ((i,c) : m) =
          let addresses  = go m
              modified   = [setBit addr i | addr <- addresses]
              currentBit = testBit original i
          in  case (c, currentBit) of
            ('1', _)     -> modified
            ('0', True)  -> modified
            ('0', False) -> addresses
            ('X', _)     -> addresses ++ modified
            _            -> error "should never happen"



-- main

main :: IO ()
main = aocMain 14 $ \rawData -> do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

example1, example2 :: String
example1 = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
example2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"
