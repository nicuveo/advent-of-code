-- import

import           Control.Monad
import           Data.Bits        (testBit)
import           Data.Bool
import           Data.Char
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC.Parsing
import           AOC.Runtime


-- AST

type Version = Int

data Packet = Packet Version Expression
  deriving Show

data Expression
  = Value       Int
  | Sum         [Packet]
  | Product     [Packet]
  | Minimum     [Packet]
  | Maximum     [Packet]
  | GreaterThan Packet Packet
  | LessThan    Packet Packet
  | Equal       Packet Packet
  deriving Show


-- parsers

type BITSParser = Parsec String Int

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' step 0
  where
    step accum isOn = 2 * accum + fromEnum isOn

bit :: BITSParser Bool
bit = do
  r <- (False <$ zero) <|> (True <$ one)
  modifyState succ
  pure r

zero :: BITSParser ()
zero = lexeme $ void $ char '0'

one :: BITSParser ()
one = lexeme $ void $ char '1'

intOfSize :: Int -> BITSParser Int
intOfSize n = bitsToInt <$> count n bit

packet :: BITSParser Packet
packet = do
  version <- intOfSize 3
  typeId  <- intOfSize 3
  Packet version <$> case typeId of
    4 -> Value   <$> value
    0 -> Sum     <$> operatorArguments
    1 -> Product <$> operatorArguments
    2 -> Minimum <$> operatorArguments
    3 -> Maximum <$> operatorArguments
    5 -> binaryOperator GreaterThan
    6 -> binaryOperator LessThan
    7 -> binaryOperator Equal
    _ -> fail $ "unrecognized operator type: " ++ show typeId
  where
    binaryOperator c = do
      args <- operatorArguments
      case args of
        [a1, a2] -> pure $ c a1 a2
        _        -> fail $ "expected two arguments, got: " ++ show (length args)

value :: BITSParser Int
value = bitsToInt <$> go
  where
    go :: BITSParser [Bool]
    go = do
      (shouldContinue:bits) <- count 5 bit
      if shouldContinue
        then do
          restOfTheBits <- go
          pure $ bits ++ restOfTheBits
        else
          pure bits

operatorArguments :: BITSParser [Packet]
operatorArguments = do
  lengthIndicator <- bit
  if lengthIndicator
    then subPacketsByCount
    else subPacketsByLength

subPacketsByLength :: BITSParser [Packet]
subPacketsByLength = do
  subPacketsLength <- intOfSize 15
  go subPacketsLength
  where
    go n
      | n < 0     = fail "packet size mismatch"
      | n == 0    = pure []
      | otherwise = do
          bitsBefore <- getState
          p <- packet
          bitsAfter <- getState
          remainingPackets <- go (n - bitsAfter + bitsBefore)
          pure $ p : remainingPackets

subPacketsByCount :: BITSParser [Packet]
subPacketsByCount = do
  packetsCount <- intOfSize 11
  count packetsCount packet


-- input parsing

transformInput :: String -> String
transformInput = unlines . map \x -> map (toChr . testBit (digitToInt x)) [3,2,1,0]
  where
    toChr = bool '0' '1'

parseInput :: String -> Packet
parseInput =
  either (error . show) id .
  runParser (packet <* padding) 0 "" .
  transformInput
  where
    padding = many zero <* eof


-- solution

part1 :: Packet -> Int
part1 = go
  where
    go (Packet version expr) = case expr of
      Value _           -> version
      Sum         ps    -> version + sum (map go ps)
      Product     ps    -> version + sum (map go ps)
      Minimum     ps    -> version + sum (map go ps)
      Maximum     ps    -> version + sum (map go ps)
      GreaterThan p1 p2 -> version + go p1 + go p2
      LessThan    p1 p2 -> version + go p1 + go p2
      Equal       p1 p2 -> version + go p1 + go p2

part2 :: Packet -> Int
part2 = eval
  where
    eval (Packet _ expr) = case expr of
      Value x           -> x
      Sum         ps    -> sum     $ map eval ps
      Product     ps    -> product $ map eval ps
      Minimum     ps    -> minimum $ map eval ps
      Maximum     ps    -> maximum $ map eval ps
      GreaterThan p1 p2 -> fromEnum $ eval p1 >  eval p2
      LessThan    p1 p2 -> fromEnum $ eval p1 <  eval p2
      Equal       p1 p2 -> fromEnum $ eval p1 == eval p2


-- main

main :: IO ()
main = aocMain 16 $ \rawData -> do
  let realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 $ parseInput "A0016C880162017C3686B18A3D4780"
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 $ parseInput "9C0141080250320F1802104A08"
  print $ part2 realInput
