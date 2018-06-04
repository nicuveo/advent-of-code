-- module

module Day10 (day10_1, day10_2) where



-- import

import           Data.List
import qualified Data.Map           as M
import           Text.Parsec        hiding (State)
import           Text.Parsec.String

import           Common



-- solution

day10_1 :: Solution
day10_1 input = head $ findBot =<< executeAll (readInput input)
  where findBot (b,_) = [bot | (bot, chips) <- M.toList b, sort chips == [17, 61]]


day10_2 :: Solution
day10_2 input = show o
  where (_,o) = last $ executeAll (readInput input)



-- helpers

type Chip = Int
type Name = String

type NameMap   = M.Map Name [Chip]
type BotMap    = NameMap
type OutputMap = NameMap

data Instruction = Input    Name Chip
                 | Transfer Name Target Target
                 deriving (Show, Eq)

data Target = ToBot    Name
            | ToOutput Name
            deriving (Show, Eq)

type State = (BotMap, OutputMap)


executeAll :: [Instruction] -> [State]
executeAll = executeAll_ (M.empty, M.empty)
  where executeAll_ _     []    = []
        executeAll_ world insts =
          if null others
          then error "NO VALID INST!"
          else nextWorld : executeAll_ nextWorld (skipped ++ tail others)
          where (skipped, others) = break (isValid world) insts
                nextWorld = execute world $ head others



-- parsing

targetName :: Parser Name
targetName = spaces >> many1 alphaNum

targetParser :: Parser Target
targetParser = tryAll [botTarget, outTarget]
  where botTarget = fmap ToBot    $ symbol "bot"    >> targetName
        outTarget = fmap ToOutput $ symbol "output" >> targetName

instructionParser :: Parser Instruction
instructionParser = tryAll [input, transfer]
  where input = do
          symbol "value"
          chip <- intParser
          symbol "goes to bot"
          bot <- targetName
          return $ Input bot chip
        transfer = do
          symbol "bot"
          source <- targetName
          symbol "gives low to"
          dest1 <- targetParser
          symbol "and high to"
          dest2 <- targetParser
          return $ Transfer source dest1 dest2

readInput :: String -> [Instruction]
readInput = parseWith $ instructionParser `sepBy` newline

getChips :: Name -> NameMap -> [Chip]
getChips = M.findWithDefault []

isValidTarget :: BotMap -> Target -> Bool
isValidTarget world (ToBot name) = length (getChips name world) < 2
isValidTarget _     _            = True

isValid :: State -> Instruction -> Bool
isValid (b,_) (Input name _) = length (getChips name b) < 2
isValid (b,_) (Transfer source dest1 dest2) =
  length (getChips source b) == 2 &&
  isValidTarget b dest1           &&
  isValidTarget b dest2

execute :: State -> Instruction -> State
execute (b,o) (Input name chip) = (M.insert name (chip:chips) b,o)
  where chips = getChips name b
execute (b,o) (Transfer source dest1 dest2) =
  give c1 dest1 $ give c2 dest2 (M.insert source [] b, o)
  where [c1, c2] = sort $ getChips source b
        give chip (ToOutput name) (bots,outs) =
          (bots, M.insert name (chip:getChips name outs) outs)
        give chip (ToBot name) (bots,outs) =
          (M.insert name (chip:getChips name bots) bots, outs)






--
