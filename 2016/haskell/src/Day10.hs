-- module

module Day10 (day10_1, day10_2,
              isItStillWorking) where



-- import

import           Data.List
import qualified Data.Map           as M
import           Text.Parsec        hiding (State)
import           Text.Parsec.String

import           Common



-- solution

day10_1 :: Solution
day10_1 input = head $ findBot =<< executeAll (readInput input)
  where findBot world = [bot | (Bot bot, chips) <- M.toList world, sort chips == [17, 61]]


day10_2 :: Solution
day10_2 input = show $ product $ concat [world M.! Out o | o <- ["0", "1", "2"]]
  where world = last $ executeAll (readInput input)



-- helpers

type Chip = Int
type Name = String

type TargetMap   = M.Map Target [Chip]

data Instruction = Input    Name Chip
                 | Transfer Name Target Target
                 deriving (Show, Eq)

data Target = Bot Name
            | Out Name
            deriving (Show, Eq, Ord)

type State = TargetMap


getChips :: Target -> State -> [Chip]
getChips = M.findWithDefault []

getNumberOfChips :: Target -> State -> Int
getNumberOfChips = length ... getChips

insertChip :: Target -> Chip -> State -> State
insertChip target chip = M.alter alt target
  where alt Nothing      = Just [chip]
        alt (Just chips) = Just $ chip:chips

isValidTarget :: State -> Target -> Bool
isValidTarget world (Bot name) = getNumberOfChips (Bot name) world < 2
isValidTarget _     _          = True

isValid :: State -> Instruction -> Bool
isValid world (Input name _) = getNumberOfChips (Bot name) world < 2
isValid world (Transfer source dest1 dest2) =
  isValidTarget world dest1 &&
  isValidTarget world dest2 &&
  getNumberOfChips (Bot source) world == 2

execute :: State -> Instruction -> State
execute world (Input name chip) = insertChip (Bot name) chip world
execute world (Transfer source dest1 dest2) =
  insertChip dest1 c1 $ insertChip dest2 c2 $ M.insert sourceBot [] world
  where sourceBot = Bot source
        [c1, c2] = sort $ getChips sourceBot world

executeAll :: [Instruction] -> [State]
executeAll = executeAll_ M.empty
  where executeAll_ _     []    = []
        executeAll_ world insts = nextWorld : executeAll_ nextWorld (skipped ++ others)
          where (skipped, next : others) = break (isValid world) insts
                nextWorld = execute world next



-- parsing

targetName :: Parser Name
targetName = spaces >> many1 alphaNum

targetParser :: Parser Target
targetParser = tryAll [botTarget, outTarget]
  where botTarget = fmap Bot $ symbol "bot"    >> targetName
        outTarget = fmap Out $ symbol "output" >> targetName

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



-- tests

isItStillWorking :: Bool
isItStillWorking = last (executeAll instructions) == expected
  where instructions = [ Input "1" 1
                       , Transfer "1" (Out "1") (Bot "2")
                       , Input "1" 0
                       ]
        expected = M.fromList [ (Bot "1", [])
                              , (Bot "2", [1])
                              , (Out "1", [0])
                              ]
