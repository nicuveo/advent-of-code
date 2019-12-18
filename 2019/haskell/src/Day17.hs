-- import

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Ratio
import qualified Data.Set            as S
import qualified Data.Vector.Unboxed as V

import           AOC
import           AOC.Debug.Animate
import           AOC.Debug.Color
import           AOC.Map.Flat
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- robozzle

data FunctionName = A | B | C deriving (Show, Eq)
data Instruction  = Forward Int
                  | TurnLeft
                  | TurnRight
                  deriving (Show, Eq)

inline :: [FunctionName]
       -> [Instruction]
       -> [Instruction]
       -> [Instruction]
       -> [Instruction]
inline mainProg a b c = mainProg >>= expand
  where expand A = a
        expand B = b
        expand C = c

compile :: [FunctionName]
        -> [Instruction]
        -> [Instruction]
        -> [Instruction]
        -> [Int]
compile pm pa pb pc =
  intercalate [10] [ uc $ map tn pm
                   , uc $ map ti pa
                   , uc $ map ti pb
                   , uc $ map ti pc
                   ]
  where uc = intercalate [44]
        tn A = ord <$> "A"
        tn B = ord <$> "B"
        tn C = ord <$> "C"
        ti (Forward x) = ord <$> show x
        ti TurnLeft    = ord <$> "L"
        ti TurnRight   = ord <$> "R"

data ShipState = ShipState { direction    :: Direction
                           , position     :: Point
                           , instructions :: [Instruction]
                           , maze         :: FlatMap Char
                           , pointsSeen   :: S.Set Point
                           }


stepOnce :: ShipState -> ShipState
stepOnce s = s { position = position s + directionVector (direction s) }

dropInstruction :: ShipState -> ShipState
dropInstruction s = s { instructions = drop 1 $ instructions s}

appendInstruction :: Instruction -> ShipState -> ShipState
appendInstruction i s = s { instructions = i : instructions s}

recordSeen :: ShipState -> ShipState
recordSeen s = s { pointsSeen = S.insert (position s) $ pointsSeen s }

execute :: Instruction -> ShipState -> ShipState
execute (Forward n) s
  | n > 1 = appendInstruction (Forward $ n-1) $ dropInstruction $ stepOnce s
  | otherwise = dropInstruction $ stepOnce s
execute TurnLeft  s = dropInstruction $ s {direction  = rotateL (direction s)}
execute TurnRight s = dropInstruction $ s {direction  = rotateR (direction s)}

step :: ShipState -> Maybe ShipState
step s = justIfValid $ recordSeen $ execute current s
  where current = head $ instructions s
        justIfValid s'
          | testValid s' = Just s'
          | otherwise    = Nothing

testValid :: ShipState -> Bool
testValid s = not (null $ instructions s) && maze s ! position s `elem` "#^"


animationStep :: ShipState -> Maybe (Logs, ShipState)
animationStep s = step s >>= \s' -> Just ([], s')

animationRender :: ShipState -> String
animationRender s = unlines [ displayWith d (maze s)
                            , show (instructions s)
                            ]
  where d p c
          | p == position s = case direction s of
                                   N -> bgColor grey $ fgColor red "^"
                                   E -> bgColor grey $ fgColor red ">"
                                   S -> bgColor grey $ fgColor red "v"
                                   W -> bgColor grey $ fgColor red "<"
                                   _ -> error "ship can't go diagonally"
          | p `S.member` pointsSeen s = fgColor green [c]
          | otherwise = [c]
        grey = interpolate (2%3) white black



-- main

main :: IO ()
main = aocMain 17 $ \rawInput -> do
  let program = parseInput rawInput
      mazeStr = map chr $ init $ snd $ run program []
      mazeMap = from2DList $ lines mazeStr

  -- part 1
  let intersections = [ p
                      | p <- allPoints mazeMap
                      , let n = fourMapNeighboursOf mazeMap p
                      , n == "####"
                      , mazeMap ! p == '#'
                      ]
  putStr "Part1: "
  print $ sum [py p * px p | p <- intersections]

  -- part 2
  let pm = [A,B,A,C,A,C,B,C,C,B]
      pa = [ TurnLeft,  Forward  4
           , TurnLeft,  Forward  4
           , TurnLeft,  Forward 10
           , TurnRight, Forward  4
           ]
      pb = [ TurnRight, Forward  4
           , TurnLeft,  Forward  4
           , TurnLeft,  Forward  4
           , TurnRight, Forward  8
           , TurnRight, Forward 10
           ]
      pc = [ TurnRight, Forward  4
           , TurnLeft,  Forward 10
           , TurnRight, Forward 10
           ]
      p  = compile pm pa pb pc ++ [10, ord 'n', 10]

  putStr "Part2: "
  print $ last $ snd $ run (program V.// [(0,2)] ) p


  -- animation
  {-
  let fs = ShipState N shipStart (inline pm pa pb pc) maze S.empty
      shipStart = head [p | p <- allPoints mazeMap, mazeMap ! p == '^']

  clearScreen
  animate 200 clearAndReset animationRender animationStep ([], fs)
  -}
