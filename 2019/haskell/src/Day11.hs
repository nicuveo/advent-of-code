{-# LANGUAGE TemplateHaskell #-}


-- import

import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List.Split
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import           Text.Printf

import           AOC
import           AOC.Map.Sparse
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- robot io

data IOState = SendingColor
             | ExpectingColor
             | ExpectingMovement
             deriving (Show, Eq)

data Color = Black
           | White
           deriving (Show, Eq, Enum)

data Robot = Robot { _position  :: Point
                   , _direction :: Vector
                   , _ioState   :: IOState
                   , _colorMap  :: SparseMap Color
                   }
makeLenses ''Robot

type RobotMonad s = StateT Robot (ST s)


fetchState :: RobotMonad s IOState
fetchState = do
  cs <- use ioState
  ioState .= nextState cs
  return cs
  where nextState SendingColor      = ExpectingColor
        nextState ExpectingColor    = ExpectingMovement
        nextState ExpectingMovement = SendingColor


inF :: RobotMonad s (Maybe Int)
inF = do
  cs   <- fetchState
  pos  <- use position
  cmap <- use colorMap
  when (cs /= SendingColor) $ error $
    printf "IO in wrong state: want SendingColor got %s" $ show cs
  return $ Just $ fromEnum $ fromMaybe Black $ cmap !? pos

outF :: Int -> RobotMonad s ()
outF x = do
  cs   <- fetchState
  when (cs == SendingColor) $
    error "IO in wrong state: got SendingColor in output"
  if cs == ExpectingColor
  then do
    pos <- use position
    colorMap %= M.insert pos (toEnum x)
  else do
    direction %= if x == 0 then rotate90L else rotate90R
    newDir <- use direction
    position += newDir


runC :: Color -> RobotMonad s a -> ST s (SparseMap Color)
runC c expr = (^. colorMap) <$> execStateT expr baseState
  where basePoint = Point 0 0
        baseMap   = M.singleton basePoint c
        baseState = Robot basePoint (Point (-1) 0) SendingColor baseMap



-- solution

part1 :: Program -> SparseMap Color
part1 program = runST $ runC Black $ runM program inF outF

part2 :: Program -> SparseMap Color
part2 program = runST $ runC White $ runM program inF outF



-- main

colorToChar :: Point -> Maybe Color -> String
colorToChar _ = toS . fromMaybe Black
  where toS White = "##"
        toS Black = "  "

main :: IO ()
main = aocMain 11 $ \rawInput -> do
  let input = parseInput rawInput
      res1  = part1 input
      res2  = part2 input
  print $ M.size res1
  putStrLn $ displayWith colorToChar res1
  putStrLn $ displayWith colorToChar res2
