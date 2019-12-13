{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}


-- import

import           Control.Concurrent
import           Control.Lens        hiding (Empty)
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.List.Split
import qualified Data.Map.Strict     as M
import qualified Data.Vector.Unboxed as V
import           System.IO
import           Text.Printf

import           AOC
import           AOC.Debug.Animate
import           AOC.Map.Sparse
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- robot io

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Show, Eq, Enum)

data Game = Game { _tiles  :: SparseMap Tile
                 , _buffer :: [Int]
                 , _score  :: Int
                 , _ball   :: Int
                 , _paddle :: Int
                 }
makeLenses ''Game

type GameMonad = StateT Game IO


displayGame :: GameMonad String
displayGame = do
  t <- use tiles
  s <- use score
  return $ unlines [ printf "Score: %d" s
                   , displayWith toc t
                   ]
  where toc _ Nothing       = "  "
        toc _ (Just Empty ) = "  "
        toc _ (Just Wall  ) = "██"
        toc _ (Just Block ) = "░░"
        toc _ (Just Paddle) = "══"
        toc _ (Just Ball  ) = "()"



inF :: GameMonad (Maybe Int)
inF = do
  b <- use ball
  p <- use paddle
  return $ Just $ if | b < p     -> (-1)
                     | b > p     -> 1
                     | otherwise -> 0

outF :: Int -> GameMonad ()
outF v = do
  buffer %= (v:)
  whenM (uses buffer $ (== 3) . length) $ do
    [t,y,x] <- use buffer
    buffer .= []
    if (x == (-1) && y == 0)
    then score .= t
    else do
      let tile = toEnum t
      when (tile == Paddle) $ paddle .= x
      when (tile == Ball  ) $ ball   .= x
      tiles %= M.insert (Point y x) tile
      s <- displayGame
      liftIO $ resetCursor >> putStr s >> threadDelay 1000


runC :: GameMonad a -> IO Game
runC = flip execStateT $ Game M.empty [] 0 0 0



-- main

main :: IO ()
main = aocMain 13 $ \rawInput -> do
  let program = parseInput rawInput V.// [(0, 2)]
  hSetEcho stdout False
  clearScreen
  res <- runC $ runM program inF outF
  printf "Final score: %d\n" $ res ^. score
