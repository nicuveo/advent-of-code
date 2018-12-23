{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}


-- module

module Day17 (day17_1, day17_2) where



-- import

import           Control.Concurrent
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.Function               ((&))
import           Data.List                   as L
import qualified Data.Map.Strict             as M
import           Text.Parsec                 hiding (State, count)
import           Text.Printf

import           Common



-- solution

day17_1 :: Solution
day17_1 = show . countIf isWater . M.elems . run step . parseInput


day17_2 :: Solution
day17_2 = show . count RestingWater . M.elems . run step . parseInput



-- cell

data Cell = Spring | Sand | Clay | FlowingWater | RestingWater
  deriving Eq

isWater :: Cell -> Bool
isWater FlowingWater = True
isWater RestingWater = True
isWater _            = False



-- position

type Point = (Int, Int)
type Map   = M.Map Point Cell

bounds :: Map -> ((Int,Int),(Int,Int))
bounds m = ( (minimum ys, minimum xs)
           , (maximum ys, maximum xs)
           )
  where ys = fst <$> M.keys m
        xs = snd <$> M.keys m

inBounds :: Map -> Point -> Bool
inBounds m (y,x) = and [ y >= minY , y <= maxY
                       , x >= minX , x <= maxX
                       ]
  where ((minY,minX),(maxY,maxX)) = bounds m


cellAt :: Point -> Map -> Cell
cellAt = M.findWithDefault Sand

left :: Point -> Point
left (y,x) = (y,x-1)

right :: Point -> Point
right (y,x) = (y,x+1)

above :: Point -> Point
above (y,x) = (y-1,x)

below :: Point -> Point
below (y,x) = (y+1,x)



-- walk

fill :: MonadState Map m => Point -> Cell -> m ()
fill p w = modify $ M.insert p w

step :: MonadState Map m => Point -> m [Point]
step p = do
  m  <- get
  let at x = cellAt x m
      down = below p
  case at down of
    FlowingWater -> return []
    Spring       -> error "found a spring below"
    Sand         -> dropFrom down
    _            -> floodFrom p
  where floodFrom s = do
          fill s FlowingWater
          (leftPoint,  leftOK,  leftFlow)  <- flood left  s
          (rightPoint, rightOK, rightFlow) <- flood right s
          unless (leftFlow || rightFlow) $ do
            let (y,lx) = leftPoint
                (_,rx) = rightPoint
            forM_ [lx..rx] $ \x -> fill (y,x) RestingWater
          if leftOK || rightOK
          then return $ [leftPoint | leftOK] ++ [rightPoint | rightOK]
          else if leftFlow || rightFlow
               then return []
               else floodFrom $ above s
        dropFrom x = do
          m <- get
          fill x FlowingWater
          let dp = below x
              c  = cellAt dp m
          if c == Sand && dp <= snd (bounds m)
          then dropFrom dp
          else return [x]

flood :: MonadState Map m => (Point -> Point) -> Point -> m (Point, Bool, Bool)
flood t p = do
  m  <- get
  let at x = cellAt x m
      nextPoint = t p
      belowNextPoint = below nextPoint
      nextCell = at nextPoint
  if nextCell `elem` [FlowingWater, Sand]
  then if at belowNextPoint == nextCell
       then fill nextPoint FlowingWater >> return (nextPoint, nextCell == Sand, True)
       else fill nextPoint FlowingWater >> flood t nextPoint
  else return (p, False, False)

runM :: Monad m => (Point -> StateT Map m [Point]) -> Map -> m Map
runM f m = M.filterWithKey (const . isOk) <$> execStateT (run_ (0,500)) m
  where ((yMin,_),(yMax,_)) = bounds m
        isOk (y,_) = y >= yMin && y <= yMax
        run_ p = do
          nextPoints <- filter isOk <$> f p
          sequence_ $ run_ <$> nextPoints

run :: (Point -> State Map [Point]) -> Map -> Map
run = runIdentity ... runM



-- parsing

parseInput :: String -> Map
parseInput = foldl' addLine M.empty . parseWith (line `sepEndBy` newline)
  where line = do
          a <- lower
          char '='
          u <- intParser
          char ','
          space
          _ <- lower
          char '='
          v <- intParser
          string ".."
          w <- intParser
          return $ if a == 'x'
                   then ([u], [v..w])
                   else ([v..w], [u])
        addLine m (xs, ys) = foldl' (&) m [M.insert (y,x) Clay | y <- ys, x <- xs]



-- debug

type Color = (Int, Int, Int)

display :: Maybe Point -> Map -> String
display here (M.insert (0,500) Spring -> m) = unlines
  [ concat [ let c = cellAt (y,x) m
             in if Just (y,x) == here
                then colorizeBG colorOfPoint $ colorizeFG (colorOf c) $ stringOf c
                else                           colorizeFG (colorOf c) $ stringOf c
           | x <- [minX .. maxX]
           ]
  | y <- [minY .. maxY]
  ]
  where ((minY,minX),(maxY,maxX)) = case here of
                                      Just (y,x) -> ((y-20, x-20),(y+20,x+20))
                                      Nothing    -> bounds m
        colorOfPoint          = (200,230,250)
        colorOf  Spring       = (200, 20, 80)
        colorOf  Sand         = (200,200,100)
        colorOf  Clay         = (120,130,140)
        colorOf  RestingWater = (100,200,255)
        colorOf  FlowingWater = (100,200,255)
        stringOf Sand         = " "
        stringOf Spring       = "@"
        stringOf Clay         = "#"
        stringOf RestingWater = "~"
        stringOf FlowingWater = "|"

animate :: Map -> IO Map
animate m = do
  putStr "\ESC[2J"
  runM stepAndDisplay m
  where stepAndDisplay ps = do
          res <- step ps
          dm  <- get
          unless (null res) $ liftIO $ do
            putStr "\ESC[;H"
            putStr $ display (Just $ head res) dm
          liftIO $ threadDelay 1000000
          return res

colorizeFG :: Color -> String -> String
colorizeFG (r,g,b) = printf "\ESC[38;2;%d;%d;%dm%s\ESC[0m" r g b
colorizeBG :: Color -> String -> String
colorizeBG (r,g,b) = printf "\ESC[48;2;%d;%d;%dm%s\ESC[0m" r g b

testData :: String
testData = "x=495, y=2..7\n\
           \y=7, x=495..501\n\
           \x=501, y=3..7\n\
           \x=498, y=2..4\n\
           \x=506, y=1..2\n\
           \x=498, y=10..13\n\
           \x=504, y=10..13\n\
           \y=13, x=498..504\n"
