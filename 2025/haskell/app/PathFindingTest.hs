import "this" Prelude

import Data.HashMap.Strict qualified as M
import Data.Ratio
import System.Random

import AOC
import AOC.Debug
import AOC.Grid.Flat


type Maze = Grid Char

distanceMap = [ interpolate (3%4) black red
              , interpolate (3%4) black yellow
              , interpolate (3%4) black green
              , interpolate (3%4) black blue
              ]
gray = interpolate (1%3) white black
w = 53
h = 53


mkMaze :: StdGen -> Maze
mkMaze = fromList w h . take (w*h) . map toCell . randoms
  where toCell :: Int -> Char
        toCell i = if mod i 100 > 73 then '#' else ' '


neighbours :: Maze -> Point -> [(Int, Point)]
neighbours m p = [(1, n) | n <- gridFourSurroundingPoints m p, m ! n /= '#']

heuristic :: Point -> Point -> Int
heuristic end p = py d + px d
  where d = abs $ end - p

render :: Point -> Maze -> PFState Point -> String
render end m s = displayWith r m
  where r _ '#' = fgColor white $ bgColor gray "▇▇"
        r p _
          | Just c <- costSoFar s p = b p $ fgColor (f c) "▒▒"
          | otherwise               = "  "
        f c = interpolateN distanceMap $ c % maxCost
        b p = q $ p `elem`
          case reconstructPath s of
            Nothing   -> [pfStart s, end, currentNode s]
            Just path -> map snd path
        q = bool id $ bgColor white
        maxCost = max 1 $ maximum $ map snd $ M.elems $ pfNodeInfo s

step :: PFState Point -> Maybe (Logs, PFState Point)
step s
  | hasFoundAnswer s = Nothing
  | otherwise        = let n = pathFindingStep s in Just ([], n)



testMaze = from2DList [ "        "
                      , "######  "
                      , "       #"
                      , "   #####"
                      , "  #     "
                      , "  #  #  "
                      , "      # "
                      ]

main :: IO ()
main = do
  n <- getStdGen
  let m = mkMaze n
      s = head $ filter (\p -> m ! p /= '#') $ allPoints m
      e = last $ filter (\p -> m ! p /= '#') $ allPoints m
      z = mkPFState (neighbours m) (heuristic e) s (== e)
  animate 20 resetCursor (render e m) step ([], z)
