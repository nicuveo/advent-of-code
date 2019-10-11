import           Data.Bool
import qualified Data.Map.Lazy as M
import           Data.Ratio
import           System.Random

import           AOC
import           AOC.Debug
import           AOC.Map.Flat


type Maze = FlatMap Char

distanceMap = [ interpolate (3%4) black red
              , interpolate (3%4) black yellow
              , interpolate (3%4) black green
              , interpolate (3%4) black blue
              ]
gray = interpolate (1%3) white black
w = 100
h = 50


mkMaze :: StdGen -> Maze
mkMaze = fromList w h . take (w*h) . map toCell . randoms
  where toCell :: Int -> Char
        toCell i = if mod i 100 > 73 then '#' else ' '


neighbours :: Maze -> Point -> [(Int, Point)]
neighbours m p = [(1, n) | n <- fourNeighbouringPointsOf m p, m ! n /= '#']

heuristic :: Point -> Point -> Int
heuristic end p = py d + px d
  where d = abs $ end - p

render :: Maze -> PFState Point -> String
render m s = displayWith r m
  where r _ '#' = fgColor white $ bgColor gray "▇▇"
        r p _
          | Just c <- pfCost s p = b p $ fgColor (f c) "▒▒"
          | otherwise            = "  "
        f c = interpolateN distanceMap $ c % maxCost
        b p
          | pfFinished s = q $ p `elem` map fst (pfPath s)
          | otherwise    = q $ p `elem` [ pfStart   s
                                        , pfEnd     s
                                        , pfCurrent s
                                        ]
        q = bool id $ bgColor white
        maxCost = max 1 $ maximum $ map snd $ M.elems $ pfMap s

step :: PFState Point -> Maybe (Logs, PFState Point)
step s
  | pfFinished s = Nothing
  | otherwise    = let n = pfStep s in Just ([], n)



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
  print distanceMap
  n <- getStdGen
  let m = mkMaze n
      s = head $ filter (\p -> m ! p /= '#') $ allPoints m
      e = last $ filter (\p -> m ! p /= '#') $ allPoints m
      z = mkPFState (neighbours m) (heuristic e) s e
  animate 20 resetCursor (render m) step ([], z)
