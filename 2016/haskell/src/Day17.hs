-- module

module Day17 (day17_1, day17_2) where



-- import

import           Data.List
import           Data.Maybe
import qualified Data.Sequence as S

import           Common



-- solution

day17_1 :: Solution
day17_1 input = head $ findPaths (0, 0, input) isEnd
  where isEnd (x, y, _) = x == 3 && y == 3


day17_2 :: Solution
day17_2 input = show $ length $ last $ findPaths (0, 0, input) isEnd
  where isEnd (x, y, _) = x == 3 && y == 3



-- helpers

type Point = (Int, Int, String)

getPath :: Point -> String
getPath (_, _, p) = p

getNextPoints :: Point -> [Point]
getNextPoints (x, y, path) = [ (nx, ny, path ++ [step])
                             | (step, (nx, ny), status) <- nextPoints
                             , nx >= 0 && nx < 4
                             , ny >= 0 && ny < 4
                             , status > 'a'
                             ]
  where nextPoints = zip3 "UDLR" nextCoordinates $ take 4 $ hashMD5 path
        nextCoordinates = [ (x, y-1)
                          , (x, y+1)
                          , (x-1, y)
                          , (x+1, y)
                          ]

findPaths :: Point -> (Point -> Bool) -> [String]
findPaths start isEnd = clear <$> findPaths_ originalQueue
  where clear = fromJust . stripPrefix (getPath start)
        originalQueue = S.viewl $ S.fromList [start]
        findPaths_ S.EmptyL = []
        findPaths_ (point S.:< rest)
          | isEnd point = getPath point : findPaths_ (S.viewl rest)
          | otherwise   = findPaths_ $ S.viewl $ rest S.>< S.fromList (getNextPoints point)
