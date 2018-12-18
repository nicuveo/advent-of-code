{-# LANGUAGE ParallelListComp #-}
-- module

module Day10 (day10_1, day10_2) where



-- import

import           Text.Parsec

import           Common



-- solution

day10_1 :: Solution
day10_1 input = concat $ render $ take 11000 $ allFrames $ parseInput input
  where render :: [Frame] -> [String]
        render fs = [ if null s then "" else show index ++ '\n' : s
                    | index <- [0..]
                    | s <- map renderFrame fs
                    ]


day10_2 :: Solution
day10_2 = const ""



-- helpers

data Point  = P { y :: Int, x :: Int } deriving Eq
type Vector = Point

data Star = Star { starPosition :: Point
                 , starVelocity :: Vector
                 }

type Frame = [Star]


step :: Star -> Star
step (Star p v) = Star newPosition v
  where newPosition = P ny nx
        ny = y p + y v
        nx = x p + x v

nextFrame :: Frame -> Frame
nextFrame = map step

allFrames :: Frame -> [Frame]
allFrames = iterate nextFrame


boundingBox :: Frame -> (Point, Vector)
boundingBox f = (P minY minX, P (maxY - minY) (maxX - minX))
  where minY = minimum $ y . starPosition <$> f
        maxY = maximum $ y . starPosition <$> f
        minX = minimum $ x . starPosition <$> f
        maxX = maximum $ x . starPosition <$> f

isSmallEnough :: Frame -> Bool
isSmallEnough f = width < 100 && height < 20
  where bb@(_, P height width) = boundingBox f

renderFrame :: Frame -> String
renderFrame f
  | not $ isSmallEnough f = ""
  | otherwise = unlines [ [ if any (\star -> starPosition star == P row column) f
                            then '#'
                            else ' '
                          | column <- [minX..minX+width]
                          ]
                        | row <- [minY..minY+height]
                        ]
  where (P minY minX, P height width) = boundingBox f



parseInput :: String -> [Star]
parseInput = parseWith $ star `sepEndBy` space
  where star = do
          symbol "position="
          position <- point
          symbol "velocity="
          velocity <- point
          return $ Star position velocity
        point = do
          symbol "<"
          px <- intParser
          symbol ","
          py <- intParser
          symbol ">"
          return $ P py px

testData :: [Star]
testData = parseInput "position=< 9,  1> velocity=< 0,  2>\n\
                      \position=< 7,  0> velocity=<-1,  0>\n\
                      \position=< 3, -2> velocity=<-1,  1>\n\
                      \position=< 6, 10> velocity=<-2, -1>\n\
                      \position=< 2, -4> velocity=< 2,  2>\n\
                      \position=<-6, 10> velocity=< 2, -2>\n\
                      \position=< 1,  8> velocity=< 1, -1>\n\
                      \position=< 1,  7> velocity=< 1,  0>\n\
                      \position=<-3, 11> velocity=< 1, -2>\n\
                      \position=< 7,  6> velocity=<-1, -1>\n\
                      \position=<-2,  3> velocity=< 1,  0>\n\
                      \position=<-4,  3> velocity=< 2,  0>\n\
                      \position=<10, -3> velocity=<-1,  1>\n\
                      \position=< 5, 11> velocity=< 1, -2>\n\
                      \position=< 4,  7> velocity=< 0, -1>\n\
                      \position=< 8, -2> velocity=< 0,  1>\n\
                      \position=<15,  0> velocity=<-2,  0>\n\
                      \position=< 1,  6> velocity=< 1,  0>\n\
                      \position=< 8,  9> velocity=< 0, -1>\n\
                      \position=< 3,  3> velocity=<-1,  1>\n\
                      \position=< 0,  5> velocity=< 0, -1>\n\
                      \position=<-2,  2> velocity=< 2,  0>\n\
                      \position=< 5, -2> velocity=< 1,  2>\n\
                      \position=< 1,  4> velocity=< 2,  1>\n\
                      \position=<-2,  7> velocity=< 2, -2>\n\
                      \position=< 3,  6> velocity=<-1, -1>\n\
                      \position=< 5,  0> velocity=< 1,  0>\n\
                      \position=<-6,  0> velocity=< 2,  0>\n\
                      \position=< 5,  9> velocity=< 1, -2>\n\
                      \position=<14,  7> velocity=<-2,  0>\n\
                      \position=<-3,  6> velocity=< 2, -1>"
