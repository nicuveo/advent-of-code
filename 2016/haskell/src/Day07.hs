-- module

module Day07 (day07_1, day07_2) where



-- import

import           Common
import           Data.Either.Utils
import           Data.List
import           Data.List.Split
import           Text.Parsec



-- solution

day07_1 :: Solution
day07_1 input = show $ length [ ()
                              | (outer, inner) <- parseIPs <$> lines input
                              , any containsABBA outer
                              , not $ any containsABBA inner
                              ]


day07_2 :: Solution
day07_2 input = show $ length [ ()
                              | (outer, inner) <- parseIPs <$> lines input
                              , let abas = outer >>= findABAs
                                    babs = inner >>= findBABs
                              , not $ null $ intersect abas babs
                              ]



-- helpers

containsABBA :: String -> Bool
containsABBA s = or [ a == d && b == c && a /= b | (a:b:c:d:_) <- tails s ]

findABAs :: String -> [String]
findABAs s = [ [a,b] | (a:b:c:_) <- tails s, a == c && a /= b ]

findBABs :: String -> [String]
findBABs s = [ [b,a] | (a:b:c:_) <- tails s, a == c && a /= b ]

parseIPs :: String -> ([String], [String])
parseIPs = fromRight . parse line ""
  where address = many1 lower
        line = do
          ips <- many (address <|> between (char '[') (char ']') address)
          let [outer, inner] = transpose $ chunksOf 2 ips
          return (outer, inner)
