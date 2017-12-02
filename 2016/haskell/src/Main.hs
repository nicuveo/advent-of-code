-- module

module Main where



-- imports

import           Control.Monad
import           Data.Char
import           Data.List
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           Text.Printf

import           Common
import           Day01
import           Day02
import           Day03
import           Day04
import           Day05



-- helpers

printUsageAndDie :: String -> IO ()
printUsageAndDie cmd = die $ "usage: " ++ cmd ++ " path_to_input_dir"

getInput :: String -> Int -> IO String
getInput dir day = dropWhileEnd isSpace <$> readFile filename
  where filename = dir </> printf "%02d" day ++ ".in"

solutions :: [(Int, [Solution])]
solutions = zip [1..] [[day01_1, day01_2],
                       [day02_1, day02_2],
                       [day03_1, day03_2],
                       [day04_1, day04_2],
                       [day05_1, day05_2]]



-- main

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ getProgName >>= printUsageAndDie
  let run day fun = fun <$> getInput (head args) day
  forM_ solutions $ \(day, parts) -> unless (day == 5) $ do
    printf "# day %d\n" day
    forM_ parts $ putStrLn <=< run day
