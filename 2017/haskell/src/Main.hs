-- module

module Main where



-- imports

import           Control.Monad
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           Text.Printf

import           Common
import           Day1


-- helpers

printUsageAndDie :: String -> IO ()
printUsageAndDie cmd = die $ "usage: " ++ cmd ++ " path_to_input_dir"

getInput :: String -> Int -> IO String
getInput dir day = readFile filename
  where filename = dir </> show day ++ ".in"

solutions :: [(Int, Solution)]
solutions = zip [1..] [day1]



-- main

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ getProgName >>= printUsageAndDie
  let run (i, s) = s <$> getInput (head args) i
  forM_ solutions $ \solution@(day, s) -> do
    printf "# day %d\n" day
    run solution >>= putStrLn
