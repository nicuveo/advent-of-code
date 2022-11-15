-- module

module AOC.Runtime (aocMain) where


-- imports

import           Control.Monad
import           Data.Char
import           Data.List             (dropWhileEnd)
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           Text.Printf


-- helpers

printUsageAndDie :: String -> IO ()
printUsageAndDie cmd = die $ "usage: " ++ cmd ++ " path_to_input_dir"

getInput :: Int -> String -> IO String
getInput day dir = dropWhileEnd isSpace <$> readFile filename
  where filename = dir </> printf "%02d" day ++ ".in"


-- main

aocMain :: Int -> (String -> IO ()) -> IO ()
aocMain day body = do
  args <- getArgs
  when (length args /= 1) $ getProgName >>= printUsageAndDie
  putStrLn $ printf "### Day %02d ###" day
  getInput day (head args) >>= body
