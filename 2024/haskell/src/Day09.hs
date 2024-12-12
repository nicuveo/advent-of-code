module Day09 where


-- import

import AOC
import "this" Prelude

import Data.Char      (digitToInt)
import Data.Sequence  (Seq (..))
import Data.Sequence  qualified as S


-- input

type Input = Seq Block

data Block
  = FreeSpace Int Int
  | FileBlock Int Int Int


parseInput :: String -> Input
parseInput = go 0 0 . map digitToInt
  where
    go _ _ [] = S.Empty
    go !fileID !index [fileInfo] = S.singleton (FileBlock fileInfo fileID index)
    go !fileID !index (fileInfo:freeSpaceInfo:input) =
      FileBlock fileInfo fileID index :<|
      FreeSpace freeSpaceInfo (index + fileInfo) :<|
      go (fileID + 1) (index + fileInfo + freeSpaceInfo) input


-- solution

fileCheckSum :: Int -> Int -> Int -> Int
fileCheckSum fileID index size = sum $ map (fileID*) [index..index+size-1]

part1 :: Input -> Int
part1 = go 0 0
  where
    go !total !index = \case
      Empty -> total
      (FileBlock size fileID _ :<| s) ->
        go (total + fileCheckSum fileID index size) (index + size) s
      (FreeSpace size _ :<| s) ->
        pack total index size s
    pack !total !index freeSpace = \case
      Empty -> total
      (s :|> FreeSpace _ _)           -> pack total index freeSpace s
      (s :|> FileBlock size fileID _) -> case compare freeSpace size of
        EQ -> go (total + fileCheckSum fileID index size)      (index + size) s
        LT -> go (total + fileCheckSum fileID index freeSpace) (index + freeSpace)
          (s :|> FileBlock (size - freeSpace) fileID (-1))
        GT -> go (total + fileCheckSum fileID index size)      (index + size)
          (FreeSpace (freeSpace - size) (-1) :<| s)

part2 :: Input -> Int
part2 = go 0
  where
    go !total = \case
      Empty ->
        total
      (s :|> FreeSpace _ _) ->
        go total s
      (s :|> FileBlock size fileID fileIndex) ->
        case S.findIndexL (bigEnough size) s of
          Nothing ->
            go (total + fileCheckSum fileID fileIndex size) s
          Just bi -> case S.splitAt bi s of
            (before, FreeSpace freeSpace freeSpaceIndex :<| after) ->
              go total (
              before <>
                (
                  FileBlock size fileID freeSpaceIndex
                  :<| FreeSpace (freeSpace - size) (freeSpaceIndex + size)
                  :<| after
                )
              )
            _ -> error "findIndex did not return a valid free space block"
    bigEnough fileSize = \case
      FileBlock _ _ _  -> False
      FreeSpace size _ -> size >= fileSize


-- main

example :: String
example = "2333133121414131402"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput
