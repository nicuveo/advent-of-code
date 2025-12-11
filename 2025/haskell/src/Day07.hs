module Day07 where

import "this" Prelude

import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as S
import Data.Time.Clock


type Input = [HashSet Int]

parseInput :: String -> Input
parseInput = map prepare . lines
  where
    prepare line = S.fromList do
      (i, c) <- zip [0..] line
      guard $ c /= '.'
      pure i


part1 :: Input -> Int
part1 = \case
  [] -> error "empty input"
  (firstLine:otherLines) ->
    snd $ foldl' go (firstLine, 0) otherLines
  where
    go :: (HashSet Int, Int) -> HashSet Int -> (HashSet Int, Int)
    go (tachyons, splits) splitters =
      let
        hits   = S.intersection tachyons splitters
        nohits = S.difference   tachyons splitters
      in
        ( nohits <> S.map (+1) hits <> S.map (subtract 1) hits
        , splits + S.size hits
        )

part2 :: Input -> Int
part2 = map (fmap (const 1) . S.toMap) >>> \case
  [] -> error "empty input"
  (firstLine:otherLines) ->
    sum $ M.elems $ foldl' go firstLine otherLines
  where
    go :: HashMap Int Int -> HashMap Int Int -> HashMap Int Int
    go tachyons splitters =
      let
        hits   = M.intersection tachyons splitters
        nohits = M.difference   tachyons splitters
      in
        nohits
          `combine` adjust (+1) hits
          `combine` adjust (subtract 1) hits
    combine = M.unionWith (+)
    adjust f m = M.fromList do
      (key, value) <- M.toList m
      pure (f key, value)

part2_2 :: Input -> Int
part2_2 = \case
  [] -> error "empty input"
  (firstLine:otherLines) -> sum
    $ flip evalState M.empty
    $ traverse (go otherLines 0)
    $ S.toList firstLine
  where
    go
      :: [HashSet Int]
      -> Int -- current depth
      -> Int -- position
      -> State (HashMap (Int, Int) Int) Int
    go [] _ _ = pure 1
    go (splitters:otherLines) depth position = do
      gets (M.lookup (depth, position)) >>= \case
        Just value -> pure value
        Nothing -> do
          result <-
            if position `S.member` splitters
            then do
              l <- go otherLines (depth+1) (position-1)
              r <- go otherLines (depth+1) (position+1)
              pure $ l + r
            else do
              go otherLines (depth+1) position
          modify $ M.insert (depth, position) result
          pure result

measure :: a -> IO (NominalDiffTime, a)
measure someValue = do
  before <- getCurrentTime
  let result = seq someValue someValue
  after <- getCurrentTime
  pure (diffUTCTime after before, result)


example :: String
example = "\
\.......S.......\n\
\...............\n\
\.......^.......\n\
\...............\n\
\......^.^......\n\
\...............\n\
\.....^.^.^.....\n\
\...............\n\
\....^.^...^....\n\
\...............\n\
\...^.^...^.^...\n\
\...............\n\
\..^...^.....^..\n\
\...............\n\
\.^.^.^.^.^...^.\n\
\...............\n\
\"

run :: String -> IO ()
run rawData = do
  let !testInput = parseInput example
      !realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print =<< measure (part2 testInput)
  print =<< measure (part2 realInput)
  print =<< measure (part2_2 testInput)
  print =<< measure (part2_2 realInput)
