import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Char (digitToInt)
import Data.Heap qualified as Heap
import Data.Set qualified as Set
import Linear (V2 (V2))

main = do
  g <- parse <$> readFile "../input/17.in"
  print $ solve2 g


parse :: String -> Grid
parse s =
  let ls = lines s
      h = length ls
      w = length (head ls)
   in listArray (V2 1 1, V2 h w) (map digitToInt (concat ls))

type Grid = Array (V2 Int) Int

data Heading = N | S | E | W deriving (Eq, Ord, Show)

data Node = MkNode !(V2 Int) !Heading !Int deriving (Eq, Ord, Show)

neighbors grid (MkNode pos hdg runLen) =
  [ (dist, MkNode pos' hdg' runLen')
    | (hdg', runLen') <- [(hdg', 1) | hdg' <- turn hdg] ++ [(hdg, runLen + 1) | runLen < 3],
      let pos' = step hdg' pos,
      inRange (bounds grid) pos',
      let dist = grid ! pos'
  ]

turn N = [W, E]
turn E = [N, S]
turn S = [E, W]
turn W = [S, N]

step N (V2 r c) = V2 (r - 1) c
step E (V2 r c) = V2 r (c + 1)
step S (V2 r c) = V2 (r + 1) c
step W (V2 r c) = V2 r (c - 1)

shortestPaths :: (Ord a) => (a -> [(Int, a)]) -> a -> [(Int, [a])]
shortestPaths next = go Set.empty . Heap.singleton . (0,) . (: [])
  where
    go visited paths = case Heap.uncons paths of
      Nothing -> []
      Just (path@(d, ns@(n : _)), paths)
        | Set.notMember n visited ->
            let newPaths = [(d + dd, n' : ns) | (dd, n') <- next n, Set.notMember n' visited]
             in path : go (Set.insert n visited) (foldr Heap.insert paths newPaths)
      Just (_, paths) -> go visited paths

solve1 grid =
  let (start, end) = bounds grid
      (path@(dist, _) : _) =
        dropWhile (\(_, MkNode pos _ _ : _) -> pos /= end) $
          shortestPaths (neighbors grid) (MkNode start E 0)
   in dist

neighbors2 grid (MkNode pos hdg n) =
  [ (dist, MkNode pos' hdg' runLen')
    | (hdg', runLen') <- [(hdg', 1) | n >= 4, hdg' <- turn hdg] ++ [(hdg, n + 1) | n < 10],
      let pos' = step hdg' pos,
      inRange (bounds grid) pos',
      let dist = grid ! pos'
  ]

solve2 grid =
  let (start, end) = bounds grid
      (path@(dist, _) : _) =
        dropWhile (\(_, MkNode pos _ runLen : _) -> pos /= end || runLen < 4) $
          shortestPaths (neighbors2 grid) (MkNode start E 0)
   in dist
