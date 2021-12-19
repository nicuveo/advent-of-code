{-# LANGUAGE OverloadedLists #-}

-- import

import           Control.Monad
import           Control.Monad.Extra        (unlessM, whenM)
import           Control.Monad.State.Strict
import           Data.Char                  (digitToInt)
import           Data.Function              (on)
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           GHC.Exts
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- tree and zipper

data SnailfishNumber
  = Leaf Int
  | Node SnailfishNumber SnailfishNumber

data Path
  = Root
  | L Path SnailfishNumber
  | R SnailfishNumber Path
  deriving Show

data Cursor = Cursor
  { cursorPath  :: Path
  , cursorDepth :: Int
  , cursorTree  :: SnailfishNumber
  }
  deriving Show

instance Show SnailfishNumber where
  show (Leaf x)   = show x
  show (Node l r) = "[" ++ show l ++ "," ++ show r ++ "]"

mkCursor :: SnailfishNumber -> Cursor
mkCursor t = Cursor Root 0 t

modifyTree :: (SnailfishNumber -> SnailfishNumber) -> (Cursor -> Cursor)
modifyTree f c = c { cursorTree = f $ cursorTree c }

goLeft :: Cursor -> Maybe Cursor
goLeft (Cursor p d (Node l r)) = Just $ Cursor (L p r) (d+1) l
goLeft _                       = Nothing

goRight :: Cursor -> Maybe Cursor
goRight (Cursor p d (Node l r)) = Just $ Cursor (R l p) (d+1) r
goRight _                       = Nothing

goUp :: Cursor -> Maybe Cursor
goUp (Cursor (L p r) d l) = Just $ Cursor p (d-1) (Node l r)
goUp (Cursor (R l p) d r) = Just $ Cursor p (d-1) (Node l r)
goUp _                    = Nothing

goRoot :: Cursor -> Cursor
goRoot c = maybe c goRoot $ goUp c

firstLeaf :: Cursor -> Cursor
firstLeaf cursor = goLeftUntilLeaf cursor
  where
    goLeftUntilLeaf c = case goLeft c of
      Nothing -> c
      Just c' -> goLeftUntilLeaf c'

nextLeaf :: Cursor -> Maybe Cursor
nextLeaf cursor = goLeftUntilLeaf <$> findFirstRightUp cursor
  where
    findFirstRightUp c = case cursorPath c of
      R _ _ -> goUp c >>= findFirstRightUp
      L _ _ -> goUp c >>= goRight
      Root  -> Nothing
    goLeftUntilLeaf c = case goLeft c of
      Nothing -> c
      Just c' -> goLeftUntilLeaf c'

previousLeaf :: Cursor -> Maybe Cursor
previousLeaf cursor = goRightUntilLeaf <$> findFirstLeftUp cursor
  where
    findFirstLeftUp c = case cursorPath c of
      L _ _ -> goUp c >>= findFirstLeftUp
      R _ _ -> goUp c >>= goLeft
      Root  -> Nothing
    goRightUntilLeaf c = case goRight c of
      Nothing -> c
      Just c' -> goRightUntilLeaf c'


-- input

type Input = [SnailfishNumber]

parseInput :: String -> Input
parseInput = parseLinesWith snailfishNumber
  where
    snailfishNumber = pair <|> leaf
    pair = do
      char '['
      l <- snailfishNumber
      char ','
      r <- snailfishNumber
      char ']'
      pure $ Node l r
    leaf = do
      n <- digitToInt <$> digit
      pure $ Leaf n


-- solution

maybeGoTo :: MonadState Cursor m => (Cursor -> Maybe Cursor) -> m Bool
maybeGoTo stepFun = do
  c <- get
  case stepFun c of
    Nothing -> pure False
    Just c' -> put c' $> True

explode  :: SnailfishNumber -> SnailfishNumber
explode = cursorTree . goRoot . execState go . firstLeaf . mkCursor
  where
    assert mb = unlessM mb $ error "assertion failed"
    addToLeaf x = modifyTree \case
      Leaf n -> Leaf $ n + x
      _      -> error "expected leaf found node"
    go = do
      depth <- gets cursorDepth
      unless (depth < 5) do
        assert $ maybeGoTo goUp
        currentTree <- gets cursorTree
        let
          (vl, vr) = case currentTree of
            Node (Leaf l) (Leaf r) -> (l, r)
            _                      -> error "the impossible has happened"
        whenM (maybeGoTo previousLeaf) $ do
          modify $ addToLeaf vl
          assert $ maybeGoTo $ goUp <=< nextLeaf
        whenM (maybeGoTo nextLeaf) $ do
          modify $ addToLeaf vr
          assert $ maybeGoTo $ goUp <=< previousLeaf
        modify $ modifyTree $ const $ Leaf 0
      whenM (maybeGoTo nextLeaf) go

split :: SnailfishNumber -> Maybe SnailfishNumber
split = fmap (cursorTree . goRoot) . evalState go . firstLeaf . mkCursor
  where
    go = do
      value <- gets cursorTree <&> \case
        Node _ _ -> error "that's impossible"
        Leaf n   -> n
      if value < 10
      then do
        wentToNext <- maybeGoTo nextLeaf
        if wentToNext then go else pure Nothing
      else do
        modify $ modifyTree $ const $
          Node (Leaf $ value `div` 2) (Leaf $ succ value `div` 2)
        Just <$> get

reduce :: SnailfishNumber -> SnailfishNumber
reduce (explode -> n) = case split n of
  Nothing -> n
  Just n' -> reduce n'

joinNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
joinNumbers = reduce ... Node

magnitude :: SnailfishNumber -> Int
magnitude = \case
  Leaf n   -> n
  Node l r -> 3 * magnitude l + 2 * magnitude r

part1 :: Input -> Int
part1 = magnitude . foldl1 joinNumbers

part2 :: Input -> Int
part2 numbers = maximum $ do
  (n1:rest) <- tails numbers
  n2 <- rest
  [magnitude $ joinNumbers n1 n2, magnitude $ joinNumbers n2 n1]


-- debug horribleness do not use cursed bad
-- THIS IS NOT A PLACE OF HONOUR

instance Num SnailfishNumber where
  (+) = joinNumbers
  fromInteger n = Leaf $ fromInteger n

instance IsList SnailfishNumber where
  type Item SnailfishNumber = SnailfishNumber
  fromList [x,y] = Node x y


-- main

main :: IO ()
main = aocMain 18 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  print $ explode [[[[[9,8],1],2],3],4]
  -- let tree = [[1,2],[3,4]]
  -- print $ mkCursor tree
  -- print $ goLeft $ mkCursor tree
  -- print $ goLeft <=< goLeft $ mkCursor tree
  -- print $ goUp <=< goLeft <=< goLeft $ mkCursor tree
  -- print $ goRight $ mkCursor tree
  -- print $ goRight <=< goRight $ mkCursor tree
  -- print $ goUp <=< goRight <=< goRight $ mkCursor tree
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
