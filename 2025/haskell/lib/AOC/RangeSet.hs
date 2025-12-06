module AOC.RangeSet
  ( Range (rangeBegin, rangeEnd)
  , range
  , rangeUnsafe
  , size
  , RangeSet
  , empty
  , singleton
  , fromList
  , toList
  , within
  , insert
  ) where

import "this" Prelude hiding (toList)

import Data.Set       qualified as S


data Range a = Range
  { rangeBegin :: a
  , rangeEnd   :: a
  } deriving (Show, Eq, Ord)

range :: Ord a => a -> a -> Maybe (Range a)
range begin end
  | begin <= end = Just $ Range begin end
  | otherwise    = Nothing

rangeUnsafe :: a -> a -> Range a
rangeUnsafe = Range

size :: Num a => Range a -> a
size (Range b e) = 1 + e - b


newtype RangeSet a = RangeSet (S.Set (Range a))
  deriving (Show, Eq, Ord)

empty :: RangeSet a
empty = RangeSet $ S.empty

singleton :: Range a -> RangeSet a
singleton = RangeSet . S.singleton

fromList :: Ord a => [Range a] -> RangeSet a
fromList = foldl' (flip insert) empty

toList :: RangeSet a -> [Range a]
toList (RangeSet s) = S.toList s

within :: Ord a => a -> RangeSet a -> Bool
x `within` (RangeSet s) =
  case S.lookupLE (Range x x) s of
    Nothing -> False
    Just r  -> x <= rangeEnd r

insert :: Ord a => Range a -> RangeSet a -> RangeSet a
insert (Range nb ne) (RangeSet s) =
  let
    (below, remaining) = S.spanAntitone (\r -> rangeEnd   r <  nb) s
    (overlap, above)   = S.spanAntitone (\r -> rangeBegin r <= ne) remaining
    overlapBegin = maybe nb (min nb . rangeBegin) $ S.lookupMin overlap
    overlapEnd   = maybe ne (max ne . rangeEnd)   $ S.lookupMax overlap
  in
    RangeSet $ below <> S.singleton (Range overlapBegin overlapEnd) <> above
