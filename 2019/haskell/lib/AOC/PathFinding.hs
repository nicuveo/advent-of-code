{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module AOC.PathFinding ( PFState
                       , mkPFState
                       , pfStart
                       , pfEnd
                       , pfCurrent
                       , pfCost
                       , pfParent
                       , pfFinished
                       , pfFound
                       , pfMap

                       , pfStep
                       , pfPath

                       , findPath
                       , maybeFindPath
                       , findPathH
                       , maybeFindPathH
                       ) where



-- imports

import           Data.Function   (on)
import           Data.List       as L
import           Data.Map.Lazy   as M
import           Data.Maybe
import           Data.PQueue.Min as Q



-- cell type

newtype Cell a = Cell (Int, Int, a) deriving Eq

instance Eq a => Ord (Cell a) where
  compare = compare `on` \(Cell (e,c,_)) -> (e,-c)

instance Show a => Show (Cell a) where
  show (Cell (e, c, a)) = concat [ "{e: "
                                 , show e
                                 , ", "
                                 , show c
                                 , ", "
                                 , show a
                                 , "}"
                                 ]

cellValue :: Cell a -> a
cellValue (Cell (_, _, a)) = a



-- path finder state

data PFState a = PFS { pfN :: a -> [(Int, a)]
                     , pfH :: a -> Int
                     , pfS :: a
                     , pfE :: a
                     , pfQ :: Q.MinQueue (Cell a)
                     , pfP :: M.Map a (a, Int)
                     }


mkPFState :: Ord a => (a -> [(Int, a)]) -> (a -> Int) -> a -> a -> PFState a
mkPFState n h s e = PFS n h s e q p
  where q = Q.singleton $ Cell (h s, 0, s)
        p = M.singleton s (s, 0)

pfStart :: PFState a -> a
pfStart = pfS

pfEnd :: PFState a -> a
pfEnd = pfE

pfCurrent :: PFState a -> a
pfCurrent = cellValue . Q.findMin . pfQ

pfCost :: Ord a => PFState a -> a -> Maybe Int
pfCost s x = snd <$> pfP s !? x

pfParent :: Ord a => PFState a -> a -> Maybe a
pfParent s x = fst <$> pfP s !? x

pfMap :: PFState a -> M.Map a (a, Int)
pfMap = pfP


pfFinished :: PFState a -> Bool
pfFinished s = Q.null (pfQ s)

pfFound :: Ord a => PFState a -> Bool
pfFound s = pfFinished s && pfE s `M.member` pfP s



-- path finding functions

pfStep :: Ord a => PFState a -> PFState a
pfStep !s
  | pfFinished s = s
  | (Cell (_, c, a), q') <- Q.deleteFindMin $ pfQ s =
      if | a == pfE s -> s { pfQ = Q.empty }
         | otherwise  -> L.foldr (insertElt a c) (s { pfQ = q' }) $ pfN s a
  where insertElt parent cost (distance, neighb) st
          | maybe True ((neighbCost <) . snd) $ pfP st !? neighb =
              st { pfQ = Q.insert (Cell ( neighbCost + pfH st neighb
                                        , neighbCost
                                        , neighb))
                         $ Q.filter ((/= neighb) . cellValue)
                         $ pfQ st
                 , pfP = M.insert neighb (parent, neighbCost) $ pfP st
                 }
          | otherwise = st
          where neighbCost = cost + distance

pfPath :: Ord a => PFState a -> [(a, Int)]
pfPath s
  | pfFound s = fill (pfEnd s) []
  | otherwise = []
  where fill cell path
          | cell == pfS s         =          (cell, 0) : path
          | (p,c) <- pfP s ! cell = fill p $ (cell, c) : path



-- putting it together

findPath :: Ord a => (a -> [(Int, a)]) -> a -> a -> [(a, Int)]
findPath = flip findPathH $ const 0

maybeFindPath :: Ord a => (a -> [(Int, a)]) -> a -> a -> Maybe [(a, Int)]
maybeFindPath = flip maybeFindPathH $ const 0

findPathH :: Ord a => (a -> [(Int, a)]) -> (a -> Int) -> a -> a -> [(a, Int)]
findPathH n h s e = fromMaybe (error "findPath: no path found") $ maybeFindPathH n h s e

maybeFindPathH :: Ord a => (a -> [(Int, a)]) -> (a -> Int) -> a -> a -> Maybe [(a, Int)]
maybeFindPathH n h s e = finalize $ until pfFinished pfStep $ mkPFState n h s e
  where finalize w
          | pfFound w = Just $ pfPath w
          | otherwise = Nothing
