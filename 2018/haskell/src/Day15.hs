{-# LANGUAGE ParallelListComp #-}



-- module

module Day15 (day15_1, day15_2) where



-- import

import           Control.Monad.RWS.Strict
import           Control.Monad.ST
import           Data.Function
import           Data.List                   as L
import           Data.List.Split             as L
import           Data.Map.Strict             as M
import           Data.Maybe
import           Data.Vector.Unboxed         as V hiding (forM, forM_, length,
                                                   (++))
import           Data.Vector.Unboxed.Mutable as V
import           Text.Printf

import           Common



-- solution

day15_1 :: Solution
day15_1 input = show $ lastFullRound * remainingHealth
  where (gs, gl) = runGame 3 input
        lastFullRound = L.last [r | TurnEnded r <- gl]
        remainingHealth = L.sum $ entityHP <$> gameEntities gs


day15_2 :: Solution
day15_2 s = findResult 3
  where input = s
        (_, entities, _) = parseGamePure 3 input
        elfNumber        = countElves entities
        findResult elfa  =
          let (gs, gl) = runGame elfa input
              lastFullRound   = L.last [r | TurnEnded r <- gl]
              remainingHealth = L.sum $ entityHP <$> gameEntities gs
              remainingElves  = countElves $ gameEntities gs
          in if remainingElves == elfNumber
             then show (lastFullRound * remainingHealth) ++ ", " ++ show elfa
             else findResult $ elfa + 1


testData :: String
testData = "#########\n\
           \#G......#\n\
           \#.E.#...#\n\
           \#..##..G#\n\
           \#...##..#\n\
           \#...#...#\n\
           \#.G...G.#\n\
           \#.....G.#\n\
           \#########\n"


{-
testData = "#########\n\
           \#G......#\n\
           \#.E.#...#\n\
           \#..##..G#\n\
           \#...##..#\n\
           \#...#...#\n\
           \#.G...G.#\n\
           \#.....G.#\n\
           \#########\n"

testData = "######\n\
           \#.G..#\n\
           \##..##\n\
           \#...E#\n\
           \#E...#\n\
           \######\n"

testData = "#######\n\
           \#E..EG#\n\
           \#.#G.E#\n\
           \#E.##E#\n\
           \#G..#.#\n\
           \#..E#.#\n\
           \#######\n"

testData = "#######\n\
           \#G..#E#\n\
           \#E#E.E#\n\
           \#G.##.#\n\
           \#...#E#\n\
           \#...E.#\n\
           \#######\n"

testData = "#######\n\
           \#.G...#\n\
           \#...EG#\n\
           \#.#.#G#\n\
           \#..G#E#\n\
           \#.....#\n\
           \#######\n"
-}



-- types

data EntityKind = Elf | Goblin deriving Eq

instance Show EntityKind where
  show Elf    = "E"
  show Goblin = "G"

readEntityKind :: Char -> Maybe EntityKind
readEntityKind 'E' = Just Elf
readEntityKind 'G' = Just Goblin
readEntityKind _   = Nothing


type Position = Int


data Entity = Entity { entityId       :: Int
                     , entityPosition :: Position
                     , entityKind     :: EntityKind
                     , entityHP       :: Int
                     , entityAttack   :: Int
                     }

instance Show Entity where
  show (Entity _ p k h _) = printf "%s(%d): %d" (show k) p h

instance Eq Entity where
  (==) = (==) `on` entityId

type Entities = M.Map Position Entity



-- game

type GameMap s = STVector s Char
type DistanceMap s = STVector s Int

data GameInfo s = GameInfo { gameMap   :: GameMap s
                           , gameWidth :: Int
                           }

newtype GameState = GameState { gameEntities :: Entities
                              }

type GameLog = [GameEvent]

data GameEvent = GameEnded
               | TurnEnded    Int
               | EntityMove   Entity Entity
               | EntityAttack Entity Entity
               | EntityDeath  Entity
               deriving Show

type GameMonad s = RWST (GameInfo s) GameLog GameState (ST s)



-- entity

enemyOf :: Entity -> Entity -> Bool
enemyOf = (/=) `on` entityKind

damage :: Int -> Entity -> Entity
damage x e = e { entityHP = entityHP e - x }

isDead :: Entity -> Bool
isDead e = entityHP e <= 0



-- map

atPos :: Position -> GameMonad s Char
atPos x = do
  theMap <- asks gameMap
  V.read theMap x

isWalkable :: Position -> GameMonad s Bool
isWalkable x = (== '.') <$> atPos x

neighbours :: Position -> GameMonad s [Position]
neighbours x = do
  mapWidth <- asks gameWidth
  return [x - mapWidth, x - 1, x + 1, x + mapWidth]



-- entities

entityAtPosition :: Position -> GameMonad s (Maybe Entity)
entityAtPosition p = M.lookup p <$> gets gameEntities

-- modifyEntity removes the oldEntity from the map
-- and inserts the new one if it's not Nothing
modifyEntity :: Entity -> Maybe Entity -> GameMonad s ()
modifyEntity oldEntity newEntity = do
  gameState <- get
  let entities    = gameEntities gameState
      entities'   = M.delete (entityPosition oldEntity) entities
      newEntities = case newEntity of
        Nothing -> entities'
        Just e  -> M.insert (entityPosition e) e entities'
  put $ gameState { gameEntities = newEntities }

updateEntity :: Entity -> GameMonad s ()
updateEntity e = modifyEntity e $ Just e

countElves :: Entities -> Int
countElves = M.size . M.filter (\e -> entityKind e == Elf)



-- running the game

runGame :: Int -> String -> (GameState, GameLog)
runGame elfAttack input = runST $ do
  (theMap, entities, width) <- parseGame elfAttack input
  execRWST (playGame 1) (GameInfo theMap width) $ GameState entities
  where playGame roundIndex = do
          canContinue <- gameRound roundIndex
          if not canContinue
            then tell [GameEnded]
            else do
              tell [TurnEnded roundIndex]
              playGame $ roundIndex + 1

gameRound :: Int -> GameMonad s Bool
gameRound roundIndex = do
  when (roundIndex > 9999) $ error "too many rounds"
  entities <- M.elems <$> gets gameEntities
  checkPositionConsistent entities
  result   <- sequence $ entityTurn <$> entities
  return $ L.and result


entityTurn :: Entity -> GameMonad s Bool
entityTurn entityAtBeginningOfTurn = do
  entities <- gets gameEntities
  case M.lookup (entityPosition entityAtBeginningOfTurn) entities of -- find the entity at this position
    Nothing -> return True                                           -- Nothing: it died
    Just  e ->                                                       -- Just e:  we found one!
      if e /= entityAtBeginningOfTurn                                -- but is it really the same?
      then return True                                               -- if no: another entity took its place, it's dead
      else let enemies = M.elems $ M.filter (enemyOf e) entities     -- if yes: where are the enemies?
           in if L.null enemies                                      -- is there any enemy actually?
              then return False                                      -- if nope: end of the fight
              else do                                                -- if yes:
                distanceMap <- createDistanceMap e enemies           -- create the distance map
                newEntity   <- doMove e distanceMap                  -- try to move
                doAttack newEntity                                   -- try to attack
                return True


actionMoveEntity :: Entity -> Position -> GameMonad s Entity
actionMoveEntity entity newPos = do
  let oldPos = entityPosition entity
  c <- atPos newPos
  when (c /= '.') $ error "trying to move to an unreachable cell"
  let newEntity = entity { entityPosition = newPos }
  tell [EntityMove entity newEntity]
  modifyEntity entity $ Just newEntity
  theMap <- asks gameMap
  V.swap theMap oldPos newPos
  return newEntity

actionKillEntity :: Entity -> GameMonad s ()
actionKillEntity entity = do
  tell [EntityDeath entity]
  modifyEntity entity Nothing
  theMap <- asks gameMap
  V.write theMap (entityPosition entity) '.'


doMove :: Entity -> DistanceMap s -> GameMonad s Entity
doMove entity dm = do
  let oldPos = entityPosition entity
  currentDistance <- V.read dm oldPos
  neighbs         <- neighbours oldPos
  -- rank all neighbouring cells
  scores          <- fmap (sort . catMaybes) $ forM neighbs $ \n -> do
    neighbDistance <- V.read dm n
    return $ if neighbDistance < currentDistance
             then Just (neighbDistance, n)
             else Nothing
  -- if one is better than the current one: do move!
  if L.null scores || currentDistance == 0
    then return entity
    else do
      let (_, newPos) = L.head scores
      actionMoveEntity entity newPos

doAttack :: Entity -> GameMonad s ()
doAttack entity = do
  let pos = entityPosition entity
  neighbs <- neighbours pos
  -- rank all neighbouring enemies (if neighbouring entities are enemies)
  scores  <- fmap (sortOn entityHP . catMaybes) $ forM neighbs $ \n -> do
    maybeOtherEntity <- entityAtPosition n
    return $ maybeOtherEntity >>= \otherEntity ->
      if otherEntity `enemyOf` entity
      then Just $ damage (entityAttack entity) otherEntity
      else Nothing
  -- if I have a nearby enemy, attack it!
  unless (L.null scores) $ do
    let damagedEnemy = L.head scores
    tell [EntityAttack entity damagedEnemy]
    if isDead damagedEnemy
      then actionKillEntity damagedEnemy
      else updateEntity damagedEnemy



-- pathfinding

createDistanceMap :: Entity -> [Entity] -> GameMonad s (DistanceMap s)
createDistanceMap entity enemies = do
  theMap      <- asks gameMap
  distanceMap <- unsafeNew $ V.length theMap
  -- create distance map fronm my entity to all reachable cells
  set distanceMap 9999
  fillMap distanceMap 0 0 [entityPosition entity]
  -- sort targets (neighbours of enemies) by distance from entity
  queue       <- fmap (sort . L.concat) $ forM enemies $ \e -> do
    allNeighbours  <- neighbours $ entityPosition e
    goodNeighbours <- forM allNeighbours $ \n -> do
      distance <- V.read distanceMap n
      return $ if distance < 9999
               then Just (distance, n)
               else Nothing
    return $ catMaybes goodNeighbours
  -- find shortest path from target to entity (if there's a target)
  set distanceMap 9999
  unless (L.null queue) $ do
    let (_, target) = L.head queue
    fillMap distanceMap (entityPosition entity) 0 [target]
  return distanceMap

fillMap :: DistanceMap s -> Position -> Int -> [Position] -> GameMonad s ()
fillMap _       _      _               []    = return ()                           -- no element left to process: no path towards the target
fillMap distMap target currentDistance queue
  | target `L.elem` queue = write distMap target currentDistance                   -- target in the queue? we found a path, stopping now
  | otherwise             = do
      nextQueue <- forM queue $ \p -> do                                           -- for each cell at `currentDistance` from my starting point
        write distMap p currentDistance                                            -- write that value in the distance map
        allNeighbours  <- neighbours p                                             -- get all the neighbours
        goodNeighbours <- forM allNeighbours $ \n -> do                            -- filter neighbours
          walkable <- isWalkable n                                                 -- is this an allowed cell?
          distance <- V.read distMap n                                             -- what distance has been found for it so far?
          return $ if (n == target || walkable) && distance > currentDistance + 1  -- if it's worth choosing it as the next target
                   then Just n                                                     -- then yeah let's keep it!
                   else Nothing
        return $ catMaybes goodNeighbours
      fillMap distMap target (currentDistance+1) $ nub $ L.concat nextQueue        -- recursive call with our new input queue



-- sanity checking

checkPositionConsistent :: [Entity] -> GameMonad s ()
checkPositionConsistent es = forM_ es $ \e -> do
  let p = entityPosition e
  c <- atPos p
  when (readEntityKind c /= Just (entityKind e)) $
    error $ "inconsistent char at position " ++ show p ++ "; expecting " ++ show (entityKind e) ++ " but got " ++ [c]



-- parsing

parseGamePure :: Int -> String -> (Vector Char, Entities, Int)
parseGamePure elfAttack input = (immutableMap, M.fromList entityList, width)
  where immutableMap = V.fromList flatInput
        entityList   = catMaybes [ case readEntityKind c of
                                     Nothing     -> Nothing
                                     Just Elf    -> Just (p, Entity p p Elf    200 elfAttack)
                                     Just Goblin -> Just (p, Entity p p Goblin 200 3)
                                 | c <- flatInput
                                 | p <- [0..]
                                 ]
        splitInput   = lines input
        width        = L.length $ L.head splitInput
        flatInput    = L.concat splitInput


parseGame :: Int -> String -> ST s (GameMap s, Entities, Int)
parseGame elfAttack input = do
  let (im, es, w) = parseGamePure elfAttack input
  mutableMap <- unsafeThaw im
  return (mutableMap, es, w)



-- debug

type DebugState = (Vector Char, Int, Entities, Int, GameLog)

debugRound :: DebugState -> DebugState
debugRound (theMap, width, entities, r, _) =
  (newMap, width, gameEntities newState, r+1, gameLog)
  where (newMap, newState, gameLog) = runST $ do
          mutableMap <- V.thaw theMap
          runRWST doOneRound (GameInfo mutableMap width) $ GameState entities
        doOneRound :: GameMonad s (Vector Char)
        doOneRound = do
          gameRound r
          mMap <- asks gameMap
          V.freeze mMap


prettyPrint :: DebugState -> String
prettyPrint (theMap, width, entities, r, gameLog) =
  unlines $ L.concat [ ["round #" ++ show r]
                     , chunksOf width $ V.toList theMap
                     , show <$> M.elems entities
                     , show <$> gameLog
                     ]

runDebug :: Int -> IO ()
runDebug ea = do
  let (gm, es, w) = parseGamePure ea testData
      initState       = (gm, w, es, 0, [])
  run initState
  where run s = do
          putStr $ prettyPrint s
          i <- getLine
          unless (i == "q") $ run $ debugRound s
