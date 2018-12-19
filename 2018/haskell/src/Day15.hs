{-# LANGUAGE ParallelListComp #-}



-- module

module Day15 (day15_1, day15_2) where



-- import

import           Control.Concurrent
import           Control.Monad.RWS.Strict
import           Control.Monad.ST
import           Data.Function
import           Data.List                   as L
import           Data.List.Split             as L
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V hiding (length)
import qualified Data.Vector.Unboxed.Mutable as V
import           Text.Printf

import           Common



-- solution

day15_1 :: Solution
day15_1 input = show $ lastFullRound * remainingHealth
  where (gs, gl) = runGame 3 input
        lastFullRound = last [r | TurnEnded r <- gl]
        remainingHealth = sum $ entityHP <$> gameEntities gs


day15_2 :: Solution
day15_2 input = findResult 3
  where findResult elfAttack  =
          let (gs, gl) = runGame elfAttack input
              lastFullRound   = last [r | TurnEnded r <- gl]
              remainingHealth = sum $ entityHP <$> gameEntities gs
              elfDeaths       = countTrue [entityKind e == Elf | EntityDeath e <- gl]
          in if elfDeaths == 0
             then show (lastFullRound * remainingHealth) ++ ", " ++ show elfAttack
             else findResult $ elfAttack + 1


testData :: String
testData = "################################\n\
           \#####################...########\n\
           \###################....G########\n\
           \###################....#########\n\
           \#######.##########......########\n\
           \#######G#########........#######\n\
           \#######G#######.G.........######\n\
           \#######.######..G.........######\n\
           \#######.......##.G...G.G..######\n\
           \########..##..#....G......G#####\n\
           \############...#.....G.....#####\n\
           \#...#######..........G.#...#####\n\
           \#...#######...#####G......######\n\
           \##...######..#######G.....#.##.#\n\
           \###.G.#####.#########G.........#\n\
           \###G..#####.#########.......#.E#\n\
           \###..######.#########..........#\n\
           \###.......#.#########.....E..E.#\n\
           \#####G...#..#########.......#..#\n\
           \####.G.#.#...#######.....G.....#\n\
           \########......#####...........##\n\
           \###########..................###\n\
           \##########.................#####\n\
           \##########.................#####\n\
           \############..E.........E.....##\n\
           \############.........E........##\n\
           \###############.#............E##\n\
           \##################...E..E..##.##\n\
           \####################.#E..####.##\n\
           \################.....######...##\n\
           \#################.#..###########\n\
           \################################\n"



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

type GameMap s = V.STVector s Char
type DistanceMap s = V.STVector s Int

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
  V.unsafeRead theMap x

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
  return $ and result


entityTurn :: Entity -> GameMonad s Bool
entityTurn entityAtBeginningOfTurn = do
  entities <- gets gameEntities
  case M.lookup (entityPosition entityAtBeginningOfTurn) entities of -- find the entity at this position
    Nothing -> return True                                           -- Nothing: it died
    Just  e ->                                                       -- Just e:  we found one!
      if e /= entityAtBeginningOfTurn                                -- but is it really the same?
      then return True                                               -- if no: another entity took its place, it's dead
      else let enemies = M.elems $ M.filter (enemyOf e) entities     -- if yes: where are the enemies?
           in if null enemies                                        -- is there any enemy actually?
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
  V.unsafeSwap theMap oldPos newPos
  return newEntity

actionKillEntity :: Entity -> GameMonad s ()
actionKillEntity entity = do
  tell [EntityDeath entity]
  modifyEntity entity Nothing
  theMap <- asks gameMap
  V.unsafeWrite theMap (entityPosition entity) '.'


doMove :: Entity -> DistanceMap s -> GameMonad s Entity
doMove entity dm = do
  let oldPos = entityPosition entity
  currentDistance <- V.unsafeRead dm oldPos
  neighbs         <- neighbours oldPos
  -- rank all neighbouring cells
  scores          <- fmap (sort . catMaybes) $ forM neighbs $ \n -> do
    neighbDistance <- V.unsafeRead dm n
    return $ if neighbDistance < currentDistance
             then Just (neighbDistance, n)
             else Nothing
  -- if one is better than the current one: do move!
  if null scores || currentDistance == 0
    then return entity
    else do
      let (_, newPos) = head scores
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
  unless (null scores) $ do
    let damagedEnemy = head scores
    tell [EntityAttack entity damagedEnemy]
    if isDead damagedEnemy
      then actionKillEntity damagedEnemy
      else updateEntity damagedEnemy



-- pathfinding

createDistanceMap :: Entity -> [Entity] -> GameMonad s (DistanceMap s)
createDistanceMap entity enemies = do
  theMap      <- asks gameMap
  distanceMap <- V.unsafeNew $ V.length theMap
  -- create distance map fronm my entity to all reachable cells
  V.set distanceMap 9999
  fillMap distanceMap 0 0 [entityPosition entity]
  -- sort targets (neighbours of enemies) by distance from entity
  queue       <- fmap (sort . concat) $ forM enemies $ \e -> do
    allNeighbours  <- neighbours $ entityPosition e
    goodNeighbours <- forM allNeighbours $ \n -> do
      distance <- V.unsafeRead distanceMap n
      return $ if distance < 9999
               then Just (distance, n)
               else Nothing
    return $ catMaybes goodNeighbours
  -- find shortest path from target to entity (if there's a target)
  V.set distanceMap 9999
  unless (null queue) $ do
    let (_, target) = head queue
    fillMap distanceMap (entityPosition entity) 0 [target]
  return distanceMap

fillMap :: DistanceMap s -> Position -> Int -> [Position] -> GameMonad s ()
fillMap _       _      _               []    = return ()                           -- no element left to process: no path towards the target
fillMap distMap target currentDistance queue
  | target `elem` queue = V.unsafeWrite distMap target currentDistance             -- target in the queue? we found a path, stopping now
  | otherwise             = do
      nextQueue <- forM queue $ \p -> do                                           -- for each cell at `currentDistance` from my starting point
        V.unsafeWrite distMap p currentDistance                                    -- write that value in the distance map
        allNeighbours  <- neighbours p                                             -- get all the neighbours
        goodNeighbours <- forM allNeighbours $ \n -> do                            -- filter neighbours
          walkable <- isWalkable n                                                 -- is this an allowed cell?
          distance <- V.unsafeRead distMap n                                       -- what distance has been found for it so far?
          return $ if (n == target || walkable) && distance > currentDistance + 1  -- if it's worth choosing it as the next target
                   then Just n                                                     -- then yeah let's keep it!
                   else Nothing
        return $ catMaybes goodNeighbours
      fillMap distMap target (currentDistance+1) $ nub $ concat nextQueue          -- recursive call with our new input queue



-- sanity checking

checkPositionConsistent :: [Entity] -> GameMonad s ()
checkPositionConsistent es = forM_ es $ \e -> do
  let p = entityPosition e
  c <- atPos p
  when (readEntityKind c /= Just (entityKind e)) $
    error $ "inconsistent char at position " ++ show p ++ "; expecting " ++ show (entityKind e) ++ " but got " ++ [c]



-- parsing

parseGamePure :: Int -> String -> (V.Vector Char, Entities, Int)
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
        width        = length $ head splitInput
        flatInput    = concat splitInput


parseGame :: Int -> String -> ST s (GameMap s, Entities, Int)
parseGame elfAttack input = do
  let (im, es, w) = parseGamePure elfAttack input
  mutableMap <- V.unsafeThaw im
  return (mutableMap, es, w)



-- debug

type DebugState = (V.Vector Char, Int, Entities, Int, GameLog)

debugRound :: DebugState -> (Bool, DebugState)
debugRound (theMap, width, entities, r, _) =
  (cc, (newMap, width, gameEntities newState, r+1, gameLog))
  where ((cc, newMap), newState, gameLog) = runST $ do
          mutableMap <- V.thaw theMap
          runRWST doOneRound (GameInfo mutableMap width) $ GameState entities
        doOneRound :: GameMonad s (Bool, V.Vector Char)
        doOneRound = do
          canContinue <- gameRound r
          imMap       <- V.freeze =<< asks gameMap
          return (canContinue, imMap)


prettyPrint :: DebugState -> String
prettyPrint (theMap, width, entities, r, gameLog) =
  unlines $ concat [ ["round #" ++ show r]
                     , chunksOf width $ V.toList theMap
                     , show <$> M.elems entities
                     , show <$> gameLog
                     ]

runDebug :: Int -> IO ()
runDebug ea = run (gm, w, es, 0, [])
  where (gm, es, w) = parseGamePure ea testData
        run s = do
          putStr $ prettyPrint s
          i <- getLine
          unless (i == "q") $ run $ snd $ debugRound s

runDebugAnimate :: Int -> IO ()
runDebugAnimate ea = run (gm, w, es, 0, [])
  where tick = 120000
        (gm, es, w) = parseGamePure ea testData
        run s@(v, _, e, _, _) = do
          render v e
          let (cc, nd@(v2, _, e2, _, _)) = debugRound s
          threadDelay tick
          if cc
            then run nd
            else render v2 e2
        render v e = do
          putStr "\ESC[2J" -- Clear terminal screen
          putStr $ unlines [ concat [ if c `elem` "EG"
                                        then let (cr,cg,cb) = color $ e M.! (y*w+x)
                                             in printf "\ESC[38;2;%d;%d;%dm%c\ESC[0m" cr cg cb c
                                        else [c]
                                    | (x,c) <- zip [0..] r
                                    ]
                           | (y,r) <- zip [0..] $ chunksOf w $ V.toList v
                           ]
        color :: Entity -> (Int,Int,Int)
        color e = let hp = max (entityHP e) 0
                  in if hp <= 100 then
                       ( 255
                       , div (255 * hp) 100
                       , 40)
                     else
                       ( div (255 * (200 - hp)) 100
                       , 255
                       , 40)
