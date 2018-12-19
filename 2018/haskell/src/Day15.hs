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
  where (gs, gl) = runGame testData
        lastFullRound = L.last [r | TurnEnded r <- gl]
        remainingHealth = L.sum $ entityHP <$> gameEntities gs


day15_2 :: Solution
day15_2 = undefined


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



           "#########\n\
           \#G..G..G#\n\
           \#.......#\n\
           \#.......#\n\
           \#G..E..G#\n\
           \#.......#\n\
           \#.......#\n\
           \#G..G..G#\n\
           \#########\n"
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


data Entity = Entity { entityPosition :: Position
                     , entityKind     :: EntityKind
                     , entityHP       :: Int
                     , entityAttack   :: Int
                     }

instance Show Entity where
  show (Entity p k h _) = printf "%s(%d): %d" (show k) p h

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

manhattanDistance :: Position -> Position -> GameMonad s Int
manhattanDistance p1 p2 = do
  w <- asks gameWidth
  let (y1, x1) = p1 `divMod` w
      (y2, x2) = p2 `divMod` w
  return $ abs (y1 - y2) + abs (x1 - x2)



-- entities

entityAtPosition :: Position -> GameMonad s (Maybe Entity)
entityAtPosition p = M.lookup p <$> gets gameEntities

modifyEntity :: Entity -> Maybe Entity -> GameMonad s ()
modifyEntity oldEntity newEntity = do
  gameState <- get
  let entities    = gameEntities gameState
      entities'   = M.delete (entityPosition oldEntity) entities
      newEntities = case newEntity of
        Nothing -> entities'
        Just e  -> M.insert (entityPosition e) e entities'
  put $ gameState { gameEntities = newEntities }



-- running the game

runGame :: String -> (GameState, GameLog)
runGame input = runST $ do
  (theMap, entities, width) <- parseGame input
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
entityTurn entity = do
  entities <- gets gameEntities
  let alreadyDead = entityPosition entity `M.notMember` entities
  if alreadyDead
    then return True
    else let enemies = M.elems $ M.filter (enemyOf entity) entities
         in if L.null enemies
            then return False
            else do
              distanceMap <- createDistanceMap entity enemies
              newEntity   <- doMove entity distanceMap
              doAttack newEntity
              return True

doMove :: Entity -> DistanceMap s -> GameMonad s Entity
doMove entity dm = do
  let oldPos = entityPosition entity
  currentDistance <- V.read dm oldPos
  neighbs         <- neighbours oldPos
  scores          <- fmap (sort . catMaybes) $ forM neighbs $ \n -> do
    neighbDistance <- V.read dm n
    return $ if neighbDistance < currentDistance
             then Just (neighbDistance, n)
             else Nothing
  if L.null scores || currentDistance == 0
    then return entity
    else do
      let (_, newPos) = L.head scores
      c <- atPos newPos
      when (c /= '.') $ error "trying to move to an unreachable cell"
      let newEntity = entity { entityPosition = newPos }
      tell [EntityMove entity newEntity]
      modifyEntity entity $ Just newEntity
      theMap <- asks gameMap
      V.swap theMap oldPos newPos
      return newEntity

doAttack :: Entity -> GameMonad s ()
doAttack entity = do
  let pos = entityPosition entity
  neighbs <- neighbours pos
  scores  <- fmap (sortOn entityHP . catMaybes) $ forM neighbs $ \n -> do
    maybeOtherEntity <- entityAtPosition n
    return $ maybeOtherEntity >>= \otherEntity ->
      if otherEntity `enemyOf` entity
      then Just $ damage (entityAttack entity) otherEntity
      else Nothing
  unless (L.null scores) $ do
    let damagedEnemy = L.head scores
    tell [EntityAttack entity damagedEnemy]
    if isDead damagedEnemy
      then do
        tell [EntityDeath damagedEnemy]
        modifyEntity damagedEnemy Nothing
        theMap <- asks gameMap
        V.write theMap (entityPosition damagedEnemy) '.'
      else modifyEntity damagedEnemy $ Just damagedEnemy



-- pathfinding

createDistanceMap :: Entity -> [Entity] -> GameMonad s (DistanceMap s)
createDistanceMap entity enemies = do
  theMap      <- asks gameMap
  distanceMap <- unsafeNew $ V.length theMap
  set distanceMap 9999
  queue       <- fmap (L.map snd . L.take 1 . sort . L.concat) $ forM enemies $ \e -> do
    allNeighbours  <- neighbours $ entityPosition e
    goodNeighbours <- forM allNeighbours $ \n -> do
      walkable <- isWalkable n
      d        <- manhattanDistance (entityPosition entity) n
      return $ if walkable || d == 0
               then Just (d, n)
               else Nothing
    return $ catMaybes goodNeighbours
  fillMap distanceMap 0 queue
  return distanceMap
  where fillMap :: DistanceMap s -> Int -> [Position] -> GameMonad s ()
        fillMap _  _ [] = return ()
        fillMap dm d q  = do
          nextQueue <- forM q $ \p -> do
            write dm p d
            allNeighbours  <- neighbours p
            goodNeighbours <- forM allNeighbours $ \n -> do
              walkable <- isWalkable n
              distance <- V.read dm n
              return $ if walkable && distance > d + 1
                       then Just n
                       else Nothing
            return $ catMaybes goodNeighbours
          fillMap dm (d+1) $ nub $ L.concat nextQueue



-- sanity checking

checkPositionConsistent :: [Entity] -> GameMonad s ()
checkPositionConsistent es = forM_ es $ \e -> do
  let p = entityPosition e
  c <- atPos p
  when (readEntityKind c /= Just (entityKind e)) $
    error $ "inconsistent char at position " ++ show p ++ "; expecting " ++ show (entityKind e) ++ " but got " ++ [c]



-- parsing

parseGamePure :: String -> (Vector Char, Entities, Int)
parseGamePure input = (immutableMap, M.fromList entityList, width)
  where immutableMap = V.fromList flatInput
        entityList   = catMaybes [ case readEntityKind c of
                                     Nothing -> Nothing
                                     Just ek -> Just (p, Entity p ek 200 3)
                                 | c <- flatInput
                                 | p <- [0..]
                                 ]
        splitInput   = lines input
        width        = L.length $ L.head splitInput
        flatInput    = L.concat splitInput


parseGame :: String -> ST s (GameMap s, Entities, Int)
parseGame input = do
  let (im, es, w) = parseGamePure input
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

runDebug :: IO ()
runDebug = do
  let (gm, es, w) = parseGamePure testData
      initState       = (gm, w, es, 0, [])
  run initState
  where run s = do
          putStr $ prettyPrint s
          i <- getLine
          unless (i == "q") $ run $ debugRound s
