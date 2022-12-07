module Main where


-- import

import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Writer
import Data.Char            (isSpace)
import Data.Either
import Data.Foldable
import Data.HashMap.Strict  qualified as M
import Data.HashSet         qualified as S
import Data.List            qualified as L
import Data.List.NonEmpty   as NE
import Text.Parsec          hiding (State)
import Text.Parsec.Char

import AOC


-- input

type Input = [Command]

data Path
  = Root
  | Down String
  | Up
  deriving (Show)

data Command
  = ChangeDirectory Path
  | ListFiles (S.HashSet String) (M.HashMap String Int)
  deriving (Show)

parseInput :: String -> Input
parseInput = parseWith $ spaces *> many1 command
  where
    command = do
      symbol "$"
      tryAll [changeDir, listFiles] <?> "valid command"
    changeDir = do
      symbol "cd"
      p <- path
      pure $ ChangeDirectory p
    name =
      lexeme $ many1 $ satisfy $ not . isSpace
    path = tryAll
      [ Root <$  symbol "/"
      , Up   <$  symbol ".."
      , Down <$> name
      ]
    listFiles = do
      symbol "ls"
      (dirs, files) <- partitionEithers <$> many entry
      pure $ ListFiles (S.fromList dirs) (M.fromList files)
    entry =
      choice [directory, file]
    directory = do
      symbol "dir"
      Left <$> name
    file = do
      size <- number
      fileName <- name
      pure $ Right (fileName, size)


-- filesystem

data Folder = Folder
  { subFolders :: M.HashMap String Folder
  , lsFiles    :: M.HashMap String Int
  }
  deriving (Show)

display :: Folder -> [String]
display = curry showFolder ""
  where
    showFolder (name, Folder {..}) = concat
      [ [name ++ "/"]
      , L.map (("  " ++) . showFile) $ M.toList lsFiles
      , L.map ("  " ++) $ concatMap showFolder $ M.toList subFolders
      ]
    showFile (name, size) = show size ++ " " ++ name

emptyFolder :: Folder
emptyFolder = Folder mempty mempty

updateSubFolder
  :: String
  -> Folder
  -> Folder
  -> Folder
updateSubFolder name folder parent =
  parent { subFolders = M.insert name folder $ subFolders parent }

setFilesAndDirs
  :: S.HashSet String
  -> M.HashMap String Int
  -> Folder
  -> Folder
setFilesAndDirs dirs files folder =
  folder { subFolders = emptyFolder <$ S.toMap dirs
         , lsFiles    = files
         }


-- guess filesystem

type FolderStack = NonEmpty (String, Folder)

getRoot :: MonadState FolderStack m => m Folder
getRoot = do
  goRoot
  snd . NE.head <$> get

goRoot :: MonadState FolderStack m => m ()
goRoot = goUp `untilM_` isRoot
  where
    isRoot = null . NE.tail <$> get

goUp :: MonadState FolderStack m => m ()
goUp = do
  (name, folder) :| stack <- get
  case stack of
    [] -> pure ()
    ((parentName, parent):rest) ->
      put $ (parentName, updateSubFolder name folder parent) :| rest

goDown :: MonadState FolderStack m => String -> m ()
goDown name = modify ((name, emptyFolder) <|)

updateFolder
  :: MonadState FolderStack m
  => S.HashSet String
  -> M.HashMap String Int
  -> m ()
updateFolder dirs files = modify \case
  (name, current) :| stack ->
    (name, setFilesAndDirs dirs files current) :| stack

buildFilesystem :: Input -> Folder
buildFilesystem commands =
  flip evalState (NE.singleton ("/", emptyFolder)) do
    traverse_ step commands
    getRoot
  where
    step = \case
      ChangeDirectory Root        -> goRoot
      ChangeDirectory Up          -> goUp
      ChangeDirectory (Down name) -> goDown name
      ListFiles dirs files        -> updateFolder dirs files


-- solution

part1 :: Input -> Int
part1 (buildFilesystem -> root) = sum $ execWriter $ go root
  where
    go Folder {..} = do
      subFoldersSize <- sum <$> traverse go subFolders
      let fileSize = sum $ M.elems lsFiles
          result = fileSize + subFoldersSize
      when (result <= 100000) $
        tell [result]
      pure result

part2 :: Input -> Int
part2 (buildFilesystem -> root) =
  minimum $ L.filter (>= usedSpace - 40000000) folderSizes
  where
    (usedSpace, folderSizes) = runWriter $ go root
    go Folder {..} = do
      subFoldersSize <- sum <$> traverse go subFolders
      let fileSize = sum $ M.elems lsFiles
          result = fileSize + subFoldersSize
      tell [result]
      pure result


-- main

main :: IO ()
main = aocMain 07 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  -- traverse_ putStrLn $ display $ buildFilesystem testInput
  -- traverse_ putStrLn $ display $ buildFilesystem realInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
