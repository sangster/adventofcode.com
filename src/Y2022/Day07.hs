{-# LANGUAGE ImportQualifiedPost #-}
module Y2022.Day07 (parts) where

import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import Data.List (find, sort)
import Data.HashMap.Strict qualified as M
import Parser

parts = ( (part1, Just "1449447")
        , (part2, Just "8679207")
        , parse commandHistory
        )

part1 :: [Command] -> String
part1 history = show $ M.foldr (+) 0 (M.filter (<= 100000) sizes)
  where
    sizes = directorySizes $ rebuildFs history


part2 :: [Command] -> String
part2 history = show . fromJust . find (>= neededSpace) . sort
              $ snd <$> M.toList sizes
  where
    sizes = directorySizes $ rebuildFs history
    neededSpace = 30000000 - unusedSpace
    unusedSpace = 70000000 - usedSpace
    usedSpace   = sizes M.! []


data Command = Cd Direction
             | Ls [Listing]
             deriving Show

data Direction = Root
               | Up
               | Down Filename
               deriving Show

data Listing = Directory Filename
             | File Int Filename
             deriving Show

type Filename = String
type FileSystem = M.HashMap Filename FsNode
data FsNode = DirNode FileSystem
            | FileNode Int


-- | A mapping of directory paths to total disk usage.
directorySizes :: FileSystem -> M.HashMap [Filename] Int
directorySizes = M.fromListWith (+) . calcSize
  where
    calcSize :: FileSystem -> [([Filename], Int)]
    calcSize fs = ([], dirSize):subdirs
      where
        dirSize = sum $ snd <$> filter ((== 1) . length . fst) subdirs
        subdirs = concat $ entrySize <$> M.toList fs

        entrySize :: (Filename, FsNode) -> [([Filename], Int)]
        entrySize (_, FileNode size) = [([], size)]
        entrySize (d, DirNode fs')   = first (d:) <$> calcSize fs'


-- | Rebuilts a 'FileSystem' by examining the output of a list of 'Command'.
rebuildFs :: [Command] -> FileSystem
rebuildFs = build [] M.empty
  where
    build _ fs [] = fs
    build cwd fs ((Cd dir):cs) = build (move dir) fs cs
      where
        move Root     = []
        move Up       = init cwd
        move (Down d) = cwd ++ [d]

    build cwd fs ((Ls listing):cs) = build cwd (addToPath cwd fs) cs
      where
        addToPath :: [Filename] -> FileSystem -> FileSystem
        addToPath []     _ = M.fromList $ mkFsNode <$> listing
        addToPath (d:ds) m =
          case m M.! d of
            DirNode m' -> M.insert d (DirNode $ addToPath ds m') m
            FileNode _ -> error "error, not a directory"

        mkFsNode (Directory name) = (name, DirNode M.empty)
        mkFsNode (File size name) = (name, FileNode size)


commandHistory :: Parser [Command]
commandHistory = splitSome whitespace (reserved "$" >> command)
  where
    command :: Parser Command
    command = symbol (Cd Root) (reserved "cd /")
          <|> symbol (Cd Up)   (reserved "cd ..")
          <|> (reserved "cd" >> Cd . Down <$> word)
          <|> (reserved "ls" >> Ls <$> listing)
    listing = splitMany whitespace
            $ Directory <$> (reserved "dir" >> word)
          <|> File <$> natural <*> (char ' ' >> word)
