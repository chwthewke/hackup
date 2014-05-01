{-# LANGUAGE TemplateHaskell #-}

module Hackup.Config.Types (
                      Config(Config), backupRootDir, defaultKeep, sections,
                      Section(Section), archiveName, archiveDir, keep, items, before, after,
                      Command(Command), command, workingDir, ignoreFailure,
                      Item(Item), itemBaseDir, itemContents,
                      RawFileSelector(Glob, Regex),
                      FileSelector(FileSelector), runFileSelector, rawFileSelector) where

import           Control.Lens (makeLenses)
import           Data.Map     (Map)

data Config = Config { _backupRootDir :: FilePath
                     , _defaultKeep :: Integer
                     , _sections :: Map String Section
                     } deriving (Show, Eq)

data Section = Section { _archiveName :: Maybe String
                       , _archiveDir :: Maybe FilePath
                       , _keep :: Maybe Integer
                       , _items :: [Item]
                       , _before :: [Command]
                       , _after :: [Command]
                          } deriving (Show, Eq)


data Command = Command { _command :: String
                       , _workingDir :: Maybe FilePath
                       , _ignoreFailure :: Bool
                       } deriving (Show, Eq)

data Item = Item { _itemBaseDir :: FilePath
                 , _itemContents :: Maybe FileSelector
                 } deriving (Show, Eq)

data RawFileSelector = Glob String | Regex String deriving (Show, Eq)

data FileSelector = FileSelector { runFileSelector :: FilePath -> IO [FilePath]
                                 , rawFileSelector :: RawFileSelector }

instance Show FileSelector where
  show = show . rawFileSelector

instance Eq FileSelector where
  a == b = rawFileSelector a == rawFileSelector b

makeLenses ''Config
makeLenses ''Section
makeLenses ''Command
makeLenses ''Item
