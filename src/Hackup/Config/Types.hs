{-# LANGUAGE TemplateHaskell #-}

module Hackup.Config.Types ( Config, RawConfig, Section, Item,
                      Config'(Config'), backupRootDir, defaultKeep, sections,
                      Section'(Section'), archiveName, archiveDir, keep, items, before, after,
                      Command(Command), command, workingDir, ignoreFailure,
                      Item'(Item'), itemBaseDir, itemContents,
                      RawFileSelector(Glob, Regex),
                      FileSelector(FileSelector)) where

import Data.Map (Map)
import Control.Lens (makeLenses)

data Config' fs = Config' { _backupRootDir :: FilePath
                          , _defaultKeep :: Integer
                          , _sections :: Map String (Section' fs) 
                          } deriving (Show, Eq)

type RawConfig = Config' RawFileSelector

type Config = Config' FileSelector


data Section' fs = Section' { _archiveName :: Maybe String
                          , _archiveDir :: Maybe FilePath
                          , _keep :: Maybe Integer
                          , _items :: [Item' fs]
                          , _before :: [Command]
                          , _after :: [Command]
                          } deriving (Show, Eq)
                       
type Section = Section' FileSelector

data Command = Command { _command :: String
                       , _workingDir :: Maybe FilePath
                       , _ignoreFailure :: Bool 
                       } deriving (Show, Eq)

data Item' a = Item' { _itemBaseDir :: FilePath
                     , _itemContents :: Maybe a
                     } deriving (Show, Eq) 

type Item = Item' FileSelector

data RawFileSelector = Glob String | Regex String deriving (Show, Eq)

newtype FileSelector = FileSelector { runFileSelector :: IO [FilePath] }

makeLenses ''Config'
makeLenses ''Section'
makeLenses ''Command
makeLenses ''Item'
