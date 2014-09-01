{-# LANGUAGE TemplateHaskell #-}

module Hackup.Config.Types (
                      Config(Config), backupRootDir, defaultKeep, sections,
                      Section(Section), archiveName, archiveDir, keep, itemsBaseDir, items, before, after,
                      Command(Command), command, workingDir, ignoreFailure) where

import           Control.Lens     (makeLenses)
import           Data.Map         (Map)

import           Hackup.Selectors

data Config = Config { _backupRootDir :: FilePath
                     , _defaultKeep   :: Integer
                     , _sections      :: Map String Section
                     } deriving (Show, Eq)

data Section = Section { _archiveName  :: Maybe String
                       , _archiveDir   :: Maybe FilePath
                       , _keep         :: Maybe Integer
                       , _itemsBaseDir :: FilePath
                       , _items        :: [FileSelector]
                       , _before       :: [Command]
                       , _after        :: [Command]
                          } deriving (Show, Eq)


data Command = Command { _command       :: String
                       , _workingDir    :: FilePath
                       , _ignoreFailure :: Bool
                       } deriving (Show, Eq)


makeLenses ''Config
makeLenses ''Section
makeLenses ''Command
