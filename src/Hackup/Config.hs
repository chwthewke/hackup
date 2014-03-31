{-# LANGUAGE TemplateHaskell #-}

module Hackup.Config (Config, readConfig) where

import Hackup.Types
import Control.Lens

data Config = Config { _backupRootDir :: FilePath
                     , _defaultKeep :: Integer
                     , _sections :: [Section] 
                     } deriving (Show, Eq)


data Section = Section { _name :: String
                       , _archiveName :: Maybe String
                       , _archiveDir :: Maybe FilePath
                       , _keep :: Maybe Integer
                       , _items :: [FileSelector]
                       , _before :: [Command]
                       , _after :: [Command]
                       } deriving (Show, Eq)
                       
data Command = Command { _command :: String
                       , _workingDir :: Maybe FilePath
                       , _fail :: Bool 
                       } deriving (Show, Eq)

data FileSelector = Glob String | Regex String deriving (Show, Eq)

makeLenses ''Config
makeLenses ''Section
makeLenses ''Command                       

readConfig :: FilePath -> TryIO Config
readConfig _ = failWith "Failed explicilty at readConfig"

