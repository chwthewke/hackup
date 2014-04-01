{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackup.Config (Config, readConfig) where

import Hackup.Types
import Control.Lens
import Data.Yaml
import Data.Text

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

instance FromJSON FileSelector where
  parseJSON (String s)
    | "glob:" `isPrefixOf` s =
      return . Glob $ unpack (Data.Text.drop 5 s)
    | "regex:" `isPrefixOf` s =
      return . Regex $ unpack (Data.Text.drop 6 s)
    | otherwise = return . Glob $ unpack s
--  parseJSON _ = mzero
  
readConfig :: FilePath -> TryIO Config
readConfig _ = failWith "Failed explicilty at readConfig"

