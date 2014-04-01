{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackup.Config (Config, readConfig) where

import Hackup.Types
import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Maybe (mapMaybe)
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

extractSelectorType :: Text -> FileSelector
extractSelectorType fs = Prelude.head $ mapMaybe matchSelector [ ("glob:", Glob), ("regex:", Regex), ("", Glob) ]  
  where matchSelector (s, st) 
          | s `isPrefixOf` fs = Just . st . unpack $ Data.Text.drop (Data.Text.length s) fs
          | otherwise         = Nothing

instance FromJSON FileSelector where
  parseJSON (String s) = return $ extractSelectorType s
  parseJSON _ = mzero

instance FromJSON Command where
  parseJSON (Object v) = Command <$>
                           v .: "command" <*>
                           v .:? "workingDir" <*>
                           v .:? "fail" .!= True
  parseJSON _          = mzero

sectionFromJSON :: Text -> Value -> Parser Section
sectionFromJSON name (Object v) = Section (unpack name) <$>
                                    v .:? "archive" <*>
                                    v .:? "targetDir" <*>
                                    v .:? "keep" <*>
                                    v .: "contents" <*>
                                    v .:? "before" .!= [] <*>
                                    v .:? "after" .!= []

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                           v .: "rootDir" <*>
                           v .:? "keep" .!= 7 <*>
                           undefined 
  parseJSON _          = mzero

readConfig :: FilePath -> TryIO Config
readConfig _ = failWith "Failed explicilty at readConfig"

