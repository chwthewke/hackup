{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hackup.Config (Config(Config), readConfig,
                      backupRootDir, defaultKeep, sections,
                      Section(Section), archiveName, archiveDir, keep, items, before, after,
                      Command(Command), command, workingDir, ignoreFailure,
                      FileSelector(Glob, Regex)) where

import Hackup.Types
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Error
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Yaml
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import Data.ByteString (readFile)
import qualified Data.HashMap.Strict as HashMap

data Config = Config { _backupRootDir :: FilePath
                     , _defaultKeep :: Integer
                     , _sections :: Map String Section 
                     } deriving (Show, Eq)


data Section = Section { _archiveName :: Maybe String
                       , _archiveDir :: Maybe FilePath
                       , _keep :: Maybe Integer
                       , _items :: [FileSelector]
                       , _before :: [Command]
                       , _after :: [Command]
                       } deriving (Show, Eq)
                       
data Command = Command { _command :: String
                       , _workingDir :: Maybe FilePath
                       , _ignoreFailure :: Bool 
                       } deriving (Show, Eq)

data FileSelector = Glob String | Regex String deriving (Show, Eq)

makeLenses ''Config
makeLenses ''Section
makeLenses ''Command

extractSelectorType :: Text -> FileSelector
extractSelectorType fs = Prelude.head $ mapMaybe matchSelector [ ("glob:", Glob), ("regex:", Regex), ("", Glob) ]  
  where matchSelector (s, st) 
          | s `isPrefixOf` fs = Just . st . Text.unpack $ Text.drop (Text.length s) fs
          | otherwise         = Nothing

instance FromJSON FileSelector where
  parseJSON (String s) = return $ extractSelectorType s
  parseJSON _ = mzero

instance FromJSON Command where
  parseJSON (Object v) = Command <$>
                           v .: "command" <*>
                           v .:? "workingDir" <*>
                           v .:? "ignoreFailure" .!= False
  parseJSON _          = mzero


instance FromJSON Section where
  parseJSON (Object v) = Section <$>
                                           v .:? "archive" <*>
                                           v .:? "targetDir" <*>
                                           v .:? "keep" <*>
                                           v .: "contents" <*>
                                           v .:? "before" .!= [] <*>
                                           v .:? "after" .!= []
  parseJSON _          = mzero


instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                           v .: backupRootDirKey <*>
                           v .:? defaultKeepKey .!= 7 <*>
                           sectionsFromJSON v
    where sectionsFromJSON = fmap Map.fromList 
                               . mapM (\(t, v') -> (Text.unpack t,) <$> parseJSON v') 
                               . HashMap.toList 
                               . HashMap.filterWithKey (\ x _ -> x `notElem` [backupRootDirKey, defaultKeepKey])
          defaultKeepKey = "keep"
          backupRootDirKey = "rootDir"
          
  parseJSON _          = mzero



readConfig :: FilePath -> TryIO Config
readConfig = ErrorT . liftM decodeEither . Data.ByteString.readFile



 
