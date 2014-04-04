module Hackup.Config.Fields where

import Data.Yaml (FromJSON, Object, Parser)
import qualified Data.Yaml
import Data.Text (Text, pack)

newtype ConfigField = ConfigField { _fieldName :: String } deriving (Show, Eq, Ord)

class AText a where
  asText :: a -> Text
  
instance AText Text where asText = id

instance AText ConfigField where asText = pack . _fieldName

(.:) :: (FromJSON a, AText k) => Object -> k -> Parser a
obj .: n = (Data.Yaml..:) obj $ asText n

(.:?) :: (FromJSON a, AText k) => Object -> k -> Parser (Maybe a)
obj .:? n = (Data.Yaml..:?) obj $ asText n

rootDirField :: ConfigField
rootDirField = ConfigField "rootDir"

defaultKeepField :: ConfigField
defaultKeepField = ConfigField "keep"

sectionArchiveNameField :: ConfigField
sectionArchiveNameField = ConfigField "archive"

sectionArchiveDirField :: ConfigField
sectionArchiveDirField = ConfigField "targetDir"

sectionKeepField :: ConfigField
sectionKeepField = ConfigField "keep"

sectionItemsField :: ConfigField
sectionItemsField = ConfigField "contents"

sectionBeforeField :: ConfigField
sectionBeforeField = ConfigField "before"

sectionAfterField :: ConfigField
sectionAfterField = ConfigField "after"

commandField :: ConfigField
commandField = ConfigField "command"

commandWorkingDirField :: ConfigField
commandWorkingDirField = ConfigField "workingDir"

commandIgnoreFailureField :: ConfigField
commandIgnoreFailureField = ConfigField "ignoreFailure"

 


