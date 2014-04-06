module Hackup.Config.Fields where

import Data.Text (Text, pack)

newtype ConfigField = ConfigField { fieldName :: String } deriving (Show, Eq, Ord)

fieldText :: ConfigField -> Text
fieldText = pack . fieldName

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

itemBaseDirField :: ConfigField
itemBaseDirField = ConfigField "baseDir"

itemFilesField :: ConfigField
itemFilesField = ConfigField "files" 


