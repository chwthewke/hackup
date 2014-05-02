{-# LANGUAGE TemplateHaskell #-}


module Hackup.Action(Action,
                     Archive, baseName, archiveItems,  keepPrevious,
                     Hackup.Config.Types.Command, Hackup.Config.Types.workingDir, Hackup.Config.Types.command, Hackup.Config.Types.ignoreFailure,
                     planActions) where

import           Control.Applicative
import           Control.Lens        hiding (Action)
import           Data.Maybe          (fromMaybe)
import           System.FilePath

import           Hackup.Config.Types
import           Hackup.Types

data Archive = Archive { _baseName :: FilePath
                       , _archiveItems :: [Item]
                       , _keepPrevious :: Integer
                       } deriving (Eq, Show)

data Action = CommandAction Command
            | ArchiveAction Archive
            deriving (Show, Eq)

archiveAction :: FilePath -> Integer -> String -> Section -> Archive
archiveAction backupRoot defKeep name section =
  Archive (archDir </> archName) (section ^. items) keepNum
  where archDir = fromMaybe backupRoot $ section ^. archiveDir
        archName = fromMaybe name $ section ^. archiveName
        keepNum = fromMaybe defKeep $ section ^. keep

planSection :: FilePath -> Integer -> (String, Section) -> [Action]
planSection backupRoot defKeep (name, section) =
  (CommandAction <$> (section ^. before)) ++
    [ArchiveAction $ archiveAction backupRoot defKeep name section] ++
    (CommandAction <$> (section ^. after))

planActions :: Config -> Try [[Action]]
planActions cfg = return $
  planSection (cfg ^. backupRootDir) (cfg ^. defaultKeep) <$>
    cfg ^@.. sections . itraversed

makeLenses ''Archive
