module Hackup.Driver.DryRun where

import           Control.Error
import           Control.Lens       hiding (Action)

import           Hackup.Action
import           Hackup.Driver.Core

runAction :: Action -> EitherT String IO [String]
runAction (CommandAction cmd) = runCommand cmd
runAction (ArchiveAction arch) = runArchive arch

runCommand :: Command -> EitherT String IO [String]
runCommand cmd = return [
  "run '" ++ cmd ^. command ++ "' in " ++ cmd ^. workingDir
  ]

runArchive :: Archive -> EitherT String IO [String]
runArchive arch = do (baseDir, items) <- selectArchiveFiles arch
                     name <- archiveName arch
                     return $ describeArchive name baseDir items (arch ^. keepPrevious)
  where describeArchive name baseDir items keep =
          ("Archive " ++ show (length items) ++ " item(s) from " ++ baseDir ++ " to " ++ name) :
            map ("  " ++) items ++ ["Keep " ++ show keep ++ " previous archives"]

