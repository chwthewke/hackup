module Hackup.Driver.DryRun (dryRunDriver) where

import           Control.Error
import           Control.Lens       hiding (Action)
import           System.FilePath

import           Hackup.Action
import           Hackup.Driver.Core

dryRunDriver :: Action -> EitherT String IO [String]
dryRunDriver = driver runCommand runArchive

runCommand :: Command -> EitherT String IO [String]
runCommand cmd = return [
  "run '" ++ cmd ^. command ++ "' in " ++ cmd ^. workingDir
  ]

runArchive :: Archive -> EitherT String IO [String]
runArchive arch = do (baseDir, files) <- selectArchiveFiles arch
                     name <- archiveName arch
                     return $ describeArchive ((arch ^. targetDirectory) </> name) baseDir files (arch ^. keepPrevious)
  where describeArchive name baseDir files keep =
          ("Archive " ++ show (length files) ++ " file(s) from " ++ baseDir ++ " to " ++ name) :
            map ("  " ++) files ++ ["Keep " ++ show keep ++ " previous archives"]

