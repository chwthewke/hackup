module Hackup.Driver.Core where

import           Control.Error
import           Control.Lens     hiding (Action)
import           Data.Time.Clock
import           Data.Time.Format
import           System.FilePath
import           System.Locale

import           Hackup.Action
import           Hackup.Errors
import           Hackup.Selectors (FileSelector, runFileSelector)


driver :: (Command -> a) -> (Archive -> a) -> Action -> a
driver runCmd _ (CommandAction cmd) = runCmd cmd
driver _ runArch (ArchiveAction arch) = runArch arch

selectArchiveFiles :: Archive -> EitherT String IO (FilePath, [FilePath])
selectArchiveFiles arch = do relativeFiles <- mapM (selectItemFiles baseDir) (arch ^. archiveItems)
                             return (baseDir, concat relativeFiles)
  where baseDir = normalise (arch ^. archiveItemsBaseDir)

selectItemFiles :: FilePath -> FileSelector -> EitherT String IO [FilePath]
selectItemFiles baseDir selector = catchIOException $ runFileSelector selector baseDir

archiveName :: Archive -> EitherT String IO FilePath
archiveName arch = do ct <- catchIOException getCurrentTime
                      return $ arch ^. baseName ++ "-" ++ formatTime defaultTimeLocale "%FT%H_%M_%S" ct ++ ".tar.gz"

flush :: (String -> EitherT String IO ()) -> EitherT String IO [String] -> EitherT String IO [String]
flush prt buf = do messages <- buf
                   _ <- mapM_ prt messages
                   return []
