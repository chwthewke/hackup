{-# LANGUAGE TupleSections #-}

module Hackup.Driver.IODriver{--(ioDriver)--} where

import qualified Codec.Archive.Tar    as Tar
--import           Control.Applicative
import           Control.Error
import           Control.Lens         hiding (Action)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.Text
import           Data.List            (sortBy)
import qualified Data.Text            as T
import           System.Directory
import           System.Exit
import           System.FilePath      ((</>))
import           System.Process       (StdStream (Inherit), createProcess, cwd,
                                       shell, std_err, std_out, waitForProcess)

import           Hackup.Action
import           Hackup.Driver.Core
import           Hackup.Errors

ioDriver :: Action -> EitherT String IO [String]
ioDriver = driver runCommand runArchive

runCommand :: Command -> EitherT String IO [String]
runCommand cmd = do _        <- lift . putStrLn $ "[" ++ cmd ^. workingDir ++ "] " ++ cmd ^. command
                    exitCode <- catchIOException runProcess
                    case exitCode of ExitSuccess   -> return []
                                     ExitFailure n -> left (cmd ^. command ++ " exited with code " ++ show n)
  where commandProcess = (shell (cmd ^. command)){ cwd = Just (cmd ^. workingDir)
                                                 , std_out = Inherit
                                                 , std_err = Inherit
                                                 }
        runProcess = do (_, _, _, ph) <- createProcess commandProcess
                        waitForProcess ph

runArchive :: Archive -> EitherT String IO [String]
runArchive arch = do archiveOut <- runArchiveOnly arch
                     rotateOut <- runRotate arch
                     return $ archiveOut ++ rotateOut

runArchiveOnly :: Archive -> EitherT String IO [String]
runArchiveOnly arch = do (baseDir, files) <- selectArchiveFiles arch
                         name <- archiveName arch
                         let fullName = archiveFullName name
                         _ <- catchIOException $ mkArchive fullName baseDir files
                         return [ "Created archive " ++ fullName ++ " with " ++ show (length files) ++ " files." ]
  where mkArchive fullName baseDir files = do _ <- createDirectoryIfMissing True archiveDir
                                              Tar.create fullName baseDir files
        archiveDir = arch ^. targetDirectory
        archiveFullName name = archiveDir </> name

runRotate :: Archive -> EitherT String IO [String]
runRotate arch = catchIOException $
                   do entriesInDir <- getDirectoryContents archiveDir
                      filesInDir <- filterM (doesFileExist . (archiveDir </>)) entriesInDir
                      let archiveFiles = filter (hasNamePattern $ arch ^. baseName) filesInDir
                      let archivesToDelete = drop keep . sortBy (flip compare) $ archiveFiles
                      mapM (deleteArchive . (archiveDir </>)) archivesToDelete
  where archiveDir = arch ^. targetDirectory
        keep = fromIntegral $ arch ^. keepPrevious
        deleteArchive f = do removeFile f
                             return ("Removed previous archive " ++ f)

namePatternParser :: String -> Parser ()
namePatternParser archBaseName = do _ <- string $ T.pack archBaseName
                                    _ <- char '-'
                                    _ <- count 4 digit
                                    _ <- char '-'
                                    _ <- count 2 digit
                                    _ <- char '-'
                                    _ <- count 2 digit
                                    _ <- char 'T'
                                    _ <- count 2 digit
                                    _ <- char '_'
                                    _ <- count 2 digit
                                    _ <- char '_'
                                    _ <- count 2 digit
                                    _ <- string $ T.pack ".tar.gz"
                                    return ()

hasNamePattern :: String -> FilePath -> Bool
hasNamePattern archName f = isRight . parseOnly (namePatternParser archName) $ T.pack f
