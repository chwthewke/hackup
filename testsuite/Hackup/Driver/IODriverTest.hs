{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Hackup.Driver.IODriverTest where

import           Control.Error
import           Control.Monad
import           Data.Foldable           (foldlM)
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           System.Directory
import           System.Directory.Layout
import           System.FilePath
import           System.IO.Temp
import           System.Locale
import           Test.Framework

import           Hackup.Action
import           Hackup.Driver.IODriver
import           Hackup.Selectors

{-# ANN module "HLint: ignore Use camelCase" #-}

srcDir :: String
srcDir = "src"

outDir :: String
outDir = "out"

sourceLayout :: Layout
sourceLayout = directory srcDir $ do
  directory "a" $ file_ "1"
  directory "b" $ file_ "2"

archiveLayout :: [(String, Integer)] -> Layout
archiveLayout = directory outDir . foldl (>>) (return ()) . map (uncurry archiveFiles)
  where archiveFiles name count = directory name . foldl (>>) (return ()) . map (file_ . archName name) $ [1..count]
        archName name i = name ++ "-" ++ time i ++ ".tar.gz"
        time n = formatTime defaultTimeLocale "%FT%H_%M_%S" $ UTCTime (fromGregorian 2014 8 20) (secondsToDiffTime $ 60 * n)

config :: Integer -> [String] -> String
config keep srcs = "\
  \{\
  \  \"rootDir\": \"" ++ outDir ++ "\",\
  \\
  \  \"keep\": " ++ show keep ++ ",\
  \\
  \" ++ intercalate "\n  ,\n" (map section srcs)

section :: String -> String
section name =
  "  " ++ show name ++ "\
  \    \"archiveName\": \"section_" ++ name ++ "\",\
  \    \"baseDir\": \"" ++ srcDir </> name ++ "\",\
  \    \"contents\": [ \"**\" ]\
  \  }"

action :: FilePath -> Integer -> FilePath -> Action
action name keep base = ArchiveAction $ Archive name (base </> outDir </> name) (base </> srcDir </> name) [Glob "**"] keep

checkArchives :: FilePath -> [(String, Int)] -> IO Bool
checkArchives dir = foldlM checkArchive True
  where checkArchive b (name, count) = do
          let target = dir </> name
          allFiles <- filterM (doesFileExist . (target </>)) =<< getDirectoryContents target
          let matchingFiles = filter (hasNamePattern name) allFiles
          return $ b && (length matchingFiles == count)


test_driverCreatesArchivesFromArchiveActions :: IO ()
test_driverCreatesArchivesFromArchiveActions =
  withSystemTempDirectory "testtmp" (\base -> do
    _ <- make sourceLayout base
    res <- runEitherT . ioDriver $ action "a" 7 base
    assertBool $ isRight res
    assertBool =<< checkArchives (base </> outDir) [("a", 1)])

test_driverRotatesArchiveFilesFromArchiveActions :: IO ()
test_driverRotatesArchiveFilesFromArchiveActions =
  withSystemTempDirectory "testtmp" (\base -> do
    _ <- make (sourceLayout >> archiveLayout [("a", 3)]) base
    res <- runEitherT . ioDriver $ action "a" 3 base
    assertBool $ isRight res
    assertBool =<< checkArchives (base </> outDir) [("a", 3)])

test_driverRotatesArchiveFilesSeparatelyBySection :: IO ()
test_driverRotatesArchiveFilesSeparatelyBySection =
  withSystemTempDirectory "testtmp" (\base -> do
    _ <- make (sourceLayout >> archiveLayout [("a", 3), ("b", 2)]) base
    resA <- runEitherT . ioDriver $ action "a" 3 base
    assertBool $ isRight resA
    resB <- runEitherT . ioDriver $ action "b" 5 base
    assertBool $ isRight resB
    assertBool =<< checkArchives (base </> outDir) [("a", 3), ("b", 3)])

