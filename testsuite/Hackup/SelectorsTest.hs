{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Hackup.SelectorsTest where

import           Distribution.System     (OS (..), buildOS)
import           System.Directory.Layout
import           System.FilePath
import           System.IO.Temp
import           Test.Framework

import           Hackup.Selectors

{-# ANN module "HLint: ignore Use camelCase" #-}

testLayout1 :: Layout
testLayout1 = do
        directory "a" $ file_ "one.txt" >> file_ "two.jpg"
        directory "b" $ do
          directory "c" $ return ()
          directory "d" $ file_ "three.txt"
          file_ "four.jpg"
        file_ "five.txt"

exerciseSelector :: (FilePath -> IO [FilePath]) -> Layout -> ([FilePath] -> IO r) -> IO r
exerciseSelector sel layout f =
  withSystemTempDirectory "testtmp" (\ base -> make layout base >> sel base >>= f)

testSelector :: (a -> FileSelector) -> a -> Layout -> [FilePath] -> IO ()
testSelector mkSel pat layout = exerciseSelector (runFileSelector . mkSel $ pat) layout . assertEqual

testGlob :: String -> Layout -> [FilePath] -> IO ()
testGlob = testSelector Glob

test_globSelectsSubDir :: IO ()
test_globSelectsSubDir = testGlob "a/**" testLayout1 [ "a" </> "one.txt", "a" </> "two.jpg" ]

test_globSelectsSubDirWithBackslash :: IO ()
test_globSelectsSubDirWithBackslash = testGlob "a\\**" testLayout1 [ "a" </> "one.txt", "a" </> "two.jpg" ]

test_globSelectsByExtension :: IO ()
test_globSelectsByExtension = testGlob "**/*.txt" testLayout1 [ "a" </> "one.txt", "b" </> "d" </> "three.txt", "five.txt" ]

test_globSelectsEmptyDirectories :: IO ()
test_globSelectsEmptyDirectories = testGlob "b/**/*" testLayout1 [ "b" </> "c", "b" </> "d", "b" </> "d" </> "three.txt", "b" </> "four.jpg" ]

-- regex are non-portable, we'll have to have conditional tests

data PortableRegex = PortableRegex { win :: String
                                   , posix :: String
                                   } deriving (Show, Eq)

testRegex :: PortableRegex -> Layout -> [FilePath] -> IO ()
testRegex pr = case buildOS of Windows -> testSelector Regex (win pr)
                               _       -> testSelector Regex (posix pr)

test_regexSelectsSubdirContents :: IO ()
test_regexSelectsSubdirContents = testRegex (PortableRegex "a\\\\.*" "a/.*") testLayout1 [ "a" </> "one.txt", "a" </> "two.jpg" ]

test_regexSelectsOnFileNames :: IO ()
test_regexSelectsOnFileNames = testRegex (PortableRegex ".*\\\\t[^\\]*" ".*/t[^/]*") testLayout1 [ "a" </> "two.jpg", "b" </> "d" </> "three.txt" ]
