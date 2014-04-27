{-# LANGUAGE PackageImports #-}

module Hackup.Config.Selectors(fileSelector) where

import Control.Applicative
import System.FilePath
import System.FilePath.Find (find, filePath)
import "Glob" System.FilePath.Glob (match, compile)
import Text.Regex.TDFA ((=~))
import Hackup.Config.Types

-- | Returns relative paths inside a directory that match a predicate.
findRelative :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findRelative p basedir = map (makeRelative basedir) <$> findRelative'
  where findRelative' = find (return True) (p . makeRelative basedir <$> filePath) basedir

globSelector :: String -> FilePath -> IO [FilePath]
globSelector pat = findRelative . match $ compile pat 
          

regexSelector :: String -> FilePath -> IO [FilePath]
regexSelector = findRelative . matchPath
  where matchPath expr path = path =~ expr == path

fileSelector :: RawFileSelector -> FileSelector
fileSelector sel = FileSelector (fileSelector' sel) sel
  where fileSelector' (Glob s) = globSelector s
        fileSelector' (Regex s) = regexSelector s

 