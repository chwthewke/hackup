{-# LANGUAGE PackageImports #-}

module Hackup.Selectors(runFileSelector, FileSelector(..)) where

import           Control.Applicative
import           System.FilePath
import           System.FilePath.Find (filePath, find)
import           "Glob" System.FilePath.Glob (compile, match)
import           Text.Regex.TDFA      ((=~))

data FileSelector = Glob String | Regex String deriving (Show, Eq)


-- | Returns relative paths inside a directory that match a predicate.
findRelative :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findRelative p basedir = map (makeRelative basedir) <$> findRelative'
  where findRelative' = find (return True) (p . makeRelative basedir <$> filePath) basedir

globSelector :: String -> FilePath -> IO [FilePath]
globSelector pat = findRelative . match $ compile pat


regexSelector :: String -> FilePath -> IO [FilePath]
regexSelector = findRelative . matchPath
  where matchPath expr path = path =~ expr == path

runFileSelector :: FileSelector -> FilePath -> IO [FilePath]
runFileSelector (Glob s) = globSelector s
runFileSelector (Regex s) = regexSelector s
