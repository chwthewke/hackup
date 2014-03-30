module Hackup.Config where

import Hackup.Types

data Config = Config {} deriving (Show, Eq)

readConfig :: FilePath -> TryIO Config
readConfig = undefined

