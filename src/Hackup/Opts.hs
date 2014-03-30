module Hackup.Opts where

import Hackup.Types

data Opts = Opts { configFile :: String
                 } deriving (Show, Eq)

cmdOpts :: TryT IO Opts
cmdOpts = undefined

