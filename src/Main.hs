module Main where

import Prelude hiding (mapM_)
import Data.Foldable

import Hackup.Types
import Hackup.Opts
import Hackup.Config
import Hackup.Action
import Hackup.Driver

main :: IO()
main = doOrDoNot $ do opts <- cmdOpts 
                      config <- readConfig $ configFile opts
                      actions <- generalizeTry $ planActions config
                      mapM_ (runActions opts) actions
