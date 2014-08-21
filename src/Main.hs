module Main where

import           Data.Foldable
import           Prelude       hiding (mapM_)

import           Hackup.Action
import           Hackup.Config
import           Hackup.Driver
import           Hackup.Opts
import           Hackup.Types

main :: IO()
main = doOrDoNot $ do opts <- cmdOpts
                      config <- readConfig $ configFile opts
                      actions <- generalizeTry $ planActions config
                      mapM_ (runActions opts) actions
