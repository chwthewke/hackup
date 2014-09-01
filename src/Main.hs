module Main where

import           Data.Foldable
import           Prelude       hiding (mapM_)

import           Hackup.Action
import           Hackup.Config
import           Hackup.Driver
import           Hackup.Opts
import           Hackup.Errors

main :: IO()
main = handleErrors $ do opts <- cmdOpts
                         config <- readConfig $ configFile opts
                         let actions = planActions config
                         mapM_ (runActions opts) actions
