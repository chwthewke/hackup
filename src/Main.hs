module Main where

import           Data.Foldable
import           Prelude                   hiding (mapM_)

import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Hackup.Action
import           Hackup.Config
import           Hackup.Driver
import           Hackup.Errors
import           Hackup.Opts

main :: IO()
main = handleErrors $
    do opts <- cmdOpts
       config <- readConfig $ configFile opts
       _ <- showWhenAsked opts config
       let actions = planActions config
       _ <- showWhenAsked opts actions
       mapM_ (runActions opts) actions
    where showWhenAsked opts = when (dumpConfig opts) . lift . print
