module Hackup.Driver(runActions) where

import           Control.Error

import           Hackup.Action
import           Hackup.Driver.Core     (flush)
import qualified Hackup.Driver.DryRun   as DryRun
import qualified Hackup.Driver.IODriver as IODriver
import           Hackup.Errors
import           Hackup.Opts

-- TODO move to Driver.Core? or move Driver.Core.driver here?

runAction :: Opts -> Action -> EitherT String IO [String]
runAction opt act =
  if dryRun opt
    then DryRun.dryRunDriver act
    else IODriver.ioDriver act


runActions :: Opts -> [Action] -> EitherT String IO [String]
runActions _ [] = return []
runActions opt (h : t) = do _ <- flush (catchIOException . putStrLn) $ runAction opt h
                            runActions opt t
