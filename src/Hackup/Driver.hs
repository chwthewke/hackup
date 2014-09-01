module Hackup.Driver(runActions) where

import           Control.Error
import           Control.Monad.Trans

import           Hackup.Action
import qualified Hackup.Driver.DryRun as DryRun
import           Hackup.Opts


runAction :: Opts -> Action -> EitherT String IO [String]
runAction opt act = 
  if dryRun opt 
    then DryRun.runAction act 
    else left "Failed explicitly at runAction"

runActions :: Opts -> [Action] -> EitherT String IO ()
runActions _ [] = return ()
runActions opt (h : t) = do o <- runAction opt h
                            _ <- mapM_ (lift . putStrLn) o
                            runActions opt t
