module Hackup.Driver where

import Control.Monad.Trans
import Hackup.Types
import Hackup.Opts
import Hackup.Action

runAction :: Opts -> Action -> TryIO [String]
runAction = undefined


runActions :: Opts ->[Action] -> TryIO ()
runActions _ [] = return ()
runActions opt (h : t) = do o <- runAction opt h
                            _ <- mapM_ (lift . putStrLn) o
                            runActions opt t
