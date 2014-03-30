module Hackup.Types where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Error
import Data.Functor.Identity

type TryT m = ErrorT String m 

type TryIO = TryT IO

type Try = TryT Identity

generalizeTry :: Monad m => Try a -> TryT m a
generalizeTry = hoist generalize

doOrDoNot :: TryIO () -> IO ()
doOrDoNot x = void . runErrorT . catchError x $ lift . putStrLn
