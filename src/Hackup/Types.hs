{-# LANGUAGE Rank2Types #-}
 
module Hackup.Types (module Hackup.Types, Control.Monad.Trans.Error.ErrorT(ErrorT)) where

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
doOrDoNot = void . runErrorT . flip catchError (lift . putStrLn)

tryT :: (Monad m) => Either String a -> TryT m a
tryT = ErrorT . return

failWith :: Monad m => String -> forall a. TryT m a
failWith = tryT . Left

