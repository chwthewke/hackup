module Hackup.Errors (module Hackup.Errors, Control.Error.EitherT(EitherT)) where

import           Control.Error

catchIOException :: IO a -> EitherT String IO a
catchIOException = fmapLT show . tryIO 

handleErrors :: EitherT String IO () -> IO ()
handleErrors = eitherT putStrLn return
