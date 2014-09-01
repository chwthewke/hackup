module Hackup.Config (module Hackup.Config.Types,
                      Hackup.Config.readConfig) where


import           Control.Monad
import qualified Data.ByteString      as ByteString

import           Hackup.Config.Parser
import           Hackup.Config.Types
import Hackup.Errors

readConfig :: FilePath -> EitherT String IO Config
readConfig = EitherT . liftM parseConfig . ByteString.readFile
