module Hackup.Config (module Hackup.Config.Types,
                      Hackup.Config.readConfig) where


import Hackup.Types
import Hackup.Config.Types
import Hackup.Config.Parser
import qualified Data.ByteString as ByteString
import Control.Monad

readConfig :: FilePath -> TryIO Config
readConfig = ErrorT . liftM parseConfig . ByteString.readFile



 
