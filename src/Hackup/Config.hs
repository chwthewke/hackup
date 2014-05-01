module Hackup.Config (module Hackup.Config.Types,
                      Hackup.Config.readConfig) where


import           Control.Monad
import qualified Data.ByteString      as ByteString
import           Hackup.Config.Parser
import           Hackup.Config.Types
import           Hackup.Types

readConfig :: FilePath -> TryIO Config
readConfig = ErrorT . liftM parseConfig . ByteString.readFile
