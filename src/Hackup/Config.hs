module Hackup.Config (module Hackup.Config.Types,
                      Hackup.Config.readConfig) where


import Hackup.Types
import Hackup.Config.Types
import Hackup.Config.Parser
import Hackup.Config.Validation

readConfig :: FilePath -> TryIO Config
readConfig f = parseConfig f >>= generalizeTry . validate



 
