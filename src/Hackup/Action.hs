module Hackup.Action where

import           Hackup.Config
import           Hackup.Types

data Action = Action {} deriving (Show, Eq)

planActions :: Config -> Try [[Action]]
planActions _ = failWith "Failed explicitly at planActions"

