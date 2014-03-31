module Hackup.Action where

import Hackup.Types
import Hackup.Config

data Action = Action {} deriving (Show, Eq)

planActions :: Config -> Try [[Action]]
planActions _ = failWith "Failed explicitly at planActions"

