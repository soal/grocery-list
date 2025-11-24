module Shared.Msg exposing (Msg(..))

import TaskPort
import Db.Items exposing (ItemState)


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
    | ItemStateUpdated Int ItemState
