module Shared.Msg exposing (Msg(..))

import TaskPort


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
