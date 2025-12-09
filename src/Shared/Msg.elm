module Shared.Msg exposing (Msg(..))

import Data.Settings exposing (DataDump)
import TaskPort


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
    | ImportData DataDump
    | Error (Maybe String)
