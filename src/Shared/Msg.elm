module Shared.Msg exposing (Msg(..))

import Db.Settings exposing (DataDump)
import TaskPort


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
    | ImportData DataDump
    | Error (Maybe String)
