module Shared.Msg exposing (Msg(..))

import Data.Settings exposing (DataDump)
import TaskPort


type Msg
    = NoOp
      -- | DbInitialized (TaskPort.Result Bool)
    | ImportData DataDump
    | Error (Maybe String)
    | GotInitSyncReq Data.Settings.Sync
    | GotInitSyncRes (TaskPort.Result Data.Settings.Sync)
