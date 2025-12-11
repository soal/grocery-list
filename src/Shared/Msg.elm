module Shared.Msg exposing (Msg(..))

import Data.Settings exposing (DataDump)
import TaskPort


type Msg
    = NoOp
    | ImportData DataDump
    | Error (Maybe String)
    | GotInitSyncReq Data.Settings.Sync
    | GotInitSyncRes (TaskPort.Result Data.Settings.Sync)
    | GotSyncStatus Data.Settings.SyncState
    | GotRefreshSyncState
