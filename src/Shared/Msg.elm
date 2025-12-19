module Shared.Msg exposing (Msg(..))

import Data.Settings exposing (DataDump)
import Data.Sync as Sync
import TaskPort


type Msg
    = NoOp
    | ImportData DataDump
    | Error (Maybe String)
    | GotInitSyncReq Sync.Config
    | GotPauseSyncReq
    | GotResumeSyncReq
    | GotInitSyncRes (TaskPort.Result Sync.Config)
    | GotSyncStatus Sync.State
    | GotRefreshSyncState
    | GotThemeChange Data.Settings.AppTheme
