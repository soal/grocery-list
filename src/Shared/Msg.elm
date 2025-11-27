module Shared.Msg exposing (Msg(..))

import Db.Categories exposing (CollapsedState)
import Db.Items exposing (Item, ItemState)
import Db.Settings exposing (CatsAndItems, DataDump)
import TaskPort


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
    | ItemStateUpdated Item ItemState
    | CatCollapsedStateUpdate String Int CollapsedState
    | ItemUpdated Item
    | EndShopping
    | ImportData DataDump
    | LoadInitial CatsAndItems
    | GotUuid (TaskPort.Result String)
    | DraftUpdated Item
    | DraftSaving Item
    | Error (Maybe String)
