module Shared.Msg exposing (Msg(..))

import Db.Categories exposing (CollapsedState)
import Db.Items exposing (ItemState)
import TaskPort


type Msg
    = NoOp
    | DbInitialized (TaskPort.Result Bool)
    | ItemStateUpdated Int ItemState
    | CatCollapsedStateUpdate String Int CollapsedState
