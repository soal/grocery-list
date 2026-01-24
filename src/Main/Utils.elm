module Main.Utils exposing (onTaskPortResult)

import Main.Msg exposing (Msg(..))
import TaskPort


onTaskPortResult : TaskPort.Result res -> Msg
onTaskPortResult res =
    case res of
        Err _ ->
            Error Nothing

        Ok _ ->
            NoOp
