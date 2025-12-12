port module DataUpdate exposing (incoming, onData, onSyncState, syncState)

import Data.Categories as Cats
import Data.Items as Items
import Data.Settings exposing (CatsAndItems)
import Data.Sync as Sync
import Json.Decode as JD


port incoming : (JD.Value -> msg) -> Sub msg


port syncState : (JD.Value -> msg) -> Sub msg


decodeDataUpdate : JD.Value -> Result JD.Error CatsAndItems
decodeDataUpdate =
    JD.map2
        CatsAndItems
        (JD.field "categories" <| JD.list Cats.decoder)
        (JD.field "items" <| JD.dict Items.decoder)
        |> JD.decodeValue


onData :
    (Maybe String -> msg)
    -> (CatsAndItems -> msg)
    -> JD.Value
    -> msg
onData onError msg message =
    case decodeDataUpdate message of
        Ok data ->
            msg data

        Err err ->
            onError (Just <| JD.errorToString err)


onSyncState :
    (Maybe String -> msg)
    -> (Sync.State -> msg)
    -> JD.Value
    -> msg
onSyncState onError msg state =
    case JD.decodeValue Sync.stateDecoder state of
        Ok state_ ->
            msg state_

        Err err ->
            onError (Just <| JD.errorToString err)
