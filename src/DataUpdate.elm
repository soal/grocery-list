port module DataUpdate exposing (incoming, on)

import Data.Categories as Cats
import Data.Items as Items
import Data.Settings exposing (CatsAndItems)
import Json.Decode as JD


port incoming : (JD.Value -> msg) -> Sub msg


decodeDataUpdate : JD.Value -> Result JD.Error CatsAndItems
decodeDataUpdate =
    JD.map2
        CatsAndItems
        (JD.field "categories" <| JD.list Cats.decoder)
        (JD.field "items" <| JD.dict Items.decoder)
        |> JD.decodeValue


on :
    (Maybe String -> msg)
    -> (CatsAndItems -> msg)
    -> JD.Value
    -> msg
on onError msg message =
    case decodeDataUpdate message of
        Ok data ->
            msg data

        Err err ->
            onError (Just <| JD.errorToString err)
