module Db.Items exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Time


type alias Image =
    { url : String
    , alt : String
    }


type DraftState
    = Editing
    | StandBy


type ItemState
    = Stuffed
    | Required
    | InBasket


stringToItemState : String -> ItemState
stringToItemState stateStr =
    case stateStr of
        "stuffed" ->
            Stuffed

        "required" ->
            Required

        "in-basket" ->
            InBasket

        _ ->
            Stuffed


itemStateToString : ItemState -> String
itemStateToString state =
    case state of
        Stuffed ->
            "stuffed"

        Required ->
            "required"

        InBasket ->
            "in-basket"


encodeState : ItemState -> JE.Value
encodeState state =
    state |> itemStateToString |> JE.string


type ItemQuantity
    = ItemQuantity Float String


getCount : ItemQuantity -> Float
getCount (ItemQuantity count _) =
    count


getUnit : ItemQuantity -> String
getUnit (ItemQuantity _ unit) =
    unit


quantityDecoder : JD.Decoder ItemQuantity
quantityDecoder =
    JD.map2
        ItemQuantity
        (JD.field "count" JD.float)
        (JD.field "unit" JD.string)


encodeQuantity : ItemQuantity -> JE.Value
encodeQuantity quantity =
    JE.object
        [ ( "count", JE.float <| getCount quantity )
        , ( "unit", JE.string <| getUnit quantity )
        ]


type alias Item =
    { id : String
    , name : String
    , quantity : ItemQuantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe String
    , state : ItemState
    , created : Time.Posix
    , updated : Time.Posix
    }


itemDecoder : JD.Decoder Item
itemDecoder =
    JD.map7
        Item
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "quantity" quantityDecoder)
        (JD.field "comment" <| JD.maybe JD.string)
        (JD.field "slug" JD.string)
        (JD.field "symbol" <| JD.maybe JD.string)
        (JD.field "state" <| JD.map stringToItemState JD.string)
        |> JD.andThen
            (\partial ->
                JD.map2 partial
                    (JD.field "created" <| JD.map Time.millisToPosix JD.int)
                    (JD.field "updated" <| JD.map Time.millisToPosix JD.int)
            )


encodeItem : Item -> JE.Value
encodeItem item =
    JE.object
        [ ( "id", JE.string item.id )
        , ( "name", JE.string item.name )
        , ( "quantity", encodeQuantity item.quantity )
        , ( "comment", JEE.maybe JE.string item.comment )
        , ( "slug", JE.string item.slug )
        , ( "symbol", JEE.maybe JE.string item.symbol )
        , ( "state", encodeState item.state )
        , ( "created", JE.int <| Time.posixToMillis item.created )
        , ( "updated", JE.int <| Time.posixToMillis item.updated )
        ]


updateItemState : Dict String Item -> String -> ItemState -> Dict String Item
updateItemState allItems id state =
    Dict.update id
        (Maybe.map (\found -> { found | state = state }))
        allItems


updateItem : Dict String Item -> Item -> Dict String Item
updateItem allItems item =
    Dict.update item.id
        (Maybe.map (always item))
        allItems


setItemId : String -> Item -> Item
setItemId id draft =
    { draft | id = id }
