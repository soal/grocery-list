module Db.Items exposing
    ( Item
    , Msg(..)
    , Quantity(..)
    , State(..)
    , alter
    , decoder
    , encode
    , isAllDone
    , queryBySlug
    , setAllStuffed
    , setId
    , setState
    , store
    , storeAll
    , getInBasketLength
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Task
import TaskPort
import Time


type alias Image =
    { url : String
    , alt : String
    }


type State
    = Stuffed
    | Required
    | InBasket


type Msg
    = GotNewState Item State
    | GotChange Item
    | GotAllBought


stringToState : String -> State
stringToState stateStr =
    case stateStr of
        "stuffed" ->
            Stuffed

        "required" ->
            Required

        "in-basket" ->
            InBasket

        _ ->
            Stuffed


stateToString : State -> String
stateToString state =
    case state of
        Stuffed ->
            "stuffed"

        Required ->
            "required"

        InBasket ->
            "in-basket"


encodeState : State -> JE.Value
encodeState state =
    state |> stateToString |> JE.string


type Quantity
    = Quantity Float String


getCount : Quantity -> Float
getCount (Quantity count _) =
    count


getUnit : Quantity -> String
getUnit (Quantity _ unit) =
    unit


quantityDecoder : JD.Decoder Quantity
quantityDecoder =
    JD.map2
        Quantity
        (JD.field "count" JD.float)
        (JD.field "unit" JD.string)


encodeQuantity : Quantity -> JE.Value
encodeQuantity quantity =
    JE.object
        [ ( "count", JE.float <| getCount quantity )
        , ( "unit", JE.string <| getUnit quantity )
        ]


type alias Item =
    { id : String
    , name : String
    , quantity : Quantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe String
    , state : State
    , created : Time.Posix
    , updated : Time.Posix
    }


decoder : JD.Decoder Item
decoder =
    JD.map7
        Item
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "quantity" quantityDecoder)
        (JD.field "comment" <| JD.maybe JD.string)
        (JD.field "slug" JD.string)
        (JD.field "symbol" <| JD.maybe JD.string)
        (JD.field "state" <| JD.map stringToState JD.string)
        |> JD.andThen
            (\partial ->
                JD.map2 partial
                    (JD.field "created" <| JD.map Time.millisToPosix JD.int)
                    (JD.field "updated" <| JD.map Time.millisToPosix JD.int)
            )


encode : Item -> JE.Value
encode item =
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


map : (String -> Item -> Item) -> Dict String Item -> Dict String Item
map fn items =
    Dict.map fn items


setState : Dict String Item -> String -> State -> Dict String Item
setState allItems id state =
    Dict.update id
        (Maybe.map (\found -> { found | state = state }))
        allItems


alter : Dict String Item -> Item -> Dict String Item
alter allItems item =
    Dict.update item.id
        (Maybe.map (always item))
        allItems


setId : String -> Item -> Item
setId id draft =
    { draft | id = id }


setAllStuffed : Dict String Item -> Dict String Item
setAllStuffed items =
    map
        (\_ item ->
            if item.state == InBasket then
                { item | state = Stuffed }

            else
                item
        )
        items


store : (TaskPort.Result Bool -> msg) -> Item -> Cmd msg
store onResult item =
    let
        call =
            TaskPort.call
                { function = "storeItem"
                , valueDecoder = JD.bool
                , argsEncoder = encode
                }
    in
    Task.attempt onResult <| call item


storeAll :
    (TaskPort.Result Bool -> msg)
    -> Dict String Item
    -> Cmd msg
storeAll onResult items =
    let
        call =
            TaskPort.call
                { function = "storeAllItems"
                , valueDecoder = JD.bool
                , argsEncoder = JE.dict identity encode
                }
    in
    Task.attempt onResult <| call items


queryBySlug : (TaskPort.Result Item -> msg) -> String -> Cmd msg
queryBySlug onResult slug =
    let
        call =
            TaskPort.call
                { function = "queryBySlug"
                , valueDecoder = decoder
                , argsEncoder = JE.string
                }
    in
    Task.attempt onResult <| call slug


getInBasketLength : Dict String Item -> Int
getInBasketLength items =
    Dict.values items
        |> List.filter (\item -> item.state == InBasket)
        |> List.length


isAllDone : Dict String Item -> Bool
isAllDone items =
    let
        statesLength =
            Dict.size items
    in
    statesLength > 0 && statesLength <= getInBasketLength items
