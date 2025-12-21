module Data.Items exposing
    ( Id
    , Item
    , ItemError(..)
    , Quantity(..)
    , State(..)
    , ValidationResult(..)
    , alter
    , decoder
    , delete
    , emptyItem
    , encode
    , filterByStates
    , getInBasketLength
    , incFrequency
    , isAllDone
    , map
    , queryBySlug
    , setAllStuffed
    , setState
    , setUpdated
    , store
    , storeAll
    , validate
    , validationMessage
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Json.Encode.Extra as JEE
import Task
import TaskPort
import Time


type alias Id =
    String


type State
    = Stuffed
    | Required
    | InBasket


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
    { id : Id
    , name : String
    , quantity : Quantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe String
    , state : State
    , created : Time.Posix
    , updated : Time.Posix
    , frequency : Int
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
                JD.map3 partial
                    (JD.field "created" <| JD.map Time.millisToPosix JD.int)
                    (JD.field "updated" <| JD.map Time.millisToPosix JD.int)
                    (JD.field "frequency" JD.int)
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
        , ( "frequency", JE.int item.frequency )
        , ( "created", JE.int <| Time.posixToMillis item.created )
        , ( "updated", JE.int <| Time.posixToMillis item.updated )
        ]


emptyItem : Maybe Id -> Item
emptyItem maybeId =
    Item
        (Maybe.withDefault "empty" maybeId)
        ""
        (Quantity 1 "штук")
        Nothing
        ""
        Nothing
        Required
        (Time.millisToPosix 0)
        (Time.millisToPosix 0)
        0


map : (Item -> a) -> Dict Id Item -> Dict Id a
map fn items =
    Dict.map (\_ v -> fn v) items


setState : State -> Id -> Dict Id Item -> Dict Id Item
setState state id allItems =
    Dict.update id
        (Maybe.map <| switchState state [])
        allItems


incFrequency : Id -> Dict Id Item -> Dict Id Item
incFrequency id allItems =
    Dict.update id
        (Maybe.map <| \item -> { item | frequency = item.frequency + 1 })
        allItems


setUpdated : Dict Id Item -> Id -> Time.Posix -> Dict Id Item
setUpdated allItems id timestamp =
    Dict.update id
        (Maybe.map <| \item -> { item | updated = timestamp })
        allItems


alter : Dict Id Item -> Item -> Dict Id Item
alter allItems item =
    Dict.insert item.id
        item
        allItems


switchState : State -> List State -> Item -> Item
switchState newState oldStates item =
    if List.isEmpty oldStates then
        { item | state = newState }

    else if List.member item.state oldStates then
        { item | state = newState }

    else
        item


setAllStuffed : Dict Id Item -> Dict Id Item
setAllStuffed items =
    map
        (switchState Stuffed [ InBasket ])
        items


store : (TaskPort.Result Bool -> msg) -> Item -> Cmd msg
store onResult item =
    let
        call : Item -> TaskPort.Task Bool
        call =
            TaskPort.call
                { function = "storeItem"
                , valueDecoder = JD.bool
                , argsEncoder = encode
                }
    in
    Task.attempt onResult <| call item


delete : (TaskPort.Result Bool -> msg) -> Id -> Cmd msg
delete onResult itemId =
    let
        call : String -> TaskPort.Task Bool
        call =
            TaskPort.call
                { function = "deleteItem"
                , valueDecoder = JD.bool
                , argsEncoder = JE.string
                }
    in
    Task.attempt onResult <| call itemId


storeAll :
    (TaskPort.Result Bool -> msg)
    -> Dict Id Item
    -> Cmd msg
storeAll onResult items =
    let
        call : Dict String Item -> TaskPort.Task Bool
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
        call : String -> TaskPort.Task Item
        call =
            TaskPort.call
                { function = "queryBySlug"
                , valueDecoder = decoder
                , argsEncoder = JE.string
                }
    in
    Task.attempt onResult <| call slug


getInBasketLength : Dict Id Item -> Int
getInBasketLength items =
    Dict.size (filterByStates items [ InBasket ])


isAllDone : Dict Id Item -> Bool
isAllDone items =
    not (Dict.isEmpty items) && Dict.size items <= getInBasketLength items


filterByStates : Dict Id Item -> List State -> Dict Id Item
filterByStates items states =
    Dict.filter (\_ { state } -> List.member state states) items


type ItemError
    = NameIsEmpty
    | QuantityIsZero
    | NameAlreadyExist


type ValidationResult
    = ValidationOk
    | ValidationError ItemError


validate : Item -> Dict Id Item -> ValidationResult
validate item items =
    let
        name =
            String.trim item.name

        isZero (Quantity count _) =
            count <= 0

        isDuplicate =
            let
                filtered =
                    Dict.filter
                        (\_ value ->
                            String.toLower value.name == String.toLower name
                        )
                        items
            in
            if Dict.size filtered == 0 then
                False

            else
                not (Dict.member item.id filtered)
    in
    if String.isEmpty name then
        ValidationError NameIsEmpty

    else if isZero item.quantity then
        ValidationError QuantityIsZero

    else if isDuplicate then
        ValidationError NameAlreadyExist

    else
        ValidationOk


validationMessage : ItemError -> String
validationMessage error =
    case error of
        NameIsEmpty ->
            "Покупке нужно имя"

        NameAlreadyExist ->
            "Такая покупка уже есть"

        QuantityIsZero ->
            "Количество должно быть больше 0"
