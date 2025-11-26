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


stateEncoder : ItemState -> JE.Value
stateEncoder state =
    state |> itemStateToString |> JE.string


type ItemQuantity
    = ItemQuantity Float String


quantityDec : JD.Decoder ItemQuantity
quantityDec =
    JD.map2
        (\number unit -> ItemQuantity number unit)
        (JD.field "count" JD.float)
        (JD.field "unit" JD.string)


quantityEncoder : ItemQuantity -> JE.Value
quantityEncoder quantity =
    case quantity of
        ItemQuantity count unit ->
            JE.object
                [ ( "count", JE.float count )
                , ( "unit", JE.string unit )
                ]


type alias Item =
    { id : Int
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
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)
        (JD.field "quantity" quantityDec)
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


itemEncoder : Item -> JE.Value
itemEncoder item =
    JE.object
        [ ( "id", JE.int item.id )
        , ( "name", JE.string item.name )
        , ( "quantity", quantityEncoder item.quantity )
        , ( "comment", JEE.maybe JE.string item.comment )
        , ( "slug", JE.string item.slug )
        , ( "symbol", JEE.maybe JE.string item.symbol )
        , ( "state", stateEncoder item.state )
        , ( "created", JE.int <| Time.posixToMillis item.created )
        , ( "updated", JE.int <| Time.posixToMillis item.updated )
        ]


updateItemState : Dict String Item -> Int -> ItemState -> Dict String Item
updateItemState allItems id state =
    Dict.update (String.fromInt id)
        (Maybe.map (\found -> { found | state = state }))
        allItems


updateItem : Dict String Item -> Item -> Dict String Item
updateItem allItems item =
    Dict.update (String.fromInt item.id)
        (Maybe.map (always item))
        allItems


items : Dict String Item
items =
    Dict.fromList
        [ ( "1"
          , Item
                1
                "Хлеб"
                (ItemQuantity 1 "батон")
                (Just "лучше побольше")
                "хлеб"
                Nothing
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( "2"
          , Item
                2
                "Бананы"
                (ItemQuantity 6 "штук")
                Nothing
                "бананы"
                (Just "🍌")
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( "3"
          , Item
                3
                "Яблоки"
                (ItemQuantity 4 "штук")
                (Just "Если большие и красивые")
                "Яблоки"
                (Just "🍏")
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( "4"
          , Item
                4
                "Томатный соус"
                (ItemQuantity 1 "банка")
                (Just
                    """
                    Если есть в Пятёрочке, а ещё лучше вообще зашоплифитить бесплатно,
                    они там вобще офанарели
                    """
                )
                "томатный-соус"
                Nothing
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( "5"
          , Item
                5
                "Какое-нибудь мясо"
                (ItemQuantity 500 "г.")
                Nothing
                "мясо"
                Nothing
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        ]
