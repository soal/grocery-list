module Db.Items exposing (..)

import Dict exposing (Dict)
import Json.Decode as D exposing (decodeValue)
import Time


type ItemMarkedAs
    = InBasket
    | ToBuy


type ItemState
    = Stuffed
    | Required


stringToItemState : String -> ItemState
stringToItemState stateStr =
    case stateStr of
        "stuffed" ->
            Stuffed

        "required" ->
            Required

        _ ->
            Stuffed


type Quantity
    = Quantity Int String


quantityDec : D.Decoder Quantity
quantityDec =
    D.map2
        (\number unit -> Quantity number unit)
        (D.field "count" D.int)
        (D.field "unit" D.string)


type alias Item =
    { id : Int
    , name : String
    , quantity : Quantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe String
    , state : ItemState
    , created : Time.Posix
    , updated : Time.Posix
    }


itemDec : D.Decoder Item
itemDec =
    D.map7
        Item
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "quantity" quantityDec)
        (D.field "comment" <| D.maybe D.string)
        (D.field "slug" D.string)
        (D.field "symbol" <| D.maybe D.string)
        (D.field "state" <| D.map stringToItemState D.string)
        |> D.andThen
            (\partial ->
                D.map2 partial
                    (D.field "created" <| D.map Time.millisToPosix D.int)
                    (D.field "updated" <| D.map Time.millisToPosix D.int)
            )


items : Dict Int Item
items =
    Dict.fromList
        [ ( 1
          , Item
                1
                "Хлеб"
                (Quantity 1 "батон")
                (Just "лучше побольше")
                "хлеб"
                Nothing
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( 2
          , Item
                2
                "Бананы"
                (Quantity 6 "штук")
                Nothing
                "бананы"
                (Just "🍌")
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( 3
          , Item
                3
                "Яблоки"
                (Quantity 4 "штук")
                (Just "Если большие и красивые")
                "Яблоки"
                (Just "🍏")
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        , ( 4
          , Item
                4
                "Томатный соус"
                (Quantity 1 "банка")
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
        , ( 5
          , Item
                5
                "Какое-нибудь мясо"
                (Quantity 500 "г.")
                Nothing
                "мясо"
                Nothing
                Stuffed
                (Time.millisToPosix 10)
                (Time.millisToPosix 10)
          )
        ]
