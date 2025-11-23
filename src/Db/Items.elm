module Db.Items exposing (..)

import Dict exposing (Dict)


type ItemMarkedAs
    = InBasket
    | ToBuy


type ItemState
    = Stuffed
    | Required


type Quantity
    = Quantity Int String


type alias Item =
    { id : Int
    , name : String
    , quantity : Quantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe Char
    , state : ItemState
    }


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
          )
        , ( 2
          , Item
                2
                "Бананы"
                (Quantity 6 "штук")
                Nothing
                "бананы"
                (Just '🍌')
                Stuffed
          )
        , ( 3
          , Item
                3
                "Яблоки"
                (Quantity 4 "штук")
                (Just "Если большие и красивые")
                "Яблоки"
                (Just '🍏')
                Stuffed
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
          )
        ]
