module Db.Items exposing (..)

import Dict exposing (Dict)


type ItemState
    = InBasket
    | ToBuy


type Quantity
    = Quantity Int String


type alias Item =
    { id : Int
    , name : String
    , quantity : Quantity
    , comment : Maybe String
    , slug : String
    , symbol : Maybe Char
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
          )
        , ( 2
          , Item
                2
                "Бананы"
                (Quantity 6 "штук")
                Nothing
                "бананы"
                (Just '🍌')
          )
        , ( 3
          , Item
                3
                "Яблоки"
                (Quantity 4 "штук")
                (Just "Если большие и красивые")
                "Яблоки"
                (Just '🍏')
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
          )
        , ( 5
          , Item
                5
                "Какое-нибудь мясо"
                (Quantity 500 "г.")
                Nothing
                "мясо"
                Nothing
          )
        ]
