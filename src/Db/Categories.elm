module Db.Categories exposing (..)


type CollapsedState
    = Open
    | Collapsed


type alias Category =
    { id : Int
    , name : String
    , items : List Int
    , state : CollapsedState
    }


categories : List Category
categories =
    [ Category 1 "Бакалея" [ 1, 4 ] Open
    , Category 2 "Фрукты и овощи" [ 2, 3 ] Open
    , Category 3 "Прочее" [ 5 ] Open
    ]
