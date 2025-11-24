module Db.Categories exposing (..)

import Json.Decode as D
import Time


type CollapsedState
    = Open
    | Collapsed


stringToCollapsedState : String -> CollapsedState
stringToCollapsedState stateStr =
    case stateStr of
        "open" ->
            Open

        "collapsed" ->
            Collapsed

        _ ->
            Open


type alias Category =
    { id : Int
    , name : String
    , items : List Int
    , state : CollapsedState
    , created : Time.Posix
    , updated : Time.Posix
    }


categoryDec : D.Decoder Category
categoryDec =
    D.map6
        Category
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "items" <| D.list D.int)
        (D.field "state" <| D.map stringToCollapsedState D.string)
        (D.field "created" <| D.map Time.millisToPosix D.int)
        (D.field "updated" <| D.map Time.millisToPosix D.int)


categories : List Category
categories =
    [ Category 1
        "Бакалея"
        [ 1, 4 ]
        Open
        (Time.millisToPosix 10)
        (Time.millisToPosix 10)
    , Category
        2
        "Фрукты и овощи"
        [ 2, 3 ]
        Open
        (Time.millisToPosix 10)
        (Time.millisToPosix 10)
    , Category 3
        "Прочее"
        [ 5 ]
        Open
        (Time.millisToPosix 10)
        (Time.millisToPosix 10)
    ]
