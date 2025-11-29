module Db.Categories exposing
    ( Category
    , CollapsedState(..)
    , Msg(..)
    , decoder
    , encode
    )

import Json.Decode as JD
import Json.Encode as JE
import Time


type Msg
    = GotCollapsedChange String Int CollapsedState


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


collapsedStateToString : CollapsedState -> String
collapsedStateToString state =
    case state of
        Open ->
            "open"

        Collapsed ->
            "collapsed"


type alias Category =
    { id : Int
    , name : String
    , items : List String
    , created : Time.Posix
    , updated : Time.Posix
    }


decoder : JD.Decoder Category
decoder =
    JD.map5
        Category
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)
        (JD.field "items" <| JD.list JD.string)
        (JD.field "created" <| JD.map Time.millisToPosix JD.int)
        (JD.field "updated" <| JD.map Time.millisToPosix JD.int)


encode : Category -> JE.Value
encode cat =
    JE.object
        [ ( "id", JE.int cat.id )
        , ( "name", JE.string cat.name )
        , ( "items", JE.list JE.string cat.items )
        , ( "created", JE.int <| Time.posixToMillis cat.created )
        , ( "updated", JE.int <| Time.posixToMillis cat.updated )
        ]
