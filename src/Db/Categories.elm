module Db.Categories exposing
    ( Category
    , CollapsedState(..)
    , addItem
    , decoder
    , encode
    , getByid
    , removeItem
    , sortItemsByFreq
    , store
    )

import Db.Items as Items
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Task
import TaskPort
import Time


type CollapsedState
    = Open
    | Collapsed



-- stringToCollapsedState : String -> CollapsedState
-- stringToCollapsedState stateStr =
--     case stateStr of
--         "open" ->
--             Open
--         "collapsed" ->
--             Collapsed
--         _ ->
--             Open
-- collapsedStateToString : CollapsedState -> String
-- collapsedStateToString state =
--     case state of
--         Open ->
--             "open"
--         Collapsed ->
--             "collapsed"


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


getByid : List Category -> Int -> Maybe Category
getByid categories catId =
    categories
        |> List.filter (\cat -> cat.id == catId)
        |> List.head


sortItemsByFreq : Dict String Items.Item -> Category -> Category
sortItemsByFreq items category =
    { category
        | items =
            List.sortBy
                (\itemId ->
                    case Dict.get itemId items of
                        Just item ->
                            0 - item.frequency

                        Nothing ->
                            0
                )
                category.items
    }


addItem : Int -> List Category -> String -> List Category
addItem catId categories itemId =
    List.map
        (\cat ->
            if cat.id == catId then
                { cat | items = List.append cat.items [ itemId ] }

            else
                cat
        )
        categories


removeItem : String -> Category -> Category
removeItem itemId category =
    if List.member itemId category.items then
        { category | items = List.filter (\id -> id /= itemId) category.items }

    else
        category


store : (TaskPort.Result Bool -> msg) -> Category -> Cmd msg
store onResult category =
    let
        call =
            TaskPort.call
                { function = "storeCategory"
                , valueDecoder = JD.bool
                , argsEncoder = encode
                }
    in
    Task.attempt onResult <| call category
