module Data.Categories exposing
    ( Category
    , CollapsedState(..)
    , Id
    , add
    , addItem
    , alter
    , decoder
    , delete
    , deleteStored
    , emptyCategory
    , encode
    , getByid
    , removeItem
    , sortItemsByFreq
    , store
    )

import Data.Items as Items
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Task
import TaskPort
import Time


type alias Id =
    String


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
    { id : Id
    , name : String
    , items : List String
    , created : Time.Posix
    , updated : Time.Posix
    }


decoder : JD.Decoder Category
decoder =
    JD.map5
        Category
        (JD.field "id" JD.string)
        (JD.field "name" JD.string)
        (JD.field "items" <| JD.list JD.string)
        (JD.field "created" <| JD.map Time.millisToPosix JD.int)
        (JD.field "updated" <| JD.map Time.millisToPosix JD.int)


encode : Category -> JE.Value
encode cat =
    JE.object
        [ ( "id", JE.string cat.id )
        , ( "name", JE.string cat.name )
        , ( "items", JE.list JE.string cat.items )
        , ( "created", JE.int <| Time.posixToMillis cat.created )
        , ( "updated", JE.int <| Time.posixToMillis cat.updated )
        ]


emptyCategory : Maybe Id -> Category
emptyCategory maybeId =
    Category
        (Maybe.withDefault "empty" maybeId)
        ""
        []
        (Time.millisToPosix 0)
        (Time.millisToPosix 0)


getByid : List Category -> Id -> Maybe Category
getByid categories catId =
    categories
        |> List.filter (\cat -> cat.id == catId)
        |> List.head


sortItemsByFreq : Dict Items.Id Items.Item -> Category -> Category
sortItemsByFreq items category =
    { category
        | items =
            List.sortBy
                (\itemId ->
                    case Dict.get itemId items of
                        Just item ->
                            -item.frequency

                        Nothing ->
                            0
                )
                category.items
    }


add : List Category -> Category -> List Category
add categories newCat =
    newCat :: categories


alter : List Category -> Category -> List Category
alter categories category =
    List.map
        (\cat ->
            if cat.id == category.id then
                category

            else
                cat
        )
        categories


addItem : Items.Id -> Category -> Category
addItem itemId category =
    { category | items = List.append category.items [ itemId ] }


removeItem : Items.Id -> Category -> Category
removeItem itemId category =
    if List.member itemId category.items then
        { category | items = List.filter (\id -> id /= itemId) category.items }

    else
        category


store : (TaskPort.Result Bool -> msg) -> Category -> Cmd msg
store onResult category =
    let
        call : Category -> TaskPort.Task Bool
        call =
            TaskPort.call
                { function = "storeCategory"
                , valueDecoder = JD.bool
                , argsEncoder = encode
                }
    in
    Task.attempt onResult <| call category


delete : Id -> List Category -> List Category
delete catId categories =
    List.filter (\{ id } -> id /= catId) categories


deleteStored : (TaskPort.Result Bool -> msg) -> Id -> Cmd msg
deleteStored onResult categoryId =
    let
        call : Id -> TaskPort.Task Bool
        call =
            TaskPort.call
                { function = "deleteCategory"
                , valueDecoder = JD.bool
                , argsEncoder = JE.string
                }
    in
    Task.attempt onResult <| call categoryId
