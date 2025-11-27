module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute
    , pushRoutePath, replaceRoutePath
    , loadExternalUrl, back
    , map, toCmd
    , endShopping, exportData, importData, initDb, queryAll, requestUuid, storeAllItems, storeDump, storeItem, updateCatCollapsedState, updateDraft, updateItem, updateItemState
    )

{-|

@docs Effect

@docs none, batch
@docs sendCmd, sendMsg

@docs pushRoute, replaceRoute
@docs pushRoutePath, replaceRoutePath

@docs loadExternalUrl, back

@docs map, toCmd

-}

import Browser.Navigation
import Db.Categories exposing (CollapsedState, categoryDec, encodeCategory)
import Db.Items exposing (Item, ItemState(..), encodeItem, itemDecoder)
import Db.Settings exposing (CatsAndItems, DataDump, dumpDecoder, encodeDump)
import Dict exposing (Dict)
import File.Download
import Json.Decode as JD
import Json.Encode as JE
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import TaskPort
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
    | Back
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- INTEROP
    | InitDb (TaskPort.Result Bool -> msg)
    | QueryAllCatsAndItems (TaskPort.Result CatsAndItems -> msg)
    | RequestUuid (TaskPort.Result String -> msg)
    | StoreItem (TaskPort.Result Bool -> msg) Item
    | StoreAllItems (TaskPort.Result Bool -> msg) (Dict String Item)
    | StoreDump (TaskPort.Result Bool -> msg) DataDump
    | ExportData



-- INIT DB


initDb : (TaskPort.Result Bool -> msg) -> Effect msg
initDb onResult =
    InitDb onResult


initDbEffect :
    Shared.Model.Model
    -> (Result TaskPort.Error Bool -> msg)
    -> Cmd msg
initDbEffect shared onResult =
    let
        params =
            { name = shared.dbConfig.name
            , version = shared.dbConfig.version
            }

        encoder args =
            JE.object
                [ ( "name", JE.string args.name )
                , ( "version", JE.int args.version )
                ]

        call =
            TaskPort.call
                { function = "initDb"
                , valueDecoder = JD.bool
                , argsEncoder = encoder
                }
    in
    Task.attempt onResult <| call params



-- QUERY ALL CATS AND ITEMS


queryAll :
    (TaskPort.Result CatsAndItems -> msg)
    -> Effect msg
queryAll onResult =
    QueryAllCatsAndItems onResult


queryAllEffect : (TaskPort.Result CatsAndItems -> msg) -> Cmd msg
queryAllEffect onResult =
    let
        valueDecoder =
            JD.map2
                CatsAndItems
                (JD.field "categories" <| JD.list categoryDec)
                (JD.field "items" <| JD.dict itemDecoder)

        call =
            TaskPort.callNoArgs
                { function = "queryAllCatsAndItems"
                , valueDecoder = valueDecoder
                }
    in
    Task.attempt onResult call



-- DATA STORING


storeItem : (TaskPort.Result Bool -> msg) -> Item -> Effect msg
storeItem onResult item =
    StoreItem onResult item


storeItemEffect : (TaskPort.Result Bool -> msg) -> Item -> Cmd msg
storeItemEffect onResult item =
    let
        call =
            TaskPort.call
                { function = "storeItem"
                , valueDecoder = JD.bool
                , argsEncoder = encodeItem
                }
    in
    Task.attempt onResult <| call item


storeAllItems :
    (TaskPort.Result Bool -> msg)
    -> Dict String Item
    -> Effect msg
storeAllItems onResult items =
    StoreAllItems onResult items


storeAllItemsEffect :
    (TaskPort.Result Bool -> msg)
    -> Dict String Item
    -> Cmd msg
storeAllItemsEffect onResult items =
    let
        call =
            TaskPort.call
                { function = "storeAllItems"
                , valueDecoder = JD.bool
                , argsEncoder = JE.dict identity encodeItem
                }
    in
    Task.attempt onResult <| call items


storeDump : (TaskPort.Result Bool -> msg) -> DataDump -> Effect msg
storeDump onResult dump =
    StoreDump onResult dump


storeDumpEffect : (TaskPort.Result Bool -> msg) -> DataDump -> Cmd msg
storeDumpEffect onResult dump =
    let
        call =
            TaskPort.call
                { function = "storeDump"
                , valueDecoder = JD.bool
                , argsEncoder = encodeDump
                }
    in
    Task.attempt onResult <| call dump


requestUuid : (TaskPort.Result String -> msg) -> Effect msg
requestUuid onResult =
    RequestUuid onResult


requestUuidEffect : (TaskPort.Result String -> msg) -> Cmd msg
requestUuidEffect onResult =
    let
        call =
            TaskPort.callNoArgs
                { function = "getUuid"
                , valueDecoder = JD.string
                }
    in
    Task.attempt onResult <| call



-- EXPORT AND IMPORT


exportData : Effect msg
exportData =
    ExportData


exportDataEffect : DataDump -> Cmd msg
exportDataEffect data =
    JE.object
        [ ( "version", JE.int data.version )
        , ( "categories", JE.list encodeCategory data.categories )
        , ( "items", JE.dict identity encodeItem data.items )
        ]
        |> JE.encode 2
        |> File.Download.string
            "grocery-list-backup.json"
            "application/json"


importData : String -> Effect msg
importData content =
    case JD.decodeString dumpDecoder content of
        Ok parsed ->
            SendSharedMsg (Shared.Msg.ImportData (Debug.log "PARSED DUMP" parsed))

        Err err ->
            JD.errorToString err
                |> Just
                |> Shared.Msg.Error
                |> SendSharedMsg



-- SHARED UPDATES


updateItemState : Item -> ItemState -> Effect msg
updateItemState item state =
    SendSharedMsg (Shared.Msg.ItemStateUpdated item state)


updateItem : Item -> Effect msg
updateItem item =
    SendSharedMsg (Shared.Msg.ItemUpdated item)


updateCatCollapsedState : String -> Int -> CollapsedState -> Effect msg
updateCatCollapsedState pagePath catId state =
    SendSharedMsg (Shared.Msg.CatCollapsedStateUpdate pagePath catId state)


endShopping : Effect msg
endShopping =
    SendSharedMsg Shared.Msg.EndShopping


updateDraft : Item -> Effect msg
updateDraft item =
    SendSharedMsg (Shared.Msg.DraftUpdated item)



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Same as `Effect.pushRoute`, but without `query` or `hash` support
-}
pushRoutePath : Route.Path.Path -> Effect msg
pushRoutePath path =
    PushUrl (Route.Path.toString path)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Same as `Effect.replaceRoute`, but without `query` or `hash` support
-}
replaceRoutePath : Route.Path.Path -> Effect msg
replaceRoutePath path =
    ReplaceUrl (Route.Path.toString path)


{-| Redirect users to a new URL, somewhere external to your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl


{-| Navigate back one page
-}
back : Effect msg
back =
    Back



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        Back ->
            Back

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        InitDb onResult ->
            InitDb (\res -> fn <| onResult res)

        QueryAllCatsAndItems onResult ->
            QueryAllCatsAndItems (\res -> fn <| onResult res)

        RequestUuid onResult ->
            RequestUuid (\res -> fn <| onResult res)

        StoreItem onResult item ->
            StoreItem (\res -> fn <| onResult res) item

        StoreAllItems onResult items ->
            StoreAllItems (\res -> fn <| onResult res) items

        ExportData ->
            ExportData

        StoreDump onResult dump ->
            StoreDump (\res -> fn <| onResult res) dump


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        Back ->
            Browser.Navigation.back options.key 1

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        InitDb onResult ->
            initDbEffect options.shared onResult

        QueryAllCatsAndItems onResult ->
            queryAllEffect onResult

        RequestUuid onResult ->
            requestUuidEffect onResult

        StoreItem onResult item ->
            storeItemEffect onResult item

        StoreAllItems onResult items ->
            storeAllItemsEffect onResult items

        ExportData ->
            exportDataEffect <|
                DataDump
                    options.shared.dbConfig.version
                    options.shared.items
                    options.shared.categories

        StoreDump onResult dump ->
            storeDumpEffect onResult dump
