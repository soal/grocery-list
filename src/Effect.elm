module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute
    , pushRoutePath, replaceRoutePath
    , loadExternalUrl, back
    , map, toCmd
    , deleteCategory, deleteItem, getTime, importData, initSync, maybe, queryAll, refreshSyncState, reqInitSync, requestUuid, selectInput, storeAllItems, storeCategory, storeDump, storeItem
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

import Browser.Events exposing (onResize)
import Browser.Navigation
import Data.Categories as Cats
import Data.Items as Items
import Data.Settings exposing (CatsAndItems, DataDump, dumpDecoder, encodeDump)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import TaskPort
import Time
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
      -- CUSTOM
    | GetTime (Time.Posix -> msg)
    | InitSync (TaskPort.Result Data.Settings.Sync -> msg) Data.Settings.Sync
    | QueryAllCatsAndItems (TaskPort.Result CatsAndItems -> msg)
    | RequestUuid (TaskPort.Result String -> msg)
    | StoreItem (TaskPort.Result Bool -> msg) Items.Item
    | DeleteItem (TaskPort.Result Bool -> msg) String
    | StoreAllItems (TaskPort.Result Bool -> msg) (Dict String Items.Item)
    | StoreDump (TaskPort.Result Bool -> msg) DataDump
    | QueryItem (TaskPort.Result Items.Item -> msg) String
    | StoreCategory (TaskPort.Result Bool -> msg) Cats.Category
    | DeleteCategory (TaskPort.Result Bool -> msg) String
    | SelectInput (TaskPort.Result Bool -> msg) String



-- UTILS


getTime : (Time.Posix -> msg) -> Effect msg
getTime onResult =
    GetTime onResult


selectInput : (TaskPort.Result Bool -> msg) -> String -> Effect msg
selectInput onResult id =
    SelectInput onResult id


selectInputEffect : (TaskPort.Result Bool -> msg) -> String -> Cmd msg
selectInputEffect onResult id =
    Task.attempt onResult
        (TaskPort.call
            { function = "selectInput"
            , valueDecoder = JD.bool
            , argsEncoder = JE.string
            }
            id
        )



-- INIT SYNC


reqInitSync : Data.Settings.Sync -> Effect msg
reqInitSync settings =
    SendSharedMsg <| Shared.Msg.GotInitSyncReq settings


initSync :
    (TaskPort.Result Data.Settings.Sync -> msg)
    -> Data.Settings.Sync
    -> Effect msg
initSync onResult settings =
    InitSync onResult settings


refreshSyncState : Effect msg
refreshSyncState =
    SendSharedMsg Shared.Msg.GotRefreshSyncState


queryAll :
    (TaskPort.Result CatsAndItems -> msg)
    -> Effect msg
queryAll onResult =
    QueryAllCatsAndItems onResult


queryAllEffect : (TaskPort.Result CatsAndItems -> msg) -> Cmd msg
queryAllEffect onResult =
    let
        valueDecoder : JD.Decoder CatsAndItems
        valueDecoder =
            JD.map2
                CatsAndItems
                (JD.field "categories" <| JD.list Cats.decoder)
                (JD.field "items" <| JD.dict Items.decoder)

        call : TaskPort.Task CatsAndItems
        call =
            TaskPort.callNoArgs
                { function = "queryAllCatsAndItems"
                , valueDecoder = valueDecoder
                }
    in
    Task.attempt onResult call



-- DATA STORING


storeItem : (TaskPort.Result Bool -> msg) -> Items.Item -> Effect msg
storeItem onResult item =
    StoreItem onResult item


deleteItem : (TaskPort.Result Bool -> msg) -> String -> Effect msg
deleteItem onResult itemId =
    DeleteItem onResult itemId


storeAllItems :
    (TaskPort.Result Bool -> msg)
    -> Dict String Items.Item
    -> Effect msg
storeAllItems onResult items =
    StoreAllItems onResult items


storeCategory : (TaskPort.Result Bool -> msg) -> Cats.Category -> Effect msg
storeCategory onResult category =
    StoreCategory onResult category


deleteCategory : (TaskPort.Result Bool -> msg) -> String -> Effect msg
deleteCategory onResult categoryId =
    DeleteCategory onResult categoryId


storeDump : (TaskPort.Result Bool -> msg) -> DataDump -> Effect msg
storeDump onResult dump =
    StoreDump onResult dump


storeDumpEffect : (TaskPort.Result Bool -> msg) -> DataDump -> Cmd msg
storeDumpEffect onResult dump =
    let
        call : DataDump -> TaskPort.Task Bool
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
        call : TaskPort.Task String
        call =
            TaskPort.callNoArgs
                { function = "getUuid"
                , valueDecoder = JD.string
                }
    in
    Task.attempt onResult <| call



-- EXPORT AND IMPORT


importData : String -> Effect msg
importData content =
    case JD.decodeString dumpDecoder content of
        Ok parsed ->
            SendSharedMsg (Shared.Msg.ImportData parsed)

        Err err ->
            JD.errorToString err
                |> Just
                |> Shared.Msg.Error
                |> SendSharedMsg



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



-- HELPER


maybe : (a -> Effect msg) -> Maybe a -> Effect msg
maybe effect maybeCond =
    maybeCond
        |> Maybe.map effect
        |> Maybe.withDefault none



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

        -- CUSTOM
        GetTime onResult ->
            GetTime (onResult >> fn)

        InitSync onResult settings ->
            InitSync (onResult >> fn) settings

        QueryAllCatsAndItems onResult ->
            QueryAllCatsAndItems (onResult >> fn)

        QueryItem onResult slug ->
            QueryItem (onResult >> fn) slug

        RequestUuid onResult ->
            RequestUuid (onResult >> fn)

        StoreItem onResult item ->
            StoreItem (onResult >> fn) item

        DeleteItem onResult item ->
            DeleteItem (onResult >> fn) item

        StoreAllItems onResult items ->
            StoreAllItems (onResult >> fn) items

        StoreCategory onResult category ->
            StoreCategory (onResult >> fn) category

        DeleteCategory onResult categoryId ->
            DeleteCategory (onResult >> fn) categoryId

        StoreDump onResult dump ->
            StoreDump (onResult >> fn) dump

        SelectInput onResult id ->
            SelectInput (onResult >> fn) id


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

        -- CUSTOM
        GetTime onResult ->
            Task.perform onResult Time.now

        InitSync onResult settings ->
            Data.Settings.initSync onResult settings

        QueryAllCatsAndItems onResult ->
            queryAllEffect onResult

        QueryItem onResult slug ->
            Items.queryBySlug onResult slug

        RequestUuid onResult ->
            requestUuidEffect onResult

        StoreItem onResult item ->
            Items.store onResult item

        DeleteItem onResult itemId ->
            Items.delete onResult itemId

        StoreAllItems onResult items ->
            Items.storeAll onResult items

        StoreCategory onResult category ->
            Cats.store onResult category

        DeleteCategory onResult categoryId ->
            Cats.deleteStored onResult categoryId

        StoreDump onResult dump ->
            storeDumpEffect onResult dump

        SelectInput onResult id ->
            selectInputEffect onResult id
