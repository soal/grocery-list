module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute
    , pushRoutePath, replaceRoutePath
    , loadExternalUrl, back
    , map, toCmd
    , CatsAndItems, initDb, queryAll
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
import Db.Categories exposing (Category, categoryDec)
import Db.Items exposing (Item, itemDec)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import TaskPort
import Url exposing (Url)
import Utils exposing (convertKeys)


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


type alias CatsAndItems =
    { categories : List Category
    , items : Dict Int Item
    }



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
            E.object
                [ ( "name", E.string args.name )
                , ( "version", E.int args.version )
                ]

        call =
            TaskPort.call
                { function = "initDb"
                , valueDecoder = D.bool
                , argsEncoder = encoder
                }
    in
    Task.attempt onResult <| call params



-- QUERY ALL CATS AND ITEMS


queryAll :
    (TaskPort.Result
        { categories : List Category, items : Dict Int Item }
     -> msg
    )
    -> Effect msg
queryAll onResult =
    QueryAllCatsAndItems onResult


queryAllEffect : (Result TaskPort.Error CatsAndItems -> msg) -> Cmd msg
queryAllEffect onResult =
    let
        valueDecoder =
            D.map2
                CatsAndItems
                (D.field "categories" <| D.list categoryDec)
                (D.field "items" <| D.map convertKeys <| D.dict itemDec)

        call =
            TaskPort.callNoArgs
                { function = "queryAllCatsAndItems"
                , valueDecoder = valueDecoder
                }
    in
    Task.attempt onResult call



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
            InitDb (\res -> fn (onResult res))

        QueryAllCatsAndItems onResult ->
            QueryAllCatsAndItems (\res -> fn (onResult res))


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
