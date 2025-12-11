module Data.Settings exposing
    ( AppSettings
    , CatsAndItems
    , DataDump
    , Sync(..)
    , SyncState(..)
    , decoder
    , defaultSettings
    , dumpDecoder
    , encodeDump
    , initSync
    )

import Data.Categories as Cats
import Data.Items as Items
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Task
import TaskPort


type alias CatsAndItems =
    { categories : List Cats.Category
    , items : Dict String Items.Item
    }


type alias DataDump =
    { version : Int
    , items : Dict String Items.Item
    , categories : List Cats.Category
    }


type AppTheme
    = Auto
    | Light
    | Dark


type Sync
    = NotConfigured
    | SyncConfig
        { room : String
        , url : String
        }


syncSettingsDecoder : JD.Decoder Sync
syncSettingsDecoder =
    JD.oneOf
        [ JD.map
            SyncConfig
          <|
            JD.map2
                (\room url -> { room = room, url = url })
                (JD.field "room" JD.string)
                (JD.field "url" JD.string)
        , JD.null NotConfigured
        ]


encodeSyncSetting : Sync -> JE.Value
encodeSyncSetting settings =
    JE.object
        [ ( "room", JE.string <| .room <| getSyncData settings )
        , ( "url", JE.string <| .url <| getSyncData settings )
        ]


getSyncData : Sync -> { room : String, url : String }
getSyncData settings =
    case settings of
        SyncConfig { room, url } ->
            { room = room, url = url }

        NotConfigured ->
            { room = "", url = "" }


type SyncState
    = None
    | SyncReady
    | Syncing
    | Synced
    | SyncError


stringToSyncState : String -> SyncState
stringToSyncState stateStr =
    case stateStr of
        "none" ->
            None

        "syncReady" ->
            SyncReady

        "syncing" ->
            Syncing

        "synced" ->
            Synced

        "error" ->
            SyncError

        _ ->
            None


type alias AppSettings =
    { theme : AppTheme
    , sync : Sync
    , syncState : SyncState
    , version : Int
    }


defaultSettings : AppSettings
defaultSettings =
    AppSettings
        Auto
        NotConfigured
        None
        1


decoder : JD.Decoder AppSettings
decoder =
    JD.map4
        AppSettings
        (JD.field "theme" <| JD.map stringToTheme JD.string)
        (JD.field "sync" <| syncSettingsDecoder)
        (JD.field "syncState" <|
            JD.oneOf
                [ JD.map stringToSyncState JD.string, JD.null None ]
        )
        (JD.field "version" JD.int)


stringToTheme : String -> AppTheme
stringToTheme themeStr =
    case themeStr of
        "auto" ->
            Auto

        "light" ->
            Light

        "dark" ->
            Dark

        _ ->
            Auto


encodeDump : DataDump -> JE.Value
encodeDump dump =
    JE.object
        [ ( "version", JE.int dump.version )
        , ( "items", JE.dict identity Items.encode dump.items )
        , ( "categories", JE.list Cats.encode dump.categories )
        ]


dumpDecoder : JD.Decoder DataDump
dumpDecoder =
    JD.map3
        DataDump
        (JD.field "version" JD.int)
        (JD.field "items" <| JD.dict Items.decoder)
        (JD.field "categories" <| JD.list Cats.decoder)


initSync : (Result TaskPort.Error Sync -> msg) -> Sync -> Cmd msg
initSync onResult settings =
    let
        call : Sync -> TaskPort.Task Sync
        call =
            TaskPort.call
                { function = "initSync"
                , valueDecoder = syncSettingsDecoder
                , argsEncoder = encodeSyncSetting
                }
    in
    Task.attempt onResult <| call settings
