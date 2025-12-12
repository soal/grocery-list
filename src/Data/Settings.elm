module Data.Settings exposing
    ( AppSettings
    , CatsAndItems
    , DataDump
    , decoder
    , defaultSettings
    , dumpDecoder
    , encodeDump
    , initSync
    , parseSyncErr
    )

import Data.Categories as Cats
import Data.Items as Items
import Data.Sync as Sync
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


type alias AppSettings =
    { theme : AppTheme
    , sync : Sync.Sync
    , version : Int
    }


defaultSettings : AppSettings
defaultSettings =
    AppSettings
        Auto
        (Sync.Sync Sync.NotConfigured Sync.None)
        1


decoder : JD.Decoder AppSettings
decoder =
    JD.map3
        AppSettings
        (JD.field "theme" <| JD.map stringToTheme JD.string)
        (JD.field "sync" Sync.decoder)
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


initSync : (Result TaskPort.Error Sync.Config -> msg) -> Sync.Config -> Cmd msg
initSync onResult settings =
    let
        call : Sync.Config -> TaskPort.Task Sync.Config
        call =
            TaskPort.call
                { function = "initSync"
                , valueDecoder = Sync.configDecoder
                , argsEncoder = Sync.encodeConfig
                }
    in
    Task.attempt onResult <| call settings


parseSyncErr : TaskPort.Error -> String
parseSyncErr err =
    case err of
        TaskPort.InteropError error ->
            TaskPort.interopErrorToString error

        TaskPort.JSError (TaskPort.ErrorObject "Error" { message }) ->
            message

        TaskPort.JSError _ ->
            "General connection error"
