module Data.Settings exposing
    ( AppSettings
    , CatsAndItems
    , DataDump
    , decoder
    , defaultSettings
    , dumpDecoder
    , encodeDump
    )

import Data.Categories as Cats
import Data.Items as Items
import Data.Sync as Sync
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE


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
