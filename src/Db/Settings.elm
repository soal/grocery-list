module Db.Settings exposing (AppSettings, AppTheme(..), CatsAndItems, DataDump, decoder, dumpDecoder, encodeDump)

import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
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
    }


decoder : Decoder AppSettings
decoder =
    JD.map
        AppSettings
        (JD.field "theme" <| JD.map stringToTheme JD.string)


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


dumpDecoder : Decoder DataDump
dumpDecoder =
    JD.map3
        DataDump
        (JD.field "version" JD.int)
        (JD.field "items" <| JD.dict Items.decoder)
        (JD.field "categories" <| JD.list Cats.decoder)
