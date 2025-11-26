module Db.Settings exposing (..)

import Db.Categories exposing (Category, categoryDec, encodeCategory)
import Db.Items exposing (Item, encodeItem, itemDecoder)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


type alias CatsAndItems =
    { categories : List Category
    , items : Dict String Item
    }


type alias DataDump =
    { version : Int
    , items : Dict String Item
    , categories : List Category
    }


type AppTheme
    = Auto
    | Light
    | Dark


type alias AppSettings =
    { theme : AppTheme
    }


settingsDec : Decoder AppSettings
settingsDec =
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
        , ( "items", JE.dict identity encodeItem dump.items )
        , ( "categories", JE.list encodeCategory dump.categories )
        ]


dumpDecoder : Decoder DataDump
dumpDecoder =
    JD.map3
        DataDump
        (JD.field "version" JD.int)
        (JD.field "items" <| JD.dict itemDecoder)
        (JD.field "categories" <| JD.list categoryDec)
