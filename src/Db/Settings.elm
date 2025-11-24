module Db.Settings exposing (..)

import Json.Decode as D exposing (Decoder)


type AppTheme
    = Auto
    | Light
    | Dark


type alias AppSettings =
    { theme : AppTheme
    }


settingsDec : Decoder AppSettings
settingsDec =
    D.map
        AppSettings
        (D.field "theme" <| D.map stringToTheme D.string)


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
