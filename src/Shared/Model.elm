module Shared.Model exposing (Model)

import Data.Settings exposing (AppSettings)


type alias Model =
    { settings : AppSettings
    , titlePrefix : String
    , error : Maybe String
    }
