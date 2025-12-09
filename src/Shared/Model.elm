module Shared.Model exposing
    ( DbConfig
    , DbStatus(..)
    , Model
    )

import Data.Settings exposing (AppSettings)


type DbStatus
    = DbInitial
    | DbReady
    | DbError


type alias DbConfig =
    { name : String
    , version : Int
    , status : DbStatus
    }


type alias Model =
    { settings : AppSettings
    , dbConfig : DbConfig
    , titlePrefix : String
    , error : Maybe String
    }
