module Shared.Model exposing
    ( CollapsedCats
    , DbConfig
    , DbStatus(..)
    , Model
    )

import Db.Settings exposing (AppSettings)
import Dict exposing (Dict)
import Set exposing (Set)


type DbStatus
    = DbInitial
    | DbReady
    | DbError


type alias DbConfig =
    { name : String
    , version : Int
    , status : DbStatus
    }


type alias CollapsedCats =
    Dict String (Set Int)


type alias Model =
    { settings : AppSettings
    , dbConfig : DbConfig
    , titlePrefix : String
    , error : Maybe String
    }
