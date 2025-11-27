module Shared.Model exposing
    ( CollapsedCats
    , DbConfig
    , DbStatus(..)
    , Model
    )

import Db.Categories exposing (Category)
import Db.Items exposing (Item)
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


type alias UiState =
    { lastRoute : String
    , collapsedCatsMap : CollapsedCats
    }


type alias Model =
    { settings : AppSettings
    , dbConfig : DbConfig
    , draft : Maybe Item
    , uiState : UiState
    , items : Dict String Item
    , categories : List Category
    , titlePrefix : String
    , error : Maybe String
    }
