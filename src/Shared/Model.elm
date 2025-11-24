module Shared.Model exposing (DbConfig, DbStatus(..), Model)

import Db.Categories exposing (Category)
import Db.Items exposing (Item)
import Db.Settings exposing (..)
import Dict exposing (Dict)


type DbStatus
    = DbInitial
    | DbReady
    | DbError


type alias DbConfig =
    { name : String
    , version : Int
    , status : DbStatus
    }


type alias InitData =
    { settings : AppSettings
    , items : Dict String Item
    , categories : List Category
    }


type alias Model =
    { settings : AppSettings
    , dbConfig : DbConfig
    , items : Dict String Item
    , categories : List Category
    }
