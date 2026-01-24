module Main.Model exposing (Model)

import Common
    exposing
        ( Draft(..)
        , FormState(..)
        , ItemField(..)
        , SyncSettingsField(..)
        , VisibilityState(..)
        )
import Data.Categories as Cats
import Data.Items as Items
import Dict exposing (Dict)
import Set exposing (Set)


type alias Model =
    { draft : Draft
    , collapsedCats : Set Cats.Id
    , catWithDraft : Maybe Cats.Id
    , items : Dict Items.Id Items.Item
    , categories : List Cats.Category
    , titlePrefix : String
    , error : Maybe String
    }
