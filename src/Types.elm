module Types exposing (..)

import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Set exposing (Set)


type Draft
    = New Items.Item
    | Existing Items.Item
    | NewCat Cats.Category
    | ExistingCat Cats.Category
    | Empty


type CheckboxKind
    = Plus
    | Check


type ItemField
    = Name
    | QCount
    | QUnit
    | Comment
    | Symbol


type alias CollapsedCats =
    Dict String (Set String)
