module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


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
    Dict String (Set Int)
