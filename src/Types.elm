module Types exposing (CheckboxKind(..), DomId, Draft(..), FormState(..), ItemField(..))

import Db.Categories as Cats
import Db.Items as Items


type FormState
    = Static
    | Form


type alias DomId =
    String


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
