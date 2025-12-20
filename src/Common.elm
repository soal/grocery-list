module Common exposing
    ( AddMenuItem(..)
    , CheckboxKind(..)
    , DomId
    , Draft(..)
    , FormState(..)
    , ItemField(..)
    , SyncSettingsField(..)
    , VisibilityState(..)
    )

import Data.Categories as Cats
import Data.Items as Items


type AddMenuItem
    = AddItem
    | AddCategory


type VisibilityState
    = Show
    | Hidden


type FormState
    = Static
    | Form


type alias DomId =
    String


type Draft
    = New ( Items.Item, Items.ValidationResult )
    | Existing ( Items.Item, Items.ValidationResult )
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


type SyncSettingsField
    = Room
    | SyncUrl
