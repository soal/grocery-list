module Main.Msg exposing (Msg(..))

import Common exposing (Draft, ItemField)
import Data.Items as Items
import Data.Settings exposing (CatsAndItems)
import TaskPort
import Time
import Views.Items.List


type Msg
    = NoOp
    | Error (Maybe String)
    | GotCatsAndItems CatsAndItems
    | GotClickOutside
    | GotItemUuid (TaskPort.Result String)
      -- ITEM LIST
    | GotItemListMsg Views.Items.List.Msg
    | GotDraftUpdateTime Draft Time.Posix
      -- CATEGORIES
    | GotCatAddClick
    | GotCatUuid (TaskPort.Result String)
      -- ITEM WITHOUT CATEGORY
    | GotItemAddClick
    | GotInput ItemField String
    | GotItemDeleteClick Items.Id
    | GotItemStateUpdateTime Items.Item Time.Posix
    | GotEnterKey
    | GotEscKey
