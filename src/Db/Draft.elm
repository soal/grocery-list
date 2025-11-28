module Db.Draft exposing (..)

import Db.Items exposing (Item)


type Msg
    = GotChange Item
    | GotSave Item


type State
    = Editing
    | StandBy
