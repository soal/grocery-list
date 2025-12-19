module Views.MainActionButton exposing (view)

import Html exposing (Html, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import LucideIcons as Icons


view : msg -> Html msg
view click =
    button [ class "main-action primary round", onClick click ]
        [ Icons.plusIcon [] ]
