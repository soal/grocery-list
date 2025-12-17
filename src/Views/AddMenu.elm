module Views.AddMenu exposing (view)

import Html exposing (Html, menu)
import Html.Attributes exposing (class)


view : Html msg
view =
    menu [ class "add-menu" ] []
