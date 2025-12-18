module Views.AddMenu exposing (view)

import Common exposing (AddMenuItem(..))
import Html exposing (Html, details, li, summary, text, ul)
import Html.Attributes exposing (class)
import Html.Attributes.Extra exposing (boolProperty)
import Html.Events exposing (onClick)
import LucideIcons as Icons


view : Bool -> msg -> msg -> Html msg
view state clickedItem clickedCat =
    details [ class "dropdown add-menu", boolProperty "open" state ]
        [ summary []
            [ Icons.plusIcon []
            ]
        , ul []
            [ li [ class "add-menu-item", onClick clickedItem ]
                [ text "Добавить покупку" ]
            , li [ class "add-menu-item", onClick clickedCat ]
                [ text "Добавить категорию" ]
            ]
        ]
