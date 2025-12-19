module Views.AddMenu exposing (view)

import Common exposing (AddMenuItem(..))
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on, onClick)
import Json.Decode as JD
import LucideIcons as Icons


view : Bool -> msg -> msg -> msg -> msg -> Html msg
view state clickedItem clickedCat clickedToggle clickHide =
    Html.node "on-click-outside"
        [ on "clickoutside" <|
            JD.succeed <|
                clickHide
        ]
        [ div
            [ class "dropdown add-menu with-click-outside"
            , onClick clickedToggle
            , classList [ ( "open", state ) ]
            ]
            [ div [ class "summary" ]
                [ Icons.plusIcon []
                ]
            , ul []
                [ li [ class "add-menu-item", onClick clickedItem ]
                    [ text "Добавить покупку" ]
                , li [ class "add-menu-item", onClick clickedCat ]
                    [ text "Добавить категорию" ]
                ]
            ]
        ]
