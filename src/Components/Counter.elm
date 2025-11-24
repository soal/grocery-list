module Components.Counter exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class, classList)


view : Dict String stateT -> List String -> stateT -> Html msg
view items listOfIds state =
    let
        inAskedState =
            listOfIds
                |> List.filter (\id -> Dict.get id items == Just state)
                |> List.length

        total =
            List.length listOfIds
    in
    span
        [ class "counter"
        , classList [ ( "all-done", inAskedState >= total ) ]
        ]
        [ text <|
            String.fromInt inAskedState
                ++ " / "
                ++ String.fromInt total
        ]
