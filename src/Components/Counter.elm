module Components.Counter exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class, classList)


view : Dict String stateT -> List String -> stateT -> Html msg
view idsAndStates listOfIds desiredState =
    let
        inAskedState : Int
        inAskedState =
            listOfIds
                |> List.filter
                    (\id -> Dict.get id idsAndStates == Just desiredState)
                |> List.length

        total : Int
        total =
            listOfIds
                |> List.filter (\id -> Dict.member id idsAndStates)
                |> List.length
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
