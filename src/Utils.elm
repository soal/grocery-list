module Utils exposing (..)

import Dict exposing (Dict)


convertKeys : Dict String v -> Dict Int v
convertKeys someDict =
    Dict.toList someDict
        |> List.indexedMap
            (\index ( k, v ) ->
                ( Maybe.withDefault index <| String.toInt k
                , v
                )
            )
        |> Dict.fromList
