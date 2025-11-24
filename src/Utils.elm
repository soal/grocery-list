module Utils exposing (..)

import Db.Categories exposing (Category, CollapsedState(..))
import Dict exposing (Dict)
import Set exposing (Set)
import Shared.Model


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


getCatStateForPage : String -> Shared.Model.CollapsedCats -> Category -> CollapsedState
getCatStateForPage pageName collapsedMap category =
    if
        Set.member category.id
            (getCollapsesCatsForPage pageName collapsedMap)
            == True
    then
        Collapsed

    else
        Open


getCollapsesCatsForPage : String -> Shared.Model.CollapsedCats -> Set Int
getCollapsesCatsForPage pageName collapsedMap =
    Maybe.withDefault Set.empty (Dict.get pageName collapsedMap)
