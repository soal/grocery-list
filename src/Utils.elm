module Utils exposing (..)

import Db.Categories exposing (Category, CollapsedState(..))
import Dict exposing (Dict)
import Set exposing (Set)
import Slug
import Types exposing (CollapsedCats)


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


keysAsStrings : Dict Int v -> Dict String v
keysAsStrings someDict =
    Dict.toList someDict
        |> List.indexedMap
            (\_ ( k, v ) -> ( String.fromInt k, v ))
        |> Dict.fromList


getCatStateForPage : String -> CollapsedCats -> Category -> CollapsedState
getCatStateForPage pageName collapsedMap category =
    if
        Set.member category.id
            (getCollapsesCatsForPage pageName collapsedMap)
            == True
    then
        Collapsed

    else
        Open


getCollapsesCatsForPage : String -> CollapsedCats -> Set String
getCollapsesCatsForPage pageName collapsedMap =
    Maybe.withDefault Set.empty (Dict.get pageName collapsedMap)


slugify : String -> String
slugify str =
    str
        |> Slug.generate
        |> Maybe.map Slug.toString
        |> Maybe.withDefault ""
