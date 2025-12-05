module Utils exposing (..)

import Db.Categories exposing (CollapsedState(..))
import Dict
import Set exposing (Set)
import Slug
import Types exposing (CollapsedCats)


getCollapsesCatsForPage : String -> CollapsedCats -> Set String
getCollapsesCatsForPage pageName collapsedMap =
    Maybe.withDefault Set.empty (Dict.get pageName collapsedMap)


slugify : String -> String
slugify str =
    str
        |> Slug.generate
        |> Maybe.map Slug.toString
        |> Maybe.withDefault ""
