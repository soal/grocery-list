module Utils exposing (..)

import Db.Categories exposing (CollapsedState(..))
import Dict
import Set exposing (Set)
import Types exposing (CollapsedCats)
import Url.Builder


getCollapsesCatsForPage : String -> CollapsedCats -> Set String
getCollapsesCatsForPage pageName collapsedMap =
    Maybe.withDefault Set.empty (Dict.get pageName collapsedMap)


slugify : String -> String
slugify str =
    Url.Builder.relative [ str ] []
        |> String.trim
        |> String.toLower
        |> String.replace " " "-"


