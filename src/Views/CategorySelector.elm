module Views.CategorySelector exposing (view)

import Data.Categories as Cats
import Data.Items as Items
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as E
import Html.Events.Extra as EE


view :
    Maybe Cats.Category
    -> List Cats.Category
    -> (Maybe Cats.Category -> Cats.Id -> msg)
    -> Html msg
view currentCat allCats onSelect =
    H.select [ At.name "categories", EE.onChange (onSelect currentCat) ] <|
        H.option [ At.value "" ] []
            :: List.map
                (viewOption currentCat)
                allCats


viewOption : Maybe Cats.Category -> Cats.Category -> Html msg
viewOption currentCat cat =
    H.option
        [ At.value cat.id
        , Maybe.map (\current -> current.id == cat.id) currentCat
            |> Maybe.withDefault False
            |> At.disabled
        ]
        [ H.text cat.name ]
