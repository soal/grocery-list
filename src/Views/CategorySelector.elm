module Views.CategorySelector exposing (view)

import Data.Categories as Cats
import Data.Items as Items
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Attributes.Extra exposing (attributeMaybe)
import Html.Events.Extra as EE
import Maybe.Extra exposing (unwrap)


view :
    Maybe Cats.Category
    -> Items.Id
    -> List Cats.Category
    -> (Maybe Cats.Category -> Cats.Id -> msg)
    -> Html msg
view currentCat itemId allCats onSelect =
    H.node "category-selector"
        []
        [ H.label
            [ Attr.for ("cat-select" ++ itemId)
            , Attr.attribute "area-label" "Выбор категории"
            ]
            []
        , H.select
            [ Attr.name "categories"
            , Attr.id ("cat-select" ++ itemId)
            , EE.onChange (onSelect currentCat)
            ]
          <|
            viewFirst currentCat
                :: viewRest allCats currentCat
        ]


viewFirst : Maybe Cats.Category -> Html msg
viewFirst currentCat =
    H.option
        [ attributeMaybe (\cat -> Attr.value cat.id) currentCat
        , Attr.disabled True
        , Attr.selected True
        ]
        [ H.text <| unwrap "Без категории" .name currentCat ]


viewRest : List Cats.Category -> Maybe Cats.Category -> List (Html msg)
viewRest cats current =
    current
        |> Maybe.map .id
        |> Maybe.map (\id_ -> List.filter (\{ id } -> id_ /= id) cats)
        |> Maybe.withDefault cats
        |> List.map (\cat -> H.option [ Attr.value cat.id ] [ H.text cat.name ])
