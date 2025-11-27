module Components.Item.Form exposing (..)

import Db.Items exposing (Item, ItemQuantity(..))
import Html exposing (Html, b, div, h1, i, input, p, span, text, textarea)
import Html.Attributes exposing (class, classList, id, name, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Extra exposing (nothing)
import ItemForm exposing (FieldMode(..), FieldName(..), ItemField(..))


viewField : Item -> ItemForm.ItemField -> Html ItemForm.Msg
viewField item field =
    case field of
        ItemField (Name _) _ ->
            viewFieldWrap [] (viewName field item.id item.name)

        ItemForm.ItemField (Comment _) _ ->
            viewFieldWrap [] (viewComment field item.id item.comment)

        ItemForm.ItemField (QCount _) _ ->
            viewFieldWrap
                [ "item-page-quantity", "item-quantity", "unit" ]
                (viewQCount field item.id item.quantity)

        ItemForm.ItemField (QUnit _) _ ->
            viewFieldWrap
                [ "item-page-quantity", "item-quantity", "count" ]
                (viewQUnit field item.id item.quantity)

        _ ->
            nothing


viewFieldWrap : List String -> Html msg -> Html msg
viewFieldWrap classes content =
    div
        [ class "item-page-field with-click-outside"
        , classList <| List.map (\c -> ( c, True )) classes
        ]
        [ content ]


viewName : ItemField -> String -> String -> Html ItemForm.Msg
viewName field itemId sharedName =
    case field of
        ItemField (Name maybeName) EditMode ->
            let
                fieldData =
                    Maybe.withDefault sharedName maybeName
            in
            h1 []
                [ input
                    [ type_ "text"
                    , value fieldData
                    , onInput (Just >> ItemForm.UpdateField field)
                    , onBlur (ItemForm.FinishEditing field)
                    , name <| "item-name-" ++ itemId
                    , id <| "item-name-" ++ itemId
                    ]
                    []
                ]

        _ ->
            h1
                [ onClick (ItemForm.StartEditing field <| Just sharedName)
                ]
                [ text sharedName ]


viewComment : ItemField -> String -> Maybe String -> Html ItemForm.Msg
viewComment field itemId existing =
    case field of
        ItemField (Comment comment) EditMode ->
            textarea
                [ value (Maybe.withDefault "" comment)
                , onInput (Just >> ItemForm.UpdateField field)
                , onBlur (ItemForm.FinishEditing field)
                , id <| "item-comment-" ++ itemId
                ]
                []

        _ ->
            case existing of
                Just comment ->
                    p [ onClick (ItemForm.StartEditing field existing) ]
                        [ i [] [ text comment ] ]

                Nothing ->
                    p []
                        [ i [] [ text "Добавить комментарий" ] ]


viewQUnit : ItemField -> String -> ItemQuantity -> Html ItemForm.Msg
viewQUnit field itemId existing =
    case field of
        ItemField (QUnit maybeUnit) EditMode ->
            let
                fieldData =
                    Maybe.withDefault "штук" maybeUnit
            in
            input
                [ type_ "text"
                , value fieldData
                , onInput (Just >> ItemForm.UpdateField field)
                , onBlur (ItemForm.FinishEditing field)
                , name <| "item-quantity-unit-" ++ itemId
                , id <| "item-quantity-unit-" ++ itemId
                ]
                []

        _ ->
            case existing of
                ItemQuantity _ unit ->
                    span [ onClick (ItemForm.StartEditing field (Just unit)) ]
                        [ text unit ]


viewQCount : ItemField -> String -> ItemQuantity -> Html ItemForm.Msg
viewQCount field itemId existing =
    case field of
        ItemField (QCount maybeCount) EditMode ->
            let
                fieldData =
                    String.fromFloat <| Maybe.withDefault 1 maybeCount
            in
            input
                [ type_ "number"
                , value fieldData
                , onInput (Just >> ItemForm.UpdateField field)
                , onBlur (ItemForm.FinishEditing field)
                , name <| "item-quantity-count-" ++ itemId
                , id <| "item-quantity-count-" ++ itemId
                ]
                []

        _ ->
            case existing of
                ItemQuantity count _ ->
                    b
                        [ count
                            |> String.fromFloat
                            |> Just
                            |> ItemForm.StartEditing field
                            |> onClick
                        ]
                        [ text (String.fromFloat count) ]
