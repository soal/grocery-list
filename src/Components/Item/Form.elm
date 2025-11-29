module Components.Item.Form exposing (..)

import Db.Items as Items
import Html exposing (Html, b, button, div, h1, i, input, p, span, text, textarea)
import Html.Attributes exposing (attribute, class, classList, id, name, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Extra exposing (nothing)
import LucideIcons as Icons


type FieldMode
    = ViewMode
    | EditMode


type FieldName
    = Name (Maybe String)
    | QCount (Maybe Float)
    | QUnit (Maybe String)
    | Comment (Maybe String)
    | Symbol (Maybe String)


type ItemField
    = ItemField FieldName FieldMode


itemFields : List ItemField
itemFields =
    [ ItemField (Name Nothing) ViewMode
    , ItemField (QCount Nothing) ViewMode
    , ItemField (QUnit Nothing) ViewMode
    , ItemField (Comment Nothing) ViewMode
    , ItemField (Symbol Nothing) ViewMode
    ]


type alias Model =
    List ItemField


alter :
    (ItemField -> ItemField)
    -> ItemField
    -> List ItemField
    -> List ItemField
alter mapper field fields =
    List.map
        (\existing ->
            if existing == field then
                mapper field

            else
                existing
        )
        fields


allToView : List ItemField -> List ItemField
allToView fields =
    List.map
        (\field ->
            case field of
                ItemField fieldName _ ->
                    ItemField fieldName ViewMode
        )
        fields


alterMode : FieldMode -> ItemField -> ItemField
alterMode mode field =
    case field of
        ItemField fieldName _ ->
            ItemField fieldName mode


alterContent : Maybe String -> ItemField -> ItemField
alterContent data field =
    case field of
        ItemField fieldName mode ->
            case fieldName of
                Name _ ->
                    ItemField (Name data) mode

                QUnit _ ->
                    ItemField (QUnit data) mode

                QCount _ ->
                    ItemField (QCount (Maybe.andThen String.toFloat data)) mode

                Comment _ ->
                    ItemField (Comment data) mode

                Symbol _ ->
                    ItemField (Symbol data) mode


type Msg
    = StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | UpdateField ItemField (Maybe String)


viewDraftButton : Html msg
viewDraftButton =
    button
        [ class "add-item-button outline"

        -- , onClick (DraftOpened category nameFieldId)
        ]
        [ Icons.plusCircleIcon [] ]


viewField : Items.Item -> ItemField -> Html Msg
viewField item field =
    case field of
        ItemField (Name _) _ ->
            viewFieldWrap [ "item-name-field" ] (viewName field item.id item.name)

        ItemField (Comment _) _ ->
            viewFieldWrap [] (viewComment field item.id item.comment)

        ItemField (QCount _) _ ->
            viewFieldWrap
                [ "item-page-quantity", "item-quantity", "unit" ]
                (viewQCount field item.id item.quantity)

        ItemField (QUnit _) _ ->
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


viewName : ItemField -> String -> String -> Html Msg
viewName field itemId sharedName =
    case field of
        ItemField (Name maybeName) EditMode ->
            let
                fieldData =
                    Maybe.withDefault sharedName maybeName
            in
            span [ class "input-resize-containter" ]
                [ span
                    [ class "input-size-helper"
                    , attribute "aria-hidden" "true"
                    ]
                    [ text fieldData ]
                , input
                    [ type_ "text"
                    , value fieldData
                    , onInput (Just >> UpdateField field)
                    , onBlur (FinishEditing field)
                    , name <| "item-name-" ++ itemId
                    , id <| "item-name-" ++ itemId
                    ]
                    []
                ]

        _ ->
            h1
                [ onClick (StartEditing field <| Just sharedName)
                ]
                [ text sharedName ]


viewComment : ItemField -> String -> Maybe String -> Html Msg
viewComment field itemId existing =
    case field of
        ItemField (Comment comment) EditMode ->
            textarea
                [ value (Maybe.withDefault "" comment)
                , onInput (Just >> UpdateField field)
                , onBlur (FinishEditing field)
                , id <| "item-comment-" ++ itemId
                ]
                []

        _ ->
            case existing of
                Just comment ->
                    p [ onClick (StartEditing field existing) ]
                        [ i [] [ text comment ] ]

                Nothing ->
                    p []
                        [ i [] [ text "Добавить комментарий" ] ]


viewQUnit : ItemField -> String -> Items.Quantity -> Html Msg
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
                , onInput (Just >> UpdateField field)
                , onBlur (FinishEditing field)
                , name <| "item-quantity-unit-" ++ itemId
                , id <| "item-quantity-unit-" ++ itemId
                ]
                []

        _ ->
            case existing of
                Items.Quantity _ unit ->
                    span [ onClick (StartEditing field (Just unit)) ]
                        [ text unit ]


viewQCount : ItemField -> String -> Items.Quantity -> Html Msg
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
                , onInput (Just >> UpdateField field)
                , onBlur (FinishEditing field)
                , name <| "item-quantity-count-" ++ itemId
                , id <| "item-quantity-count-" ++ itemId
                ]
                []

        _ ->
            case existing of
                Items.Quantity count _ ->
                    b
                        [ count
                            |> String.fromFloat
                            |> Just
                            |> StartEditing field
                            |> onClick
                        ]
                        [ text (String.fromFloat count) ]
