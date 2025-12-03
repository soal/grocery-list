module Components.Items.Form exposing (..)

import Db.Items as Items
import Html exposing (Html, b, div, h4, input, span, text, textarea)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , id
        , name
        , rows
        , type_
        , value
        )
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Extra exposing (nothing)
import LucideIcons as Icons
import Svg.Attributes
import Types exposing (CheckboxKind(..), ItemField(..))


viewCheckbox : (Bool -> msg) -> Bool -> CheckboxKind -> Bool -> Html msg
viewCheckbox onCheck disabled kind checked =
    div
        [ role "checkbox"
        , attribute "aria-role" "checkbox"
        , classList
            [ ( "checked", checked )
            , ( "check", kind == Check )
            , ( "disabled", disabled )
            ]
        , onClick (onCheck <| not checked)
        ]
        [ if kind == Plus then
            Icons.plusIcon [ Svg.Attributes.strokeWidth "3" ]

          else
            Icons.checkIcon [ Svg.Attributes.strokeWidth "3" ]
        ]


viewName :
    { a
        | itemId : String
        , inputChange : Maybe (String -> msg)
        , blurred : Maybe msg
        , focused : Maybe msg
        , onOpen : ItemField -> String -> msg
        , content : String
        , static : Bool
        , open : Bool
    }
    -> Html msg
viewName ({ static, open } as config) =
    let
        fieldId =
            "item-name-" ++ config.itemId
    in
    if not static && open then
        Maybe.withDefault nothing
            (Maybe.map3
                (\i b f ->
                    viewNameField
                        { fieldId = fieldId
                        , inputChange = i
                        , blurred = b
                        , focused = f
                        , content = config.content
                        }
                )
                config.inputChange
                config.blurred
                config.focused
            )

    else
        viewNameStatic (config.onOpen Name fieldId) config.content fieldId


viewNameStatic : msg -> String -> String -> Html msg
viewNameStatic onOpen content fieldId =
    span [ id fieldId, onClick onOpen ] [ text content ]


viewNameField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , blurred : msg
        , focused : msg
        , content : String
    }
    -> Html msg
viewNameField { fieldId, inputChange, blurred, focused, content } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-name-field with-click-outside"
            , value content
            , onInput inputChange
            , onBlur blurred
            , onFocus focused
            , name fieldId
            , id fieldId
            ]
            []
        ]


viewComment :
    { a
        | itemId : String
        , inputChange : Maybe (String -> msg)
        , blurred : Maybe msg
        , focused : Maybe msg
        , onOpen : ItemField -> String -> msg
        , content : Maybe String
        , static : Bool
        , open : Bool
    }
    -> Html msg
viewComment ({ static, open } as config) =
    let
        fieldId =
            "item-comment-" ++ config.itemId
    in
    if not static && open then
        Maybe.withDefault nothing
            (Maybe.map3
                (\i b f ->
                    viewCommentField
                        { fieldId = fieldId
                        , inputChange = i
                        , blurred = b
                        , focused = f
                        , content = config.content
                        }
                )
                config.inputChange
                config.blurred
                config.focused
            )

    else
        viewCommentStatic (config.onOpen Comment fieldId) config.content fieldId


viewCommentStatic : msg -> Maybe String -> String -> Html msg
viewCommentStatic onOpen content fieldId =
    span [ class "item-comment", id fieldId, onClick onOpen ]
        [ case content of
            Just comment ->
                text comment

            Nothing ->
                span [ class "add-item-comment with-click-outside button" ]
                    [ Icons.messageSquareIcon [] ]
        ]


viewCommentField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , blurred : msg
        , focused : msg
        , content : Maybe String
    }
    -> Html msg
viewCommentField { fieldId, inputChange, blurred, focused, content } =
    span
        [ class "grow-container"
        , attribute "data-content" (Maybe.withDefault "" content)
        ]
        [ textarea
            [ value (Maybe.withDefault "" content)
            , class "with-click-outside"
            , onInput inputChange
            , onBlur blurred
            , onFocus focused
            , name fieldId
            , rows 1
            , id fieldId
            ]
            []
        ]


viewQuantity :
    { a
        | itemId : String
        , inputChange : Maybe (ItemField -> String -> msg)
        , blurred : Maybe msg
        , focused : Maybe msg
        , onOpen : ItemField -> String -> msg
        , static : Bool
        , open : Bool
    }
    -> Items.Quantity
    -> Html msg
viewQuantity ({ static, open } as config) (Items.Quantity count unit) =
    let
        countFieldId =
            "item-quantity-count" ++ config.itemId

        unitFieldId =
            "item-quantity-unit" ++ config.itemId
    in
    if not static && open then
        span []
            [ Maybe.withDefault
                nothing
                (Maybe.map3
                    (\i b f ->
                        viewQCountField
                            { fieldId = countFieldId
                            , inputChange = i QCount
                            , blurred = b
                            , focused = f
                            , content = count
                            }
                    )
                    config.inputChange
                    config.blurred
                    config.focused
                )
            , Maybe.withDefault
                nothing
                (Maybe.map3
                    (\i b f ->
                        viewQUnitField
                            { fieldId = unitFieldId
                            , inputChange = i QUnit
                            , blurred = b
                            , focused = f
                            , content = unit
                            }
                    )
                    config.inputChange
                    config.blurred
                    config.focused
                )
            ]

    else
        span [ onClick (config.onOpen Name countFieldId) ]
            [ b [] [ text (String.fromFloat count) ]
            , span [] [ text unit ]
            ]


viewQCountField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , blurred : msg
        , focused : msg
        , content : Float
    }
    -> Html msg
viewQCountField { fieldId, content, inputChange, focused, blurred } =
    b
        [ class "input-resize-containter"
        , attribute "data-content" (String.fromFloat content)
        ]
        [ input
            [ type_ "text"
            , attribute "inputmode" "decimal"
            , class "item-quantity-count-field with-click-outside"
            , value <| String.fromFloat content
            , onInput inputChange
            , onBlur blurred
            , onFocus focused
            , name fieldId
            , id fieldId
            ]
            []
        ]


viewQUnitField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , blurred : msg
        , focused : msg
        , content : String
    }
    -> Html msg
viewQUnitField { fieldId, content, inputChange, focused, blurred } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-quantity-unit-field with-click-outside"
            , value content
            , onInput inputChange
            , onBlur blurred
            , onFocus focused
            , name fieldId
            , id fieldId
            ]
            []
        ]
