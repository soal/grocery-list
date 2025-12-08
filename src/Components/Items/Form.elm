module Components.Items.Form exposing (..)

import Db.Items as Items
import Html exposing (Html, b, div, input, span, text, textarea)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , id
        , name
        , placeholder
        , rows
        , type_
        , value
        )
import Html.Attributes.Extra exposing (attributeMaybe, role)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (nothing)
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard
import LucideIcons as Icons
import Svg.Attributes
import Types exposing (CheckboxKind(..), ItemField(..))


viewCheckbox : Maybe (Bool -> msg) -> Bool -> CheckboxKind -> Bool -> Html msg
viewCheckbox onCheck disabled kind checked =
    div
        [ role "checkbox"
        , attribute "aria-role" "checkbox"
        , classList
            [ ( "checked", checked )
            , ( "check", kind == Check )
            , ( "disabled", disabled )
            ]
        , attributeMaybe onClick (Maybe.map (\f -> f <| not checked) onCheck)
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
        , onOpen : Maybe (ItemField -> String -> msg)
        , content : String
        , editable : Bool
        , open : Bool
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewName props =
    let
        fieldId =
            "item-name-" ++ props.itemId
    in
    if props.open then
        viewNameField
            { fieldId = fieldId
            , inputChange = props.inputChange
            , content = props.content
            , onEnter = props.onEnter
            , onEsc = props.onEsc
            }

    else
        viewNameStatic
            (Maybe.map (\f -> f Name fieldId) props.onOpen)
            props.content
            fieldId


viewNameStatic : Maybe msg -> String -> String -> Html msg
viewNameStatic onOpen content fieldId =
    span
        [ id fieldId
        , attributeMaybe onClick onOpen
        ]
        [ text content ]


viewNameField :
    { a
        | fieldId : String
        , inputChange : Maybe (String -> msg)
        , content : String
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewNameField { fieldId, inputChange, content, onEnter, onEsc } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-name-field with-click-outside"
            , value content
            , attributeMaybe onInput inputChange
            , name fieldId
            , id fieldId
            , attributeMaybe
                (Keyboard.on Keyboard.Keydown)
                (maybeKbd onEnter onEsc)
            ]
            []
        ]


viewComment :
    { a
        | itemId : String
        , inputChange : Maybe (String -> msg)
        , onOpen : Maybe (ItemField -> String -> msg)
        , content : Maybe String
        , open : Bool
        , editable : Bool
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewComment props =
    let
        fieldId =
            "item-comment-" ++ props.itemId
    in
    if props.open then
        viewCommentField
            { fieldId = fieldId
            , inputChange = props.inputChange
            , content = props.content
            , onEnter = props.onEnter
            , onEsc = props.onEsc
            }

    else
        viewCommentStatic
            (Maybe.map (\f -> f Comment fieldId) props.onOpen)
            props.content
            fieldId
            props.editable


viewCommentStatic : Maybe msg -> Maybe String -> String -> Bool -> Html msg
viewCommentStatic onOpen content fieldId editable =
    span
        [ class "item-comment"
        , id fieldId
        , attributeMaybe onClick onOpen
        ]
        [ case content of
            Just comment ->
                text comment

            Nothing ->
                if editable then
                    span [ class "add-item-comment with-click-outside button" ]
                        [ Icons.messageSquareIcon [] ]

                else
                    nothing
        ]


viewCommentField :
    { a
        | fieldId : String
        , inputChange : Maybe (String -> msg)
        , content : Maybe String
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewCommentField { fieldId, inputChange, content, onEnter, onEsc } =
    span
        [ class "grow-container"
        , attribute "data-content" (Maybe.withDefault "" content)
        ]
        [ textarea
            [ value (Maybe.withDefault "" content)
            , class "with-click-outside"
            , attributeMaybe onInput inputChange
            , name fieldId
            , rows 1
            , id fieldId
            , placeholder "Комментарий"
            , attributeMaybe
                (Keyboard.on Keyboard.Keydown)
                (maybeKbd onEnter onEsc)
            ]
            []
        ]


viewQuantity :
    { a
        | itemId : String
        , inputChange : Maybe (ItemField -> String -> msg)
        , onOpen : Maybe (ItemField -> String -> msg)
        , open : Bool
        , editable : Bool
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Items.Quantity
    -> Html msg
viewQuantity props (Items.Quantity count unit) =
    let
        countFieldId =
            "item-quantity-count" ++ props.itemId

        unitFieldId =
            "item-quantity-unit" ++ props.itemId
    in
    if props.open then
        span [ class "item-quantity" ]
            [ viewQCountField
                { fieldId = countFieldId
                , inputChange = Maybe.map (\f -> f QCount) props.inputChange
                , content = count
                , onEnter = props.onEnter
                , onEsc = props.onEsc
                }
            , viewQUnitField
                { fieldId = unitFieldId
                , inputChange = Maybe.map (\f -> f QUnit) props.inputChange
                , content = unit
                , onEnter = props.onEnter
                , onEsc = props.onEsc
                }
            ]

    else
        span
            [ attributeMaybe onClick <|
                Maybe.map (\f -> f Name countFieldId) props.onOpen
            ]
            [ b [] [ text (String.fromFloat count) ]
            , text " "
            , span [] [ text unit ]
            ]


viewQCountField :
    { a
        | fieldId : String
        , inputChange : Maybe (String -> msg)
        , content : Float
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewQCountField { fieldId, content, inputChange, onEnter, onEsc } =
    b
        [ class "input-resize-containter"
        , attribute "data-content" (String.fromFloat content)
        ]
        [ input
            [ type_ "text"
            , attribute "inputmode" "decimal"
            , class "item-quantity-count-field with-click-outside"
            , value <| String.fromFloat content
            , attributeMaybe onInput inputChange
            , name fieldId
            , id fieldId
            , attributeMaybe
                (Keyboard.on Keyboard.Keydown)
                (maybeKbd onEnter onEsc)
            ]
            []
        ]


viewQUnitField :
    { a
        | fieldId : String
        , inputChange : Maybe (String -> msg)
        , content : String
        , onEnter : Maybe msg
        , onEsc : Maybe msg
    }
    -> Html msg
viewQUnitField { fieldId, content, inputChange, onEnter, onEsc } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-quantity-unit-field with-click-outside"
            , value content
            , attributeMaybe onInput inputChange
            , name fieldId
            , id fieldId
            , attributeMaybe
                (Keyboard.on Keyboard.Keydown)
                (maybeKbd onEnter onEsc)
            ]
            []
        ]


maybeKbd : Maybe keyFunc -> Maybe keyFunc -> Maybe (List ( Key, keyFunc ))
maybeKbd onEnter onEsc =
    Maybe.map2
        (\enter esc -> [ ( Enter, enter ), ( Escape, esc ) ])
        onEnter
        onEsc
