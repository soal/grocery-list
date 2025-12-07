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
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (nothing)
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard
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
        , inputChange : String -> msg
        , onOpen : ItemField -> String -> msg
        , content : String
        , editable : Bool
        , open : Bool
        , onEnter : msg
        , onEsc : msg
    }
    -> Html msg
viewName ({ editable, open } as props) =
    let
        fieldId =
            "item-name-" ++ props.itemId
    in
    if editable && open then
        viewNameField
            { fieldId = fieldId
            , inputChange = props.inputChange
            , content = props.content
            , onEnter = props.onEnter
            , onEsc = props.onEsc
            }

    else
        viewNameStatic (props.onOpen Name fieldId) props.content fieldId


viewNameStatic : msg -> String -> String -> Html msg
viewNameStatic onOpen content fieldId =
    span [ id fieldId, onClick onOpen ] [ text content ]


viewNameField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , content : String
        , onEnter : msg
        , onEsc : msg
    }
    -> Html msg
viewNameField { fieldId, inputChange, content, onEnter, onEsc } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-name-field with-click-outside"
            , value content
            , onInput inputChange
            , name fieldId
            , id fieldId
            , Keyboard.on Keyboard.Keydown
                [ ( Enter, onEnter ), ( Escape, onEsc ) ]
            ]
            []
        ]


viewComment :
    { a
        | itemId : String
        , inputChange : String -> msg
        , onOpen : ItemField -> String -> msg
        , content : Maybe String
        , open : Bool
        , editable : Bool
        , onEnter : msg
        , onEsc : msg
    }
    -> Html msg
viewComment ({ open, editable } as props) =
    let
        fieldId =
            "item-comment-" ++ props.itemId
    in
    if editable && open then
        viewCommentField
            { fieldId = fieldId
            , inputChange = props.inputChange
            , content = props.content
            , onEnter = props.onEnter
            , onEsc = props.onEsc
            }

    else
        viewCommentStatic
            (props.onOpen Comment fieldId)
            props.content
            fieldId
            editable


viewCommentStatic : msg -> Maybe String -> String -> Bool -> Html msg
viewCommentStatic onOpen content fieldId editable =
    span [ class "item-comment", id fieldId, onClick onOpen ]
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
        , inputChange : String -> msg
        , content : Maybe String
        , onEnter : msg
        , onEsc : msg
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
            , onInput inputChange
            , name fieldId
            , rows 1
            , id fieldId
            , placeholder "Комментарий"
            , Keyboard.on Keyboard.Keydown
                [ ( Enter, onEnter ), ( Escape, onEsc ) ]
            ]
            []
        ]


viewQuantity :
    { a
        | itemId : String
        , inputChange : ItemField -> String -> msg
        , onOpen : ItemField -> String -> msg
        , open : Bool
        , editable : Bool
        , onEnter : msg
        , onEsc : msg
    }
    -> Items.Quantity
    -> Html msg
viewQuantity ({ editable, open } as props) (Items.Quantity count unit) =
    let
        countFieldId =
            "item-quantity-count" ++ props.itemId

        unitFieldId =
            "item-quantity-unit" ++ props.itemId
    in
    if editable && open then
        span [ class "item-quantity" ]
            [ viewQCountField
                { fieldId = countFieldId
                , inputChange = props.inputChange QCount
                , content = count
                , onEnter = props.onEnter
                , onEsc = props.onEsc
                }
            , viewQUnitField
                { fieldId = unitFieldId
                , inputChange = props.inputChange QUnit
                , content = unit
                , onEnter = props.onEnter
                , onEsc = props.onEsc
                }
            ]

    else
        span [ onClick (props.onOpen Name countFieldId) ]
            [ b [] [ text (String.fromFloat count) ]
            , text " "
            , span [] [ text unit ]
            ]


viewQCountField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , content : Float
        , onEnter : msg
        , onEsc : msg
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
            , onInput inputChange
            , name fieldId
            , id fieldId
            , Keyboard.on Keyboard.Keydown
                [ ( Enter, onEnter ), ( Escape, onEsc ) ]
            ]
            []
        ]


viewQUnitField :
    { a
        | fieldId : String
        , inputChange : String -> msg
        , content : String
        , onEnter : msg
        , onEsc : msg
    }
    -> Html msg
viewQUnitField { fieldId, content, inputChange, onEnter, onEsc } =
    span [ class "input-resize-containter", attribute "data-content" content ]
        [ input
            [ type_ "text"
            , class "item-quantity-unit-field with-click-outside"
            , value content
            , onInput inputChange
            , name fieldId
            , id fieldId
            , Keyboard.on Keyboard.Keydown
                [ ( Enter, onEnter ), ( Escape, onEsc ) ]
            ]
            []
        ]
