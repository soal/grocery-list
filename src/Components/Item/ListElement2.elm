module Components.Item.ListElement2 exposing (..)

import Html exposing (Html, article, div, h4, input, span, text)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , id
        , name
        , type_
        , value
        )
import Html.Attributes.Extra exposing (attributeMaybe, role)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Extra exposing (nothing)
import LucideIcons as Icons
import Svg.Attributes
import Types exposing (CheckboxKind(..), ItemField(..))



-- type FieldMode
--     = ViewMode
--     | EditMode
-- type FieldName
--     = Name (Maybe String)
--     | QCount (Maybe Float)
--     | QUnit (Maybe String)
--     | Comment (Maybe String)
--     | Symbol (Maybe String)
-- type ItemField
--     = ItemField FieldName FieldMode
-- itemFields : List ItemField
-- itemFields =
--     [ ItemField (Name Nothing) ViewMode
--     , ItemField (QCount Nothing) ViewMode
--     , ItemField (QUnit Nothing) ViewMode
--     , ItemField (Comment Nothing) ViewMode
--     , ItemField (Symbol Nothing) ViewMode
--     ]


view : Html msg
view =
    article
        [ class "grocery-item" ]
        [ div [ class "label" ]
            [ --checkbox slot
              h4 []
                [--name slot
                ]
            ]
        ]


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
        , onOpen : ItemField -> msg
        , content : String
        , static : Bool
        , open : Bool
    }
    -> Html msg
viewName ({ static, open } as config) =
    if not static && open then
        Maybe.withDefault nothing
            (Maybe.map3
                (\i b f ->
                    viewNameField
                        { itemId = config.itemId
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
        viewNameStatic config.content config.onOpen


viewNameStatic : String -> (ItemField -> msg) -> Html msg
viewNameStatic content onOpen =
    span [ onClick (onOpen Name) ] [ text content ]


viewNameField :
    { a
        | itemId : String
        , inputChange : String -> msg
        , blurred : msg
        , focused : msg
        , content : String
    }
    -> Html msg
viewNameField { itemId, inputChange, blurred, focused, content } =
    span [ class "input-resize-containter" ]
        [ span
            [ class "input-size-helper"
            , attribute "aria-hidden" "true"
            ]
            [ text content ]
        , input
            [ type_ "text"
            , class "item-name-field with-click-outside"
            , value content
            , onInput inputChange
            , onBlur blurred
            , onFocus focused
            , name <| "item-name-" ++ itemId
            , id <| "item-name-" ++ itemId
            ]
            []
        ]
