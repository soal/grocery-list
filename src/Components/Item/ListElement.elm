module Components.Item.ListElement exposing
    ( ItemListElement
    , Msg(..)
    , new
    , updItemState
    , view
    , withLink
    , withMark
    )

import Db.Items exposing (Item, ItemMarkedAs(..), ItemState(..), Quantity(..))
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html
    exposing
        ( Html
        , a
        , article
        , b
        , div
        , h4
        , i
        , input
        , label
        , small
        , span
        , text
        )
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onCheck, onClick)
import Route.Path


type ItemListElement
    = Settings
        { item : Item
        , markedAs : Maybe ItemMarkedAs
        , link : Bool
        }


new : { item : Item } -> ItemListElement
new props =
    Settings
        { item = props.item
        , markedAs = Nothing
        , link = False
        }


withMark : ItemMarkedAs -> ItemListElement -> ItemListElement
withMark markedAs (Settings settings) =
    Settings { settings | markedAs = Just markedAs }


withLink : ItemListElement -> ItemListElement
withLink (Settings settings) =
    Settings { settings | link = True }


type Msg
    = ItemClicked Int
    | ItemChecked Int Bool


view : ItemListElement -> Html Msg
view (Settings settings) =
    let
        link =
            if settings.link == True then
                [ a
                    [ Route.Path.href
                        (Route.Path.Items_Item_ { item = settings.item.slug })
                    ]
                    [ Icons.chevronRight |> Icons.toHtml [] ]
                ]

            else
                []
    in
    article
        [ class "grocery-item"
        , classList [ ( "in-basket", settings.markedAs == Just InBasket ) ]
        , onClick (ItemClicked settings.item.id)
        ]
        [ div []
            [ label []
                [ input
                    [ type_ "checkbox"
                    , name "to-buy"
                    , class "contrast"
                    , checked (itemStateToBool settings.item.state)
                    , onCheck (ItemChecked settings.item.id)
                    ]
                    []
                , h4 []
                    [ span [] [ text settings.item.name ]
                    , span []
                        [ text (Maybe.withDefault " " settings.item.symbol) ]
                    ]
                ]
            , span
                [ class "item-quantity" ]
                (viewQuantity settings.item.quantity)
            ]
        , div [] <|
            i [ class "item-comment" ]
                [ text (Maybe.withDefault "" settings.item.comment) ]
                :: link
        ]


viewQuantity : Quantity -> List (Html msg)
viewQuantity (Quantity quantity unit) =
    [ b [] [ text (String.fromInt quantity) ]
    , small [] [ text unit ]
    ]


itemMarkedToBool : ItemMarkedAs -> Bool
itemMarkedToBool state =
    if state == InBasket then
        True

    else
        False


itemStateToBool : ItemState -> Bool
itemStateToBool state =
    if state == Required then
        True

    else
        False


updItemState : Dict Int Item -> Int -> Bool -> Dict Int Item
updItemState items id checked =
    let
        newState =
            if checked == True then
                Required

            else
                Stuffed
    in
    Dict.update id
        (Maybe.map (\found -> { found | state = newState }))
        items
