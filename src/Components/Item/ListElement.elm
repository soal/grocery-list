module Components.Item.ListElement exposing
    ( ItemListElement
    , Msg(..)
    , new
    , view
    , withLink
    , withMark
    )

import Db.Items exposing (Item, ItemQuantity(..), ItemState(..))
import FeatherIcons as Icons
import Html
    exposing
        ( Html
        , a
        , article
        , b
        , div
        , h4
        , input
        , label
        , span
        , text
        )
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onCheck, onClick)
import Route.Path


type ItemListElement
    = Settings
        { item : Item
        , link : Bool
        , checkedSates : List ItemState
        , mark : Bool
        }


new : { item : Item, checkedSates : List ItemState } -> ItemListElement
new props =
    Settings
        { item = props.item
        , link = False
        , mark = False
        , checkedSates = props.checkedSates
        }


withLink : ItemListElement -> ItemListElement
withLink (Settings settings) =
    Settings { settings | link = True }


withMark : ItemListElement -> ItemListElement
withMark (Settings settings) =
    Settings { settings | mark = True }


type Msg
    = ItemClicked Int ItemState
    | ItemChecked Int Bool


view : ItemListElement -> Html Msg
view (Settings settings) =
    let
        link =
            if settings.link == True then
                a
                    [ Route.Path.href
                        (Route.Path.Items_Item_ { item = settings.item.slug })
                    ]
                    [ Icons.chevronRight |> Icons.toHtml [] ]

            else
                text ""

        checkMark =
            settings.mark && settings.item.state == InBasket
    in
    article
        [ class "grocery-item"
        , classList [ ( "in-basket", checkMark ) ]
        , onClick (ItemClicked settings.item.id settings.item.state)
        ]
        [ div []
            [ label []
                [ input
                    [ type_ "checkbox"
                    , name "to-buy"
                    , class "contrast"
                    , checked
                        (itemStateToBool
                            settings.item.state
                            settings.checkedSates
                        )
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
        , div []
            [ span [ class "item-comment" ]
                [ text (Maybe.withDefault "" settings.item.comment) ]
            , link
            ]
        ]


viewQuantity : ItemQuantity -> List (Html msg)
viewQuantity (ItemQuantity quantity unit) =
    [ b [] [ text (String.fromFloat quantity) ]
    , span [] [ text unit ]
    ]


itemStateToBool : ItemState -> List ItemState -> Bool
itemStateToBool state checkedSates =
    List.member state checkedSates
