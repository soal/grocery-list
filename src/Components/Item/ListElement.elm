module Components.Item.ListElement exposing
    ( ItemListElement
    , Msg(..)
    , new
    , view
    , withLink
    , withMark
    , withSwitch
    )

import Db.Items as Items
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
import Html.Attributes exposing (attribute, checked, class, classList, id, type_)
import Html.Attributes.Extra exposing (attributeIf, role)
import Html.Events exposing (onCheck, onClick)
import Route.Path


type ItemListElement
    = Settings
        { item : Items.Item
        , link : Bool
        , checkedSates : List Items.State
        , mark : Bool
        , switch : Bool
        }


new : { item : Items.Item, checkedSates : List Items.State } -> ItemListElement
new props =
    Settings
        { item = props.item
        , link = False
        , mark = False
        , switch = False
        , checkedSates = props.checkedSates
        }


withLink : ItemListElement -> ItemListElement
withLink (Settings settings) =
    Settings { settings | link = True }


withMark : ItemListElement -> ItemListElement
withMark (Settings settings) =
    Settings { settings | mark = True }


withSwitch : ItemListElement -> ItemListElement
withSwitch (Settings settings) =
    Settings { settings | switch = True }


type Msg
    = ItemClicked Items.Item Items.State
    | ItemChecked Items.Item Bool


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
            settings.mark && settings.item.state == Items.InBasket
    in
    article
        [ class "grocery-item"
        , classList [ ( "in-basket", checkMark ) ]
        , onClick (ItemClicked settings.item settings.item.state)
        ]
        [ label []
            [ input
                [ type_ "checkbox"
                , attributeIf settings.switch (role "switch")
                , attributeIf
                    (not settings.switch && checkMark)
                    (attribute "aria-invalid" "false")
                , id "to-buy"
                , class "contrast"
                , checked
                    (itemStateToBool
                        settings.item.state
                        settings.checkedSates
                    )
                , onCheck (ItemChecked settings.item)
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
        , div []
            [ span [ class "item-comment" ]
                [ text (Maybe.withDefault "" settings.item.comment) ]
            , link
            ]
        ]


viewQuantity : Items.Quantity -> List (Html msg)
viewQuantity (Items.Quantity quantity unit) =
    [ b [] [ text (String.fromFloat quantity) ]
    , span [] [ text unit ]
    ]


itemStateToBool : Items.State -> List Items.State -> Bool
itemStateToBool state checkedSates =
    List.member state checkedSates
