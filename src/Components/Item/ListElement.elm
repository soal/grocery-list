module Components.Item.ListElement exposing
    ( ItemListElement
    , Msg(..)
    , new
    , view
    , withLink
    , withMark
    , withSwitch
    )

-- import Components.Item.List exposing (Msg(..))

import Components.Item.ListElement2 exposing (viewCheckbox, viewName)
import Db.Items as Items
import Html
    exposing
        ( Html
        , a
        , article
        , b
        , div
        , h4
        , span
        , text
        )
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewMaybe)
import LucideIcons as Icons
import Route.Path
import Types exposing (CheckboxKind(..), ItemField(..))


type ItemListElement
    = Settings
        { item : Items.Item
        , link : Bool
        , checkedSates : List Items.State
        , mark : Bool
        , switch : Bool
        , open : Bool
        }


new : { item : Items.Item, checkedSates : List Items.State, open : Bool } -> ItemListElement
new props =
    Settings
        { item = props.item
        , link = False
        , mark = False
        , switch = False
        , checkedSates = props.checkedSates
        , open = props.open
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
    | ItemChecked Items.Item Items.State
    | EditStarted Items.Item ItemField
    | InputChanged Items.Item ItemField String
    | NoOp


view : ItemListElement -> Html Msg
view (Settings settings) =
    let
        link =
            if settings.link == True then
                a
                    [ Route.Path.href
                        (Route.Path.Items_Item_ { item = settings.item.slug })
                    ]
                    [ Icons.chevronRightIcon [] ]

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
        [ div [ class "label" ]
            [ viewCheckbox
                (\_ -> ItemChecked settings.item settings.item.state)
                False
                (if settings.switch then
                    Plus

                 else
                    Check
                )
                (itemStateToBool
                    settings.item.state
                    settings.checkedSates
                )
            , h4 []
                [ viewName
                    { itemId = settings.item.id
                    , static = not settings.switch
                    , onOpen = EditStarted settings.item
                    , blurred = Just NoOp
                    , focused = Just NoOp
                    , inputChange = Just <| InputChanged settings.item Name
                    , content = settings.item.name
                    , open = settings.open
                    }
                , viewMaybe (\s -> span [] [ text s ]) settings.item.symbol
                ]
            ]
        , span
            [ class "item-quantity" ]
            (viewQuantity settings.item.quantity)
        , div [ class "item-comment-box" ]
            [ viewMaybe
                (\c -> span [ class "item-comment" ] [ text c ])
                settings.item.comment
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
