module Components.Items.Item exposing
    ( ItemListElement
    , Msg(..)
    , asDraft
    , new
    , view
    , withLink
    , withMark
    , withSwitch
    )

import Components.Items.Form
    exposing
        ( viewCheckbox
        , viewComment
        , viewName
        , viewQuantity
        )
import Db.Items as Items
import Html
    exposing
        ( Html
        , a
        , article
        , div
        , h4
        , span
        , text
        )
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf, viewMaybe)
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
        , draft : Bool
        , editable : Bool
        }


new :
    { item : Items.Item
    , checkedSates : List Items.State
    , open : Bool
    , editable : Bool
    }
    -> ItemListElement
new props =
    Settings
        { item = props.item
        , link = False
        , mark = False
        , switch = False
        , draft = False
        , checkedSates = props.checkedSates
        , open = props.open
        , editable = props.editable
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


asDraft : ItemListElement -> ItemListElement
asDraft (Settings settings) =
    Settings { settings | draft = True }


type Msg
    = ItemClicked Items.Item Items.State
    | ItemChecked Items.Item Items.State
    | EditStarted Items.Item ItemField String
    | InputChanged Items.Item ItemField String
    | DeleteClicked String
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
        , classList
            [ ( "in-basket", checkMark )
            , ( "item-draft", settings.draft )
            ]
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
            , span [ class "item-title" ]
                [ viewName
                    { itemId = settings.item.id
                    , static = not settings.editable
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
            [ viewQuantity
                { itemId = settings.item.id
                , static = not settings.switch
                , onOpen = EditStarted settings.item
                , blurred = Just NoOp
                , focused = Just NoOp
                , inputChange = Just <| InputChanged settings.item
                , open = settings.open
                }
                settings.item.quantity
            ]
        , div [ class "item-comment-box" ]
            [ viewComment
                { itemId = settings.item.id
                , static = not settings.switch
                , onOpen = EditStarted settings.item
                , blurred = Just NoOp
                , focused = Just NoOp
                , inputChange = Just <| InputChanged settings.item Comment
                , content = settings.item.comment
                , open = settings.open
                }
            , viewIf settings.switch <|
                div
                    [ class "button delete-item-button"
                    , onClick (DeleteClicked settings.item.id)
                    ]
                    [ Icons.trashIcon [] ]
            , link
            ]
        ]


itemStateToBool : Items.State -> List Items.State -> Bool
itemStateToBool state checkedSates =
    List.member state checkedSates
