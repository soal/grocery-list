module Components.Items.Item exposing
    ( ItemListElement
    , asForm
    , new
    , view
    , withCheck
    , withClick
    , withEditing
    , withLink
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
        , span
        , text
        )
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extra exposing (attributeMaybe)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import LucideIcons as Icons
import Route.Path
import Types exposing (CheckboxKind(..), ItemField(..))


type alias Handlers msg =
    { click : Maybe msg
    , check : Maybe (Bool -> msg)
    , edit : Maybe (ItemField -> String -> msg)
    , input : Maybe (ItemField -> String -> msg)
    , delete : Maybe msg
    , enter : Maybe msg
    , esc : Maybe msg
    }


defaultHandlers : Handlers msg
defaultHandlers =
    { click = Nothing
    , check = Nothing
    , edit = Nothing
    , input = Nothing
    , delete = Nothing
    , enter = Nothing
    , esc = Nothing
    }


type ItemListElement msg
    = Settings
        { item : Items.Item
        , link : Bool
        , checkedSates : List Items.State
        , clickable : Bool
        , checkable : Bool
        , open : Bool
        , editable : Bool
        , on : Handlers msg
        }


new :
    { item : Items.Item
    , checkedSates : List Items.State
    , open : Bool
    }
    -> ItemListElement msg
new props =
    Settings
        { item = props.item
        , link = False
        , clickable = False
        , checkable = False
        , checkedSates = props.checkedSates
        , open = props.open
        , editable = False
        , on = defaultHandlers
        }


withClick : msg -> ItemListElement msg -> ItemListElement msg
withClick onClick (Settings settings) =
    let
        on =
            settings.on
    in
    Settings { settings | clickable = True, on = { on | click = Just onClick } }


withCheck : (Bool -> msg) -> ItemListElement msg -> ItemListElement msg
withCheck onCheck (Settings settings) =
    let
        on =
            settings.on
    in
    Settings { settings | checkable = True, on = { on | check = Just onCheck } }


withLink : ItemListElement msg -> ItemListElement msg
withLink (Settings settings) =
    Settings { settings | link = True }


withEditing :
    { edit : ItemField -> String -> msg, delete : msg }
    -> ItemListElement msg
    -> ItemListElement msg
withEditing handlers (Settings settings) =
    let
        on =
            settings.on
    in
    Settings
        { settings
            | editable = True
            , on =
                { on
                    | edit = Just handlers.edit
                    , delete = Just handlers.delete
                }
        }


asForm :
    { input : ItemField -> String -> msg
    , delete : msg
    , enter : msg
    , esc : msg
    }
    -> ItemListElement msg
    -> ItemListElement msg
asForm handlers (Settings settings) =
    let
        on =
            settings.on
    in
    Settings
        { settings
            | open = True
            , on =
                { on
                    | input = Just handlers.input
                    , delete = Just handlers.delete
                    , enter = Just handlers.enter
                    , esc = Just handlers.esc
                }
        }


view : ItemListElement msg -> Html msg
view (Settings ({ on } as settings)) =
    let
        link =
            if settings.link == True then
                a
                    [ Route.Path.href
                        (Route.Path.Items_Item_ { item = settings.item.slug })
                    ]
                    [ Icons.arrowRightIcon [] ]

            else
                text ""

        checkMark =
            settings.clickable && settings.item.state == Items.InBasket
    in
    article
        [ class "grocery-item"
        , classList
            [ ( "in-basket", checkMark )
            , ( "item-form", settings.open )
            ]
        , attributeMaybe onClick on.click
        ]
        [ viewCheckbox
            on.check
            False
            (if settings.checkable then
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
                , onOpen = on.edit
                , inputChange = Maybe.map (\f -> f Name) on.input
                , content = settings.item.name
                , editable = settings.editable
                , open = settings.open
                , onEnter = on.enter
                , onEsc = on.esc
                }
            ]
        , viewQuantity
            { itemId = settings.item.id
            , onOpen = on.edit
            , inputChange = on.input
            , open = settings.open
            , editable = settings.editable
            , onEnter = on.enter
            , onEsc = on.esc
            }
            settings.item.quantity
        , div [ class "item-comment-box" ]
            [ viewComment
                { itemId = settings.item.id
                , onOpen = on.edit
                , inputChange = Maybe.map (\f -> f Comment) on.input
                , content = settings.item.comment
                , open = settings.open
                , editable = settings.editable
                , onEnter = on.enter
                , onEsc = on.esc
                }
            , viewIf settings.editable <|
                div
                    [ class "button delete-button with-click-outside"
                    , attributeMaybe onClick on.delete
                    ]
                    [ Icons.trashIcon [] ]
            , link
            ]
        ]


itemStateToBool : Items.State -> List Items.State -> Bool
itemStateToBool state checkedSates =
    List.member state checkedSates
