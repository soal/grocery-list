module Views.Items.Item exposing
    ( ItemListElement
    , asForm
    , new
    , view
    , withCheck
    , withClick
    , withEditing
    , withLink
    )

import Common exposing (CheckboxKind(..), FormState(..), ItemField(..))
import Data.Categories as Cats
import Data.Items as Items
import Html as H exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extra exposing (attributeMaybe)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing, viewIf)
import LucideIcons as Icons
import Route.Path
import Views.CategorySelector
import Views.Items.Form
    exposing
        ( viewCheckbox
        , viewComment
        , viewName
        , viewQuantity
        )


type alias Handlers msg =
    { click : Maybe msg
    , check : Maybe (Bool -> msg)
    , edit : Maybe (ItemField -> String -> msg)
    , input : Maybe (ItemField -> String -> msg)
    , changeCat : Maybe (Maybe Cats.Category -> Maybe Cats.Id -> msg)
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
    , changeCat = Nothing
    , delete = Nothing
    , enter = Nothing
    , esc = Nothing
    }


type ItemListElement msg
    = Settings
        { item : Items.Item
        , validation : Items.ValidationResult
        , link : Bool
        , allCats : Maybe (List Cats.Category)
        , currentCat : Maybe Cats.Category
        , checkedSates : List Items.State
        , clickable : Bool
        , checkable : Bool
        , formState : FormState
        , editable : Bool
        , on : Handlers msg
        }


new :
    { item : Items.Item
    , validation : Items.ValidationResult
    , checkedSates : List Items.State
    , formState : FormState
    }
    -> ItemListElement msg
new props =
    Settings
        { item = props.item
        , validation = props.validation
        , link = False
        , clickable = False
        , checkable = False
        , checkedSates = props.checkedSates
        , formState = props.formState
        , editable = False
        , allCats = Nothing
        , currentCat = Nothing
        , on = defaultHandlers
        }


withClick : msg -> ItemListElement msg -> ItemListElement msg
withClick onClick (Settings settings) =
    let
        on : Handlers msg
        on =
            settings.on
    in
    Settings { settings | clickable = True, on = { on | click = Just onClick } }


withCheck : (Bool -> msg) -> ItemListElement msg -> ItemListElement msg
withCheck onCheck (Settings settings) =
    let
        on : Handlers msg
        on =
            settings.on
    in
    Settings { settings | checkable = True, on = { on | check = Just onCheck } }


withLink : ItemListElement msg -> ItemListElement msg
withLink (Settings settings) =
    Settings { settings | link = True }


withEditing :
    { edit : ItemField -> String -> msg
    , delete : msg
    , changeCat : Maybe Cats.Category -> Maybe Cats.Id -> msg
    }
    ->
        { allCats : Maybe (List Cats.Category)
        , currentCat : Maybe Cats.Category
        }
    -> ItemListElement msg
    -> ItemListElement msg
withEditing handlers cats (Settings settings) =
    let
        on : Handlers msg
        on =
            settings.on
    in
    Settings
        { settings
            | editable = True
            , allCats = cats.allCats
            , currentCat = cats.currentCat
            , on =
                { on
                    | edit = Just handlers.edit
                    , delete = Just handlers.delete
                    , changeCat = Just handlers.changeCat
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
        on : Handlers msg
        on =
            settings.on
    in
    Settings
        { settings
            | formState = Form
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
        link : Html msg
        link =
            if settings.link then
                H.a
                    [ Route.Path.href
                        (Route.Path.Items_Item_ { item = settings.item.slug })
                    ]
                    [ Icons.arrowRightIcon [] ]

            else
                H.text ""

        checkMark : Bool
        checkMark =
            settings.clickable && settings.item.state == Items.InBasket
    in
    H.div
        [ class "grocery-item"
        , classList
            [ ( "in-basket", checkMark )
            , ( "item-form", settings.formState == Form )
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
        , H.div [ class "item-content" ]
            [ viewValidationError settings.validation
                [ Items.NameIsEmpty, Items.NameAlreadyExist ]
            , viewName
                { itemId = settings.item.id
                , onOpen = on.edit
                , inputChange = Maybe.map (\f -> f Name) on.input
                , content = settings.item.name
                , editable = settings.editable
                , formState = settings.formState
                , onEnter = on.enter
                , onEsc = on.esc
                }
            , viewValidationError settings.validation
                [ Items.QuantityIsZero ]
            , viewQuantity
                { itemId = settings.item.id
                , onOpen = on.edit
                , inputChange = on.input
                , formState = settings.formState
                , editable = settings.editable
                , onEnter = on.enter
                , onEsc = on.esc
                }
                settings.item.quantity
            , viewComment
                { itemId = settings.item.id
                , onOpen = on.edit
                , inputChange = Maybe.map (\f -> f Comment) on.input
                , content = settings.item.comment
                , formState = settings.formState
                , editable = settings.editable
                , onEnter = on.enter
                , onEsc = on.esc
                }
            , viewIf (settings.formState == Form) <|
                H.div
                    [ attributeMaybe onClick on.esc
                    , class "icon-button cancel-button with-click-outside"
                    ]
                    [ Icons.xIcon [] ]
            , viewIf settings.editable <|
                Maybe.withDefault (H.text "nothing")
                    (Maybe.map2
                        (Views.CategorySelector.view
                            settings.currentCat
                            settings.item.id
                        )
                        settings.allCats
                        settings.on.changeCat
                    )
            ]
        , viewIf settings.editable <|
            H.div
                [ class "icon-button delete-button with-click-outside"
                , attributeMaybe onClick on.delete
                ]
                [ Icons.trashIcon [] ]
        , link
        ]


itemStateToBool : Items.State -> List Items.State -> Bool
itemStateToBool state checkedSates =
    List.member state checkedSates


viewValidationError : Items.ValidationResult -> List Items.ItemError -> Html msg
viewValidationError validationResult errorTypes =
    case validationResult of
        Items.ValidationOk ->
            nothing

        Items.ValidationError error ->
            if List.member error errorTypes then
                H.div [ class "item-validation-error" ]
                    [ H.div [ class "error-message" ]
                        [ H.text <| Items.validationMessage error ]
                    ]

            else
                nothing
