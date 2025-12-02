module Components.Items.List exposing
    ( ItemsList
    , Msg(..)
    , new
    , view
    , withCounter
    , withDraft
    , withEditing
    , withLink
    , withMark
    , withSwitch
    )

import Components.Category.Body
import Components.Category.Header
import Components.Items.Item
import Db.Categories as Cats exposing (Category)
import Db.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, button, div, label)
import Html.Attributes exposing (checked, class, disabled, id, type_)
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Html.Keyed
import Html.Lazy exposing (lazy7)
import LucideIcons as Icons
import Set exposing (Set)
import Types exposing (ItemField(..))


type alias Options =
    { items : Dict String Items.Item
    , draft : Maybe Items.Item
    , tempItem : Maybe Items.Item
    , catWithDraft : Maybe Int
    , categories : List Cats.Category
    , collapsedCatIds : Set Int
    , link : Bool
    , mark : Bool
    , switch : Bool
    , counter : Bool
    , checkedStates : List Items.State
    , editable : Bool
    }


type ItemsList
    = Settings Options


new :
    { items : Dict String Items.Item
    , categories : List Cats.Category
    , collapsedCatIds : Set Int
    , checkedSates : List Items.State
    }
    -> ItemsList
new props =
    Settings
        { items = props.items
        , draft = Nothing
        , tempItem = Nothing
        , catWithDraft = Nothing
        , categories = props.categories
        , collapsedCatIds = props.collapsedCatIds
        , link = False
        , mark = False
        , switch = False
        , counter = False
        , checkedStates = props.checkedSates
        , editable = False
        }


withLink : ItemsList -> ItemsList
withLink (Settings settings) =
    Settings { settings | link = True }


withMark : ItemsList -> ItemsList
withMark (Settings settings) =
    Settings { settings | mark = True }


withCounter : ItemsList -> ItemsList
withCounter (Settings settings) =
    Settings { settings | counter = True }


withSwitch : ItemsList -> ItemsList
withSwitch (Settings settings) =
    Settings { settings | switch = True, editable = True }


withDraft :
    Maybe Int
    -> Maybe Items.Item
    -> ItemsList
    -> ItemsList
withDraft catWithDraft draft (Settings settings) =
    Settings
        { settings
            | draft = draft
            , catWithDraft = catWithDraft
        }


withEditing : Maybe Items.Item -> ItemsList -> ItemsList
withEditing tempItem (Settings settings) =
    Settings
        { settings | tempItem = tempItem, editable = True }


type Msg
    = CollapseClicked Int Cats.CollapsedState
    | ItemClicked Items.Item Items.State
    | ItemChecked Items.Item Items.State
    | DraftOpened Cats.Category String
    | StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | DraftInputChanged ItemField String
    | DraftClosed Cats.Category
    | EditStarted Items.Item ItemField String
    | InputChanged Items.Item ItemField String
    | DeleteClicked String
    | NoOp


view : ItemsList -> Html Msg
view (Settings settings) =
    settings.categories
        |> List.map (viewCategory settings)
        |> Html.Keyed.node "div" []


viewCategory :
    Options
    -> Cats.Category
    -> ( String, Html Msg )
viewCategory options category =
    let
        state =
            if Set.member category.id options.collapsedCatIds then
                Cats.Collapsed

            else
                Cats.Open
    in
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ viewCatHeader options state category
        , Components.Category.Body.view state
            [ viewItems options category
            , case options.draft of
                Just draft ->
                    viewDraft
                        { item = draft
                        , open =
                            Maybe.withDefault
                                -1
                                options.catWithDraft
                                == category.id
                        , category = category
                        }

                _ ->
                    nothing
            ]
        ]
    )


viewCatHeader :
    Options
    -> Cats.CollapsedState
    -> Cats.Category
    -> Html Msg
viewCatHeader options state category =
    Components.Category.Header.new
        { category = category
        , items = options.items
        , state = state
        }
        |> (if options.counter == True then
                Components.Category.Header.withCounter

            else
                identity
           )
        |> Components.Category.Header.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Category.Header.Toggle id ->
                        CollapseClicked id <|
                            if state == Cats.Open then
                                Cats.Collapsed

                            else
                                Cats.Open
            )


viewItems :
    Options
    -> Cats.Category
    -> Html Msg
viewItems options category =
    ( options.items, category )
        |> getCatItems
        |> List.map
            (\( id, item ) ->
                let
                    isItemOpen =
                        options.tempItem
                            |> Maybe.map (\temp -> temp.id == id)
                            |> Maybe.withDefault False

                    activeItem =
                        if isItemOpen then
                            Maybe.withDefault item options.tempItem

                        else
                            item
                in
                ( id
                , lazy7 viewItem
                    activeItem
                    options.mark
                    options.link
                    options.switch
                    options.checkedStates
                    isItemOpen
                    options.editable
                )
            )
        |> Html.Keyed.node "div" []


getCatItems :
    ( Dict String Items.Item, Cats.Category )
    -> List ( String, Items.Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get id allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( item.id, item ))


viewItem :
    Items.Item
    -> Bool
    -> Bool
    -> Bool
    -> List Items.State
    -> Bool
    -> Bool
    -> Html Msg
viewItem item mark link switch checkedStates open editable =
    Components.Items.Item.new
        { item = item
        , checkedSates = checkedStates
        , open = open
        , editable = editable
        }
        |> (if link == True then
                Components.Items.Item.withLink

            else
                identity
           )
        |> (if mark == True then
                Components.Items.Item.withMark

            else
                identity
           )
        |> (if switch == True then
                Components.Items.Item.withSwitch

            else
                identity
           )
        |> Components.Items.Item.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Items.Item.ItemChecked clickedItem state ->
                        ItemChecked clickedItem state

                    Components.Items.Item.ItemClicked clickedItem state ->
                        ItemClicked clickedItem state

                    Components.Items.Item.EditStarted openItem field fieldId ->
                        if editable then
                            EditStarted openItem field fieldId

                        else
                            NoOp

                    Components.Items.Item.InputChanged activeItem field content ->
                        if editable then
                            InputChanged activeItem field content

                        else
                            NoOp

                    Components.Items.Item.DeleteClicked clickedItem ->
                        DeleteClicked clickedItem

                    _ ->
                        NoOp
            )


viewDraft :
    { item : Items.Item
    , open : Bool
    , category : Cats.Category
    }
    -> Html Msg
viewDraft { item, open, category } =
    if open == True then
        Components.Items.Item.new
            { item = item, checkedSates = [], open = True, editable = True }
            |> Components.Items.Item.asDraft
            |> Components.Items.Item.view
            |> Html.map
                (\msg ->
                    case msg of
                        Components.Items.Item.InputChanged _ field content ->
                            DraftInputChanged field content

                        _ ->
                            NoOp
                )

    else
        button
            [ class "add-item-button outline"
            , onClick (DraftOpened category <| "item-name-" ++ item.id)
            ]
            [ Icons.plusCircleIcon [] ]
