module Components.Items.List exposing
    ( ItemsList
    , Msg(..)
    , new
    , view
    , withCounter
    , withDraft
    , withLink
    , withMark
    , withSwitch
    )

import Components.Category.Body
import Components.Category.Header
import Components.Items.Item
import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Html.Keyed
import LucideIcons as Icons
import Set exposing (Set)
import Types exposing (Draft(..), ItemField(..))


type alias Options =
    { items : Dict String Items.Item
    , draft : Draft
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
        , draft = Empty
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
    Settings { settings | switch = True }


withDraft :
    Maybe Int
    -> Draft
    -> ItemsList
    -> ItemsList
withDraft catWithDraft draft (Settings settings) =
    Settings
        { settings
            | draft = draft
            , catWithDraft = catWithDraft
            , editable = True
        }


type Msg
    = CollapseClicked Int Cats.CollapsedState
    | ItemClicked Items.Item Items.State
    | ItemChecked Items.Item Items.State
    | StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | DraftOpened Cats.Category
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
            , viewIf options.editable <|
                viewDraft
                    { draft = options.draft
                    , open =
                        Maybe.withDefault
                            -1
                            options.catWithDraft
                            == category.id
                    , category = category
                    }
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
                    -- TODO: something more elegant
                    ( isItemOpen, activeItem ) =
                        case options.draft of
                            Empty ->
                                ( False, item )

                            New draft ->
                                if draft.id == id then
                                    ( True, draft )

                                else
                                    ( False, item )

                            Existing draft ->
                                if draft.id == id then
                                    ( True, draft )

                                else
                                    ( False, item )
                in
                ( id
                , viewItem
                    { item = activeItem
                    , mark = options.mark
                    , link = options.link
                    , switch = options.switch
                    , checkedStates = options.checkedStates
                    , open = isItemOpen
                    , editable = options.editable
                    }
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
    { item : Items.Item
    , mark : Bool
    , link : Bool
    , switch : Bool
    , checkedStates : List Items.State
    , open : Bool
    , editable : Bool
    }
    -> Html Msg
viewItem { item, mark, link, switch, checkedStates, open, editable } =
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
    { draft : Draft
    , category : Cats.Category
    , open : Bool
    }
    -> Html Msg
viewDraft { draft, open, category } =
    case ( open, draft ) of
        ( True, New item ) ->
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

        _ ->
            button
                [ class "add-item-button outline"
                , onClick (DraftOpened category)
                ]
                [ Icons.plusCircleIcon [] ]
