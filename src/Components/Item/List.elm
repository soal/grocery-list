module Components.Item.List exposing
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
import Components.Item.Form exposing (viewField)
import Components.Item.ListElement
import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, article, button, div, input, label)
import Html.Attributes exposing (checked, class, disabled, id, type_)
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Html.Keyed
import ItemForm exposing (FieldMode(..), FieldName(..), ItemField(..))
import LucideIcons as Icons
import Set exposing (Set)
import Types exposing (ItemField(..))


type alias Options =
    { items : Dict String Items.Item
    , draftFields : Maybe (List ItemField)
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
        , draftFields = Nothing
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
    Settings { settings | switch = True }


withDraft :
    Maybe Int
    -> Maybe (List ItemField)
    -> Maybe Items.Item
    -> ItemsList
    -> ItemsList
withDraft catWithDraft draftFields draft (Settings settings) =
    Settings
        { settings
            | draft = draft
            , catWithDraft = catWithDraft
            , draftFields = draftFields
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
    | DraftFieldUpdated ItemField (Maybe String)
    | DraftClosed Cats.Category
    | EditStarted Items.Item ItemField
    | InputChanged Items.Item ItemField String
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

        catHeader =
            viewCatHeader options
    in
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ catHeader state category
        , Components.Category.Body.view state
            [ viewItems options category

            -- , case ( options.draft, options.draftFields ) of
            --     ( Just draft, Just fields ) ->
            --         viewDraft options.catWithDraft draft category fields
            --     ( _, _ ) ->
            --         nothing
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
    Components.Item.ListElement.new
        { item = item, checkedSates = checkedStates, open = open }
        |> (if link == True then
                Components.Item.ListElement.withLink

            else
                identity
           )
        |> (if mark == True then
                Components.Item.ListElement.withMark

            else
                identity
           )
        |> (if switch == True then
                Components.Item.ListElement.withSwitch

            else
                identity
           )
        |> Components.Item.ListElement.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Item.ListElement.ItemChecked clickedItem state ->
                        ItemChecked clickedItem state

                    Components.Item.ListElement.ItemClicked clickedItem state ->
                        ItemClicked clickedItem state

                    Components.Item.ListElement.EditStarted openItem field ->
                        if editable then
                            EditStarted openItem field

                        else
                            NoOp

                    Components.Item.ListElement.InputChanged activeItem field content ->
                        if editable then
                            InputChanged activeItem field content

                        else
                            NoOp

                    _ ->
                        NoOp
            )



-- viewDraft :
--     Maybe Int
--     -> Items.Item
--     -> Cats.Category
--     -> List ItemField
--     -> Html Msg
-- viewDraft catWithDraft draft category fields =
--     let
--         nameFieldId =
--             "item-name-" ++ draft.id
--     in
--     case Maybe.map (\id -> id == category.id) catWithDraft of
--         Just True ->
--             article [ class "grocery-item item-draft" ] <|
--                 label []
--                     [ input
--                         [ type_ "checkbox"
--                         , role "switch"
--                         , disabled True
--                         , id nameFieldId
--                         , class "contrast"
--                         , checked False
--                         ]
--                         []
--                     ]
--                     :: List.map
--                         (viewMappedField draft)
--                         fields
--         _ ->
--             button
--                 [ class "add-item-button outline"
--                 , onClick (DraftOpened category nameFieldId)
--                 ]
--                 [ Icons.plusCircleIcon [] ]
-- viewMappedField : Items.Item -> ItemField -> Html Msg
-- viewMappedField draft field =
--     viewField draft field
--         |> Html.map
--             (\msg ->
--                 case msg of
--                     ItemForm.StartEditing draftField data ->
--                         StartEditing draftField data
--                     ItemForm.FinishEditing draftField ->
--                         FinishEditing draftField
--                     ItemForm.UpdateField draftField data ->
--                         DraftFieldUpdated draftField data
--             )
