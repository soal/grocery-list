module Components.Item.List exposing
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
import Components.Item.Form exposing (viewField)
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (DraftState(..), Item, ItemState(..))
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (Html, article, button, div, h3, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Html.Keyed
import ItemForm exposing (FieldMode(..), FieldName(..), ItemField(..))
import Set exposing (Set)


type alias Options =
    { items : Dict String Item
    , draftFields : Maybe (List ItemField)
    , draft : Maybe Item
    , catWithDraft : Maybe Int
    , categories : List Category
    , collapsedCatIds : Set Int
    , link : Bool
    , mark : Bool
    , switch : Bool
    , counter : Bool
    , checkedStates : List ItemState
    }


type ItemsList
    = Settings Options


new :
    { items : Dict String Item
    , categories : List Category
    , collapsedCatIds : Set Int
    , checkedSates : List ItemState
    }
    -> ItemsList
new props =
    Settings
        { items = props.items
        , draftFields = Nothing
        , draft = Nothing
        , catWithDraft = Nothing
        , categories = props.categories
        , collapsedCatIds = props.collapsedCatIds
        , link = False
        , mark = False
        , switch = False
        , counter = False
        , checkedStates = props.checkedSates
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
    -> Maybe Item
    -> ItemsList
    -> ItemsList
withDraft catWithDraft draftFields draft (Settings settings) =
    Settings
        { settings
            | draft = draft
            , catWithDraft = catWithDraft
            , draftFields = draftFields
        }


type Msg
    = CollapseClicked Int CollapsedState
    | ItemClicked Item ItemState
    | ItemChecked Item Bool
    | DraftOpened Category String
    | StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | DraftFieldUpdated ItemField (Maybe String)
    | DraftClosed Category
    | NoOp


view : ItemsList -> Html Msg
view (Settings settings) =
    div []
        [ if Dict.size settings.items > 0 then
            Html.Keyed.node "div" [] <|
                List.map
                    (viewCategory settings)
                    settings.categories

          else
            h3 [] [ text "Пусто" ]
        ]


viewCategory :
    Options
    -> Category
    -> ( String, Html Msg )
viewCategory options category =
    let
        state =
            if Set.member category.id options.collapsedCatIds then
                Collapsed

            else
                Open

        catHeader =
            viewCatHeader options
    in
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ catHeader state category
        , Components.Category.Body.view state
            [ viewItems options category
            , case ( options.draft, options.draftFields ) of
                ( Just draft, Just fields ) ->
                    viewDraft options.catWithDraft draft category fields

                ( _, _ ) ->
                    nothing
            ]
        ]
    )


viewCatHeader :
    Options
    -> CollapsedState
    -> Category
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
                            if state == Open then
                                Collapsed

                            else
                                Open
            )


viewItems :
    Options
    -> Category
    -> Html Msg
viewItems options category =
    ( options.items, category )
        |> getCatItems
        |> List.map
            (\( id, item ) ->
                ( id
                , viewItem
                    { item = item
                    , mark = options.mark
                    , link = options.link
                    , switch = options.switch
                    , checkedStates = options.checkedStates
                    }
                )
            )
        |> Html.Keyed.node "div" []


getCatItems : ( Dict String Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get id allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( item.id, item ))


viewItem :
    { item : Item
    , mark : Bool
    , link : Bool
    , switch : Bool
    , checkedStates : List ItemState
    }
    -> Html Msg
viewItem { item, mark, link, switch, checkedStates } =
    Components.Item.ListElement.new
        { item = item, checkedSates = checkedStates }
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
                    Components.Item.ListElement.ItemChecked clickedItem check ->
                        ItemChecked clickedItem check

                    Components.Item.ListElement.ItemClicked clickedItem state ->
                        ItemClicked clickedItem state
            )


viewDraft : Maybe Int -> Item -> Category -> List ItemField -> Html Msg
viewDraft catWithDraft draft category fields =
    let
        nameFieldId =
            "item-name-" ++ draft.id
    in
    case Maybe.map (\id -> id == category.id) catWithDraft of
        Just True ->
            article [ class "grocery-item" ] <|
                List.map
                    (viewMappedField draft)
                    fields

        _ ->
            button
                [ class "add-item-button outline"
                , onClick (DraftOpened category nameFieldId)
                ]
                [ Icons.plusCircle |> Icons.toHtml [] ]


viewMappedField : Item -> ItemField -> Html Msg
viewMappedField draft field =
    viewField draft field
        |> Html.map
            (\msg ->
                case msg of
                    ItemForm.StartEditing draftField data ->
                        StartEditing draftField data

                    ItemForm.FinishEditing _ ->
                        NoOp

                    ItemForm.UpdateField draftField data ->
                        DraftFieldUpdated draftField data
            )
