module Components.Item.List exposing
    ( ItemsList
    , Msg(..)
    , new
    , view
    , withCounter
    , withLink
    , withMark
    )

import Components.Category.Body
import Components.Category.Header
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemState)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Html.Keyed
import Set exposing (Set)


type alias Options =
    { items : Dict String Item
    , categories : List Category
    , collapsedCatIds : Set Int
    , link : Bool
    , mark : Bool
    , counter : Bool
    , checkedStates : List ItemState
    , pageName : String
    , emptyText : String
    }


type ItemsList
    = Settings Options


new :
    { items : Dict String Item
    , categories : List Category
    , collapsedCatIds : Set Int
    , checkedSates : List ItemState
    , pageName : String
    }
    -> ItemsList
new props =
    Settings
        { items = props.items
        , categories = props.categories
        , collapsedCatIds = props.collapsedCatIds
        , link = False
        , mark = False
        , counter = False
        , checkedStates = props.checkedSates
        , pageName = props.pageName
        , emptyText = "Пусто"
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


type Msg
    = CollapseClicked Int CollapsedState
    | ItemClicked Item ItemState
    | ItemChecked Item Bool
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
        , Components.Category.Body.view state (viewItems options category)
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
                    item
                    options.mark
                    options.link
                    options.checkedStates
                )
            )
        |> Html.Keyed.node "div" []


getCatItems : ( Dict String Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get id allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( item.id, item ))


viewItem : Item -> Bool -> Bool -> List ItemState -> Html Msg
viewItem item mark link checkedStates =
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
        |> Components.Item.ListElement.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Item.ListElement.ItemChecked clickedItem check ->
                        ItemChecked clickedItem check

                    Components.Item.ListElement.ItemClicked clickedItem state ->
                        ItemClicked clickedItem state
            )
