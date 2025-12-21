module Views.Items.List exposing
    ( ItemsList
    , Msg(..)
    , new
    , view
    , withCheck
    , withClick
    , withCounter
    , withDraft
    , withLink
    )

import Common exposing (DomId, Draft(..), FormState(..), ItemField(..))
import Data.Categories as Cats
import Data.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Html.Keyed
import LucideIcons as Icons
import Set exposing (Set)
import Views.Category.Header
import Views.Items.Item


type alias Options =
    { items : Dict Items.Id Items.Item
    , draft : Draft
    , catWithDraft : Maybe Cats.Id
    , categories : List Cats.Category
    , collapsedCatIds : Set Cats.Id
    , link : Bool
    , clickable : Bool
    , checkable : Bool
    , counter : Bool
    , checkedStates : List Items.State
    , editable : Bool
    }


type ItemsList
    = Settings Options


new :
    { items : Dict Items.Id Items.Item
    , categories : List Cats.Category
    , collapsedCatIds : Set Cats.Id
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
        , clickable = False
        , checkable = False
        , counter = False
        , checkedStates = props.checkedSates
        , editable = False
        }


withLink : ItemsList -> ItemsList
withLink (Settings settings) =
    Settings { settings | link = True }


withClick : ItemsList -> ItemsList
withClick (Settings settings) =
    Settings { settings | clickable = True }


withCounter : ItemsList -> ItemsList
withCounter (Settings settings) =
    Settings { settings | counter = True }


withCheck : ItemsList -> ItemsList
withCheck (Settings settings) =
    Settings { settings | checkable = True }


withDraft :
    Maybe Cats.Id
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
    = -- CATEGORY
      CollapseClicked Cats.Id Cats.CollapsedState
    | CatTitleClicked Cats.Category
    | CatDeleteClicked Cats.Id
      -- ITEM
    | ItemClicked Items.Item Items.State
    | ItemChecked Items.Item Items.State
    | ItemDeleteClicked Items.Id
      -- DRAFT
    | DraftOpened Cats.Category
    | InputChanged ItemField String
    | EditStarted Items.Item ItemField DomId
    | EnterPressed
      -- | ShiftEnterPressed
      -- | CtrlEnterPressed
    | EscPressed


view : ItemsList -> Html Msg
view (Settings settings) =
    -- Categories and their items
    (settings.categories
        |> List.map (viewCategory settings)
        |> List.append
            (case settings.draft of
                NewCat cat ->
                    [ ( cat.id
                      , div [ class "grocery-category" ]
                            [ viewCatHeader settings Cats.Collapsed cat ]
                      )
                    ]

                _ ->
                    []
            )
    )
        -- Items without categories
        ++ (viewItems settings <|
                getItemsWithoutCat settings.items settings.categories
           )
        |> Html.Keyed.node "div" []


viewCategory :
    Options
    -> Cats.Category
    -> ( String, Html Msg )
viewCategory options category =
    let
        state : Cats.CollapsedState
        state =
            if Set.member category.id options.collapsedCatIds then
                Cats.Collapsed

            else
                Cats.Open
    in
    ( category.id
    , div
        [ class "grocery-category"
        , classList [ ( "shopping-page", options.clickable ) ]
        ]
        [ viewCatHeader options state category
        , viewCatItems options category
            ++ [ ( "empty"
                 , viewIf options.editable <|
                    viewDraft
                        { draft = options.draft
                        , open =
                            Maybe.withDefault
                                "-1"
                                options.catWithDraft
                                == category.id
                        , category = category
                        }
                 )
               ]
            |> Html.Keyed.node "div"
                [ class "category-body"
                , classList [ ( "collapsed", state == Cats.Collapsed ) ]
                ]
        ]
    )


viewCatHeader :
    Options
    -> Cats.CollapsedState
    -> Cats.Category
    -> Html Msg
viewCatHeader options state category =
    Views.Category.Header.new
        { category = category
        , items = options.items
        , state = state
        , on = { toggle = onCatToggle state category.id }
        }
        |> (if options.counter then
                Views.Category.Header.withCounter

            else
                identity
           )
        |> (if options.editable then
                Views.Category.Header.withDraft options.draft
                    { input = InputChanged Name
                    , click = CatTitleClicked category
                    , delete = CatDeleteClicked category.id
                    , enter = EnterPressed
                    , esc = EscPressed
                    }

            else
                identity
           )
        |> Views.Category.Header.view


onCatToggle : Cats.CollapsedState -> Cats.Id -> Msg
onCatToggle state catId =
    CollapseClicked catId <|
        if state == Cats.Open then
            Cats.Collapsed

        else
            Cats.Open


viewItems : Options -> List ( Items.Id, Items.Item ) -> List ( Items.Id, Html Msg )
viewItems options itemsKeyed =
    List.map
        (\( id, item ) ->
            let
                -- TODO: something more elegant
                ( formState, activeItem, validation ) =
                    case options.draft of
                        Empty ->
                            ( Static, item, Items.ValidationOk )

                        New ( draft, validation_ ) ->
                            if draft.id == id then
                                ( Form, draft, validation_ )

                            else
                                ( Static, item, Items.ValidationOk )

                        Existing ( draft, validation_ ) ->
                            if draft.id == id then
                                ( Form, draft, validation_ )

                            else
                                ( Static, item, Items.ValidationOk )

                        NewCat _ ->
                            ( Static, item, Items.ValidationOk )

                        ExistingCat _ ->
                            ( Static, item, Items.ValidationOk )
            in
            ( id
            , viewItem
                { item = activeItem
                , validation = validation
                , clickable = options.clickable
                , link = options.link
                , checkable = options.checkable
                , checkedStates = options.checkedStates
                , formState = formState
                , editable = options.editable
                }
            )
        )
        itemsKeyed


viewCatItems :
    Options
    -> Cats.Category
    -> List ( Items.Id, Html Msg )
viewCatItems options category =
    ( options.items, category )
        |> getCatItems
        |> viewItems options


getCatItems :
    ( Dict Items.Id Items.Item, Cats.Category )
    -> List ( Items.Id, Items.Item )
getCatItems ( allItems, category ) =
    List.filterMap (\id -> Dict.get id allItems) category.items
        |> List.map (\item -> ( item.id, item ))


getItemsWithoutCat :
    Dict Items.Id Items.Item
    -> List Cats.Category
    -> List ( Items.Id, Items.Item )
getItemsWithoutCat allItems categories =
    allItems
        |> Dict.toList
        |> (\itemList ->
                ( itemList
                , List.concatMap .items categories
                )
           )
        |> (\( itemPairs, itemIds ) ->
                List.filter
                    (\( id, _ ) ->
                        not
                            (List.member id itemIds)
                    )
                    itemPairs
           )


viewItem :
    { item : Items.Item
    , validation : Items.ValidationResult
    , link : Bool
    , clickable : Bool
    , checkable : Bool
    , checkedStates : List Items.State
    , formState : FormState
    , editable : Bool
    }
    -> Html Msg
viewItem props =
    Views.Items.Item.new
        { item = props.item
        , validation = props.validation
        , checkedSates = props.checkedStates
        , formState = props.formState
        }
        |> (if props.link then
                Views.Items.Item.withLink

            else
                identity
           )
        |> (if props.clickable then
                Views.Items.Item.withClick
                    (ItemClicked props.item props.item.state)

            else
                identity
           )
        |> (if props.checkable then
                Views.Items.Item.withCheck
                    (\_ -> ItemChecked props.item props.item.state)

            else
                identity
           )
        |> (if props.editable then
                Views.Items.Item.withEditing
                    { edit = EditStarted props.item
                    , delete = ItemDeleteClicked props.item.id
                    }

            else
                identity
           )
        |> (if props.formState == Form then
                Views.Items.Item.asForm
                    { input = InputChanged
                    , delete = ItemDeleteClicked props.item.id
                    , enter = EnterPressed
                    , esc = EscPressed
                    }

            else
                identity
           )
        |> Views.Items.Item.view


viewDraft :
    { draft : Draft
    , category : Cats.Category
    , open : Bool
    }
    -> Html Msg
viewDraft { draft, open, category } =
    case ( open, draft ) of
        ( True, New ( item, validation ) ) ->
            viewItem
                { item = item
                , validation = validation
                , link = False
                , clickable = False
                , checkable = False
                , checkedStates = []
                , formState = Form
                , editable = False
                }

        _ ->
            button
                [ class "add-item-button"
                , onClick (DraftOpened category)
                ]
                [ Icons.plusIcon [] ]
