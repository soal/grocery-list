module Pages.Home_ exposing (Model, Msg, page)

import Components.Counter
import Components.Item.List
import Db.Categories as Cats
import Db.Draft as Draft
import Db.Items as Items exposing (Msg(..), State(..))
import Db.Settings exposing (CatsAndItems)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
import Shared
import TaskPort
import Time
import Utils exposing (getCollapsesCatsForPage)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withOnQueryParameterChanged
            { key = "f", onChange = SectionChanged }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.MainNav
        { onClickedOutside = GotClickOutside
        }


type alias CollapsedCats =
    Dict String (Set Int)


type Section
    = All
    | Shopping


type alias UiState =
    { collapsedCatsMap : CollapsedCats }



-- INIT


type alias Model =
    { draft : Maybe Items.Item
    , uiState : UiState
    , items : Dict String Items.Item
    , categories : List Cats.Category
    , titlePrefix : String
    , section : Section
    , error : Maybe String
    }


init : Route () -> () -> ( Model, Effect Msg )
init route () =
    let
        section =
            case Dict.get "f" route.query of
                Just "all" ->
                    All

                Just "shopping" ->
                    Shopping

                _ ->
                    All
    in
    ( { uiState =
            { collapsedCatsMap =
                Dict.fromList
                    [ ( "all", Set.empty )
                    , ( "shopping", Set.empty )
                    ]
            }
      , items = Dict.empty
      , categories = []
      , titlePrefix = "Покупки: "
      , error = Nothing
      , section = section
      , draft =
            Just <|
                Items.Item "0"
                    ""
                    (Items.Quantity 1 "штук")
                    Nothing
                    ""
                    Nothing
                    Items.Required
                    (Time.millisToPosix 0)
                    (Time.millisToPosix 0)
      }
    , Effect.batch
        [ Effect.requestUuid GotUuid
        , Effect.queryAll
            (\loaded ->
                case loaded of
                    Ok data ->
                        GotCatsAndItems data

                    Err _ ->
                        -- err
                        --     |> Json.Decode.errorToString
                        --     |> Just
                        --     |> Shared.Msg.Error
                        -- Error err
                        Error Nothing
            )
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | SectionChanged { from : Maybe String, to : Maybe String }
    | Error (Maybe String)
    | GotItemMsg Items.Msg
    | GotCatsMsg Cats.Msg
    | GotCatsAndItems CatsAndItems
    | GotUuid (TaskPort.Result String)
    | GotDraftMsg Draft.Msg
    | GotClickOutside


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        Error error ->
            ( { model | error = error }, Effect.none )

        SectionChanged { from, to } ->
            if from /= to then
                case to of
                    Just "all" ->
                        ( { model | section = All }, Effect.none )

                    Just "shopping" ->
                        ( { model | section = Shopping }, Effect.none )

                    _ ->
                        ( model, Effect.none )

            else
                ( model, Effect.none )

        GotUuid uuid ->
            case ( Result.toMaybe uuid, model.draft ) of
                ( Just id, Just draft ) ->
                    ( { model | draft = Just (Items.setId id draft) }
                    , Effect.none
                    )

                ( _, _ ) ->
                    ( model, Effect.none )

        GotClickOutside ->
            ( model, Effect.none )

        GotCatsAndItems data ->
            ( { model | categories = data.categories, items = data.items }
            , Effect.none
            )

        -- ITEM
        GotItemMsg itemMsg ->
            case itemMsg of
                Items.GotNewState item state ->
                    ( { model
                        | items = Items.setState model.items item.id state
                      }
                    , Effect.storeItem
                        (\res ->
                            case res of
                                Ok True ->
                                    NoOp

                                _ ->
                                    Error Nothing
                        )
                        { item | state = state }
                    )

                Items.GotChange item ->
                    ( { model | items = Items.alter model.items item }
                    , Effect.storeItem
                        (\res ->
                            case res of
                                Ok True ->
                                    NoOp

                                _ ->
                                    Error Nothing
                        )
                        item
                    )

                Items.GotAllBought ->
                    let
                        updated =
                            Items.setAllStuffed model.items
                    in
                    ( { model | items = updated }
                    , Effect.storeAllItems
                        (\res ->
                            case res of
                                Ok True ->
                                    NoOp

                                _ ->
                                    Error Nothing
                        )
                        updated
                    )

        -- CATEGORIES
        GotCatsMsg catsMsg ->
            case catsMsg of
                Cats.GotCollapsedChange section catId state ->
                    let
                        uiState =
                            model.uiState
                    in
                    ( { model
                        | uiState =
                            { uiState
                                | collapsedCatsMap =
                                    alterCollapsedCats
                                        section
                                        catId
                                        uiState.collapsedCatsMap
                                        state
                            }
                      }
                    , Effect.none
                    )

        -- DRAFT
        GotDraftMsg draftMsg ->
            case draftMsg of
                Draft.GotChange draft ->
                    ( { model | draft = Just draft }, Effect.none )

                Draft.GotSave draft ->
                    ( model
                    , Effect.batch
                        [ Effect.requestUuid GotUuid ]
                    )


alterCollapsedCats :
    String
    -> Int
    -> CollapsedCats
    -> Cats.CollapsedState
    -> CollapsedCats
alterCollapsedCats pageName catId catsMap state =
    Dict.update pageName
        (Maybe.map
            (\ids ->
                if state == Cats.Open then
                    Set.remove catId ids

                else
                    Set.insert catId ids
            )
        )
        catsMap



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ "Список"
    , body =
        case model.section of
            All ->
                viewFullList model

            Shopping ->
                viewShopping model
    }


viewFullList : Model -> List (Html Msg)
viewFullList model =
    [ Components.Item.List.new
        { items = model.items
        , categories = model.categories
        , checkedSates = [ Items.Required, Items.InBasket ]
        , collapsedCatIds =
            getCollapsesCatsForPage "all" model.uiState.collapsedCatsMap
        }
        |> Components.Item.List.withLink
        |> Components.Item.List.withSwitch
        -- |> Components.Item.List.withDraft
        --     model.catWithDraft
        --     (Just model.draftFields)
        --     model.draft
        |> Components.Item.List.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Item.List.CollapseClicked clickedItem state ->
                        GotCatsMsg <|
                            Cats.GotCollapsedChange
                                "all"
                                clickedItem
                                state

                    Components.Item.List.ItemChecked checkedItem check ->
                        GotItemMsg <|
                            Items.GotNewState checkedItem
                                (if check == True then
                                    Items.Required

                                 else
                                    Items.Stuffed
                                )

                    -- Components.Item.List.DraftOpened category fieldId ->
                    --     DraftOpened category fieldId
                    -- Components.Item.List.DraftFieldUpdated field data ->
                    --     DraftFieldUpdated field data
                    -- Components.Item.List.FinishEditing field ->
                    --     DraftFieldFinished field
                    -- Components.Item.List.StartEditing field data ->
                    --     DraftFieldStarted field data
                    _ ->
                        NoOp
            )
    ]


viewShopping : Model -> List (Html Msg)
viewShopping model =
    if Dict.size model.items > 0 then
        let
            filtered =
                Items.filterByStates
                    model.items
                    [ Items.Required, Items.InBasket ]

            categories =
                filterCategories filtered model.categories
        in
        [ Components.Item.List.new
            { items = filtered
            , categories = categories
            , checkedSates = [ Items.InBasket ]
            , collapsedCatIds =
                getCollapsesCatsForPage "shopping"
                    model.uiState.collapsedCatsMap
            }
            |> Components.Item.List.withMark
            |> Components.Item.List.withCounter
            |> Components.Item.List.view
            |> Html.map
                (\msg ->
                    case msg of
                        Components.Item.List.CollapseClicked clickedItem state ->
                            GotCatsMsg <|
                                Cats.GotCollapsedChange
                                    "shopping"
                                    clickedItem
                                    state

                        Components.Item.List.ItemClicked item state ->
                            GotItemMsg <|
                                Items.GotNewState item <|
                                    case state of
                                        Required ->
                                            InBasket

                                        InBasket ->
                                            Required

                                        _ ->
                                            InBasket

                        _ ->
                            NoOp
                )
        , button
            [ class "end-shopping-button"
            , classList [ ( "all-done", Items.isAllDone filtered ) ]
            , onClick (GotItemMsg Items.GotAllBought)
            , disabled (Items.getInBasketLength filtered <= 0)
            ]
            [ Components.Counter.view
                (Dict.map (\_ item -> item.state) filtered)
                (Dict.keys filtered)
                Items.InBasket
            , text "Закончить покупки"
            ]
        ]

    else
        [ viewEmpty ]


filterCategories :
    Dict String Items.Item
    -> List Cats.Category
    -> List Cats.Category
filterCategories items categories =
    List.filter
        (\cat ->
            List.any
                (\id -> Dict.member id items)
                cat.items
        )
        categories


viewEmpty : Html msg
viewEmpty =
    div [ class "empty-page" ] [ h3 [] [ text "Всё куплено!" ] ]
