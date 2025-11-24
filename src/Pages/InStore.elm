module Pages.InStore exposing (Model, Msg, page)

import Components.Category.Body as CategoryBody
import Components.Category.Header
import Components.Counter
import Components.Item.List
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemState(..), Quantity(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class, classList)
import Html.Keyed
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Utils exposing (getCatStateForPage, getCollapsesCatsForPage)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.MainNav {}



-- INIT


type alias Model =
    { collapsedCatMap : Dict String CollapsedState
    }


filterItems : Dict String Item -> Dict String Item
filterItems allItems =
    Dict.filter (\_ item -> item.state /= Stuffed) allItems


filterCategories : Dict String Item -> List Category -> List Category
filterCategories items categories =
    List.filter
        (\cat ->
            List.any
                (\id -> Dict.member (String.fromInt id) items)
                cat.items
        )
        categories


init : () -> ( Model, Effect Msg )
init () =
    ( { collapsedCatMap = Dict.empty }
    , Effect.none
    )



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemClicked Int ItemState
    | NoOp


update : Msg -> Model -> ( Model, Effect msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            ( model, Effect.updateCatCollapsedState "in-store" id state )

        ItemClicked id currentState ->
            ( model
            , Effect.updateItemState id (toggleItemState currentState)
            )

        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    let
        items =
            filterItems shared.items

        categories =
            filterCategories items shared.categories
    in
    { title = "В магазине"
    , body =
        [ Components.Item.List.new
            { items = items
            , categories = categories
            , checkedSates = [ InBasket ]
            , collapsedCatIds =
                getCollapsesCatsForPage "in-store" shared.uiState.collapsedCatsMap
            , pageName = "in-store"
            }
            |> Components.Item.List.withMark
            |> Components.Item.List.view
            |> Html.map
                (\msg ->
                    case msg of
                        Components.Item.List.CollapseClicked id state ->
                            CollapseClicked id state

                        Components.Item.List.ItemClicked id state ->
                            ItemClicked id state

                        _ ->
                            NoOp
                )
        , button
            [ class "end-shopping-button"
            , classList [ ( "all-done", isAllDone items ) ]
            ]
            [ Components.Counter.view
                (Dict.map (\_ item -> item.state) items)
                (Dict.keys items)
                InBasket
            , text "Закончить покупки"
            ]
        ]
    }


isAllDone : Dict String Item -> Bool
isAllDone itemStates =
    let
        statesLength =
            List.length (Dict.values itemStates)
    in
    statesLength > 0 && statesLength <= getInBasketLength itemStates


getInBasketLength : Dict String Item -> Int
getInBasketLength items =
    Dict.values items
        |> List.filter (\item -> item.state == InBasket)
        |> List.length


toggleItemState : ItemState -> ItemState
toggleItemState state =
    case state of
        InBasket ->
            Required

        Required ->
            InBasket

        Stuffed ->
            Stuffed
