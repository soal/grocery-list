module Pages.InStore exposing (Model, Msg, page)

import Components.Counter
import Components.Item.List
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemQuantity(..), ItemState(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Utils exposing (getCollapsesCatsForPage)
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
    Layouts.MainNav { onClickedOutside = NoOp }



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
                (\id -> Dict.member id items)
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
    | ItemClicked Item ItemState
    | EndShopping
    | NoOp


update : Msg -> Model -> ( Model, Effect msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            ( model, Effect.updateCatCollapsedState "in-store" id state )

        ItemClicked item currentState ->
            ( model
            , Effect.updateItemState item (toggleItemState currentState)
            )

        EndShopping ->
            ( model, Effect.endShopping )

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
    { title = shared.titlePrefix ++ "В магазине"
    , body =
        if Dict.size items > 0 then
            [ Components.Item.List.new
                { items = items
                , categories = categories
                , checkedSates = [ InBasket ]
                , collapsedCatIds =
                    getCollapsesCatsForPage
                        "in-store"
                        shared.uiState.collapsedCatsMap
                , pageName = "in-store"
                }
                |> Components.Item.List.withMark
                |> Components.Item.List.withCounter
                |> Components.Item.List.view
                |> Html.map
                    (\msg ->
                        case msg of
                            Components.Item.List.CollapseClicked id state ->
                                CollapseClicked id state

                            Components.Item.List.ItemClicked item state ->
                                ItemClicked item state

                            _ ->
                                NoOp
                    )
            , button
                [ class "end-shopping-button"
                , classList [ ( "all-done", isAllDone items ) ]
                , onClick EndShopping
                , disabled (getInBasketLength items <= 0)
                ]
                [ Components.Counter.view
                    (Dict.map (\_ item -> item.state) items)
                    (Dict.keys items)
                    InBasket
                , text "Закончить покупки"
                ]
            ]

        else
            [ viewEmpty ]
    }


viewEmpty : Html msg
viewEmpty =
    div [ class "empty-page" ] [ h3 [] [ text "Всё куплено!" ] ]


isAllDone : Dict String Item -> Bool
isAllDone items =
    let
        statesLength =
            Dict.size items
    in
    statesLength > 0 && statesLength <= getInBasketLength items


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
