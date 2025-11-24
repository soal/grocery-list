module Pages.InStore exposing (Model, Msg, page)

import Components.Category.Body as CategoryBody
import Components.Category.Header exposing (toggleCategory)
import Components.Counter
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemState(..), Quantity(..), updateItemState)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (checked, class, classList)
import Html.Keyed
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
            -- ( { model
            --     | categories = toggleCategory model.categories id state
            --   }
            -- , Effect.none
            -- )
            ( model, Effect.none )

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
view shared model =
    { title = "В магазине"
    , body =
        [ Html.Keyed.node "div" [] <|
            List.map
                (viewCategory shared.items)
                shared.categories
        , button
            [ class "end-shopping-button"
            , classList [ ( "all-done", isAllDone shared.items ) ]
            ]
            [ Components.Counter.view
                (Dict.map (\_ item -> item.state) shared.items)
                (Dict.keys shared.items)
                InBasket
            , text "Закончить покупки"
            ]
        ]
    }


viewCategory :
    Dict String Item
    -> Category
    -> ( String, Html Msg )
viewCategory allItems category =
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ viewCatHeader allItems category
        , CategoryBody.view
            category.state
            (viewItems allItems category)
        ]
    )


isAllDone : Dict String Item -> Bool
isAllDone itemStates =
    getInBasketLength itemStates >= List.length (Dict.values itemStates)


viewCatHeader : Dict String Item -> Category -> Html Msg
viewCatHeader items category =
    Components.Category.Header.new { category = category, items = items }
        |> Components.Category.Header.withCounter
        |> Components.Category.Header.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Category.Header.Toggle id state ->
                        CollapseClicked id state
            )


viewItems : Dict String Item -> Category -> Html Msg
viewItems allItems category =
    ( allItems, category )
        |> getCatItems
        |> List.filter
            (\( _, item ) -> item.state /= Stuffed)
        |> List.map
            (\( id, item ) -> ( id, viewItem item ))
        |> Html.Keyed.node "div" []


getCatItems : ( Dict String Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get (String.fromInt id) allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( String.fromInt item.id, item ))


viewItem : Item -> Html Msg
viewItem item =
    Components.Item.ListElement.new
        { item = item, checkedSates = [ InBasket ] }
        |> Components.Item.ListElement.withMark
        |> Components.Item.ListElement.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Item.ListElement.ItemClicked id currentState ->
                        ItemClicked id currentState

                    _ ->
                        NoOp
            )


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
