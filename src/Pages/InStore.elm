module Pages.InStore exposing (Model, Msg, page)

import Components.Category.Body as CategoryBody
import Components.Category.Header exposing (toggleCategory)
import Components.Counter
import Db.Categories exposing (Category, CollapsedState(..), categories)
import Db.Items exposing (Item, ItemState(..), Quantity(..), items)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
    exposing
        ( Html
        , article
        , b
        , button
        , div
        , h4
        , i
        , input
        , label
        , small
        , span
        , text
        )
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.MainNav {}



-- INIT


toggleItemState : ItemState -> ItemState
toggleItemState state =
    if state == InBasket then
        ToBuy

    else
        InBasket


type ItemView
    = ItemView Item ItemState


type alias Model =
    { categories : List Category
    , items : Dict Int Item
    , itemStates : Dict Int ItemState
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { categories = categories
      , items = items
      , itemStates =
            Dict.keys items
                |> List.map (\id -> ( id, ToBuy ))
                |> Dict.fromList
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemClicked Int
    | NoOp


update : Msg -> Model -> ( Model, Effect msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            ( { model
                | categories = toggleCategory model.categories id state
              }
            , Effect.none
            )

        ItemClicked itemId ->
            ( { model
                | itemStates =
                    Dict.update
                        itemId
                        (\_ ->
                            getItemState model.itemStates itemId
                                |> toggleItemState
                                |> Just
                        )
                        model.itemStates
              }
            , Effect.none
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


view : Model -> View Msg
view model =
    { title = "В магазине"
    , body =
        [ Html.Keyed.node "div" [] <|
            List.map
                (viewCategory model.items model.itemStates)
                model.categories
        , button
            [ class "end-shopping-button"
            , classList [ ( "all-done", isAllDone model.itemStates ) ]
            ]
            [ Components.Counter.view
                model.itemStates
                (Dict.keys model.items)
                InBasket
            , text "Закончить покупки"
            ]
        ]
    }


viewCategory :
    Dict Int Item
    -> Dict Int ItemState
    -> Category
    -> ( String, Html Msg )
viewCategory allItems itemStates category =
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ viewCatHeader itemStates category
        , CategoryBody.view
            category.state
            (viewItems allItems itemStates category)
        ]
    )


isAllDone : Dict Int ItemState -> Bool
isAllDone itemStates =
    getInBasketLength itemStates >= List.length (Dict.values itemStates)


viewCatHeader : Dict Int ItemState -> Category -> Html Msg
viewCatHeader itemStates category =
    Components.Category.Header.new { category = category }
        |> Components.Category.Header.withCounter itemStates
        |> Components.Category.Header.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Category.Header.Toggle id state ->
                        CollapseClicked id state
            )


viewItems : Dict Int Item -> Dict Int ItemState -> Category -> Html Msg
viewItems allItems itemStates category =
    ( allItems, category )
        |> getCatItems
        |> List.map
            (\( id, item ) ->
                ( id
                , viewItem <| ItemView item <| getItemState itemStates item.id
                )
            )
        |> Html.Keyed.node "div" []


getCatItems : ( Dict Int Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get id allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( String.fromInt item.id, item ))


viewItem : ItemView -> Html Msg
viewItem (ItemView item state) =
    article
        [ class "grocery-item"
        , classList [ ( "in-basket", state == InBasket ) ]
        , onClick (ItemClicked item.id)
        ]
        [ div []
            [ label []
                [ input
                    [ type_ "checkbox"
                    , name "to-buy"
                    , class "contrast"
                    , checked (itemStateToBool state)
                    ]
                    []
                , h4 []
                    [ span [] [ text item.name ]
                    , span []
                        [ text <|
                            String.fromChar <|
                                Maybe.withDefault ' ' item.symbol
                        ]
                    ]
                ]
            , span [ class "item-quantity" ] (viewQuantity item.quantity)
            ]
        , div []
            [ i [ class "item-comment" ]
                [ text (Maybe.withDefault "" item.comment) ]
            ]
        ]


getItemState : Dict Int ItemState -> Int -> ItemState
getItemState itemStates itemId =
    Maybe.withDefault ToBuy (Dict.get itemId itemStates)


getInBasketLength : Dict Int ItemState -> Int
getInBasketLength itemStates =
    Dict.values itemStates
        |> List.filter (\v -> v == InBasket)
        |> List.length


itemStateToBool : ItemState -> Bool
itemStateToBool state =
    if state == InBasket then
        True

    else
        False


viewQuantity : Quantity -> List (Html msg)
viewQuantity (Quantity quantity unit) =
    [ b [] [ text (String.fromInt quantity) ]
    , small [] [ text unit ]
    ]
