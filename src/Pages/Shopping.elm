module Pages.Shopping exposing (Model, Msg, page)

import Components.Counter
import Components.Items.List
import DataUpdate
import Db.Categories as Cats
import Db.Items as Items
import Db.Settings exposing (CatsAndItems)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
import Shared
import TaskPort
import Time
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
    Layouts.MainNav
        { onClickOutside = NoOp }



-- INIT


type ContentState
    = Initial
    | Loading
    | Ready


type alias Model =
    { collapsedCats : Set Int
    , items : Dict String Items.Item
    , categories : List Cats.Category
    , state : ContentState
    , error : Maybe String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { collapsedCats = Set.empty
      , items = Dict.empty
      , categories = []
      , state = Initial
      , error = Nothing
      }
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
                    GotError Nothing
        )
    )



-- UPDATE


type Msg
    = NoOp
    | Error (Maybe String)
    | GotCatsAndItems CatsAndItems
    | GotError (Maybe String)
    | ClickedEndShopping
    | GotStateUpdateTime Items.Item Time.Posix
    | GotItemListMsg Components.Items.List.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        Error error ->
            ( { model | error = error }, Effect.none )

        GotCatsAndItems data ->
            ( { model
                | categories = data.categories
                , items = data.items
                , state = Ready
              }
            , Effect.none
            )

        ClickedEndShopping ->
            let
                updated =
                    Items.setAllStuffed model.items
            in
            ( { model | items = updated }
            , Effect.storeAllItems onTaskPortResult updated
            )

        GotError _ ->
            ( model, Effect.none )

        GotStateUpdateTime item timestamp ->
            ( { model | items = Items.setUpdated model.items item.id timestamp }
            , Effect.storeItem onTaskPortResult { item | updated = timestamp }
            )

        GotItemListMsg listMsg ->
            case listMsg of
                Components.Items.List.CollapseClicked catId state ->
                    let
                        updated =
                            if state == Cats.Open then
                                Set.remove catId model.collapsedCats

                            else
                                Set.insert catId model.collapsedCats
                    in
                    ( { model | collapsedCats = updated }
                    , Effect.none
                    )

                Components.Items.List.ItemClicked item state ->
                    let
                        newState =
                            case state of
                                Items.Required ->
                                    Items.InBasket

                                _ ->
                                    Items.Required

                        altered =
                            Items.setState newState item.id model.items
                    in
                    ( { model | items = altered }
                    , Effect.getTime
                        (Dict.get item.id altered
                            |> Maybe.withDefault item
                            |> GotStateUpdateTime
                        )
                    )

                _ ->
                    ( model, Effect.none )


onTaskPortResult : TaskPort.Result res -> Msg
onTaskPortResult res =
    case res of
        Err _ ->
            GotError Nothing

        Ok _ ->
            NoOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    DataUpdate.incoming (DataUpdate.on Error GotCatsAndItems)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ "В магазине"
    , body =
        let
            filteredItems =
                Items.filterByStates
                    model.items
                    [ Items.Required, Items.InBasket ]

            filteredCats =
                filterCategories filteredItems model.categories
        in
        case model.state of
            Initial ->
                [ nothing ]

            Loading ->
                [ nothing ]

            Ready ->
                if Dict.size filteredItems > 0 then
                    [ Components.Items.List.new
                        { items = filteredItems
                        , categories = filteredCats
                        , checkedSates = [ Items.InBasket ]
                        , collapsedCatIds = model.collapsedCats
                        }
                        |> Components.Items.List.withMark
                        |> Components.Items.List.withCounter
                        |> Components.Items.List.view
                        |> Html.map GotItemListMsg
                    , viewEndButton filteredItems
                    ]

                else
                    [ viewEmpty ]
    }


viewEndButton : Dict String Items.Item -> Html Msg
viewEndButton items =
    if Dict.size items > 0 then
        button
            [ class "end-shopping-button"
            , classList [ ( "all-done", Items.isAllDone items ) ]
            , onClick ClickedEndShopping
            , disabled (Items.getInBasketLength items <= 0)
            ]
            [ Components.Counter.view
                (Items.map .state items)
                (Dict.keys items)
                Items.InBasket
            , text "Закончить покупки"
            ]

    else
        nothing


viewEmpty : Html msg
viewEmpty =
    div [ class "empty-page" ] [ h3 [] [ text "Всё куплено!" ] ]


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
