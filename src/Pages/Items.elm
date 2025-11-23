module Pages.Items exposing (Model, Msg, page)

import Components.Category.Body as CategoryBody
import Components.Category.Header exposing (toggleCategory)
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..), categories)
import Db.Items exposing (Item, Quantity(..), items)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
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


type alias Model =
    { categories : List Category
    , items : Dict Int Item
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { categories = categories
      , items = items
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemChecked Int Bool
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            ( { model
                | categories = toggleCategory model.categories id state
              }
            , Effect.none
            )

        ItemChecked id checked ->
            ( model, Effect.none )

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
    { title = "Всё сразу"
    , body =
        [ Html.Keyed.node "div" [] <|
            List.map (viewCategory model.items) model.categories
        ]
    }


viewCategory :
    Dict Int Item
    -> Category
    -> ( String, Html Msg )
viewCategory allItems category =
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ viewCatHeader category
        , CategoryBody.view
            category.state
            (viewItems allItems category)
        ]
    )


viewCatHeader : Category -> Html Msg
viewCatHeader category =
    Components.Category.Header.new { category = category }
        |> Components.Category.Header.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Category.Header.Toggle id state ->
                        CollapseClicked id state
            )


getCatItems : ( Dict Int Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get id allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( String.fromInt item.id, item ))


viewItems : Dict Int Item -> Category -> Html Msg
viewItems allItems category =
    ( allItems, category )
        |> getCatItems
        |> List.map (\( id, item ) -> ( id, viewItem item ))
        |> Html.Keyed.node "div" []


viewItem : Item -> Html Msg
viewItem item =
    Components.Item.ListElement.new { item = item }
        |> Components.Item.ListElement.withLink
        |> Components.Item.ListElement.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Item.ListElement.ItemChecked id check ->
                        ItemChecked id check

                    _ ->
                        NoOp
            )
