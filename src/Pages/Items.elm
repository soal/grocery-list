module Pages.Items exposing (Model, Msg, page)

import Components.Category.Body as CategoryBody
import Components.Category.Header exposing (toggleCategory)
import Components.Item.ListElement
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemState(..), Quantity(..))
import Dict exposing (Dict)
import Effect exposing (CatsAndItems, Effect)
import Html exposing (Html, div)
import Html.Attributes exposing (checked, class)
import Html.Keyed
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import TaskPort
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
    { collapsedCatMap : Dict String CollapsedState }


init : () -> ( Model, Effect Msg )
init () =
    ( { collapsedCatMap = Dict.empty }
    , Effect.queryAll GotCatsAndItems
    )



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemChecked Int Bool
    | GotCatsAndItems (TaskPort.Result CatsAndItems)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            -- ( { model
            --     | categories = toggleCategory model.categories id state
            --   }
            -- , Effect.none
            -- )
            ( model, Effect.none )

        ItemChecked id checked ->
            let
                newState =
                    if checked == True then
                        Required

                    else
                        Stuffed
            in
            ( model
            , Effect.updateItemState id newState
            )

        GotCatsAndItems result ->
            case result of
                Ok data ->
                    ( model, Effect.none )

                Err _ ->
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


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Всё сразу"
    , body =
        [ Html.Keyed.node "div" [] <|
            List.map (viewCategory shared.items) shared.categories
        ]
    }


viewCategory :
    Dict String Item
    -> Category
    -> ( String, Html Msg )
viewCategory allItems category =
    ( String.fromInt category.id
    , div [ class "grocery-category" ]
        [ viewCatHeader category allItems
        , CategoryBody.view
            category.state
            (viewItems allItems category)
        ]
    )


viewCatHeader : Category -> Dict String Item -> Html Msg
viewCatHeader category items =
    Components.Category.Header.new { category = category, items = items }
        |> Components.Category.Header.view
        |> Html.map
            (\msg ->
                case msg of
                    Components.Category.Header.Toggle id state ->
                        CollapseClicked id state
            )


getCatItems : ( Dict String Item, Category ) -> List ( String, Item )
getCatItems ( allItems, category ) =
    List.map (\id -> Dict.get (String.fromInt id) allItems) category.items
        |> List.filterMap identity
        |> List.map (\item -> ( String.fromInt item.id, item ))


viewItems : Dict String Item -> Category -> Html Msg
viewItems allItems category =
    ( allItems, category )
        |> getCatItems
        |> List.map (\( id, item ) -> ( id, viewItem item ))
        |> Html.Keyed.node "div" []


viewItem : Item -> Html Msg
viewItem item =
    Components.Item.ListElement.new
        { item = item, checkedSates = [ Required, InBasket ] }
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
