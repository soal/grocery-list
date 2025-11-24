module Pages.Items exposing (Model, Msg, page)

import Components.Item.List
import Db.Categories exposing (CollapsedState(..))
import Db.Items exposing (ItemState(..), Quantity(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (checked)
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
    Layouts.MainNav {}



-- INIT


type alias Model =
    { collapsedCatMap : Dict String CollapsedState }


init : () -> ( Model, Effect Msg )
init () =
    ( { collapsedCatMap = Dict.fromList [ ( "1", Debug.log "ON INIT" Collapsed ) ] }
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
            ( model, Effect.updateCatCollapsedState "all" id state )

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
    { title = "Всё сразу"
    , body =
        [ Components.Item.List.new
            { items = shared.items
            , categories = shared.categories
            , checkedSates = [ Required, InBasket ]
            , collapsedCatIds =
                getCollapsesCatsForPage "all" shared.uiState.collapsedCatsMap
            , pageName = "all"
            }
            |> Components.Item.List.withLink
            |> Components.Item.List.view
            |> Html.map
                (\msg ->
                    case msg of
                        Components.Item.List.CollapseClicked id state ->
                            CollapseClicked id state

                        Components.Item.List.ItemChecked id check ->
                            ItemChecked id check

                        _ ->
                            NoOp
                )
        ]
    }
