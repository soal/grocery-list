module Pages.Items exposing (Model, Msg, page)

import Browser.Dom as Dom
import Components.Item.List exposing (Msg(..))
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (DraftState(..), Item, ItemQuantity(..), ItemState(..))
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (checked)
import ItemForm
    exposing
        ( FieldMode(..)
        , FieldName(..)
        , ItemField(..)
        , itemFields
        , updateData
        , updateFields
        , updateMode
        )
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
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
    { catWithDraft : Maybe Int
    , draftFields : List ItemField
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { catWithDraft = Nothing
      , draftFields = buildFields itemFields
      }
    , Effect.none
    )


buildFields : List ItemField -> List ItemField
buildFields fields =
    List.map
        (\(ItemField name _) -> ItemField name EditMode)
        fields



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemChecked Item Bool
    | DraftOpened Category String
    | DraftClosed Category
    | DraftFieldUpdated ItemField (Maybe String)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            ( model, Effect.updateCatCollapsedState "all" id state )

        ItemChecked item checked ->
            let
                newState =
                    if checked == True then
                        Required

                    else
                        Stuffed
            in
            ( model
            , Effect.updateItemState item newState
            )

        DraftOpened category fieldId ->
            ( { model
                | catWithDraft = Just category.id
                , draftFields = buildFields itemFields
              }
            , Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Dom.focus fieldId)
            )

        DraftFieldUpdated field data ->
            ( { model
                | draftFields =
                    updateFields (updateData data) field model.draftFields
              }
            , Effect.none
            )

        DraftClosed category ->
            ( { model | catWithDraft = Just category.id }, Effect.none )

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
    { title = shared.titlePrefix ++ "Всё сразу"
    , body =
        [ Components.Item.List.new
            { items = shared.items
            , categories = shared.categories
            , checkedSates = [ Required, InBasket ]
            , collapsedCatIds =
                getCollapsesCatsForPage "all" shared.uiState.collapsedCatsMap
            }
            |> Components.Item.List.withLink
            |> Components.Item.List.withSwitch
            |> Components.Item.List.withDraft
                model.catWithDraft
                (Just model.draftFields)
                shared.draft
            |> Components.Item.List.view
            |> Html.map
                (\msg ->
                    case msg of
                        Components.Item.List.CollapseClicked clickedItem state ->
                            CollapseClicked clickedItem state

                        Components.Item.List.ItemChecked checkedItem check ->
                            ItemChecked checkedItem check

                        Components.Item.List.DraftOpened category fieldId ->
                            DraftOpened category fieldId

                        Components.Item.List.DraftFieldUpdated field data ->
                            DraftFieldUpdated field data

                        -- Components.Item.List.StartEditing field data ->
                        --     DraftFieldUpdated field data
                        _ ->
                            NoOp
                )
        ]
    }
