module Pages.Items exposing (Model, Msg, page)

import Browser.Dom as Dom
import Components.Item.List exposing (Msg(..))
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Draft as Draft
import Db.Items as Items
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (checked)
import Html.Extra exposing (nothing)
import ItemForm
    exposing
        ( FieldMode(..)
        , FieldName(..)
        , ItemField(..)
        , allToView
        , alter
        , alterContent
        , alterMode
        , itemFields
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
        (\(ItemField name _) ->
            case name of
                Name _ ->
                    ItemField name EditMode

                _ ->
                    ItemField name ViewMode
        )
        fields



-- UPDATE


type Msg
    = CollapseClicked Int CollapsedState
    | ItemChecked Items.Item Bool
    | DraftOpened Category String
    | DraftClosed Category
    | DraftFieldUpdated ItemField (Maybe String)
    | DraftFieldFinished ItemField
    | DraftFieldStarted ItemField (Maybe String)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CollapseClicked id state ->
            -- ( model, Effect.updateCatCollapsedState "all" id state )
            ( model, Effect.none )

        ItemChecked item checked ->
            let
                newState =
                    if checked == True then
                        Items.Required

                    else
                        Items.Stuffed
            in
            ( model
              -- , Effect.updateItemState item newState
            , Effect.none
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
                    alter (alterContent data) field model.draftFields
              }
            , Effect.none
            )

        DraftClosed category ->
            ( { model | catWithDraft = Just category.id }, Effect.none )

        DraftFieldFinished field ->
            ( { model
                | draftFields =
                    alter (alterMode ViewMode) field model.draftFields
              }
            , Effect.none
            )

        DraftFieldStarted field data ->
            ( { model
                | draftFields =
                    model.draftFields
                        |> allToView
                        |> alter
                            (alterMode EditMode >> alterContent data)
                            field
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


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ "Всё сразу"
    , body = [ nothing ]

    -- [ Components.Item.List.new
    --     { items = shared.items
    --     , categories = shared.categories
    --     , checkedSates = [ Required, InBasket ]
    --     , collapsedCatIds =
    --         getCollapsesCatsForPage "all" shared.uiState.collapsedCatsMap
    --     }
    --     |> Components.Item.List.withLink
    --     |> Components.Item.List.withSwitch
    --     |> Components.Item.List.withDraft
    --         model.catWithDraft
    --         (Just model.draftFields)
    --         shared.draft
    --     |> Components.Item.List.view
    --     |> Html.map
    --         (\msg ->
    --             case msg of
    --                 Components.Item.List.CollapseClicked clickedItem state ->
    --                     CollapseClicked clickedItem state
    --                 Components.Item.List.ItemChecked checkedItem check ->
    --                     ItemChecked checkedItem check
    --                 Components.Item.List.DraftOpened category fieldId ->
    --                     DraftOpened category fieldId
    --                 Components.Item.List.DraftFieldUpdated field data ->
    --                     DraftFieldUpdated field data
    --                 Components.Item.List.FinishEditing field ->
    --                     DraftFieldFinished field
    --                 Components.Item.List.StartEditing field data ->
    --                     DraftFieldStarted field data
    --                 _ ->
    --                     NoOp
    --         )
    -- ]
    }
