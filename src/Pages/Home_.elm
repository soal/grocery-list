module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom
import Common
    exposing
        ( Draft(..)
        , FormState(..)
        , ItemField(..)
        , SyncSettingsField(..)
        , VisibilityState(..)
        )
import Data.Categories as Cats
import Data.Items as Items
import Data.Sync as Sync
import DataUpdate
import Dict
import Effect exposing (Effect)
import Html exposing (a, button, div, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Layouts
import LucideIcons as Icons
import Main.EditItem exposing (alterDraft, endEditAndSave)
import Main.ListUpdates as ListUpdates
import Main.Model
import Main.Msg exposing (Msg(..))
import Main.Utils exposing (onTaskPortResult)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Set
import Shared
import Task
import View exposing (View)
import Views.Items.Item
import Views.Items.List
import Views.MainActionButton


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
        { onClickOutside = GotClickOutside
        , onAddCatClick = GotCatAddClick
        , onAddItemClick = GotItemAddClick
        }



-- INIT


type alias Model =
    Main.Model.Model


init : () -> ( Model, Effect Msg )
init () =
    ( { collapsedCats = Set.empty
      , catWithDraft = Nothing
      , items = Dict.empty
      , categories = []
      , titlePrefix = "Покупки: "
      , error = Nothing
      , draft = Empty
      }
    , Effect.batch
        [ Effect.queryAll
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
                        Error Nothing
            )
        ]
    )



-- UPDATE


type alias Msg =
    Main.Msg.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        Error error ->
            ( { model | error = error }, Effect.none )

        GotItemUuid uuid_ ->
            case uuid_ of
                Ok uuid ->
                    let
                        fieldId : String
                        fieldId =
                            "item-name-" ++ uuid
                    in
                    ( { model | draft = New ( Items.emptyItem <| Just uuid, Items.ValidationOk ) }
                    , Effect.sendCmd <|
                        Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
                    )

                Err _ ->
                    ( model, Effect.none )

        GotClickOutside ->
            endEditAndSave model False

        GotCatsAndItems data ->
            let
                sorted : List Cats.Category
                sorted =
                    List.map (Cats.sortItemsByFreq data.items) data.categories
            in
            ( { model | categories = sorted, items = data.items }
            , Effect.none
            )

        GotCatAddClick ->
            ( model, Effect.requestUuid GotCatUuid )

        GotItemAddClick ->
            ( { model | catWithDraft = Nothing }
            , Effect.requestUuid GotItemUuid
            )

        GotCatUuid uuid_ ->
            case uuid_ of
                Ok uuid ->
                    ( { model
                        | draft = NewCat (Cats.emptyCategory <| Just uuid)
                      }
                    , Effect.sendCmd <|
                        Task.attempt
                            (\_ -> NoOp)
                            (Browser.Dom.focus <| "category-name-" ++ uuid)
                    )

                Err _ ->
                    ( model, Effect.none )

        -- ITEM
        GotItemListMsg msg_ ->
            ListUpdates.update model msg_

        GotDraftUpdateTime draft timestamp ->
            let
                altered : Draft
                altered =
                    case draft of
                        New ( item, validation ) ->
                            New
                                ( { item | updated = timestamp }
                                , validation
                                )

                        Existing ( item, validation ) ->
                            Existing
                                ( { item | updated = timestamp }
                                , validation
                                )

                        NewCat category ->
                            NewCat { category | updated = timestamp }

                        ExistingCat category ->
                            ExistingCat { category | updated = timestamp }

                        Empty ->
                            Empty
            in
            ( { model | draft = altered }, Effect.none )

        GotItemStateUpdateTime item timestamp ->
            ( { model | items = Items.setUpdated model.items item.id timestamp }
            , Effect.storeItem onTaskPortResult { item | updated = timestamp }
            )

        GotInput field content ->
            if model.draft == Empty then
                ( model, Effect.none )

            else
                let
                    altered : Draft
                    altered =
                        alterDraft model.draft field content
                in
                ( { model | draft = altered }
                , Effect.getTime (GotDraftUpdateTime altered)
                )

        GotEscKey ->
            ( { model | draft = Empty }, Effect.none )

        GotEnterKey ->
            endEditAndSave model True

        GotItemDeleteClick itemId ->
            let
                category : Maybe Cats.Category
                category =
                    model.categories
                        |> List.filter (\c -> List.member itemId c.items)
                        |> List.head
            in
            ( { model
                | items = Dict.remove itemId model.items
                , draft = Empty
                , categories =
                    List.map
                        (Cats.removeItem itemId)
                        model.categories
              }
            , Effect.batch
                [ Effect.deleteItem onTaskPortResult itemId
                , Effect.maybe (Effect.storeCategory onTaskPortResult) category
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    DataUpdate.incoming (DataUpdate.onData Error GotCatsAndItems)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Список"
    , body =
        [ if
            Dict.size model.items
                == 0
                && List.length model.categories
                == 0
                && model.draft
                == Empty
          then
            div [ class "empty-actions group" ]
                [ button [ class "outline", onClick GotItemAddClick ]
                    [ text "Добавить" ]
                , button [ class "outline", onClick GotCatAddClick ]
                    [ text "Добавить категорию" ]
                , if shared.settings.sync.config == Sync.NotConfigured then
                    a
                        [ Route.href
                            { path = Route.Path.Settings
                            , query = Dict.empty
                            , hash = Just "settings-sync-section"
                            }
                        , role "button"
                        , class "outline"
                        ]
                        [ text "Настроить синхронизацию" ]

                  else
                    nothing
                ]

          else
            Views.Items.List.new
                { items = model.items
                , categories = model.categories
                , checkedSates = [ Items.Required, Items.InBasket ]
                , collapsedCatIds = model.collapsedCats
                }
                -- |> Views.Items.List.withLink
                |> Views.Items.List.withCheck
                |> Views.Items.List.withDraft
                    model.catWithDraft
                    model.draft
                |> Views.Items.List.view
                |> Html.map GotItemListMsg
        , case ( model.draft, model.catWithDraft ) of
            ( New ( item, validation ), Nothing ) ->
                Views.Items.Item.new
                    { item = item
                    , validation = validation
                    , checkedSates = []
                    , formState = Form
                    }
                    |> Views.Items.Item.asForm
                        { input = GotInput
                        , delete = GotItemDeleteClick item.id
                        , enter = GotEnterKey
                        , esc = GotEscKey
                        }
                    |> Views.Items.Item.view

            _ ->
                button
                    [ class "add-item-button no-cat "
                    , onClick GotItemAddClick
                    ]
                    [ Icons.plusIcon [] ]
        , Views.MainActionButton.view GotItemAddClick
        ]
    }
