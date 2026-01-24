module Main.ListUpdates exposing (update)

import Browser.Dom
import Common exposing (Draft(..), ItemField(..))
import Data.Categories as Cats
import Data.Items as Items
import Dict exposing (Dict)
import Effect exposing (Effect)
import Main.EditItem exposing (endEditAndSave)
import Main.Model exposing (Model)
import Main.Msg exposing (Msg(..))
import Main.Utils exposing (onTaskPortResult)
import Maybe.Extra exposing (values)
import Set exposing (Set)
import Task
import Views.Items.List


update : Model -> Views.Items.List.Msg -> ( Model, Effect Msg )
update model msg =
    case msg of
        Views.Items.List.CollapseClicked catId state ->
            let
                altered : Set Cats.Id
                altered =
                    if state == Cats.Open then
                        Set.remove catId model.collapsedCats

                    else
                        Set.insert catId model.collapsedCats
            in
            ( { model | collapsedCats = altered }
            , Effect.none
            )

        Views.Items.List.ItemChecked item state ->
            toggleItemState model item state

        Views.Items.List.EditStarted item _ fieldId ->
            ( { model | draft = Existing ( item, Items.ValidationOk ) }
            , Effect.sendCmd <|
                Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
            )

        Views.Items.List.InputChanged field content ->
            ( model, Effect.sendMsg (GotInput field content) )

        Views.Items.List.DraftOpened category ->
            ( { model | catWithDraft = Just category.id }
            , Effect.requestUuid GotItemUuid
            )

        Views.Items.List.ItemDeleteClicked itemId ->
            ( model, Effect.sendMsg (GotItemDeleteClick itemId) )

        Views.Items.List.CatTitleClicked category ->
            ( { model | draft = ExistingCat category }
            , Effect.sendCmd <|
                Task.attempt
                    (\_ -> NoOp)
                    (Browser.Dom.focus <| "category-name-" ++ category.id)
            )

        Views.Items.List.CatDeleteClicked catId ->
            case model.draft of
                NewCat _ ->
                    ( { model | draft = Empty }, Effect.none )

                ExistingCat _ ->
                    ( { model
                        | categories = Cats.delete catId model.categories
                        , draft = Empty
                      }
                    , Effect.deleteCategory onTaskPortResult catId
                    )

                _ ->
                    ( model, Effect.none )

        Views.Items.List.EnterPressed ->
            endEditAndSave model True

        Views.Items.List.EscPressed ->
            ( model, Effect.sendMsg GotEscKey )

        Views.Items.List.NewCatSelected itemId maybeOldCat maybeNewCatId ->
            let
                updatedNewCat =
                    Maybe.andThen
                        (\catId ->
                            model.categories
                                |> List.filter (\cat -> cat.id == catId)
                                |> List.head
                                |> Maybe.map (Cats.addItem itemId)
                        )
                        maybeNewCatId

                updatedOldCat =
                    maybeOldCat
                        |> Maybe.map (Cats.removeItem itemId)

                updates =
                    values [ updatedNewCat, updatedOldCat ]

                updatedCats =
                    List.foldl Cats.alterFlipped model.categories updates

                effects =
                    List.map
                        (Effect.storeCategory onTaskPortResult)
                        updates
            in
            ( { model | categories = updatedCats }
            , Effect.batch effects
            )

        _ ->
            ( model, Effect.none )


toggleItemState : Model -> Items.Item -> Items.State -> ( Model, Effect Msg )
toggleItemState model item state =
    let
        altered : Dict Items.Id Items.Item
        altered =
            case state of
                Items.Stuffed ->
                    model.items
                        |> Items.setState Items.Required item.id
                        |> Items.incFrequency item.id

                _ ->
                    Items.setState Items.Stuffed item.id model.items
    in
    ( { model | items = altered }
    , Effect.maybe
        (GotItemStateUpdateTime >> Effect.getTime)
        (Dict.get item.id altered)
    )
