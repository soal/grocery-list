module Main.EditItem exposing (alterDraft, endEditAndSave, endEditingWithError, endItemDraft)

import Common exposing (Draft(..), ItemField(..))
import Data.Categories as Cats
import Data.Items as Items
import Effect exposing (Effect)
import Main.Model exposing (Model)
import Main.Msg exposing (Msg(..))
import Main.Utils exposing (onTaskPortResult)
import Utils exposing (slugify)
import Views.Items.Form


endItemDraft : Model -> Items.Item -> Bool -> ( Model, Effect Msg )
endItemDraft model item addNew =
    let
        alteredCat : Maybe Cats.Category
        alteredCat =
            model.catWithDraft
                |> Maybe.andThen
                    (Cats.getByid model.categories)
                |> Maybe.map
                    (Cats.addItem item.id)

        newItem : Items.Item
        newItem =
            { item | slug = slugify item.name }

        catWithDraft : Maybe Cats.Id
        catWithDraft =
            if addNew then
                model.catWithDraft

            else
                Nothing
    in
    ( { model
        | items = Items.alter model.items newItem
        , draft = Empty
        , catWithDraft = catWithDraft
        , categories =
            alteredCat
                |> Maybe.map (Cats.alter model.categories)
                |> Maybe.withDefault model.categories
      }
    , Effect.batch
        [ Effect.storeItem onTaskPortResult newItem
        , Effect.maybe (Effect.storeCategory onTaskPortResult) alteredCat
        , if addNew then
            Effect.requestUuid GotItemUuid

          else
            Effect.none
        ]
    )


endEditAndSave : Model -> Bool -> ( Model, Effect Msg )
endEditAndSave model addNew =
    case model.draft of
        Empty ->
            ( model, Effect.none )

        Existing ( item, _ ) ->
            let
                newItem : Items.Item
                newItem =
                    { item | slug = slugify item.name }
            in
            case Items.validate newItem model.items of
                Items.ValidationOk ->
                    ( { model
                        | items = Items.alter model.items newItem
                        , draft = Empty
                        , catWithDraft = Nothing
                      }
                    , Effect.storeItem onTaskPortResult newItem
                    )

                Items.ValidationError error ->
                    endEditingWithError model error item

        New ( item, _ ) ->
            if String.isEmpty item.name then
                ( { model
                    | catWithDraft = Nothing
                    , draft = Empty
                  }
                , Effect.none
                )

            else
                case Items.validate item model.items of
                    Items.ValidationOk ->
                        endItemDraft model item addNew

                    Items.ValidationError error ->
                        ( { model
                            | draft =
                                New ( item, Items.ValidationError error )
                          }
                        , Effect.none
                        )

        NewCat cat ->
            if String.isEmpty cat.name then
                ( { model
                    | catWithDraft = Nothing
                    , draft = Empty
                  }
                , Effect.none
                )

            else
                ( { model
                    | draft = Empty
                    , catWithDraft = Nothing
                    , categories = Cats.add model.categories cat
                  }
                , Effect.storeCategory
                    onTaskPortResult
                    cat
                )

        ExistingCat cat ->
            if String.isEmpty cat.name then
                ( { model
                    | catWithDraft = Nothing
                    , draft = Empty
                  }
                , Effect.none
                )

            else
                ( { model
                    | draft = Empty
                    , catWithDraft = Nothing
                    , categories = Cats.alter model.categories cat
                  }
                , Effect.storeCategory
                    onTaskPortResult
                    cat
                )


endEditingWithError : Model -> Items.ItemError -> Items.Item -> ( Model, Effect Msg )
endEditingWithError model error item =
    ( { model | draft = Existing ( item, Items.ValidationError error ) }
    , case error of
        Items.NameAlreadyExist ->
            Effect.sendCmd <|
                Views.Items.Form.focusField Name item.id NoOp

        Items.NameIsEmpty ->
            Effect.sendCmd <|
                Views.Items.Form.focusField Name item.id NoOp

        Items.QuantityIsZero ->
            Effect.sendCmd <|
                Views.Items.Form.focusField QCount item.id NoOp
    )


alterDraft : Draft -> ItemField -> String -> Draft
alterDraft draft field content =
    case draft of
        New ( item, _ ) ->
            New ( updateItemContent item field content, Items.ValidationOk )

        Existing ( item, _ ) ->
            Existing ( updateItemContent item field content, Items.ValidationOk )

        NewCat cat ->
            NewCat { cat | name = content }

        ExistingCat cat ->
            ExistingCat { cat | name = content }

        Empty ->
            Empty


updateItemContent : Items.Item -> ItemField -> String -> Items.Item
updateItemContent item field content =
    case field of
        Name ->
            { item | name = content }

        Comment ->
            { item | comment = Just content }

        QCount ->
            let
                (Items.Quantity _ unit) =
                    item.quantity

                newCount : Float
                newCount =
                    Maybe.withDefault 0 (String.toFloat content)
            in
            { item | quantity = Items.Quantity newCount unit }

        QUnit ->
            let
                (Items.Quantity count _) =
                    item.quantity
            in
            { item | quantity = Items.Quantity count content }
