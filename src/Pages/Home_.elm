module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom
import Components.Items.Item
import Components.Items.List
import DataUpdate
import Db.Categories as Cats
import Db.Items as Items
import Db.Settings exposing (CatsAndItems)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Layouts
import LucideIcons as Icons
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
import Shared exposing (update)
import Task
import TaskPort
import Time
import Types exposing (Draft(..), ItemField(..))
import Utils exposing (slugify)
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
        { onClickOutside = GotClickOutside
        , onAddClick = GotCatAddClick
        }



-- INIT


type alias Model =
    { draft : Draft
    , collapsedCats : Set String
    , catWithDraft : Maybe String
    , items : Dict String Items.Item
    , categories : List Cats.Category
    , titlePrefix : String
    , error : Maybe String
    }


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


type Msg
    = NoOp
    | Error (Maybe String)
    | GotCatsAndItems CatsAndItems
    | GotClickOutside
    | GotItemUuid (TaskPort.Result String)
    | GotCatUuid (TaskPort.Result String)
    | GotItemListMsg Components.Items.List.Msg
    | GotDraftUpdateTime Draft Time.Posix
    | GotItemStateUpdateTime Items.Item Time.Posix
    | GotCatAddClick
      -- ITEM WITHOUT CATEGORY
    | GotItemAddClick
    | GotInput ItemField String
    | GotItemDeleteClick String
    | GotEnterKey
    | GotEscKey


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
                        fieldId =
                            "item-name-" ++ uuid
                    in
                    ( { model | draft = New (Items.emptyItem <| Just uuid) }
                    , Effect.sendCmd <|
                        Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
                    )

                Err _ ->
                    ( model, Effect.none )

        GotClickOutside ->
            endEditAndSave model False

        GotCatsAndItems data ->
            let
                sorted =
                    List.map (Cats.sortItemsByFreq data.items) data.categories
            in
            ( { model | categories = sorted, items = data.items }
            , Effect.none
            )

        GotCatAddClick ->
            ( model
            , Effect.requestUuid GotCatUuid
            )

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
            onListMsg model msg_

        GotDraftUpdateTime draft timestamp ->
            let
                altered =
                    case draft of
                        New item ->
                            New { item | updated = timestamp }

                        Existing item ->
                            Existing { item | updated = timestamp }

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
                category =
                    model.categories
                        |> List.filter (\c -> List.member itemId c.items)
                        |> List.head
            in
            ( { model
                | items = Dict.remove itemId model.items
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


onListMsg : Model -> Components.Items.List.Msg -> ( Model, Effect Msg )
onListMsg model msg =
    case msg of
        Components.Items.List.CollapseClicked catId state ->
            let
                altered =
                    if state == Cats.Open then
                        Set.remove catId model.collapsedCats

                    else
                        Set.insert catId model.collapsedCats
            in
            ( { model | collapsedCats = altered }
            , Effect.none
            )

        Components.Items.List.ItemChecked item state ->
            toggleItemState model item state

        Components.Items.List.EditStarted item _ fieldId ->
            ( { model | draft = Existing item }
            , Effect.batch
                [ Effect.sendCmd <|
                    Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
                , Effect.selectInput (\_ -> NoOp) fieldId
                ]
            )

        Components.Items.List.InputChanged field content ->
            ( model, Effect.sendMsg (GotInput field content) )

        Components.Items.List.DraftOpened category ->
            ( { model | catWithDraft = Just category.id }
            , Effect.requestUuid GotItemUuid
            )

        Components.Items.List.ItemDeleteClicked itemId ->
            ( model, Effect.sendMsg (GotItemDeleteClick itemId) )

        Components.Items.List.CatTitleClicked category ->
            ( { model | draft = ExistingCat category }
            , Effect.sendCmd <|
                Task.attempt
                    (\_ -> NoOp)
                    (Browser.Dom.focus <| "category-name-" ++ category.id)
            )

        Components.Items.List.CatDeleteClicked catId ->
            ( { model | categories = Cats.delete catId model.categories }
            , Effect.deleteCategory onTaskPortResult catId
            )

        Components.Items.List.EnterPressed ->
            endEditAndSave model True

        Components.Items.List.EscPressed ->
            ( model, Effect.sendMsg GotEscKey )

        _ ->
            ( model, Effect.none )


alterDraft : Draft -> ItemField -> String -> Draft
alterDraft draft field content =
    case draft of
        New item ->
            New (updateItemContent item field content)

        Existing item ->
            Existing (updateItemContent item field content)

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

        _ ->
            item


endEditAndSave : Model -> Bool -> ( Model, Effect Msg )
endEditAndSave model addNew =
    case model.draft of
        Empty ->
            ( model, Effect.none )

        Existing item ->
            let
                newItem =
                    { item | slug = slugify item.name }
            in
            ( { model
                | items = Items.alter model.items newItem
                , draft = Empty
                , catWithDraft = Nothing
              }
            , Effect.storeItem onTaskPortResult newItem
            )

        New item ->
            if String.isEmpty item.name then
                ( { model
                    | catWithDraft = Nothing
                    , draft = Empty
                  }
                , Effect.none
                )

            else
                endItemDraft model item addNew

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    DataUpdate.incoming (DataUpdate.on Error GotCatsAndItems)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ "Список"
    , body =
        [ Components.Items.List.new
            { items = model.items
            , categories = model.categories
            , checkedSates = [ Items.Required, Items.InBasket ]
            , collapsedCatIds = model.collapsedCats
            }
            |> Components.Items.List.withLink
            |> Components.Items.List.withCheck
            |> Components.Items.List.withDraft
                model.catWithDraft
                model.draft
            |> Components.Items.List.view
            |> Html.map GotItemListMsg
        , case ( model.draft, model.catWithDraft ) of
            ( New item, Nothing ) ->
                Components.Items.Item.new
                    { item = item
                    , checkedSates = []
                    , open = True
                    }
                    |> Components.Items.Item.asForm
                        { input = GotInput
                        , delete = GotItemDeleteClick item.id
                        , enter = GotEnterKey
                        , esc = GotEscKey
                        }
                    |> Components.Items.Item.view

            ( _, _ ) ->
                nothing
        , button [ class "main-action", onClick GotItemAddClick ]
            [ Icons.plusIcon []
            ]
        ]
    }


toggleItemState : Model -> Items.Item -> Items.State -> ( Model, Effect Msg )
toggleItemState model item state =
    let
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


onTaskPortResult : TaskPort.Result res -> Msg
onTaskPortResult res =
    case res of
        Err _ ->
            Error Nothing

        Ok _ ->
            NoOp


endItemDraft : Model -> Items.Item -> Bool -> ( Model, Effect Msg )
endItemDraft model item addNew =
    let
        alteredCat =
            model.catWithDraft
                |> Maybe.andThen
                    (Cats.getByid model.categories)
                |> Maybe.andThen
                    (Cats.addItem item.id >> Just)

        newItem =
            { item | slug = slugify item.name }

        catWithDraft =
            if addNew == True then
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
        , if addNew == True then
            Effect.requestUuid GotItemUuid

          else
            Effect.none
        ]
    )
