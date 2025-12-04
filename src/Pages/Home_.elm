module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom
import Components.Items.List
import DataUpdate
import Db.Categories as Cats
import Db.Items as Items
import Db.Settings exposing (CatsAndItems)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
import Shared exposing (update)
import Task
import TaskPort
import Time
import Types exposing (Draft(..), ItemField(..))
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
        { onClickOutside = GotClickOutside }



-- INIT


type alias Model =
    { draft : Draft
    , collapsedCats : Set Int
    , catWithDraft : Maybe Int
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


emptyItem : Maybe String -> Items.Item
emptyItem maybeId =
    Items.Item
        (Maybe.withDefault "empty" maybeId)
        ""
        (Items.Quantity 1 "штук")
        Nothing
        ""
        Nothing
        Items.Required
        (Time.millisToPosix 0)
        (Time.millisToPosix 0)
        0



-- UPDATE


type Msg
    = NoOp
    | Error (Maybe String)
    | GotCatsAndItems CatsAndItems
    | GotUuid (TaskPort.Result String)
    | GotClickOutside
    | GotItemListMsg Components.Items.List.Msg
    | GotEditUpdateTime Draft Time.Posix
    | GotStateUpdateTime Items.Item Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        Error error ->
            ( { model | error = error }, Effect.none )

        GotUuid uuid ->
            case uuid of
                Ok id ->
                    let
                        fieldId =
                            "item-name-" ++ id
                    in
                    ( { model | draft = New (emptyItem <| Just id) }
                    , Effect.sendCmd <|
                        Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
                    )

                Err _ ->
                    ( model, Effect.none )

        GotClickOutside ->
            endEditing model

        GotCatsAndItems data ->
            let
                sorted =
                    List.map (Cats.sortItemsByFreq data.items) data.categories
            in
            ( { model | categories = sorted, items = data.items }
            , Effect.none
            )

        -- ITEM
        GotItemListMsg itemListMsg ->
            onListMsg model itemListMsg

        GotEditUpdateTime openItem timestamp ->
            let
                altered =
                    case openItem of
                        Empty ->
                            Empty

                        New item ->
                            New { item | updated = timestamp }

                        Existing item ->
                            Existing { item | updated = timestamp }
            in
            ( { model | draft = altered }, Effect.none )

        GotStateUpdateTime item timestamp ->
            ( { model | items = Items.setUpdated model.items item.id timestamp }
            , Effect.storeItem onTaskPortResult { item | updated = timestamp }
            )


onListMsg : Model -> Components.Items.List.Msg -> ( Model, Effect Msg )
onListMsg model itemMsg =
    case itemMsg of
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
            , Effect.sendCmd <|
                Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
            )

        Components.Items.List.InputChanged _ field content ->
            case model.draft of
                Empty ->
                    ( model, Effect.none )

                _ ->
                    let
                        altered =
                            alterDraft model.draft field content
                    in
                    ( { model | draft = altered }
                    , Effect.getTime (GotEditUpdateTime altered)
                    )

        Components.Items.List.DraftOpened category ->
            ( { model | catWithDraft = Just category.id }
            , Effect.requestUuid GotUuid
            )

        Components.Items.List.DraftInputChanged field content ->
            let
                altered =
                    alterDraft model.draft field content
            in
            ( { model | draft = altered }
            , Effect.getTime (GotEditUpdateTime altered)
            )

        Components.Items.List.DeleteClicked itemId ->
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
                , Maybe.withDefault Effect.none <|
                    Maybe.map
                        (Effect.storeCategory onTaskPortResult)
                        category
                ]
            )

        _ ->
            ( model, Effect.none )


alterDraft : Draft -> ItemField -> String -> Draft
alterDraft openItem field content =
    case openItem of
        New item ->
            New (updateItemContent item field content)

        Existing item ->
            Existing (updateItemContent item field content)

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


endEditing : Model -> ( Model, Effect Msg )
endEditing model =
    case model.draft of
        Empty ->
            ( model, Effect.none )

        Existing item ->
            ( { model
                | items = Dict.insert item.id item model.items
                , draft = Empty
              }
            , Effect.storeItem onTaskPortResult item
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
                let
                    alteredCat =
                        model.catWithDraft
                            |> Maybe.andThen
                                (Cats.getByid model.categories)
                            |> Maybe.andThen
                                (\cat ->
                                    Just
                                        { cat
                                            | items =
                                                List.append cat.items [ item.id ]
                                        }
                                )
                in
                ( { model
                    | items = Dict.insert item.id item model.items
                    , draft = Empty
                    , catWithDraft = Nothing
                    , categories =
                        case alteredCat of
                            Just updated ->
                                List.map
                                    (\cat ->
                                        if cat.id == updated.id then
                                            updated

                                        else
                                            cat
                                    )
                                    model.categories

                            Nothing ->
                                model.categories
                  }
                , Effect.batch
                    [ Effect.storeItem onTaskPortResult item
                    , Effect.requestUuid GotUuid
                    , Maybe.withDefault Effect.none
                        (Maybe.map
                            (\category ->
                                Effect.storeCategory
                                    onTaskPortResult
                                    category
                            )
                            alteredCat
                        )
                    ]
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
        [ -- if Dict.size model.items > 0 then
          Components.Items.List.new
            { items = model.items
            , categories = model.categories
            , checkedSates = [ Items.Required, Items.InBasket ]
            , collapsedCatIds = model.collapsedCats
            }
            |> Components.Items.List.withLink
            |> Components.Items.List.withSwitch
            |> Components.Items.List.withDraft
                model.catWithDraft
                model.draft
            |> Components.Items.List.view
            |> Html.map GotItemListMsg
        ]
    }


toggleItemState : Model -> Items.Item -> Items.State -> ( Model, Effect Msg )
toggleItemState model item state =
    let
        newState =
            case state of
                Items.Stuffed ->
                    Items.Required

                _ ->
                    Items.Stuffed

        altered =
            (Items.incFrequency item.id >> Items.setState newState item.id)
                model.items
    in
    ( { model | items = altered }
    , Effect.getTime
        (Dict.get item.id altered
            |> Maybe.withDefault item
            |> GotStateUpdateTime
        )
    )


onTaskPortResult : TaskPort.Result res -> Msg
onTaskPortResult res =
    case res of
        Err _ ->
            Error Nothing

        Ok _ ->
            NoOp
