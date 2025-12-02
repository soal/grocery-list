module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom
import Components.Items.List
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
import Types exposing (ItemField(..))
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
        { onClickedOutside = GotClickOutside }



-- INIT


type alias Model =
    { draft : Maybe Items.Item
    , tempItem : Maybe Items.Item
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
      , draft = Just <| emptyItem Nothing
      , tempItem = Nothing
      }
    , Effect.batch
        [ Effect.requestUuid GotUuid
        , Effect.queryAll
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
    | GotEditSave
    | GotDraftSave
    | GotItemListMsg Components.Items.List.Msg
    | GotUpdateTime (Maybe Items.Item) Time.Posix


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
            case ( Result.toMaybe uuid, model.draft ) of
                ( Just id, Just draft ) ->
                    ( { model | draft = Just (Items.setId id draft) }
                    , Effect.none
                    )

                ( _, _ ) ->
                    ( model, Effect.none )

        GotClickOutside ->
            ( model
            , Effect.batch
                [ Effect.sendMsg GotEditSave, Effect.sendMsg GotDraftSave ]
            )

        GotEditSave ->
            endExistingEditing model

        GotDraftSave ->
            endDraft model

        GotCatsAndItems data ->
            let
                sorted =
                    List.map (Cats.sortItemsByFreq data.items) data.categories
            in
            ( { model | categories = sorted, items = data.items }
            , Effect.none
            )

        -- ITEM
        GotItemListMsg itemMsg ->
            onListMsg model itemMsg

        GotUpdateTime maybeItem timeStamp ->
            case maybeItem of
                Just item ->
                    let
                        updated =
                            { item | updated = timeStamp }
                    in
                    ( { model | tempItem = Just updated }, Effect.none )

                Nothing ->
                    ( model, Effect.none )


onListMsg : Model -> Components.Items.List.Msg -> ( Model, Effect Msg )
onListMsg model itemMsg =
    case itemMsg of
        Components.Items.List.CollapseClicked catId state ->
            let
                updated =
                    if state == Cats.Open then
                        Set.remove catId model.collapsedCats

                    else
                        Set.insert catId model.collapsedCats
            in
            ( { model | collapsedCats = updated }
            , Effect.none
            )

        Components.Items.List.ItemChecked item state ->
            toggleItemState model item state

        Components.Items.List.EditStarted item _ fieldId ->
            ( { model | tempItem = Just item }
            , Effect.sendCmd <|
                Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
            )

        Components.Items.List.InputChanged _ field content ->
            let
                updated =
                    updateItemContent model.tempItem field content
            in
            ( { model | tempItem = updated }
            , Effect.getTime (GotUpdateTime updated)
            )

        Components.Items.List.DraftOpened category fieldId ->
            ( { model
                | catWithDraft = Just category.id
              }
            , Effect.sendCmd <|
                Task.attempt (\_ -> NoOp) (Browser.Dom.focus fieldId)
            )

        Components.Items.List.DraftInputChanged field content ->
            ( { model
                | draft =
                    updateItemContent model.draft field content
              }
            , Effect.none
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
                , categories = List.map (Cats.removeItem itemId) model.categories
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


updateItemContent : Maybe Items.Item -> ItemField -> String -> Maybe Items.Item
updateItemContent itemToUpdate field content =
    case ( itemToUpdate, field ) of
        ( Just item, Name ) ->
            Just { item | name = content }

        ( Just item, Comment ) ->
            Just { item | comment = Just content }

        ( Just item, QCount ) ->
            let
                (Items.Quantity _ unit) =
                    item.quantity

                newCount =
                    Maybe.withDefault 0 (String.toFloat content)
            in
            Just { item | quantity = Items.Quantity newCount unit }

        ( Just item, QUnit ) ->
            let
                (Items.Quantity count _) =
                    item.quantity
            in
            Just { item | quantity = Items.Quantity count content }

        ( _, _ ) ->
            itemToUpdate


endExistingEditing : Model -> ( Model, Effect Msg )
endExistingEditing model =
    case model.tempItem of
        Just item ->
            if item.id /= "empty" then
                ( { model
                    | items = Dict.insert item.id item model.items
                    , tempItem = Just (emptyItem Nothing)
                  }
                , Effect.storeItem onTaskPortResult item
                )

            else
                ( model, Effect.none )

        Nothing ->
            ( model, Effect.none )


endDraft : Model -> ( Model, Effect Msg )
endDraft model =
    case model.draft of
        Just item ->
            if String.isEmpty item.name then
                ( { model
                    | catWithDraft = Nothing
                    , draft = Just <| emptyItem <| Just item.id
                  }
                , Effect.none
                )

            else
                let
                    updatedCat =
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
                    , draft = Just (emptyItem Nothing)
                    , catWithDraft = Nothing
                    , categories =
                        case updatedCat of
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
                            updatedCat
                        )
                    ]
                )

        Nothing ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
            |> Components.Items.List.withEditing model.tempItem
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
    in
    ( { model
        | items =
            (Items.incFrequency item.id >> Items.setState newState item.id)
                model.items
      }
    , Effect.storeItem onTaskPortResult { item | state = newState }
    )


onTaskPortResult : TaskPort.Result res -> Msg
onTaskPortResult res =
    case res of
        Err _ ->
            Error Nothing

        Ok _ ->
            NoOp
