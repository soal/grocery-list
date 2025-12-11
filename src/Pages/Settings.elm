module Pages.Settings exposing (Model, Msg, page)

import Common exposing (VisibilityState(..))
import Data.Categories as Cats
import Data.Items as Items
import Data.Settings exposing (CatsAndItems)
import Effect exposing (Effect)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (button, div, h1, h2, p, text)
import Html.Attributes exposing (class, id)
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick)
import Json.Encode as JE
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
import TaskPort
import View exposing (View)
import Views.SyncSettings


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.MainNav { onClickOutside = NoOp, onAddClick = NoOp }



-- INIT


type alias Model =
    { imported : Maybe String
    , syncSettingsForm : Views.SyncSettings.Model
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    ( { imported = Nothing
      , syncSettingsForm =
            Views.SyncSettings.init
                shared.settings.sync
                (case route.hash of
                    Just "settings-sync-section" ->
                        Show

                    _ ->
                        Hidden
                )
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | ExportRequested
    | ImportRequested
    | ImportFileSelected File
    | ImportFileLoaded String
    | GotExportedData (TaskPort.Result CatsAndItems)
      -- SYNC SETTINGS
    | GotSyncSettingsMsg (Views.SyncSettings.Msg Msg)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        ExportRequested ->
            ( model, Effect.queryAll GotExportedData )

        ImportRequested ->
            ( model
            , Effect.sendCmd <|
                File.Select.file
                    [ "application/json" ]
                    ImportFileSelected
            )

        ImportFileSelected file ->
            ( model
            , Effect.sendCmd <|
                Task.perform ImportFileLoaded (File.toString file)
            )

        ImportFileLoaded content ->
            ( model, Effect.importData content )

        GotExportedData result ->
            case result of
                Ok data ->
                    ( model
                    , Effect.sendCmd <|
                        (JE.object
                            [ ( "version", JE.int shared.settings.version )
                            , ( "categories", JE.list Cats.encode data.categories )
                            , ( "items", JE.dict identity Items.encode data.items )
                            ]
                            |> JE.encode 2
                            |> File.Download.string
                                "grocery-list-backup.json"
                                "application/json"
                        )
                    )

                Err _ ->
                    ( model, Effect.none )

        GotSyncSettingsMsg msg_ ->
            Views.SyncSettings.update
                { msg = msg_
                , model = model.syncSettingsForm
                , toModel = \m -> { model | syncSettingsForm = m }
                , toMsg = GotSyncSettingsMsg
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ "Настройки"
    , body =
        [ h1 [] [ text "Настройки" ]
        , p [ id "settings-theme-section" ]
            [ h2 [] [ text "Тема" ]
            , div [ class "button-row", role "group" ]
                [ button [] [ text "Как в системе" ]
                , button [ class "secondary" ] [ text "Светлая" ]
                , button [ class "secondary" ] [ text "Тёмная" ]
                ]
            ]
        , p [ id "settings-export-section" ]
            [ h2 [] [ text "Экспорт и импорт" ]
            , button [ onClick ExportRequested ] [ text "Экспорт данных" ]
            , button [ onClick ImportRequested ] [ text "Импорт из файла" ]
            ]
        , p [ id "settings-sync-section" ]
            [ h2 [] [ text "Синхронизация" ]
            , Views.SyncSettings.new
                { model = model.syncSettingsForm
                , toMsg = GotSyncSettingsMsg
                , state = shared.settings.syncState
                , sync = shared.settings.sync
                }
                |> Views.SyncSettings.view
            ]
        ]
    }
