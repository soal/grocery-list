module Pages.Settings exposing (Model, Msg, page)

import Db.Categories as Cats
import Db.Items as Items
import Db.Settings exposing (CatsAndItems)
import Effect exposing (Effect)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (button, div, h1, h2, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extra exposing (role)
import Html.Events exposing (onClick)
import Json.Encode as JE
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import Task
import TaskPort
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
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
    { imported : Maybe String }


init : () -> ( Model, Effect Msg )
init () =
    ( { imported = Nothing }
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
                            [ ( "version", JE.int shared.dbConfig.version )
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = shared.titlePrefix ++ "Настройки"
    , body =
        [ h1 [] [ text "Настройки" ]
        , div []
            [ h2 [] [ text "Тема" ]
            , div [ class "button-row", role "group" ]
                [ button [] [ text "Как в системе" ]
                , button [ class "secondary" ] [ text "Светлая" ]
                , button [ class "secondary" ] [ text "Тёмная" ]
                ]
            ]
        , div []
            [ h2 [] [ text "Экспорт и импорт" ]
            , button [ onClick ExportRequested ] [ text "Экспорт данных" ]
            , button [ onClick ImportRequested ] [ text "Импорт из файла" ]
            ]
        ]
    }
