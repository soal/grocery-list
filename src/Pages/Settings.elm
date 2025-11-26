module Pages.Settings exposing (Model, Msg, page)

import Effect exposing (Effect)
import File exposing (File)
import File.Select
import Html exposing (button, div, h1, h2, text)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import Task
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        ExportRequested ->
            ( model, Effect.exportData )

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
            [ h2 [] [ text "Тема" ] ]
        , div []
            [ h2 [] [ text "Экспорт и импорт" ]
            , button [ onClick ExportRequested ] [ text "Экспорт данных" ]
            , button [ onClick ImportRequested ] [ text "Импорт из файла" ]
            ]
        ]
    }
