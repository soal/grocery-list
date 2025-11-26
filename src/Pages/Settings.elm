module Pages.Settings exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (button, div, h1, h2, input, text)
import Html.Attributes exposing (type_)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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
view shared _ =
    { title = shared.titlePrefix ++ "Настройки"
    , body =
        [ h1 [] [ text "Настройки" ]
        , div []
            [ h2 [] [ text "Тема" ] ]
        , div []
            [ h2 [] [ text "Экспорт и импорт" ]
            , button [] [ text "Экспорт данных" ]
            , input [ type_ "file" ] [ text "Импорт из файла" ]
            ]
        ]
    }
