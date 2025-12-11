module Views.SyncSettings exposing (Model, Msg, init, new, update, view, withOpen)

import Common exposing (SyncSettingsField(..), VisibilityState(..))
import Data.Settings
import Effect exposing (Effect)
import Html exposing (Html, button, div, form, h2, h3, input, label, span, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import LucideIcons as Icons
import TaskPort


type SyncSettingsForm msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , state : Data.Settings.SyncState
        , sync : Data.Settings.Sync
        }


type Model
    = Model
        { room : String
        , url : String
        , formState : VisibilityState
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , state : Data.Settings.SyncState
    , sync : Data.Settings.Sync
    }
    -> SyncSettingsForm msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , state = props.state
        , sync = props.sync
        }


init : Data.Settings.Sync -> VisibilityState -> Model
init sync visibility =
    case sync of
        Data.Settings.NotConfigured ->
            Model
                { room = ""
                , url = ""
                , formState = visibility
                }

        Data.Settings.SyncConfig { room, url } ->
            Model
                { room = room
                , url = url
                , formState = visibility
                }


withOpen : SyncSettingsForm msg -> SyncSettingsForm msg
withOpen (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    Settings { settings | model = Model { model | formState = Show } }


type Msg msg
    = GotToggleClick
    | GotUrlInput String
    | GotRoomInput String
    | GotNewRoomClick
    | GotSubmit
    | GotRoomUuid (TaskPort.Result String)


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            GotToggleClick ->
                let
                    newState =
                        if model.formState == Hidden then
                            Show

                        else
                            Hidden
                in
                ( Model { model | formState = newState }
                , Effect.none
                )

            GotUrlInput content ->
                ( Model { model | url = content }, Effect.none )

            GotRoomInput content ->
                ( Model { model | room = content }, Effect.none )

            GotSubmit ->
                let
                    syncUrl : String
                    syncUrl =
                        if
                            String.startsWith "ws://" model.url
                                || String.startsWith "wss://" model.url
                        then
                            model.url

                        else
                            "ws://" ++ model.url
                in
                ( Model model
                , Effect.reqInitSync
                    (Data.Settings.SyncConfig
                        { room = model.room
                        , url = syncUrl
                        }
                    )
                )

            GotNewRoomClick ->
                ( Model model, Effect.requestUuid (GotRoomUuid >> props.toMsg) )

            GotRoomUuid uuid_ ->
                case uuid_ of
                    Ok uuid ->
                        ( Model { model | room = uuid }
                        , Effect.none
                        )

                    Err _ ->
                        ( Model model, Effect.none )


view : SyncSettingsForm msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        viewForm_ =
            viewForm
                { room = model.room
                , url = model.url
                , toMsg = settings.toMsg
                }

        viewButton_ =
            viewButton settings.sync settings.toMsg
    in
    case settings.state of
        Data.Settings.None ->
            if model.formState == Show then
                viewForm_

            else
                viewButton_

        Data.Settings.Syncing ->
            viewConnecting

        Data.Settings.SyncError err ->
            if model.formState == Show then
                div []
                    [ text <| "Ошибка: " ++ err
                    , viewForm_
                    ]

            else
                viewButton_

        _ ->
            viewButton_


viewConnecting : Html msg
viewConnecting =
    div [] [ text "Подключаемся к серверу..." ]


viewButton : Data.Settings.Sync -> (Msg msg -> msg) -> Html msg
viewButton sync toMsg =
    if sync == Data.Settings.NotConfigured then
        button [ onClick (GotToggleClick |> toMsg) ]
            [ text "Настроить синхронизацию" ]

    else
        div []
            [ h3 [] [ text "Синхронизация включена" ]
            , button [ onClick (GotToggleClick |> toMsg) ]
                [ text "Изменить настройки" ]
            ]


viewForm :
    { a | room : String, url : String, toMsg : Msg msg -> msg }
    -> Html msg
viewForm { room, url, toMsg } =
    div []
        [ h2 []
            [ text "Настройки синхронизации"
            , span [ class "button", onClick (toMsg GotToggleClick) ] [ Icons.xIcon [] ]
            ]
        , div []
            [ label []
                [ text "Адрес сервера"
                , input [ value url, onInput (GotUrlInput >> toMsg) ] []
                ]
            , label [ class "group" ]
                [ text "Имя комнаты"
                , input [ value room, onInput (GotRoomInput >> toMsg) ] []
                , button [ onClick (GotNewRoomClick |> toMsg) ] [ text "Создать новую" ]
                ]
            ]
        , button [ onClick (GotSubmit |> toMsg) ] [ text "Подключиться" ]
        ]
