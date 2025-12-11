module Views.SyncSettings exposing (Model, Msg, init, new, update, view)

import Common exposing (SyncSettingsField(..), VisibilityState(..))
import Data.Settings
import Effect exposing (Effect)
import Html exposing (Html, button, div, form, h2, input, label, span, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra exposing (nothing)
import LucideIcons as Icons


type SyncSettingsForm msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , state : Data.Settings.SyncState
        }



-- type alias Props =
--     { syncState : Data.Settings.SyncState
--     }


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
    }
    -> SyncSettingsForm msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , state = props.state
        }


init : Model
init =
    Model
        { room = ""
        , url = ""
        , formState = Hidden
        }


type Msg msg
    = GotToggleClick
    | GotUrlInput String
    | GotRoomInput String
    | GotNewRoomClick
    | GotSubmit


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
                    ( newState, room, url ) =
                        if model.formState == Hidden then
                            ( Show, model.room, model.url )

                        else
                            ( Hidden, "", "" )
                in
                ( Model { model | formState = newState, room = room, url = url }
                , Effect.refreshSyncState
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

            _ ->
                ( Model model
                , Effect.none
                )


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
    in
    case settings.state of
        Data.Settings.None ->
            if model.formState == Show then
                viewForm_

            else
                viewButton settings.toMsg

        Data.Settings.Syncing ->
            viewConnecting

        Data.Settings.SyncError err ->
            if model.formState == Show then
                div []
                    [ text <| "Ошибка: " ++ err
                    , viewForm_
                    ]

            else
                viewButton settings.toMsg

        _ ->
            nothing


viewConnecting =
    div [] [ text "Подключаемся к серверу..." ]


viewButton : (Msg msg -> msg) -> Html msg
viewButton toMsg =
    button [ onClick (GotToggleClick |> toMsg) ]
        [ text "Настроить синхронизацию" ]


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
