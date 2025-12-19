module Views.SyncSettings exposing (Model, Msg, init, new, update, view, withOpen)

import Common exposing (SyncSettingsField(..), VisibilityState(..))
import Data.Sync as Sync
import Effect exposing (Effect)
import Html exposing (Html, button, div, form, h2, h3, input, label, span, text)
import Html.Attributes exposing (class, disabled, for, name, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import LucideIcons as Icons
import TaskPort


type SyncForm msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , state : Sync.State
        , config : Sync.Config
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
    , state : Sync.State
    , config : Sync.Config
    }
    -> SyncForm msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , state = props.state
        , config = props.config
        }


init : Sync.Config -> VisibilityState -> Model
init syncConfig visibility =
    case syncConfig of
        Sync.NotConfigured ->
            Model
                { room = ""
                , url = ""
                , formState = visibility
                }

        Sync.Options { room, url } ->
            Model
                { room = room
                , url = url
                , formState = visibility
                }


withOpen : SyncForm msg -> SyncForm msg
withOpen (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    Settings { settings | model = Model { model | formState = Show } }


type Msg msg
    = UserClickedToggle
    | UserInputUrl String
    | UserInputRoom String
    | UserClickedNewRoom
    | UserClickedPause
    | UserClickedResume
    | UserClickedSubmit
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
            UserClickedToggle ->
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

            UserInputUrl content ->
                ( Model { model | url = content }, Effect.none )

            UserInputRoom content ->
                ( Model { model | room = content }, Effect.none )

            UserClickedSubmit ->
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
                    (Sync.Options
                        { room = model.room
                        , url = syncUrl
                        }
                    )
                )

            UserClickedNewRoom ->
                ( Model model, Effect.requestUuid (GotRoomUuid >> props.toMsg) )

            UserClickedPause ->
                ( Model model, Effect.reqPauseSync )

            UserClickedResume ->
                ( Model model, Effect.reqResumeSync )

            GotRoomUuid uuid_ ->
                case uuid_ of
                    Ok uuid ->
                        ( Model { model | room = uuid }
                        , Effect.none
                        )

                    Err _ ->
                        ( Model model, Effect.none )


view : SyncForm msg -> Html msg
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
            viewButton settings.config settings.toMsg
    in
    if model.formState == Show then
        case settings.state of
            Sync.Syncing ->
                div []
                    [ viewConnecting
                    , button [ onClick (settings.toMsg UserClickedPause) ] [ text "Поставить на паузу" ]
                    ]

            Sync.SyncError err ->
                div [] [ text <| "Ошибка: " ++ err, viewForm_ ]

            Sync.Synced ->
                div []
                    [ viewForm_
                    , button [ onClick (settings.toMsg UserClickedPause) ] [ text "Поставить на паузу" ]
                    ]

            Sync.Paused ->
                div []
                    [ viewForm_
                    , button [ onClick (settings.toMsg UserClickedResume) ] [ text "Включить" ]
                    ]

            Sync.None ->
                viewForm_

            Sync.Offline ->
                viewForm_

    else
        viewButton_


viewConnecting : Html msg
viewConnecting =
    div [] [ text "Подключаемся к серверу..." ]


viewButton : Sync.Config -> (Msg msg -> msg) -> Html msg
viewButton syncConfig toMsg =
    if syncConfig == Sync.NotConfigured then
        button [ onClick (UserClickedToggle |> toMsg) ]
            [ text "Настроить синхронизацию" ]

    else
        div []
            [ h3 [] [ text "Синхронизация включена" ]
            , button [ onClick (UserClickedToggle |> toMsg) ]
                [ text "Изменить настройки" ]
            ]


viewForm :
    { a | room : String, url : String, toMsg : Msg msg -> msg }
    -> Html msg
viewForm { room, url, toMsg } =
    form [ class "sync-settings-form" ]
        [ h3 []
            [ text "Настройки синхронизации"
            ]
        , span [ onClick (toMsg UserClickedToggle) ] [ Icons.xIcon [] ]
        , label [ for "url" ] [ text "Адрес сервера" ]
        , input
            [ type_ "url"
            , value url
            , onInput (UserInputUrl >> toMsg)
            , name "url"
            ]
            []
        , label [ for "room" ] [ text "Код комнаты" ]
        , input
            [ type_ "text"
            , value room
            , onInput (UserInputRoom >> toMsg)
            , name "room"
            ]
            []
        -- , button [ onClick (UserClickedNewRoom |> toMsg) ]
        --     [ text "Создать" ]
        , button
            [ class "large"
            , disabled (isDisabled room url)
            , onClick (UserClickedSubmit |> toMsg)
            ]
            [ text "Подключиться" ]
        ]


isDisabled room url =
    String.isEmpty room || String.isEmpty url
