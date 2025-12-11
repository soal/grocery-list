module Views.SyncSettings exposing (view)

import Common exposing (SyncSettingsField(..))
import Html exposing (button, div, form, h2, input, label, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)


view :
    { a | url : String, room : String }
    ->
        { fieldInput : SyncSettingsField -> String -> msg
        , newRoomClick : msg
        , submit : msg
        }
    -> Html.Html msg
view { url, room } { fieldInput, newRoomClick, submit } =
    div []
        [ h2 []
            [ text "Настройки синхронизации" ]
        , div []
            [ label []
                [ text "Адрес сервера"
                , input [ value url, onInput (fieldInput SyncUrl) ] []
                ]
            , label [ class "group" ]
                [ text "Имя комнаты"
                , input [ value room, onInput (fieldInput Room) ] []
                , button [ onClick newRoomClick ] [ text "Создать новую" ]
                ]
            ]
        , button [ onClick submit ] [ text "Подключиться" ]
        ]
