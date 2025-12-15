module Data.Sync exposing
    ( Config(..)
    , Form
    , State(..)
    , Sync
    , configDecoder
    , decoder
    , encodeConfig
    , init
    , parseErr
    , pause
    , resume
    , setConfig
    , setState
    , stateDecoder
    )

import Json.Decode as JD
import Json.Encode as JE
import Task
import TaskPort


type alias Sync =
    { config : Config
    , state : State
    }


decoder : JD.Decoder Sync
decoder =
    JD.map2
        Sync
        (JD.field "config" configDecoder)
        (JD.field "state" stateDecoder)


setState : State -> Sync -> Sync
setState state sync =
    { sync | state = state }


setConfig : Config -> Sync -> Sync
setConfig config sync =
    { sync | config = config }


type Config
    = NotConfigured
    | Options
        { room : String
        , url : String
        }


type alias Form =
    { room : String
    , url : String
    }


configDecoder : JD.Decoder Config
configDecoder =
    JD.oneOf
        [ JD.map
            Options
          <|
            JD.map2
                (\room url -> { room = room, url = url })
                (JD.field "room" JD.string)
                (JD.field "url" JD.string)
        , JD.null NotConfigured
        ]


encodeConfig : Config -> JE.Value
encodeConfig settings =
    JE.object
        [ ( "room", JE.string <| .room <| getSyncData settings )
        , ( "url", JE.string <| .url <| getSyncData settings )
        ]


getSyncData : Config -> { room : String, url : String }
getSyncData config =
    case config of
        Options { room, url } ->
            { room = room, url = url }

        NotConfigured ->
            { room = "", url = "" }


type State
    = None
    | Syncing
    | Synced
    | Offline
    | Paused
    | SyncError String


stringToState : Maybe String -> String -> State
stringToState maybeError stateStr =
    case stateStr of
        "none" ->
            None

        "offline" ->
            Offline

        "syncing" ->
            Syncing

        "synced" ->
            Synced

        "paused" ->
            Paused

        "error" ->
            SyncError <| Maybe.withDefault "" maybeError

        _ ->
            None


stateDecoder : JD.Decoder State
stateDecoder =
    JD.oneOf
        [ JD.map (stringToState Nothing) JD.string
        , JD.null None
        , JD.map
            SyncError
            (JD.field "error" <| JD.string)
        ]


init : (Result TaskPort.Error Config -> msg) -> Config -> Cmd msg
init onResult settings =
    let
        call : Config -> TaskPort.Task Config
        call =
            TaskPort.call
                { function = "initSync"
                , valueDecoder = configDecoder
                , argsEncoder = encodeConfig
                }
    in
    Task.attempt onResult <| call settings


parseErr : TaskPort.Error -> String
parseErr err =
    case err of
        TaskPort.InteropError error ->
            TaskPort.interopErrorToString error

        TaskPort.JSError (TaskPort.ErrorObject "Error" { message }) ->
            message

        TaskPort.JSError _ ->
            "General connection error"


pause : (TaskPort.Result Bool -> msg) -> Cmd msg
pause onResult =
    TaskPort.callNoArgs
        { function = "pauseSync"
        , valueDecoder = JD.bool
        }
        |> Task.attempt onResult


resume : (TaskPort.Result Bool -> msg) -> Cmd msg
resume onResult =
    TaskPort.callNoArgs
        { function = "resumeSync"
        , valueDecoder = JD.bool
        }
        |> Task.attempt onResult
