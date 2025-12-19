module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Data.Settings
import Data.Sync as Sync
import DataUpdate
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg exposing (Msg(..))
import TaskPort exposing (JSError(..))



-- FLAGS


type alias Flags =
    Data.Settings.AppSettings


decoder : Json.Decode.Decoder Flags
decoder =
    Data.Settings.decoder



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flags _ =
    let
        settings =
            case flags of
                Ok data ->
                    data

                Err _ ->
                    Data.Settings.defaultSettings
    in
    ( { settings = settings
      , titlePrefix = "Покупки: "
      , error = Nothing
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        Shared.Msg.Error error ->
            ( { model | error = error }, Effect.none )

        Shared.Msg.ImportData imported ->
            ( model
            , Effect.storeDump (\_ -> NoOp) imported
            )

        Shared.Msg.GotInitSyncReq config ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings =
                    { settings
                        | sync = Sync.setState Sync.Syncing model.settings.sync
                    }
              }
            , Effect.initSync Shared.Msg.GotInitSyncRes config
            )

        Shared.Msg.GotInitSyncRes res ->
            let
                settings =
                    model.settings
            in
            case res of
                Ok syncConfig ->
                    let
                        state =
                            case syncConfig of
                                Sync.Options _ ->
                                    Sync.Syncing

                                Sync.NotConfigured ->
                                    Sync.SyncError "Cannot connect"
                    in
                    ( { model
                        | settings =
                            { settings
                                | sync = { config = syncConfig, state = state }
                            }
                      }
                    , Effect.pushRoutePath Route.Path.Home_
                    )

                Err err ->
                    let
                        error =
                            Sync.parseErr err
                    in
                    ( { model
                        | settings =
                            { settings
                                | sync =
                                    { state = Sync.SyncError error
                                    , config = Sync.NotConfigured
                                    }
                            }
                      }
                    , Effect.none
                    )

        Shared.Msg.GotPauseSyncReq ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings =
                    { settings
                        | sync = Sync.setState Sync.Paused model.settings.sync
                    }
              }
            , Effect.pauseSync (\_ -> Shared.Msg.NoOp)
            )

        Shared.Msg.GotResumeSyncReq ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings =
                    { settings
                        | sync = Sync.setState Sync.Syncing model.settings.sync
                    }
              }
            , Effect.resumeSync (\_ -> Shared.Msg.NoOp)
            )

        Shared.Msg.GotSyncStatus state ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings =
                    { settings
                        | sync = Sync.setState state model.settings.sync
                    }
              }
            , Effect.none
            )

        Shared.Msg.GotRefreshSyncState ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings =
                    { settings
                        | sync = Sync.setState Sync.None model.settings.sync
                    }
              }
            , Effect.none
            )

        Shared.Msg.GotThemeChange theme ->
            let
                settings =
                    model.settings
            in
            ( { model | settings = { settings | theme = theme } }
            , Effect.sendCmd (Effect.storeTheme (\_ -> NoOp) theme)
            )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    DataUpdate.syncState
        (DataUpdate.onSyncState Shared.Msg.Error Shared.Msg.GotSyncStatus)
