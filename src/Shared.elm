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

import Data.Settings exposing (Sync(..), SyncState(..))
import DataUpdate
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
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
                    { settings | syncState = Data.Settings.Syncing }
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
                                SyncConfig _ ->
                                    Syncing

                                NotConfigured ->
                                    SyncError "Cannot connect"
                    in
                    ( { model
                        | settings =
                            { settings
                                | syncState = state
                                , sync = syncConfig
                            }
                      }
                    , Effect.none
                    )

                Err err ->
                    let
                        error =
                            Data.Settings.parseSyncErr err
                    in
                    ( { model
                        | settings =
                            { settings
                                | syncState = Data.Settings.SyncError error
                                , sync = Data.Settings.NotConfigured
                            }
                      }
                    , Effect.none
                    )

        Shared.Msg.GotSyncStatus state ->
            let
                settings =
                    model.settings
            in
            ( { model | settings = { settings | syncState = state } }
            , Effect.none
            )

        Shared.Msg.GotRefreshSyncState ->
            let
                settings =
                    model.settings
            in
            ( { model
                | settings = { settings | syncState = Data.Settings.None }
              }
            , Effect.none
            )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    DataUpdate.syncState
        (DataUpdate.onSyncState Shared.Msg.Error Shared.Msg.GotSyncStatus)
