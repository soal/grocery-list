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
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Shared.Model
import Shared.Msg exposing (Msg(..))



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
      -- , Effect.initDb Shared.Msg.DbInitialized
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

        Shared.Msg.GotInitSyncReq settings ->
            ( model, Effect.initSync Shared.Msg.GotInitSyncRes settings )

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
                                    SyncReady

                                NotConfigured ->
                                    SyncError
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

                Err _ ->
                    ( { model
                        | settings =
                            { settings
                                | syncState = Data.Settings.SyncError
                                , sync = Data.Settings.NotConfigured
                            }
                      }
                    , Effect.none
                    )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
