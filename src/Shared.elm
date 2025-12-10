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

import Data.Settings as AppSettings
import Effect exposing (Effect)
import Json.Decode exposing (field, map)
import Route exposing (Route)
import Shared.Model exposing (DbConfig, DbStatus)
import Shared.Msg exposing (Msg(..))



-- FLAGS


type alias Flags =
    { settings : AppSettings.AppSettings }


decoder : Json.Decode.Decoder Flags
decoder =
    map Flags
        (field "settings" AppSettings.decoder)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init _ _ =
    ( { settings = { theme = AppSettings.Dark }
      , dbConfig =
            { name = "grocery-list"
            , version = 1
            , status = Shared.Model.DbInitial
            }
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

        Shared.Msg.DbInitialized result ->
            let
                res : DbStatus
                res =
                    case result of
                        Ok _ ->
                            Shared.Model.DbReady

                        Err _ ->
                            Shared.Model.DbError
            in
            ( { model | dbConfig = updateDbStatus model.dbConfig res }
              -- , if res == Shared.Model.DbReady then
              --     Effect.queryAll
              --         (\loaded ->
              --             case loaded of
              --                 Ok data ->
              --                     Shared.Msg.LoadInitial data
              --                 Err _ ->
              --                     -- err
              --                     --     |> Json.Decode.errorToString
              --                     --     |> Just
              --                     --     |> Shared.Msg.Error
              --                     Shared.Msg.Error Nothing
              --         )
              --   else
            , Effect.none
            )

        Shared.Msg.Error error ->
            ( { model | error = error }, Effect.none )

        Shared.Msg.ImportData imported ->
            ( model
            , Effect.storeDump (\_ -> NoOp) imported
            )


updateDbStatus : DbConfig -> DbStatus -> DbConfig
updateDbStatus dbConfig status =
    { dbConfig | status = status }


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
