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

import Db.Categories exposing (Category, categories)
import Db.Items exposing (Item, items, updateItemState)
import Db.Settings exposing (AppSettings, AppTheme(..), settingsDec)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Json.Decode as Dec
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (DbConfig, DbStatus(..))
import Shared.Msg



-- FLAGS


type alias Flags =
    { settings : AppSettings }


decoder : Dec.Decoder Flags
decoder =
    Dec.map Flags
        (Dec.field "settings" settingsDec)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Dec.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    ( { settings = { theme = Dark }
      , dbConfig =
            { name = "grocery-list"
            , version = 1
            , status = Shared.Model.DbInitial
            }
      , items = items
      , categories = categories
      }
    , Effect.initDb Shared.Msg.DbInitialized
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        Shared.Msg.DbInitialized result ->
            ( { model
                | dbConfig =
                    updateDbStatus
                        model.dbConfig
                        (case result of
                            Ok _ ->
                                Shared.Model.DbReady

                            Err _ ->
                                Shared.Model.DbError
                        )
              }
            , Effect.none
            )

        Shared.Msg.ItemStateUpdated itemId state ->
            ( { model | items = updateItemState model.items itemId state }
            , Effect.none
            )


updateDbStatus : DbConfig -> DbStatus -> DbConfig
updateDbStatus dbConfig status =
    { dbConfig | status = status }



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
