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

import Db.Categories as Cats
import Db.Items as Items
import Db.Settings as AppSettings
import Dict exposing (Dict)
import Effect exposing (Effect)
import Json.Decode exposing (field, map)
import Route exposing (Route)
import Route.Path exposing (toString)
import Set
import Shared.Model exposing (CollapsedCats, DbConfig, DbStatus(..))
import Shared.Msg exposing (Msg(..))
import Time



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
init _ route =
    ( { settings = { theme = AppSettings.Dark }
      , dbConfig =
            { name = "grocery-list"
            , version = 1
            , status = Shared.Model.DbInitial
            }
      , titlePrefix = "Покупки: "
      , error = Nothing
      }
    , Effect.initDb Shared.Msg.DbInitialized
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

        -- Shared.Msg.CatCollapsedStateUpdate pagePath catId state ->
        --     ( { model
        --         | uiState =
        --             { lastRoute = model.uiState.lastRoute
        --             , collapsedCatsMap =
        --                 updateCollapsedCats
        --                     pagePath
        --                     catId
        --                     model.uiState.collapsedCatsMap
        --                     state
        --             }
        --       }
        --     , Effect.none
        --     )
        Shared.Msg.Error error ->
            ( { model | error = error }, Effect.none )

        Shared.Msg.ImportData imported ->
            -- ( { model
            --     | categories = imported.categories
            --     , items = imported.items
            --     , dbConfig =
            --         { name = model.dbConfig.name
            --         , version = imported.version
            --         , status = model.dbConfig.status
            --         }
            --   }
            -- , Effect.storeDump (\_ -> NoOp) imported
            -- )
            ( model, Effect.none )


updateDbStatus : DbConfig -> DbStatus -> DbConfig
updateDbStatus dbConfig status =
    { dbConfig | status = status }



-- endShopping : Dict String Item -> Dict String Item
-- endShopping items =
--     Dict.map
--         (\_ item ->
--             if item.state == InBasket then
--                 { item | state = Stuffed }
--             else
--                 item
--         )
--         items
-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
