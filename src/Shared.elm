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

import Db.Categories exposing (CollapsedState(..), categories)
import Db.Items exposing (Item, ItemState(..), items, updateItem, updateItemState)
import Db.Settings exposing (AppSettings, AppTheme(..), settingsDec)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Json.Decode exposing (field, map)
import Route exposing (Route)
import Route.Path exposing (toString)
import Set
import Shared.Model exposing (CollapsedCats, DbConfig, DbStatus(..))
import Shared.Msg



-- FLAGS


type alias Flags =
    { settings : AppSettings }


decoder : Json.Decode.Decoder Flags
decoder =
    map Flags
        (field "settings" settingsDec)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init _ route =
    ( { settings = { theme = Dark }
      , dbConfig =
            { name = "grocery-list"
            , version = 1
            , status = Shared.Model.DbInitial
            }
      , uiState =
            { lastRoute = toString route.path
            , collapsedCatsMap =
                Dict.fromList
                    [ ( "all", Set.empty )
                    , ( "to-buy", Set.empty )
                    , ( "in-store", Set.empty )
                    ]
            }
      , items = items
      , categories = categories
      , titlePrefix = "Покупки: "
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

        Shared.Msg.CatCollapsedStateUpdate pagePath catId state ->
            ( { model
                | uiState =
                    { lastRoute = model.uiState.lastRoute
                    , collapsedCatsMap =
                        updateCollapsedCats
                            pagePath
                            catId
                            model.uiState.collapsedCatsMap
                            state
                    }
              }
            , Effect.none
            )

        Shared.Msg.EndShopping ->
            ( { model | items = endShopping model.items }, Effect.none )

        Shared.Msg.ItemUpdated item ->
            ( { model | items = updateItem model.items item }
            , Effect.none
            )


updateDbStatus : DbConfig -> DbStatus -> DbConfig
updateDbStatus dbConfig status =
    { dbConfig | status = status }


updateCollapsedCats :
    String
    -> Int
    -> CollapsedCats
    -> CollapsedState
    -> CollapsedCats
updateCollapsedCats pageName catId catsMap state =
    Dict.update pageName
        (Maybe.map
            (\ids ->
                if state == Open then
                    Set.remove catId ids

                else
                    Set.insert catId ids
            )
        )
        catsMap


endShopping : Dict String Item -> Dict String Item
endShopping items =
    Dict.map
        (\_ item ->
            if item.state == InBasket then
                { item | state = Stuffed }

            else
                item
        )
        items



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
