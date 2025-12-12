module Layouts.MainNav exposing (Model, Msg, Props, layout, map)

import Data.Settings
import Data.Sync as Sync
import Dict
import Effect exposing (Effect)
import Html exposing (Html, a, header, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on, onClick)
import Html.Extra exposing (nothing)
import Html.Lazy exposing (lazy, lazy3)
import Json.Decode
import Layout exposing (Layout)
import LucideIcons as Icons
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import Svg
import View exposing (View)


type alias Props contentMsg =
    { onClickOutside : contentMsg
    , onAddClick : contentMsg
    }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { onClickOutside = fn props.onClickOutside
    , onAddClick = fn props.onAddClick
    }


layout :
    Props contentMsg
    -> Shared.Model
    -> Route ()
    -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init route
        , update = update props
        , view = view props shared.settings.sync.state
        , subscriptions = subscriptions
        }
        |> Layout.withOnUrlChanged UrlChanged



-- MODEL


type alias NavLink msg =
    { path : Path
    , text : String
    , icon : List (Svg.Attribute msg) -> Html msg
    }


type alias Model =
    { currentRoute : Route ()
    }


init : Route () -> () -> ( Model, Effect msg )
init route () =
    ( { currentRoute = route
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UrlChanged { from : Route (), to : Route () }


update : Props contentMsg -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        UrlChanged { to } ->
            ( { model | currentRoute = to }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Props contentMsg
    -> Sync.State
    ->
        { toContentMsg : Msg -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view props syncState { model, content } =
    { title = content.title
    , body =
        [ Html.node "on-click-outside"
            [ on "clickoutside" <|
                Json.Decode.succeed <|
                    props.onClickOutside
            ]
            [ header [ class "nav-header app-container" ]
                [ lazy3
                    viewNavBar
                    model.currentRoute
                    syncState
                    props.onAddClick
                ]
            , main_ [ class "app-main app-container" ] content.body
            ]
        ]
    }


viewNavBar : Route () -> Sync.State -> msg -> Html msg
viewNavBar currentRoute syncState onAddClick =
    let
        links : List (NavLink msg)
        links =
            [ { path = Route.Path.Home_
              , text = "Список"
              , icon = Icons.listIcon
              }
            , { path = Route.Path.Shopping
              , text = "В магазине"
              , icon = Icons.shoppingCartIcon
              }
            ]
    in
    nav [ class "main-nav" ]
        [ ul []
            [ li []
                [ span [ class "link" ]
                    [ span [ class "icon-wrapper button", onClick onAddClick ]
                        [ Icons.plusIcon [] ]
                    , span [ class "icon-wrapper button" ]
                        [ lazy viewSyncIcon syncState ]
                    ]
                ]
            ]
        , ul [ class "group" ] <|
            (List.map
                (viewNavLink currentRoute)
             <|
                links
            )
        , ul []
            [ viewNavLink currentRoute
                (NavLink Route.Path.Settings "" Icons.settingsIcon)
            ]
        ]


viewSyncIcon : Sync.State -> Html msg
viewSyncIcon syncState =
    case syncState of
        Sync.None ->
            span [ class "icon-wrapper" ]
                [ nothing ]

        Sync.Offline ->
            span [ class "icon-wrapper" ]
                [ Icons.cloudOffIcon [] ]

        Sync.Syncing ->
            span [ class "icon-wrapper working" ]
                [ Icons.cloudDownloadIcon [] ]

        Sync.Synced ->
            span [ class "icon-wrapper success" ]
                [ Icons.cloudCheckIcon [] ]

        Sync.SyncError _ ->
            span [ class "icon-wrapper error" ]
                [ Icons.cloudAlertIcon [] ]


viewNavLink : Route () -> NavLink msg -> Html msg
viewNavLink currentRoute page =
    li []
        [ a
            [ Route.href
                { path = page.path
                , query = Dict.empty
                , hash = Nothing
                }
            , class "link"
            , classList
                [ ( "active"
                  , currentRoute.path == page.path
                  )
                ]
            ]
          <|
            if page.text == "" then
                [ span [ class "icon-wrapper" ] [ page.icon [] ] ]

            else
                [ span [ class "icon-wrapper" ] [ page.icon [] ]
                , text page.text
                ]
        ]
