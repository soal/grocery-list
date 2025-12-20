module Layouts.MainNav exposing (Model, Msg, Props, layout, map)

import Common exposing (AddMenuItem(..), VisibilityState(..))
import Data.Sync as Sync
import Dict
import Effect exposing (Effect)
import Html exposing (Html, a, header, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Html.Extra exposing (nothing)
import Json.Decode
import Layout exposing (Layout)
import LucideIcons as Icons
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import View exposing (View)
import Views.AddMenu


type alias Props contentMsg =
    { onClickOutside : contentMsg
    , onAddItemClick : contentMsg
    , onAddCatClick : contentMsg
    }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { onClickOutside = fn props.onClickOutside
    , onAddItemClick = fn props.onAddItemClick
    , onAddCatClick = fn props.onAddCatClick
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
        , view = view props shared
        , subscriptions = subscriptions
        }
        |> Layout.withOnUrlChanged UrlChanged



-- MODEL


type alias NavLink =
    { path : Path
    , text : String
    , icon : Html Msg
    }


type alias Model =
    { currentRoute : Route ()
    , addMenuState : VisibilityState
    }


init : Route () -> () -> ( Model, Effect msg )
init route () =
    ( { currentRoute = route
      , addMenuState = Hidden
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UrlChanged { from : Route (), to : Route () }
    | UserClickedAddMenu
    | UserClickedOutside


update : Props contentMsg -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        UrlChanged { to } ->
            ( { model | currentRoute = to }
            , Effect.none
            )

        UserClickedAddMenu ->
            ( { model
                | addMenuState =
                    case model.addMenuState of
                        Show ->
                            Hidden

                        Hidden ->
                            Show
              }
            , Effect.none
            )

        UserClickedOutside ->
            ( { model | addMenuState = Hidden }
              -- , Effect.sendMsg props.onClickOutside
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Props contentMsg
    -> Shared.Model
    ->
        { toContentMsg : Msg -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view props shared { model, content, toContentMsg } =
    { title = shared.titlePrefix ++ content.title
    , body =
        [ Html.node "on-click-outside"
            [ on "clickoutside" <|
                Json.Decode.succeed <|
                    props.onClickOutside
            ]
            [ header [ class "nav-header app-container" ]
                [ nav [ class "main-nav" ]
                    [ ul []
                        [ li []
                            [ Views.AddMenu.view
                                (model.addMenuState == Show)
                                props.onAddItemClick
                                props.onAddCatClick
                                (toContentMsg UserClickedAddMenu)
                                (toContentMsg UserClickedOutside)
                            ]
                        ]
                    , viewPages model.currentRoute
                        |> Html.map toContentMsg
                    , viewSetting model.currentRoute shared.settings.sync.state
                        |> Html.map toContentMsg
                    ]
                ]
            , main_ [ class "app-main app-container" ] content.body
            ]
        ]
    }


viewPages : Route () -> Html Msg
viewPages currentRoute =
    let
        links : List NavLink
        links =
            [ { path = Route.Path.Home_
              , text = "Список"
              , icon = Icons.listIcon []
              }
            , { path = Route.Path.Shopping
              , text = "В магазине"
              , icon = Icons.shoppingCartIcon []
              }
            ]
    in
    ul [] <|
        (List.map
            (viewNavLink currentRoute)
         <|
            links
        )


viewSetting : Route () -> Sync.State -> Html Msg
viewSetting currentRoute syncState =
    ul []
        [ viewNavLink currentRoute
            (NavLink Route.Path.Settings "" (viewSyncIcon syncState))
        ]


viewSyncIcon : Sync.State -> Html Msg
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

        Sync.Paused ->
            span [ class "icon-wrapper" ]
                [ Icons.cloudIcon [] ]

        Sync.SyncError _ ->
            span [ class "icon-wrapper error" ]
                [ Icons.cloudAlertIcon [] ]


viewNavLink : Route () -> NavLink -> Html Msg
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
                [ page.icon ]

            else
                [ page.icon
                , text page.text
                ]
        ]
