module Layouts.MainNav exposing (Model, Msg, Props, layout, map)

import Dict
import Effect exposing (Effect)
import Html exposing (Html, a, footer, header, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Html.Lazy exposing (lazy)
import Json.Decode
import Layout exposing (Layout)
import LucideIcons as Icons
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import Svg
import View exposing (View)


type alias Props contentMsg =
    { onClickedOutside : contentMsg
    }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { onClickedOutside = fn props.onClickedOutside
    }


layout :
    Props contentMsg
    -> Shared.Model
    -> Route ()
    -> Layout () Model Msg contentMsg
layout props _ route =
    Layout.new
        { init = init route
        , update = update props
        , view = view props
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



-- | ClickedOutside


update : Props contentMsg -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        UrlChanged { to } ->
            ( { model | currentRoute = to }
            , Effect.none
            )



-- ClickedOutside ->
--     ( model, Task.perform props.onClickedOutside Time.now )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Props contentMsg
    ->
        { toContentMsg :
            Msg
            -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view props { model, content } =
    { title = content.title
    , body =
        [ Html.node "on-click-outside"
            [ on "clickoutside" <|
                Json.Decode.succeed <|
                    props.onClickedOutside
            ]
            [ header [ class "nav-header container" ]
                [ lazy viewNavBar model.currentRoute ]
            , main_ [ class "app-main container" ] content.body
            , footer [ class "nav-footer container" ]
                [ lazy viewNavBar model.currentRoute ]
            ]
        ]
    }


viewNavBar : Route () -> Html msg
viewNavBar currentRoute =
    let
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
                    [ span [ class "icon-wrapper" ] [ Icons.cloudOffIcon [] ] ]
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
