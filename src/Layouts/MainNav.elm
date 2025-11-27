module Layouts.MainNav exposing (Model, Msg, Props, layout, map)

import Dict
import Effect exposing (Effect)
import FeatherIcons as Icons
import Html exposing (Html, a, footer, header, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Html.Extra exposing (nothing)
import Json.Decode
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import Shared.Msg exposing (Msg(..))
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


type alias NavLink =
    { path : Path
    , text : String
    , icon : Icons.Icon
    }


type alias Model =
    { currentRoute : Route ()
    , links : List NavLink
    }


init : Route () -> () -> ( Model, Effect Msg )
init route () =
    ( { currentRoute = route
      , links =
            [ { path = Route.Path.Items
              , text = "Всё"
              , icon = Icons.list
              }

            -- , { path = Route.Path.ToBuy
            --   , text = "Купить"
            --   , icon = Icons.shoppingBag
            --   }
            , { path = Route.Path.InStore
              , text = "В магазине"
              , icon = Icons.shoppingCart
              }
            ]
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
            [ header [ class "nav-header container" ] [ viewNavBar model ]
            , main_ [ class "app-main container" ] content.body
            , footer [ class "nav-footer container" ]
                [ viewNavBar model ]
            ]
        ]
    }


viewNavBar : Model -> Html msg
viewNavBar model =
    nav [ class "main-nav" ]
        [ ul []
            [ li []
                [ span [ class "link" ]
                    [ Icons.cloudOff |> Icons.toHtml [] ]
                ]
            ]
        , ul [ class "group" ] <|
            (List.map
                (viewNavLink model.currentRoute)
             <|
                model.links
            )
        , ul []
            [ viewNavLink model.currentRoute
                (NavLink Route.Path.Settings "" Icons.settings)
            ]
        ]


viewNavLink : Route () -> NavLink -> Html msg
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
                [ page.icon |> Icons.toHtml [] ]

            else
                [ page.icon |> Icons.toHtml []
                , text page.text
                ]
        ]
