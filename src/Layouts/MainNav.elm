module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Dict
import Effect exposing (Effect)
import FeatherIcons as Icons
import Html exposing (Html, a, footer, header, li, main_, nav, text, ul)
import Html.Attributes exposing (class, classList)
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ _ route =
    Layout.new
        { init = init route
        , update = update
        , view = view
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
            , { path = Route.Path.ToBuy
              , text = "Купить"
              , icon = Icons.shoppingBag
              }
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
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
    { toContentMsg :
        Msg
        -> contentMsg
    , content : View contentMsg
    , model : Model
    }
    -> View contentMsg
view { model, content } =
    { title = content.title
    , body =
        [ header [ class "nav-header container" ] [ viewNavBar model ]
        , main_ [ class "app-main container" ] content.body
        , footer [ class "nav-footer container" ]
            [ viewNavBar model ]
        ]
    }


viewNavBar : Model -> Html msg
viewNavBar model =
    nav [ class "main-nav" ]
        [ ul [ class "group" ] <|
            List.map
                (viewNavLink model.currentRoute)
            <|
                model.links
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
