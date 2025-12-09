module Pages.Items.Item_ exposing (Model, Msg, page)

import Data.Items as Items
import Effect exposing (Effect)
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Url exposing (percentDecode)
import View exposing (View)


page : Shared.Model -> Route { item : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route.params
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.MainNav
        { onClickOutside = ClickedOutside, onAddClick = NoOp }



-- INIT


type alias Model =
    { slug : String
    , draft : Maybe Items.Item
    , item : Maybe Items.Item
    }


init : Shared.Model -> { item : String } -> () -> ( Model, Effect Msg )
init _ { item } _ =
    let
        slug : String
        slug =
            Maybe.withDefault "" <| percentDecode item
    in
    ( { slug = slug
      , draft = Nothing
      , item = Nothing
      }
    , Effect.none
    )



-- UPDATE


type
    Msg
    -- = UpdateField ItemField (Maybe String)
    = ClickedOutside
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ _ model =
    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = shared.titlePrefix ++ model.slug
    , body =
        [ case model.item of
            Just item ->
                h1 [] [ text item.name ]

            _ ->
                div [ class "empty-page" ] [ text "Ничего не найдено" ]
        ]
    }
