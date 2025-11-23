module Pages.Items.Item_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route { item : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav {}



-- INIT


type ItemState
    = Stuffed
    | BuyNow
    | BuyLater


type alias Image =
    { url : String
    , alt : String
    }


type Quantity
    = Quantity Int String


type alias Item =
    { id : Int
    , name : String
    , img : Maybe Image
    , state : ItemState
    , lastPurchase : Maybe Time.Posix
    , quantity : Maybe Quantity
    }


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Items.Item_"
    , body = [ Html.text "/items/:item" ]
    }
