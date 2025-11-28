module Components.Category.Header exposing
    ( CategoryHeader
    , Msg(..)
    , init
    , new
    , view
    , withCounter
    )

import Components.Counter
import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (Html, h3, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type CategoryHeader
    = Settings
        { category : Cats.Category
        , state : Cats.CollapsedState
        , items : Dict String Items.Item
        , counter : Bool
        }


new :
    { category : Cats.Category
    , items : Dict String Items.Item
    , state : Cats.CollapsedState
    }
    -> CategoryHeader
new props =
    Settings
        { category = props.category
        , state = props.state
        , items = props.items
        , counter = False
        }


withCounter : CategoryHeader -> CategoryHeader
withCounter (Settings settings) =
    Settings { settings | counter = True }


type alias Model =
    {}


init : {} -> Model
init _ =
    {}


type Msg
    = Toggle Int


view : CategoryHeader -> Html Msg
view (Settings settings) =
    let
        chevron =
            span
                [ class "chevron"
                , onClick (Toggle settings.category.id)
                ]
                [ if settings.state == Cats.Open then
                    Icons.chevronDown |> Icons.toHtml []

                  else
                    Icons.chevronRight |> Icons.toHtml []
                ]

        title =
            span
                []
                [ text settings.category.name ]
    in
    h3 []
        (title
            :: viewOptionalCounter settings.items
                settings.category.items
                settings.counter
            ++ [ chevron ]
        )


viewOptionalCounter :
    Dict String Items.Item
    -> List String
    -> Bool
    -> List (Html msg)
viewOptionalCounter items catItemIs counter =
    if counter == True then
        [ Components.Counter.view
            (Dict.map (\_ item -> item.state) items)
            catItemIs
            Items.InBasket
        ]

    else
        []
