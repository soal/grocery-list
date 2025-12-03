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
import Html exposing (Html, h3, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import LucideIcons as Icons


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
                    Icons.chevronDownIcon []

                  else
                    Icons.chevronRightIcon []
                ]

        title =
            span
                []
                [ text settings.category.name ]
    in
    h3 []
        [ title
        , viewIf settings.counter
            (viewOptionalCounter settings.items
                settings.category.items
            )
        , chevron
        , viewIf
            (not settings.counter)
            (span
                [ class "button category-edit-button" ]
                [ Icons.editIcon [] ]
            )
        , viewIf
            (not settings.counter)
            (span
                [ class "button category-add-button" ]
                [ Icons.plusIcon [] ]
            )
        ]


viewOptionalCounter :
    Dict String Items.Item
    -> List String
    -> Html msg
viewOptionalCounter items catItemIs =
    Components.Counter.view
        (Dict.map (\_ item -> item.state) items)
        catItemIs
        Items.InBasket
