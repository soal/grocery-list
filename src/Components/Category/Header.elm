module Components.Category.Header exposing
    ( CategoryHeader
    , Msg(..)
    , init
    , new
    , toggleCategory
    , view
    , withCounter
    )

import Components.Counter
import Db.Categories exposing (Category, CollapsedState(..))
import Db.Items exposing (Item, ItemState(..))
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (Html, h3, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type CategoryHeader
    = Settings
        { category : Category
        , state : CollapsedState
        , items : Dict String Item
        , counter : Bool
        }


new :
    { category : Category
    , items : Dict String Item
    , state : CollapsedState
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
                [ if settings.state == Open then
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


viewOptionalCounter : Dict String Item -> List Int -> Bool -> List (Html msg)
viewOptionalCounter items catItemIs counter =
    if counter == True then
        [ Components.Counter.view
            (Dict.map (\_ item -> item.state) items)
            (List.map String.fromInt catItemIs)
            InBasket
        ]

    else
        []


toggleCategory : List Category -> Int -> CollapsedState -> List Category
toggleCategory categories toggledId state =
    List.map
        (\cat ->
            if cat.id == toggledId then
                { cat | state = state }

            else
                cat
        )
        categories
