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
import Db.Items exposing (ItemMarkedAs(..))
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (Html, h3, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type CategoryHeader
    = Settings
        { itemStates : Maybe (Dict Int ItemMarkedAs)
        , category : Category
        , counter : Bool
        }


new : { category : Category } -> CategoryHeader
new props =
    Settings
        { itemStates = Nothing
        , category = props.category
        , counter = False
        }


withCounter : Dict Int ItemMarkedAs -> CategoryHeader -> CategoryHeader
withCounter itemStates (Settings settings) =
    Settings { settings | counter = True, itemStates = Just itemStates }


type alias Model =
    { collapsed : CollapsedState }


init : { collapsed : Maybe CollapsedState } -> Model
init props =
    { collapsed = Maybe.withDefault Open props.collapsed
    }


type Msg
    = Toggle Int CollapsedState


view : CategoryHeader -> Html Msg
view (Settings settings) =
    let
        chevron =
            span
                [ class "chevron"
                , onClick
                    (Toggle settings.category.id <|
                        if settings.category.state == Open then
                            Collapsed

                        else
                            Open
                    )
                ]
                [ if settings.category.state == Open then
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
            :: viewOptionalCounter settings.itemStates
                settings.category.items
                settings.counter
            ++ [ chevron ]
        )


viewOptionalCounter :
    Maybe (Dict Int ItemMarkedAs)
    -> List Int
    -> Bool
    -> List (Html msg)
viewOptionalCounter itemStates catItems counter =
    case ( counter, itemStates ) of
        ( True, Just states ) ->
            [ Components.Counter.view states catItems InBasket ]

        ( _, _ ) ->
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
