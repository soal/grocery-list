module Components.Category.Header exposing
    ( CategoryHeader
    , Msg(..)
    , init
    , new
    , view
    , withCounter
    , withDraft
    )

import Components.Counter
import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, h4, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import LucideIcons as Icons
import Types exposing (Draft(..))


type CategoryHeader
    = Settings
        { category : Cats.Category
        , state : Cats.CollapsedState
        , items : Dict String Items.Item
        , counter : Bool

        -- , catDraft : CategoryDraft
        , catDraft : Draft
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
        , catDraft = Empty
        }


withCounter : CategoryHeader -> CategoryHeader
withCounter (Settings settings) =
    Settings { settings | counter = True }


withDraft : Draft -> CategoryHeader -> CategoryHeader
withDraft catDraft (Settings settings) =
    Settings { settings | catDraft = catDraft }


type alias Model =
    {}


init : {} -> Model
init _ =
    {}


type Msg
    = Toggle String
    | InputChanged String
    | TitleClicked String


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

        staticTitle catId =
            span [ onClick (TitleClicked catId) ]
                [ text settings.category.name ]

        title =
            case settings.catDraft of
                NewCat cat ->
                    if cat.id == settings.category.id then
                        input
                            [ type_ "text"
                            , id ("category-name-" ++ cat.id)
                            , class "with-click-outside"
                            , onInput InputChanged
                            ]
                            []

                    else
                        staticTitle cat.id

                ExistingCat cat ->
                    if cat.id == settings.category.id then
                        input
                            [ type_ "text"
                            , id ("category-name-" ++ cat.id)
                            , onInput InputChanged
                            ]
                            []

                    else
                        staticTitle cat.id

                _ ->
                    span [] [ text settings.category.name ]
    in
    h4 []
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

        -- , viewIf
        --     (not settings.counter)
        --     (span
        --         [ class "button category-add-button" ]
        --         [ Icons.plusIcon [] ]
        --     )
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
