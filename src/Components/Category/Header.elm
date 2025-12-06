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
import Components.Items.Item exposing (Msg(..))
import Db.Categories as Cats
import Db.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, div, h4, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (nothing, viewIf)
import LucideIcons as Icons
import Types exposing (Draft(..))


type CategoryHeader
    = Settings
        { category : Cats.Category
        , state : Cats.CollapsedState
        , items : Dict String Items.Item
        , counter : Bool
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
    | TitleClicked Cats.Category
    | DeleteClicked String


view : CategoryHeader -> Html Msg
view (Settings settings) =
    let
        chevron =
            span
                [ class "chevron category-action"
                , onClick (Toggle settings.category.id)
                ]
                [ if settings.state == Cats.Open then
                    Icons.chevronDownIcon []

                  else
                    Icons.chevronRightIcon []
                ]

        deleteButton catId =
            span
                [ class "button delete-button category-action with-click-outside"
                , onClick (DeleteClicked catId)
                ]
                [ Icons.trashIcon [] ]

        staticTitle =
            span [ onClick (TitleClicked settings.category) ]
                [ text settings.category.name ]

        ( title, action ) =
            case settings.catDraft of
                NewCat cat ->
                    if cat.id == settings.category.id then
                        ( span
                            [ class "input-resize-containter"
                            , attribute "data-content" cat.name
                            ]
                            [ input
                                [ type_ "text"
                                , id ("category-name-" ++ cat.id)
                                , class "with-click-outside"
                                , onInput InputChanged
                                , value cat.name
                                ]
                                []
                            ]
                        , deleteButton cat.id
                        )

                    else
                        ( staticTitle, chevron )

                ExistingCat cat ->
                    if cat.id == settings.category.id then
                        ( span
                            [ class "input-resize-containter"
                            , attribute "data-content" cat.name
                            ]
                            [ input
                                [ type_ "text"
                                , id ("category-name-" ++ cat.id)
                                , onInput InputChanged
                                , value cat.name
                                ]
                                []
                            ]
                        , deleteButton cat.id
                        )

                    else
                        ( staticTitle, chevron )

                _ ->
                    ( staticTitle, chevron )
    in
    h4 []
        [ title
        , viewIf settings.counter
            (viewOptionalCounter settings.items settings.category.items)
        , action
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
