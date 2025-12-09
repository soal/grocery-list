module Views.Category.Header exposing
    ( CategoryHeader
    , new
    , view
    , withCounter
    , withDraft
    )

import Views.Counter
import Data.Categories as Cats
import Data.Items as Items
import Dict exposing (Dict)
import Html exposing (Html, h4, input, span, text)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Html.Attributes.Extra exposing (attributeMaybe)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import Keyboard.Events as Keyboard
import LucideIcons as Icons
import Types exposing (Draft(..))
import Utils exposing (maybeKbd)


type CategoryHeader msg
    = Settings
        { category : Cats.Category
        , state : Cats.CollapsedState
        , items : Dict String Items.Item
        , counter : Bool
        , catDraft : Draft
        , on : Handlers msg
        }


type alias Handlers msg =
    { toggle : msg
    , input : Maybe (String -> msg)
    , click : Maybe msg
    , delete : Maybe msg
    , enter : Maybe msg
    , esc : Maybe msg
    }


defaultHandlers : { a | toggle : msg } -> Handlers msg
defaultHandlers handlers =
    { toggle = handlers.toggle
    , input = Nothing
    , click = Nothing
    , delete = Nothing
    , enter = Nothing
    , esc = Nothing
    }


new :
    { category : Cats.Category
    , items : Dict String Items.Item
    , state : Cats.CollapsedState
    , on : { toggle : msg }
    }
    -> CategoryHeader msg
new props =
    Settings
        { category = props.category
        , state = props.state
        , items = props.items
        , counter = False
        , catDraft = Empty
        , on = defaultHandlers props.on
        }


withCounter : CategoryHeader msg -> CategoryHeader msg
withCounter (Settings settings) =
    Settings { settings | counter = True }


withDraft :
    Draft
    ->
        { input : String -> msg
        , click : msg
        , delete : msg
        , enter : msg
        , esc : msg
        }
    -> CategoryHeader msg
    -> CategoryHeader msg
withDraft catDraft handlers (Settings settings) =
    let
        on : Handlers msg
        on =
            settings.on
    in
    Settings
        { settings
            | catDraft = catDraft
            , on =
                { on
                    | input = Just handlers.input
                    , click = Just handlers.click
                    , delete = Just handlers.delete
                    , enter = Just handlers.enter
                    , esc = Just handlers.esc
                }
        }


view : CategoryHeader msg -> Html msg
view (Settings ({ on } as settings)) =
    let
        chevron : Html msg
        chevron =
            span
                [ class "chevron category-action button"
                , onClick on.toggle
                ]
                [ if settings.state == Cats.Open then
                    Icons.chevronDownIcon []

                  else
                    Icons.chevronRightIcon []
                ]

        deleteButton : Html msg
        deleteButton =
            span
                [ class "delete-button category-action button with-click-outside"
                , attributeMaybe onClick on.delete
                ]
                [ Icons.trashIcon [] ]

        staticTitle : Html msg
        staticTitle =
            span [ attributeMaybe onClick on.click ]
                [ text settings.category.name ]

        ( title, action ) =
            case settings.catDraft of
                NewCat cat ->
                    if cat.id == settings.category.id then
                        ( span
                            [ class "input-resize-container"
                            , attribute "data-content" cat.name
                            ]
                            [ input
                                [ type_ "text"
                                , id ("category-name-" ++ cat.id)
                                , class "with-click-outside"
                                , attributeMaybe onInput on.input
                                , value cat.name
                                , attributeMaybe
                                    (Keyboard.on Keyboard.Keydown)
                                    (maybeKbd on.enter on.esc)
                                ]
                                []
                            ]
                        , deleteButton
                        )

                    else
                        ( staticTitle, chevron )

                ExistingCat cat ->
                    if cat.id == settings.category.id then
                        ( span
                            [ class "input-resize-container"
                            , attribute "data-content" cat.name
                            ]
                            [ input
                                [ type_ "text"
                                , id ("category-name-" ++ cat.id)
                                , attributeMaybe onInput on.input
                                , value cat.name
                                , attributeMaybe
                                    (Keyboard.on Keyboard.Keydown)
                                    (maybeKbd on.enter on.esc)
                                ]
                                []
                            ]
                        , deleteButton
                        )

                    else
                        ( staticTitle, chevron )

                Empty ->
                    ( staticTitle, chevron )

                New _ ->
                    ( staticTitle, chevron )

                Existing _ ->
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
    Views.Counter.view
        (Dict.map (\_ item -> item.state) items)
        catItemIs
        Items.InBasket
