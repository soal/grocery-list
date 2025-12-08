module Components.Category.Header exposing
    ( CategoryHeader
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
import Html.Attributes.Extra exposing (attributeMaybe)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import LucideIcons as Icons
import Types exposing (Draft(..))


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
    }


defaultHandlers : { a | toggle : msg } -> Handlers msg
defaultHandlers handlers =
    { toggle = handlers.toggle
    , input = Nothing
    , click = Nothing
    , delete = Nothing
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
        }
    -> CategoryHeader msg
    -> CategoryHeader msg
withDraft catDraft handlers (Settings settings) =
    let
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
                    , toggle = on.toggle
                }
        }


view : CategoryHeader msg -> Html msg
view (Settings ({ on } as settings)) =
    let
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

        deleteButton =
            span
                [ class "delete-button category-action button with-click-outside"
                , attributeMaybe onClick on.delete
                ]
                [ Icons.trashIcon [] ]

        staticTitle =
            span [ attributeMaybe onClick on.click ]
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
                                , attributeMaybe onInput on.input
                                , value cat.name
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
                            [ class "input-resize-containter"
                            , attribute "data-content" cat.name
                            ]
                            [ input
                                [ type_ "text"
                                , id ("category-name-" ++ cat.id)
                                , attributeMaybe onInput on.input
                                , value cat.name
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
    Components.Counter.view
        (Dict.map (\_ item -> item.state) items)
        catItemIs
        Items.InBasket
