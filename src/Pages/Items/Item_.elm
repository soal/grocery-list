module Pages.Items.Item_ exposing (Model, Msg, page)

import Db.Items exposing (Item, Quantity(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html, b, div, h1, input, p, small, span, text, textarea)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
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
    Layouts.MainNav {}



-- INIT


type FieldMode
    = ViewMode
    | EditMode


type FieldName
    = Name (Maybe String)
    | QCount (Maybe Int)
    | QUnit (Maybe String)
    | Comment (Maybe String)
    | Symbol (Maybe String)


type ItemField
    = ItemField FieldName FieldMode


type alias Model =
    { slug : String
    , draft : Maybe Item
    , fields : List ItemField
    }


init : Shared.Model -> { item : String } -> () -> ( Model, Effect Msg )
init shared { item } _ =
    let
        slug =
            Maybe.withDefault "" <| percentDecode item

        filtered =
            getItem shared.items slug
    in
    ( { slug = slug
      , draft = filtered
      , fields =
            [ ItemField (Name Nothing) ViewMode
            , ItemField (QCount Nothing) ViewMode
            , ItemField (QUnit Nothing) ViewMode
            , ItemField (Comment Nothing) ViewMode
            , ItemField (Symbol Nothing) ViewMode
            ]
      }
    , Effect.none
    )


makeItemFromFields : List ItemField -> Item -> Item
makeItemFromFields fields item =
    fields
        |> List.foldl
            (\field acc ->
                case field of
                    ItemField (Name data) _ ->
                        case data of
                            Just value ->
                                { acc | name = value }

                            Nothing ->
                                acc

                    ItemField (Comment data) _ ->
                        case data of
                            Just value ->
                                { acc | comment = Just value }

                            Nothing ->
                                acc

                    ItemField (Symbol data) _ ->
                        case data of
                            Just value ->
                                { acc | symbol = Just value }

                            Nothing ->
                                acc

                    _ ->
                        acc
            )
            item



-- UPDATE


type Msg
    = StartEditing ItemField (Maybe String)
    | FinishEditing ItemField
    | UpdateField ItemField (Maybe String)
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        StartEditing field data ->
            ( { model
                | fields =
                    updateFields
                        (updateMode EditMode >> updateData data)
                        model.fields
                        field
              }
            , Effect.none
            )

        FinishEditing field ->
            let
                eixisting =
                    getItem shared.items model.slug
            in
            ( { model
                | fields =
                    updateFields
                        (updateMode ViewMode)
                        model.fields
                        field
              }
            , case eixisting of
                Just item ->
                    Effect.updateItem (makeItemFromFields model.fields item)

                Nothing ->
                    Effect.none
            )

        UpdateField field data ->
            ( { model
                | fields =
                    updateFields
                        (updateData data)
                        model.fields
                        field
              }
            , Effect.none
            )

        NoOp ->
            ( model
            , Effect.none
            )


updateMode : FieldMode -> ItemField -> ItemField
updateMode mode field =
    case field of
        ItemField fieldName _ ->
            ItemField fieldName mode


updateData : Maybe String -> ItemField -> ItemField
updateData data field =
    case ( field, data ) of
        ( ItemField fieldName mode, Just _ ) ->
            case fieldName of
                Name _ ->
                    ItemField (Name data) mode

                QUnit _ ->
                    ItemField (QUnit data) mode

                QCount _ ->
                    ItemField (QCount (Maybe.andThen String.toInt data)) mode

                Comment _ ->
                    ItemField (Comment data) mode

                Symbol _ ->
                    ItemField (Symbol data) mode

        _ ->
            field


updateFields :
    (ItemField -> ItemField)
    -> List ItemField
    -> ItemField
    -> List ItemField
updateFields mapper fields field =
    List.map
        (\existing ->
            if existing == field then
                mapper field

            else
                existing
        )
        fields



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        filtered =
            getItem shared.items model.slug
    in
    { title = shared.titlePrefix ++ model.slug
    , body =
        [ case filtered of
            Just sharedItem ->
                viewItemPage model.fields sharedItem

            _ ->
                div [ class "empty-page" ] [ text "Ничего не найдено" ]
        ]
    }


viewItemPage : List ItemField -> Item -> Html.Html Msg
viewItemPage fields sharedItem =
    div [ class "item-page" ] <| List.map (viewField sharedItem) fields



--     [ h1 []
--         [ viewName fields.name sharedItem.name localItem.name
--         , span []
--             [ text
--                 (Maybe.withDefault ""
--                     sharedItem.symbol
--                 )
--             ]
--         ]
--     , div [ class "quantity" ]
--         (viewQuantity sharedItem.quantity)
--     , div []
--         [ case
--             sharedItem.comment
--           of
--             Just comment ->
--                 text comment
--             Nothing ->
--                 text ""
--         ]
-- ]


viewField : Item -> ItemField -> Html Msg
viewField item field =
    case field of
        ItemField (Name _) _ ->
            viewName field item.name

        ItemField (Comment _) _ ->
            viewComment field item.comment

        _ ->
            div [] []


viewQuantity : Quantity -> List (Html msg)
viewQuantity (Quantity quantity unit) =
    [ b [] [ text (String.fromInt quantity) ]
    , small [] [ text (" " ++ unit) ]
    ]


getItem : Dict String Item -> String -> Maybe Item
getItem items slug =
    items
        |> Dict.filter (\_ v -> slug == v.slug)
        |> Dict.values
        |> List.head


viewName : ItemField -> String -> Html Msg
viewName field sharedName =
    case field of
        ItemField (Name name) EditMode ->
            h1 []
                [ input
                    [ type_ "text"
                    , value (Maybe.withDefault sharedName name)
                    , onInput (Just >> UpdateField field)
                    , onBlur (FinishEditing field)
                    ]
                    []
                ]

        _ ->
            h1
                [ onClick (StartEditing field <| Just sharedName)
                ]
                [ text sharedName ]


viewComment : ItemField -> Maybe String -> Html Msg
viewComment field existing =
    case field of
        ItemField (Comment comment) EditMode ->
            textarea
                [ value (Maybe.withDefault "" comment)
                , onInput (Just >> UpdateField field)
                , onBlur (FinishEditing field)
                ]
                []

        _ ->
            case existing of
                Just comment ->
                    p [ onClick (StartEditing field existing) ] [ text comment ]

                Nothing ->
                    p [] [ text "Добавить комментарий" ]
