module Components.Category.Body exposing (view)

import Db.Categories exposing (CollapsedState(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList)


view : CollapsedState -> Html msg -> Html msg
view state content =
    div
        [ class "category-body"
        , classList [ ( "collapsed", state == Collapsed ) ]
        ]
        [ content ]
