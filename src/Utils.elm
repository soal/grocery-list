module Utils exposing (maybeKbd, slugify)

import Keyboard exposing (Key(..))
import Url.Builder


slugify : String -> String
slugify str =
    Url.Builder.relative [ str ] []
        |> String.trim
        |> String.toLower
        |> String.replace " " "-"


maybeKbd : Maybe keyFunc -> Maybe keyFunc -> Maybe (List ( Key, keyFunc ))
maybeKbd onEnter onEsc =
    Maybe.map2
        (\enter esc -> [ ( Enter, enter ), ( Escape, esc ) ])
        onEnter
        onEsc
