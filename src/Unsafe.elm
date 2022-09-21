module Unsafe exposing (url)

import Url exposing (Url)


url : String -> Url
url text =
    case Url.fromString text of
        Just url_ ->
            url_

        Nothing ->
            unreachable ()


unreachable : () -> a
unreachable () =
    let
        _ =
            (\() -> 0) == (\() -> 0)
    in
    unreachable ()
