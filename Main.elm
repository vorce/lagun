module Main exposing (..)

import Lagun exposing (init, update)
import View exposing (view)
import Html.App as Html


main : Program { specUrl : String }
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
