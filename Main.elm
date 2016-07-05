module Main exposing (..)

import Lagun exposing (init, update)
import View exposing (view)
import Html.App as Html


main : Program Never
main =
    Html.program
        { init = init "http://petstore.swagger.io/v2/swagger.json"
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
