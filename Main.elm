module Main (..) where

import Effects exposing (Never)
import Lagun exposing (init, update, view)
import Html
import StartApp
import Task


app : StartApp.App Lagun.Model
app =
  StartApp.start
    { init = init "http://petstore.swagger.io/v2/swagger.json"
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
