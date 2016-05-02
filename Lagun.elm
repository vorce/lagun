module Lagun (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Task


-- MODEL


type alias Model =
  { specUrl : String, spec : Maybe Spec }


init : String -> ( Model, Effects Action )
init url =
  ( Model url Maybe.Nothing, getJsonSpec url )



-- UPDATE


type Action
  = FetchSpec (Maybe String)
  | RenderSpec (Maybe Spec)
  | TryRequest


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    FetchSpec maybeUrl ->
      ( model
      , getJsonSpec (Maybe.withDefault model.specUrl maybeUrl)
      )

    RenderSpec maybeSpec ->
      ( Model "hello" maybeSpec
      , Effects.none
      )

    TryRequest ->
      ( model, Effects.none )



--RenderSpec maybeSpec ->
-- VIEW


view : Signal.Address Action -> Model -> Html
view address { specUrl, spec } =
  case spec of
    Maybe.Just spec ->
      div
        []
        [ input [ placeholder specUrl, value specUrl ] []
        , button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Fetch & Render spec" ]
        , div
            []
            [ h1 [] [ text spec.info.title ]
            , h2 [] [ text spec.info.description ]
            , p [] [ text spec.info.version ]
            ]
        ]

    Maybe.Nothing ->
      div
        []
        [ p [] [ text "No API specification found. Try fetching one!" ]
        , input [ placeholder specUrl, value specUrl ] []
        , button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Fetch & Render spec" ]
        ]



-- Effects


getJsonSpec : String -> Effects Action
getJsonSpec url =
  Http.get decodeSpec url
    |> Task.toMaybe
    |> Task.map RenderSpec
    |> Effects.task



-- JSON decoders


type alias Spec =
  { info : Info }


type alias Info =
  { title : String, description : String, version : String }


decodeInfo : Json.Decoder Info
decodeInfo =
  Json.at
    [ "info" ]
    <| Json.object3
        Info
        ("title" := Json.string)
        ("description" := Json.string)
        ("version" := Json.string)


decodeSpec : Json.Decoder Spec
decodeSpec =
  Json.object1
    Spec
    ("info"
      := Json.object3
          Info
          ("title" := Json.string)
          ("description" := Json.string)
          ("version" := Json.string)
    )



--title description version
